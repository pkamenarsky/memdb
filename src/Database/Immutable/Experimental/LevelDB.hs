{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Immutable.Experimental.LevelDB
  ( DB
  , withDB
  , Backend (..)

  , testLDB
  ) where

import           Control.DeepSeq (force)
import           Control.Exception (evaluate)
import           Control.Monad (when)

import           Control.Concurrent

import qualified Data.ByteString.Char8 as BC
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Maybe (fromMaybe, isJust)
import           Data.Monoid ((<>))
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Serialize as S
import           Data.Word

import qualified Data.Stream.Monadic as Stream

import           Database.Immutable.Experimental
import qualified Database.LevelDB.Iterator as LDB
import qualified Database.LevelDB.Streaming as LDB
import qualified Database.LevelDB.Base as LDB

import           System.IO.Unsafe (unsafePerformIO)

import           Prelude hiding (lookup, length)

keyTableBatch :: BC.ByteString
keyTableBatch = "b"

keyTableSize :: BC.ByteString -> BC.ByteString
keyTableSize table = "s:" <> table

keyTableRecord :: BC.ByteString -> Word64 -> Word64 -> BC.ByteString
keyTableRecord table batch index = "r:" <> table <> ":" <> serialize batch <> ":" <> serialize (batch, index)

keyTableId :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString
keyTableId table field value = "i:" <> table <> ":" <> field <> ":" <> value

data DB = DB
  { _dbLevelDB :: LDB.DB
  , _dbOffsets :: MVar (OffsetMap, Word64)
  }

startsWith :: LDB.DB -> LDB.ReadOptions -> BC.ByteString -> IO [LDB.Entry]
startsWith db opts prefix = LDB.withIter db opts $ \i -> Stream.toList $ LDB.entrySlice i range LDB.Asc
  where
    range = LDB.KeyRange
      { LDB.start = prefix
      , LDB.end = \cur -> compare (BC.take (BC.length prefix) cur) prefix
      }

withDB :: LDB.Options -> FilePath -> (DB -> IO a) -> IO a
withDB opts path f = LDB.withDB path opts $ \db -> do
  offsetsBS <- startsWith db readOpts "s:"

  batchBS <- LDB.get db readOpts keyTableBatch

  offsetMap <- case (,) <$> sizeMap "s:" offsetsBS <*> maybe (pure 0) (S.runGet S.get) batchBS of
    Left e -> error e
    Right (kvs, batch) -> newMVar (M.fromList [ (unpack k, v) | (k, v) <- kvs ], batch)

  f (DB db offsetMap)

  where
    readOpts = LDB.defaultReadOptions
      { LDB.useSnapshot = Nothing
      }

    sizeMap prefix sizesBS = sequence
      [ (,) <$> pure (BC.drop prefixLength table) <*> S.runGet S.get count
      | (table, count) <- sizesBS
      ]
      where
        prefixLength = BC.length prefix

instance Backend DB where
  type Snapshot DB = LDB.Snapshot

  withSnapshot (DB db _) f = do
    a <- LDB.withSnapshot db (pure . f)
    evaluate (force a)

  lookupRecord (DB db _) snapshot table field k = unsafePerformIO $ do
    indexBS <- LDB.get db opts (keyTableId (pack table) (pack field) (serialize k))

    case S.runGet S.get <$> indexBS of
      Just (Right (batch, index)) -> LDB.get db opts (keyTableRecord (pack table) batch index)
      _ -> pure Nothing

    where
      opts = LDB.defaultReadOptions
        { LDB.useSnapshot = Just snapshot
        }

  insertTables (DB db lock) opts f = do
    modifyMVar_ lock $ \(offsetMap, batch) -> do
      let (missingFids, tables) = f offsetMap
          newOffsets = M.fromList
            [ (unpack table, offset + count)
            | (table, count) <- tableCounts tables
            , let offset = fromMaybe 0 $ M.lookup (unpack table) offsetMap
            ]
          offsetMap' = newOffsets <> offsetMap

      missingFidIds <- sequence
        [ (\x -> (v, x)) <$> idExists (pack table) (pack field) value
        | v@(table, field, value) <- missingFids
        ]

      -- TODO: cleanup
      anyMissingFid <- not . and . fmap snd <$> sequence
        [ (\x -> (v, x)) <$> idExists (pack table) (pack field) value
        | v@(table, field, value) <- missingFids
        ]

      when (not $ and $ fmap snd missingFidIds) $ error
        $ "Consistency violation: missing foreign ids: " <> show (map fst $ filter (not . snd) missingFidIds)

      case opts of
        ErrorOnDuplicates -> do
          anyDuplicateId <- or <$> sequence
            [ case eid of
                EId field value -> idExists (pack table) (pack field) (serialize value)
                EAbsolutizedId field value -> idExists (pack table) (pack field) (serialize value)
                _ -> pure True

            | (table, records) <- tables
            , (eids, _) <- records
            , eid <- eids
            ]

          when anyDuplicateId $ error "Consistency violation: duplicate ids"
        _ -> pure ()

      let recordsBatch = sequence $ mconcat
            [ Right (LDB.Put (keyTableRecord (pack table) batch index) record):mconcat
                [ case eid of
                    EId field value ->
                      [ Right $ LDB.Put (keyTableId (pack table) (pack field) (serialize value)) (serialize (batch, index)) ]
                    EAbsolutizedId field value
                      | (value > final) ->
                          [ Left "Consistency violation: relative id out of bounds" ]
                      | otherwise ->
                          [ Right $ LDB.Put (keyTableId (pack table) (pack field) (serialize value)) (serialize (batch, index)) ]
                    _ -> []
                | eid <- eids
                ]
            | (table, records) <- tables

            , let offset = fromMaybe 0 $ M.lookup table offsetMap
                  final  = fromMaybe 0 $ M.lookup table offsetMap'

            , ((eids, record), index) <- zip records [offset..]
            ]

          newOffsetsBatch =
            [ LDB.Put (keyTableSize table) (serialize (offset + count))
            | (table, count) <- tableCounts tables
            , let offset = fromMaybe 0 $ M.lookup (unpack table) offsetMap
            ]
      
      LDB.write db writeOpts $ mconcat
        [ newOffsetsBatch

        , case recordsBatch of
            Left e -> error e
            Right recordsBatch' -> recordsBatch'

        -- Batch index
        , [ LDB.Put keyTableBatch (serialize (batch + 1)) ]
        ]

      pure (offsetMap', batch + 1)

    where
      idExists :: BC.ByteString -> BC.ByteString -> BC.ByteString -> IO Bool
      idExists table field value = isJust <$> LDB.get db readOpts (keyTableId table field value)

      writeOpts = LDB.defaultWriteOptions
      readOpts = LDB.defaultReadOptions
        { LDB.useSnapshot = Nothing
        }

      tableCounts :: [SerializedTable] -> [(BC.ByteString, Word64)]
      tableCounts tables = 
        [ (pack table, fromIntegral $ L.length records)
        | (table, records) <- tables
        ]

  tableSize (DB db _) snapshot table = undefined
    where
      readOpts = LDB.defaultReadOptions
        { LDB.useSnapshot = Just snapshot
        }

  tableRecords = undefined

--------------------------------------------------------------------------------

testLDB = do
  withDB opts "ldb" $ \db -> do
    insert db OverwriteDuplicates companyI

    p <- withSnapshot db (lookupTest db)

    print p
    putStrLn "DONE"
  where
    opts = LDB.defaultOptions
      { LDB.createIfMissing = True
      }

    lookupTest db snapshot = (name <$> person, fmap (fmap name) f')
      where
        person = lookup (pid $ persons lookups) 0
        f' = (fmap get . friend) <$> person
        lookups = lookupFields db snapshot
