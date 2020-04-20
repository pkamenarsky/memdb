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
import           Data.ByteString.Char8 (pack)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Map as M
import qualified Data.Serialize as S
import           Data.Word

import qualified Data.Stream.Monadic as Stream

import           Database.Immutable.Experimental
import qualified Database.LevelDB.Iterator as LDB
import qualified Database.LevelDB.Streaming as LDB
import qualified Database.LevelDB.Base as LDB

import           System.IO.Unsafe (unsafePerformIO)

import           Prelude hiding (lookup)

serialize :: S.Serialize a => a -> BC.ByteString
serialize = S.runPut . S.put

tableSize :: BC.ByteString -> BC.ByteString
tableSize table = "s:" <> table

tableRecord :: BC.ByteString -> Word64 -> BC.ByteString
tableRecord table index = "r:" <> table <> ":" <> serialize index

tableId :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString
tableId table field value = "i:" <> table <> ":" <> field <> ":" <> value

data DB = DB
  { _dbLevelDB :: LDB.DB
  , _dbOffsets :: MVar (M.Map BC.ByteString Word64)
  }

startsWith :: LDB.DB -> BC.ByteString -> IO [LDB.Entry]
startsWith db prefix = LDB.withIter db readOpts $ \i -> Stream.toList $ LDB.entrySlice i range LDB.Asc
  where
    readOpts = LDB.defaultReadOptions
      { LDB.useSnapshot = Nothing
      }

    range = LDB.KeyRange
      { LDB.start = prefix
      , LDB.end = \cur -> compare (BC.take (BC.length prefix) cur) prefix
      }

withDB :: LDB.Options -> FilePath -> (DB -> IO a) -> IO a
withDB opts path f = LDB.withDB path opts $ \db -> do
  offsetsBS <- startsWith db "s:"

  offsetMap <- case sizeMap "s:" offsetsBS of
    Left e -> error e
    Right m -> newMVar (M.fromList m)

  f (DB db offsetMap)

  where
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

  lookupRecord db'@(DB db _) snapshot table field k = unsafePerformIO $ do
    indexBS <- LDB.get db opts (tableId (pack table) (pack field) (serialize k))

    case S.runGet S.get <$> indexBS of
      Just (Right index) -> do
        recordBS <- LDB.get db opts (tableRecord (pack table) index)

        case S.runGet S.get <$> recordBS of
          Just (Right v) -> pure $ Just (resolve db' snapshot v)
          _ -> pure Nothing
      _ -> pure Nothing
    where
      opts = LDB.defaultReadOptions
        { LDB.useSnapshot = Just snapshot
        }

  insertTables (DB db lock) opts missingFids tables = do
    offsets <- modifyMVar lock $ \offsetMap -> do
      (offsets, offsetMap') <- unzip <$> sequence
        [ do
            LDB.put db writeOpts (tableSize table) (serialize (offset + count))
            pure ((offset, count), (table, offset + count))
        | (table, count) <- tableCounts
        , let offset = fromMaybe 0 $ M.lookup table offsetMap
        ]
      pure (offsetMap <> M.fromList offsetMap', offsets)

    -- TODO: check if absolute foreign ids reference existing records in db
    -- TODO: check if both absolute and relative ids overwrite anything in db if opts == ErrorOnDuplicates

    sequence_
      [ do
          LDB.put db writeOpts (tableRecord (pack table) index) record

          sequence
            [ case eid of
                EId field k -> LDB.put db writeOpts (tableId (pack table) (pack field) k) (serialize index)
                ERelativeId field k -> do
                  -- Relative indexes must always be less than the table batch count
                  when (k >= count) $ error "insertTables: k >= count"

                  LDB.put db writeOpts (tableId (pack table) (pack field) (serialize (offset + k))) (serialize index)
                _ -> pure ()
            | eid <- eids
            ]
      | ((table, records), (offset, count)) <- zip tables offsets
      , ((eids, record), index) <- zip records [offset..]
      ]

    where
      writeOpts = LDB.defaultWriteOptions

      readOpts = LDB.defaultReadOptions
        { LDB.useSnapshot = Nothing
        }

      tableCounts :: [(BC.ByteString, Word64)]
      tableCounts = 
        [ (pack table, fromIntegral $ length records)
        | (table, records) <- tables
        ]

--------------------------------------------------------------------------------

testLDB = do
  withDB opts "test" $ \db -> do
    insert db OverwriteDuplicates companyI

    p <- withSnapshot db (lookupTest db)

    print p
    pure "DONE"
  where
    opts = LDB.defaultOptions
      { LDB.createIfMissing = True
      }

    lookupTest db snapshot = (name <$> person, fmap (fmap name) f')
      where
        person = lookup (pid $ persons lookups) 5
        f' = (fmap get . friend) <$> person
        lookups = lookupTables db snapshot
