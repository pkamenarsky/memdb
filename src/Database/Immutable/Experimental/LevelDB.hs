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

import           Control.Concurrent

import qualified Data.ByteString.Char8 as BC
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

tableSize :: BC.ByteString -> BC.ByteString
tableSize table = "s:" <> table

tableRecord :: BC.ByteString -> Word64 -> BC.ByteString
tableRecord table index = "r:" <> table <> ":" <> S.runPut (S.put index)

tableId :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString
tableId table field value = "i:" <> table <> ":" <> field <> ":" <> value

data DB = DB
  { _dbLevelDB :: LDB.DB
  , _dbOffsets :: MVar (M.Map BC.ByteString Word64)
  }

withDB :: LDB.Options -> FilePath -> (DB -> IO a) -> IO a
withDB opts path f = LDB.withDB path opts $ \db -> do
  sizesBS <- LDB.withIter db readOpts $ \i -> Stream.toList $ LDB.entrySlice i (range "s:") LDB.Asc

  offsetMap <- case sizeMap "s:" sizesBS of
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

    range prefix = LDB.KeyRange
      { LDB.start = prefix
      , LDB.end = \cur -> compare (BC.take (BC.length prefix) cur) prefix
      }

    readOpts = LDB.defaultReadOptions
      { LDB.useSnapshot = Nothing
      }

instance Backend DB where
  type Snapshot DB = LDB.Snapshot

  withSnapshot (DB db _) f = do
    a <- LDB.withSnapshot db (pure . f)
    evaluate (force a)

  lookupRecord db'@(DB db _) snapshot table field k = unsafePerformIO $ do
    indexBS <- LDB.get db opts (tableId (BC.pack table) (BC.pack field) (S.runPut (S.put k)))

    case S.runGet S.get <$> indexBS of
      Just (Right index) -> do
        recordBS <- LDB.get db opts (tableRecord (BC.pack table) index)

        case S.runGet S.get <$> recordBS of
          Just (Right v) -> pure $ Just (resolve db' snapshot v)
          _ -> pure Nothing
      _ -> pure Nothing
    where
      opts = LDB.defaultReadOptions
        { LDB.useSnapshot = Just snapshot
        }

  insertTables (DB db lock) opts fids tables = do
    offsets <- modifyMVar lock $ \offsetMap -> do
      (offsets, offsetMap') <- unzip <$> sequence
        [ do
            LDB.put db writeOpts (tableSize table) (S.runPut $ S.put (offset + count))
            pure (offset, (table, offset + count))
        | (table, count) <- tableCounts
        , let offset = fromMaybe 0 $ M.lookup table offsetMap
        ]
      pure (offsetMap <> M.fromList offsetMap', offsets)

    -- TODO: check if absolute foreign ids reference existing records in db
    -- TODO: disallow relative foreign ids not in batch
    -- TODO: check if both absolute and relative ids overwrite anything in db if opts == ErrorOnDuplicates

    sequence_
      [ do
          LDB.put db writeOpts (tableRecord (BC.pack table) index) record
          sequence
            [ LDB.put db writeOpts (tableId (BC.pack table) (BC.pack field) k) (S.runPut $ S.put index)
            | EId' (EId field k) <- eids -- TODO: relative ids
            ]
      | ((table, records), offset) <- zip tables offsets
      , ((eids, record), index) <- zip records [offset..]
      ]

    where
      writeOpts = LDB.defaultWriteOptions

      readOpts = LDB.defaultReadOptions
        { LDB.useSnapshot = Nothing
        }

      tableCounts :: [(BC.ByteString, Word64)]
      tableCounts = 
        [ (BC.pack table, fromIntegral $ length records)
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
