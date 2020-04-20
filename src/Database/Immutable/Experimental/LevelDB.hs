{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Immutable.Experimental.LevelDB
  ( DB
  , open
  , Backend (..)

  , testLDB
  ) where

import           Control.DeepSeq (force)
import           Control.Exception (evaluate)

import           Control.Concurrent

import qualified Data.ByteString.Char8 as BC
import           Data.Monoid ((<>))
import qualified Data.Serialize as S
import           Data.Word

import           Database.Immutable.Experimental
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
  , _dbSizeLock :: MVar ()
  }

open :: LDB.Options -> FilePath -> IO DB
open opts path = do
  db <- LDB.open path opts
  lock <- newMVar ()
  pure $ DB db lock

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
    -- TODO: don't read here, store sizes in MVar (db can't be opened multiple times)
    offsets <- withMVar lock $ \_ -> do
      sequence
        [ do
            offsetBS <- LDB.get db readOpts (tableSize $ BC.pack table)
            case S.runGet S.get <$> offsetBS of
              Just (Right offset) -> do
                LDB.put db writeOpts (tableSize $ BC.pack table) (S.runPut $ S.put (offset + count))
                pure offset
              _ -> do
                LDB.put db writeOpts (tableSize $ BC.pack table) (S.runPut $ S.put count)
                pure 0
                                                                                      
        | (table, count) <- tableCounts
        ]

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

      tableCounts :: [(String, Word64)]
      tableCounts = 
        [ (table, fromIntegral $ length records)
        | (table, records) <- tables
        ]

--------------------------------------------------------------------------------

testLDB = do
  db <- flip open "test" $ LDB.defaultOptions
    { LDB.createIfMissing = True
    }

  insert db OverwriteDuplicates companyI

  p <- withSnapshot db (lookupTest db)

  print p
  pure "DONE"
  where
    lookupTest db snapshot = (name <$> person, fmap (fmap name) f')
      where
        person = lookup (pid $ persons lookups) 5
        f' = (fmap get . friend) <$> person
        lookups = lookupTables db snapshot
