{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Immutable.Experimental.LDB where

import qualified Data.ByteString.Char8 as BC
import           Data.Monoid ((<>))
import qualified Data.Serialize as S

import           Database.Immutable.Experimental
import qualified Database.LevelDB.Base as LDB

import           System.IO.Unsafe (unsafePerformIO)

instance Backend LDB.DB where
  type Snapshot LDB.DB = LDB.Snapshot

  withSnapshot db f = LDB.withSnapshot db (pure . f)

  lookupRecord db snapshot table field k = unsafePerformIO $ do
    let key = BC.pack table <> ":" <> BC.pack field <> ":" <> S.runPut (S.put k)
    r <- LDB.get db opts key

    case S.runGet S.get <$> r of
      Just (Right v) -> pure $ Just (resolve db snapshot v)
      _ -> pure Nothing
    where
      opts = LDB.defaultReadOptions
        { LDB.useSnapshot = Just snapshot
        }
