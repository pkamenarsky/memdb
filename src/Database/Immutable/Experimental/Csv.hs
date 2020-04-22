{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

module Database.Immutable.Experimental.Csv where

import           Control.DeepSeq (force)
import           Control.Exception (evaluate)
import           Control.Monad (when)

import           Control.Concurrent

import qualified Data.ByteString.Char8 as BC
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Maybe (catMaybes, fromMaybe, isJust)
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

import           System.IO
import           System.IO.Unsafe

import           Prelude hiding (lookup)

import           Debug.Trace

data DB = DB FilePath (MVar (M.Map TableName Word64, M.Map TableName Handle))

newDB :: FilePath -> IO DB
newDB path = do
  var <- newMVar (M.empty, M.empty)
  pure $ DB path var

instance Backend DB where
  insertTables (DB path db) opts f = modifyMVar_ db $ \(offsets, handles) -> do
    let (missingFids, srs) = f offsets
        newOffsets = M.fromList
          [ (table, offset + fromIntegral (length records))
          | (table, records) <- srs
          , let offset = fromMaybe 0 $ M.lookup table offsets
          ]

    newHandles' <- M.fromList <$> sequence
      [ (table,) <$> openFile (path <> "/" <> table <> ".csv") WriteMode
      | (table, _) <- srs
      , Nothing <- [ M.lookup table handles ]
      ]

    let newHandles = newHandles' <> handles

    sequence_
      [ BC.hPut handle (record <> "\n")
      | (table, records) <- srs
      , Just handle <- [ M.lookup table newHandles ]
      , (_, record) <- records
      ]

    pure (newOffsets <> offsets, newHandles)
