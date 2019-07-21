module Database.Immutable.Write
  (
  -- * Offset and Limit data types
    I.Id(..)
  , I.Limit(..)
  , I.incId
  , I.subIds

  -- * Writing
  , writeDB
  , fromList
  ) where

import qualified Data.ByteString as B
import           Data.IORef
import           Data.Monoid ((<>))
import qualified Data.Serialize as S
import           Data.Word

import qualified Database.Immutable.Internal as I

import           System.IO

-- | Creata a database from a list. Databases can be created
-- back using 'Database.Immutable.Read.createDB'.
fromList
  :: S.Serialize a
  => [a]
  -> B.ByteString
fromList = foldr B.append B.empty . (map S.encode)

-- | Write data sequentially to a database file. Database files can be read
-- back using 'Database.Immutable.Read.readDB'.
--
-- An additional file @path.meta@ is produced holding the number of
-- written elements.
writeDB
  :: S.Serialize a

  => FilePath
  -- ^ File path, must be writeable

  -> ((a -> IO ()) -> IO r)
  -- ^ Writing function; the continuation can be called multiple times

  -> IO r
writeDB path f = do
  count <- newIORef (0 :: Word32)
  r <- withFile path WriteMode $ \handle -> f $ \a -> do
    modifyIORef count (+1)
    B.hPut handle (S.encode a)
  withFile (path <> ".meta") WriteMode $ \handle -> do
    count' <- readIORef count
    B.hPut handle (S.encode count')
  pure r
