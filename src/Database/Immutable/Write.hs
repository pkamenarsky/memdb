module Database.Immutable.Write
  (
  -- * Offset and Limit data types
    I.Offset(..)
  , I.Limit(..)

  -- * Writing
  , writeDB
  , fromList
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.ByteString as B
import           Data.IORef
import qualified Data.Serialize as S
import           Data.Word

import qualified Database.Immutable.Internal as I

import           System.IO

-- | Creata a database from a list. Databases can be created
-- back using 'Database.Immutable.Read.createDB'.
fromList
  :: S.Serialize a
  => [a] -- ^ Writing function; the continuation can be called multiple times
  -> B.ByteString
fromList = foldr B.append B.empty . (map S.encode)

-- | Write data sequentially to a database file. Database files can be read
-- back using 'Database.Immutable.Read.readDB'.
--
-- An additional file @path.meta@ is produced holding the number of
-- written elements.
writeDB
  :: S.Serialize a
  => MonadIO m

  => ((Handle -> m ()) -> m ())
  -- ^ Bracketed writing function; see 'System.IO.withFile'

  -> ((a -> m ()) -> m ())
  -- ^ Output function; the continuation can be called multiple times

  -> m ()
writeDB with f = do
  count <- liftIO $ newIORef (0 :: Word32)
  with $ \handle -> f $ \a -> liftIO $ do
    modifyIORef count (+1)
    B.hPut handle (S.encode a)
  with $ \handle -> liftIO $ do
    count' <- readIORef count
    B.hPut handle (S.encode count')
