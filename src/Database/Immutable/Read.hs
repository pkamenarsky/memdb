{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Database.Immutable.Read
  (
  -- * Database
    DB

  -- * Reading
  , readDB

  -- * Indexes
  , I.Name
  , I.Indexes

  , unindexed
  , byteStringIndex
  , word32Index
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Maybe (maybe)
import           Data.Monoid ((<>))
import qualified Data.Serialize as S
import           Data.Word

import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GVM

import qualified Data.Vector as V

import qualified Multimap.ByteString as MMB
import qualified Multimap.Word32 as MMW

import           GHC.Compact
import           GHC.OverloadedLabels
import           GHC.TypeLits

import           System.IO
import           System.IO.Unsafe (unsafePerformIO)

import qualified Database.Immutable.Internal as I

-- | Specialized version of 'DB.DB' using boxed vectors as the
-- underlying storage.
type DB = I.DB V.Vector

-- | Default 'I.Indexes' description, specifying an unindexed database.
unindexed :: I.Indexes '[] a
unindexed = I.Indexes ()

-- | Add a 'Word32' index to an 'I.Indexes' description to be built when
-- reading a database.
word32Index
  :: KnownSymbol s

  => I.Name s
  -- ^ Index name

  -> (a -> Word32)
  -- ^ Index computing function

  -> I.Indexes indexes a
  -- ^ 'Indexes' description

  -> I.Indexes ('(s, I.Word32Index a):indexes) a
  -- ^ Resulting 'I.Indexes' desctiption, inlcuding the new 'Word32' index
word32Index _ f (I.Indexes indexes) = unsafePerformIO $ do
  mm <- MMW.new
  pure $ I.Indexes (mm, f, indexes)
{-# NOINLINE word32Index #-}

-- | Add a 'B.ByteString' index to an 'I.Indexes' description to be built when
-- reading a database.
byteStringIndex
  :: KnownSymbol s

  => I.Name s
  -- ^ Index name

  -> (a -> B.ByteString)
  -- ^ Index computing function

  -> I.Indexes indexes a
  -- ^ 'Indexes' description

  -> I.Indexes ('(s, I.ByteStringIndex a):indexes) a
  -- ^ Resulting 'I.Indexes' desctiption, inlcuding the new 'B.ByteString'
  -- index
byteStringIndex _ f (I.Indexes indexes) = unsafePerformIO $ do
  mm <- MMB.new
  pure $ I.Indexes (mm, f, indexes)
{-# NOINLINE byteStringIndex #-}

-- | Read a database from a file path and build up in-memory indexes
-- according to the 'I.Indexes' description.
readDB
  :: forall indexes a vector
  . S.Serialize a
  => I.InsertIndex indexes a
  => GV.Vector vector a

  => FilePath
  -- ^ File path to database

  -> (Maybe (Word32 -> Word32 -> IO ()))
  -- ^ Progress indicating function, called with the number of elements
  -- currently read and the total count of elements

  -> I.Indexes indexes a
  -- ^ 'Indexes' description

  -> IO (Either String (I.DB vector indexes a))
  -- ^ Resulting database or a parse/deserializiation error
readDB path progress indexes = do
  meta <- B.readFile (path <> ".meta")

  case S.decode meta :: Either String Word32 of
    Left e -> pure $ Left e
    Right count -> do
      v <- GVM.new (fromIntegral count)

      withFile path ReadMode $ \handle -> do
        r <- go count v handle B.empty 0 (S.runGetPartial S.get)
        case r of
          Left e  -> pure $ Left e
          Right () -> do
            vfr  <- GV.unsafeFreeze v
            vcmp <- compact vfr

            pure $ Right (I.DB indexes vcmp)
        where
          go count v handle bs (!x) f = do
            case f bs of
              S.Fail e _  -> pure $ Left e
              S.Partial k -> do
                bs <- B.hGet handle (1024 * 4)
                if B.null bs
                  then pure $ Right ()
                  else go count v handle bs x k
              S.Done a bs -> do
                GVM.write v (fromIntegral x) a
                I.insertIndex indexes (fromIntegral x) a

                maybe (pure ()) (\f -> f x count) progress

                go count v handle bs (x + 1) (S.runGetPartial S.get)
