{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Database.Immutable.Read
  (
  -- * Construction / reading
    createDB
  , readDB

  -- * Indexes
  , I.Name
  , I.Indexes

  , unindexed
  , byteStringIndex
  , word32Index
  ) where

import qualified Data.ByteString as B
import           Data.Maybe (maybe)
import           Data.Monoid ((<>))
import qualified Data.Serialize as S
import           Data.Word

import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM

import qualified Multimap.ByteString as MMB
import qualified Multimap.Word32 as MMW

import           GHC.TypeLits

import qualified Database.Immutable.Internal as I

-- | Default 'I.Indexes' description, specifying an unindexed database.
unindexed :: I.Indexes '[] a
unindexed = I.Indexes (pure ())

-- | Add a 'Word32' index to an 'I.Indexes' description to be built when
-- reading a database.
word32Index
  :: KnownSymbol s

  => I.Name s
  -- ^ Index name

  -> (a -> b)
  -- ^ Projecting function

  -> (b -> Word32)
  -- ^ Index computing function

  -> I.Indexes indexes a
  -- ^ 'Indexes' description

  -> I.Indexes ('(s, I.Word32Index b):indexes) a
  -- ^ Resulting 'I.Indexes' desctiption, inlcuding the new 'Word32' index
word32Index _ p f (I.Indexes indexes') = I.Indexes $ do
  indexes <- indexes'
  mm <- MMW.new
  pure (mm, p, f, indexes)

-- | Add a 'B.ByteString' index to an 'I.Indexes' description to be built when
-- reading a database.
byteStringIndex
  :: KnownSymbol s

  => I.Name s
  -- ^ Index name

  -> (a -> b)
  -- ^ Projecting function

  -> (b -> B.ByteString)
  -- ^ Index computing function

  -> I.Indexes indexes a
  -- ^ 'Indexes' description

  -> I.Indexes ('(s, I.ByteStringIndex b):indexes) a
  -- ^ Resulting 'I.Indexes' desctiption, inlcuding the new 'B.ByteString'
  -- index
byteStringIndex _ p f (I.Indexes indexes') = I.Indexes $ do
  indexes <- indexes'
  mm <- MMB.new
  pure (mm, p, f, indexes)

-- | Create a database from a 'B.ByteString' and build up
-- in-memory indexes according to the 'I.Indexes' description.
createDB
  :: forall indexes a
  . S.Serialize a
  => I.InsertIndex indexes a

  => B.ByteString
  -- ^ Serialised elements

  -> Int
  -- ^ Element count
  
  -> (Maybe (Int -> Int -> IO ()))
  -- ^ Progress indicating function, called with the number of elements
  -- currently read and the total count of elements

  -> I.Indexes indexes a
  -- ^ 'Indexes' description

  -> IO (Either String (I.DB indexes a))
  -- ^ Resulting database or a parse/deserializiation error
createDB contents count progress (I.Indexes indexes') = do
  indexes <- indexes'
  offsets <- VSM.new (fromIntegral count)

  let go bs (!x) f
        | not (B.null bs) = do
          case f bs of
            S.Fail e _   -> pure $ Left e
            S.Partial _  -> pure $ Left "Unexpected Partial"
            S.Done a bs' -> do
              VSM.write offsets x (fromIntegral (B.length contents - B.length bs'))
              I.insertIndex
                (I.Indexes' indexes :: I.Indexes' indexes a)
                (fromIntegral x)
                a

              maybe (pure ()) (\f' -> f' x (fromIntegral count)) progress

              go bs' (x + 1) (S.runGetPartial S.get)
        | otherwise = pure $ Right ()

  r <- go contents 0 (S.runGetPartial S.get)

  case r of
    Left e  -> pure $ Left e
    Right () -> do
      voffsets <- VS.unsafeFreeze offsets
      pure $ Right (I.DB indexes contents voffsets)

-- | Read a database from a file path and build up in-memory indexes
-- according to the 'I.Indexes' description.
readDB
  :: forall indexes a
  . S.Serialize a
  => I.InsertIndex indexes a

  => FilePath
  -- ^ File path to database

  -> (Maybe (Int -> Int -> IO ()))
  -- ^ Progress indicating function, called with the number of elements
  -- currently read and the total count of elements

  -> I.Indexes indexes a
  -- ^ 'Indexes' description

  -> IO (Either String (I.DB indexes a))
  -- ^ Resulting database or a parse/deserializiation error
readDB path progress indexes = do
  meta <- B.readFile (path <> ".meta")

  case S.decode meta of
    Left e -> pure $ Left e
    Right (count :: Word32) -> do
      contents <- B.readFile path
      createDB contents (fromIntegral count) progress indexes
