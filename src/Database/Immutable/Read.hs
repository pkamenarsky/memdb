{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Immutable.Read where

import qualified Data.ByteString as B
import           Data.Maybe (maybe)
import           Data.Monoid ((<>))
import qualified Data.Serialize as S
import           Data.Word
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as VM

import qualified Multimap.ByteString as MMB
import qualified Multimap.Word32 as MMW

import           GHC.Compact
import           GHC.OverloadedLabels
import           GHC.TypeLits

import           System.IO
import           System.IO.Unsafe

data Name (a :: Symbol) = Name

instance l ~ l' => IsLabel (l :: Symbol) (Name l') where
  fromLabel = Name

data ByteStringIndex a = ByteStringIndex
data Word32Index     a = Word32Index

type family MapIndexes (indexes :: [(Symbol, *)]) a :: * where
  MapIndexes '[] a = ()
  MapIndexes ('(s, ByteStringIndex a):xs) a = ( MMB.Multimap
                                              , a -> B.ByteString
                                              , MapIndexes xs a
                                              )
  MapIndexes ('(s, Word32Index a):xs)     a = ( MMW.Multimap
                                              , a -> Word32
                                              , MapIndexes xs a
                                              )

class LookupIndex indexes (s :: Symbol) t a | indexes s a -> t where
  lookupIndex :: Indexes indexes a -> Name s -> t -> IO [Word32]

instance LookupIndex ('(s, ByteStringIndex a):xs) s B.ByteString a where
  lookupIndex (Indexes (mm, _, _)) _ = MMB.lookup mm

instance LookupIndex ('(s, Word32Index a):xs) s Word32 a where
  lookupIndex (Indexes (mm, _, _)) _ = MMW.lookup mm

instance {-# OVERLAPPABLE #-}
  ( MapIndexes (x:xs) a ~ (y, f, MapIndexes xs a)
  , LookupIndex xs s t a
  ) => LookupIndex (x:xs) s t a where
    lookupIndex (Indexes (_, _, xs))
      = lookupIndex (Indexes xs :: Indexes xs a)

class InsertIndex indexes a where
  insertIndex :: Indexes indexes a -> Word32 -> a -> IO ()

instance InsertIndex '[] a where
  insertIndex _ _ _ = pure ()

instance InsertIndex xs a => InsertIndex ('(s, Word32Index a):xs) a where
  insertIndex (Indexes (mm, f, xs)) index a = do
    MMW.insert mm (f a) index
    insertIndex (Indexes xs :: Indexes xs a) index a

instance InsertIndex xs a => InsertIndex ('(s, ByteStringIndex a):xs) a where
  insertIndex (Indexes (mm, f, xs)) index a = do
    MMB.insert mm (f a) index
    insertIndex (Indexes xs :: Indexes xs a) index a

data Indexes (indexes :: [(Symbol, *)]) a = Indexes (MapIndexes indexes a)
data DB    v (indexes :: [(Symbol, *)]) a = DB (Indexes indexes a) (Compact (v a))

-- | Default 'Indexes' description, specifying an unindexed database.
unindexed :: Indexes '[] a
unindexed = Indexes ()

-- | Add a 'Word32' index to an 'Indexes' description to be built when
-- reading a database.
word32Index
  :: KnownSymbol s
  => Name s
  -> (a -> Word32)
  -> Indexes indexes a
  -> Indexes ('(s, Word32Index a):indexes) a
word32Index _ f (Indexes indexes) = unsafePerformIO $ do
  mm <- MMW.new
  pure $ Indexes (mm, f, indexes)
{-# NOINLINE word32Index #-}

-- | Add a 'B.ByteString' index to an 'Indexes' description to be built when
-- reading a database.
byteStringIndex
  :: KnownSymbol s
  => Name s
  -> (a -> B.ByteString)
  -> Indexes indexes a
  -> Indexes ('(s, ByteStringIndex a):indexes) a
byteStringIndex _ f (Indexes indexes) = unsafePerformIO $ do
  mm <- MMB.new
  pure $ Indexes (mm, f, indexes)
{-# NOINLINE byteStringIndex #-}

readDB
  :: forall indexes a v
  . S.Serialize a
  => InsertIndex indexes a
  => V.Vector v a

  -- | File path of database
  => FilePath

  -- | Progress indicating function, called with the number of elements
  -- currently read and the total count of elements
  -> (Maybe (Word32 -> Word32 -> IO ())) 

  -- | 'Indexes' description
  -> Indexes indexes a

  -> IO (Either String (DB v indexes a))
readDB path progress indexes = do
  meta <- B.readFile (path <> ".meta")

  case S.decode meta :: Either String Word32 of
    Left e -> pure $ Left e
    Right count -> do
      v <- VM.new (fromIntegral count)

      withFile path ReadMode $ \handle -> do
        r <- go count v handle B.empty 0 (S.runGetPartial S.get)
        case r of
          Left e  -> pure $ Left e
          Right () -> do
            vfr  <- V.unsafeFreeze v
            vcmp <- compact vfr

            pure $ Right (DB indexes vcmp)
        where
          go count v handle bs (!x) f = do
            case f bs of
              S.Fail e _  -> pure $ Left e
              S.Partial k -> do
                bs <- B.hGet handle (1024 * 4)
                if B.null bs
                  then pure $ Right ()
                  else go count v handle bs x f
              S.Done a bs -> do
                VM.write v (fromIntegral x) a
                insertIndex indexes (fromIntegral x) a

                maybe (pure ()) (\f -> f x count) progress

                go count v handle bs (x + 1) (S.runGetPartial S.get)
