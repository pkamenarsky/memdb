{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

import Control.Concurrent
import Control.Monad
import Control.Monad.ST

import Codec.Serialise
import Codec.Serialise.IO

import Data.IORef
import Data.Monoid ((<>))

import Foreign.Ptr
import Foreign.Storable

import qualified Data.ByteString as B
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.Word
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as V
import Data.Vector.Unboxed.Deriving

import qualified Multimap.ByteString as MMB
import qualified Multimap.Word32 as MMW

import GHC.Compact
import GHC.OverloadedLabels
import GHC.TypeLits

import System.IO
import System.IO.MMap
import System.IO.Unsafe

import GHC.Generics

import Prelude hiding (lookup)

newtype Key a = Key { getKey :: Word32 } deriving (Show, Generic, Serialise)

withDB :: Serialise a => FilePath -> ((a -> IO ()) -> IO ()) -> IO ()
withDB path f = do
  count <- newIORef (0 :: Word32)
  withFile path WriteMode $ \handle -> f $ \a -> do
    modifyIORef count (+1)
    hPutSerialise handle a
  withFile (path <> ".meta") WriteMode $ \handle -> do
    count' <- readIORef count
    hPutSerialise handle count'

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

class Lookup indexes (s :: Symbol) t a | indexes s a -> t where
  lookup :: Indexes indexes a -> Name s -> t -> IO [Word32]

instance Lookup ('(s, ByteStringIndex a):xs) s B.ByteString a where
  lookup (Indexes (mm, _, _)) _ = MMB.lookup mm

instance Lookup ('(s, Word32Index a):xs) s Word32 a where
  lookup (Indexes (mm, _, _)) _ = MMW.lookup mm

instance {-# OVERLAPPABLE #-}
  ( MapIndexes (x:xs) a ~ (y, f, MapIndexes xs a)
  , Lookup xs s t a
  ) => Lookup (x:xs) s t a where
    lookup (Indexes (_, _, xs)) = lookup (Indexes xs :: Indexes xs a)

class Insert indexes a where
  insertIndex :: Indexes indexes a -> Word32 -> a -> IO ()

instance Insert '[] a where
  insertIndex _ _ _ = pure ()

instance Insert xs a => Insert ('(s, Word32Index a):xs) a where
  insertIndex (Indexes (mm, f, xs)) index a = do
    MMW.insert mm (f a) index
    insertIndex (Indexes xs :: Indexes xs a) index a

instance Insert xs a => Insert ('(s, ByteStringIndex a):xs) a where
  insertIndex (Indexes (mm, f, xs)) index a = do
    MMB.insert mm (f a) index
    insertIndex (Indexes xs :: Indexes xs a) index a

data Indexes (indexes :: [(Symbol, *)]) a = Indexes (MapIndexes indexes a)
data DB    v (indexes :: [(Symbol, *)]) a = DB (Indexes indexes a) (v a)

newtype CompactedVector a = CompactedVector (Compact (V.Vector a))

(!) :: V.Unbox a => DB CompactedVector indexes a -> Word32 -> Maybe a
(!) (DB _ (CompactedVector as)) index = getCompact as V.!? fromIntegral index

unindexed :: Indexes '[] a
unindexed = Indexes ()

word32Index
  :: KnownSymbol s
  => Serialise a
  => Name s
  -> (a -> Word32)
  -> Indexes indexes a
  -> Indexes ('(s, Word32Index a):indexes) a
word32Index _ f (Indexes indexes) = unsafePerformIO $ do
  mm <- MMW.new
  pure $ Indexes (mm, f, indexes)
{-# NOINLINE word32Index #-}

byteStringIndex
  :: KnownSymbol s
  => Serialise a
  => Name s
  -> (a -> B.ByteString)
  -> Indexes indexes a
  -> Indexes ('(s, ByteStringIndex a):indexes) a
byteStringIndex _ f (Indexes indexes) = unsafePerformIO $ do
  mm <- MMB.new
  pure $ Indexes (mm, f, indexes)
{-# NOINLINE byteStringIndex #-}

--------------------------------------------------------------------------------

data Person = Person { name :: String, age :: Int } deriving (Generic, Show, Serialise)

personDB
  = word32Index     #ageIndex  (fromIntegral . age)
  $ byteStringIndex #nameIndex (BC.pack . name)
    unindexed

-- pickTest :: _
-- pickTest = lookup personDB #ageIndex 4
-- 
-- insertTest :: _
-- insertTest = insertIndex personDB 50 (undefined :: Person)

-- readDB :: Serialise a => FilePath -> indexes -> DB indexes a
-- readDB = undefined

--------------------------------------------------------------------------------

{-
* Storable packed array for ScheduledStops, load in memory
* Serialise array for Stops (in memory or offset/limit into mmap buffer?)
* Field indices must be computed for both
* Accessable by array index for both
* TODO Use GC ranges after data is loaded!
-}

data ScheduledStop = ScheduledStop
  { stopType      :: {-# UNPACK #-} !Word32
  , arrivalTime   :: {-# UNPACK #-} !Word32
  , departureTime :: {-# UNPACK #-} !Word32
  , atcocode      :: {-# UNPACK #-} !(Key ScheduledStop)
  } deriving (Show, Generic, Serialise)

derivingUnbox "ScheduledStop"
  [t| ScheduledStop -> (Word32, Word32, Word32, Word32) |]
  [| \(ScheduledStop a b c (Key d)) -> (a, b, c, d) |]
  [| \(a, b, c, d) -> (ScheduledStop a b c (Key d)) |]

scheduledStopIndexes
  = word32Index #atcocodeIndex (getKey . atcocode)
    unindexed

writeOut :: IO ()
writeOut = do
  let go (!x) write
        | x == 65000000 = pure ()
        | otherwise = do
            if x `mod` 1000000 == 0
              then print x
              else pure ()

            write $ ScheduledStop
              { stopType = 1
              , arrivalTime = fromIntegral x
              , departureTime = fromIntegral x
              , atcocode = Key (fromIntegral x)
              }
            go (x + 1) write

  withDB "out.bin" (go 0)

readDB
  :: forall indexes a
  . Serialise a
  => Insert indexes a
  => V.Unbox a
  => FilePath
  -> Indexes indexes a
  -> IO (DB CompactedVector indexes a)
readDB path indexes = do
  count <- readFileDeserialise (path <> ".meta") :: IO Word32
  mm <- MMW.new
  v <- V.new (fromIntegral count)
  fp <- openFile path ReadMode
  p <- (stToIO (deserialiseIncremental :: (ST s (IDecode s a))))
  eval mm v 0 0 fp "" p
  print "Freezing"
  v' <- V.unsafeFreeze v
  print "Compacting"
  v'' <- compact v'
  pure $ DB indexes (CompactedVector v'')
  where
    eval mm v (!x) (!y) fp (!bs) p =  do
      if x `mod` 1000000 == 0
        then do
          print x
          print y
        else pure ()
      case p of
        Done next _ a -> do
          p <- (stToIO (deserialiseIncremental :: (ST s (IDecode s a))))
          V.write v x a
          insertIndex indexes (fromIntegral x) a
          eval mm v (x + 1) (y + 1) fp next p
        Partial k -> do
          if B.length bs == 0
            then do
              bs <- B.hGet fp (1024 * 8)
              if B.length bs == 0
                then pure ()
                else eval mm v x y fp bs (Partial k)
            else do
              r <- stToIO (k $ Just bs)
              eval mm v x y fp "" r
        Fail _ _ e -> print e

readOut :: IO ()
readOut = do
  mm <- MMW.new
  v <- V.new 65000000
  fp <- openFile "out.cbor" ReadMode
  p <- (stToIO (deserialiseIncremental :: (ST s (IDecode s ScheduledStop))))
  eval mm v 0 0 fp "" p
  -- print "Freezing"
  -- v' <- V.unsafeFreeze v
  -- print "Compacting"
  -- v'' <- compact v'
  -- print "Done"
  -- print $ (getCompact v'' V.! 10000000)
  where
    eval mm v (!x) (!y) fp (!bs) p =  do
      if x `mod` 1000000 == 0
        then do
          print x
          print y
        else pure ()
      case p of
        Done next _ a -> do
          p <- (stToIO (deserialiseIncremental :: (ST s (IDecode s ScheduledStop))))
          V.write v x a
          -- MM.insert mm (B.singleton $ fromIntegral $ departureTime a) (fromIntegral $ departureTime a)
          -- MM.insert mm (B.singleton $ fromIntegral x) (fromIntegral x)
          eval mm v (x + 1) (y + departureTime a) fp next p
        Partial k -> do
          if B.length bs == 0
            then do
              bs <- B.hGet fp (1024 * 8)
              if B.length bs == 0
                then pure ()
                else eval mm v x y fp bs (Partial k)
            else do
              r <- stToIO (k $ Just bs)
              eval mm v x y fp "" r
        Fail _ _ e -> print e

readTest = do
  db@(DB indexes _) <- readDB "out.bin"
     $ word32Index #atcocodeIndex (getKey . atcocode)
     $ word32Index #arrDepTimeIndex (\s -> arrivalTime s + departureTime s)
       unindexed
  print $ db ! 1000000
  indexes <- lookup indexes #arrDepTimeIndex 2000
  print indexes
