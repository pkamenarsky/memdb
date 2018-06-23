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

newtype Key a = Key Word32 deriving (Show, Generic, Serialise)

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

-- type family Pick (indexes :: [(Symbol, *)]) a (s :: Symbol) :: Maybe * where
--   Pick '[] a s = Nothing
--   Pick ('(s, ByteStringIndex a):xs) a s = Just MMB.Multimap
--   Pick ('(s, Word32Index a):xs) a s     = Just MMW.Multimap
--   Pick ('(other, index):xs) a s         = Pick xs a s

class PickIndex indexes (s :: Symbol) a mm | indexes s a -> mm where
  pick :: Name s -> DB indexes a -> mm

instance PickIndex ('(s, ByteStringIndex a):xs) s a MMB.Multimap where
  pick _ (DB (mm, _, _) _) = mm

instance PickIndex ('(s, Word32Index a):xs) s a MMW.Multimap where
  pick _ (DB (mm, _, _) _) = mm

instance {-# OVERLAPPABLE #-}
  ( MapIndexes (x:xs) a ~ (y, f, MapIndexes xs a)
  , PickIndex xs s a mm
  ) => PickIndex (x:xs) s a mm where
    pick s (DB (_, _, xs) as :: DB (x:xs) a) = pick s (DB xs as :: DB xs a)

class Insert indexes a where
  insert :: DB indexes a -> a -> Word32 -> IO ()

instance Insert ('(s, Word32Index a):xs) a where
  insert (DB (mm, f, _) _) a index = do
    MMW.insert mm (f a) index
    -- TODO: insert in vector

instance Insert ('(s, ByteStringIndex a):xs) a where
  insert (DB (mm, f, _) _) a index = do
    MMB.insert mm (f a) index
    -- TODO: insert in vector

instance {-# OVERLAPPABLE #-}
  ( MapIndexes (x:xs) a ~ (y, f, MapIndexes xs a)
  , Insert xs a
  ) => Insert (x:xs) a where
    insert (DB (_, _, xs) as :: DB (x:xs) a) a index
      = insert (DB xs as :: DB xs a) a index

data DB (indexes :: [(Symbol, *)]) a = DB (MapIndexes indexes a) (V.Vector a)

emptyDB :: DB '[] a
emptyDB = DB () undefined

word32Index
  :: KnownSymbol s
  => Serialise a
  => Name s
  -> (a -> Word32)
  -> DB indexes a
  -> DB ('(s, Word32Index a):indexes) a
word32Index _ f (DB indexes v) = unsafePerformIO $ do
  mm <- MMW.new
  pure $ DB (mm, f, indexes) v
{-# NOINLINE word32Index #-}

byteStringIndex
  :: KnownSymbol s
  => Serialise a
  => Name s
  -> (a -> B.ByteString)
  -> DB indexes a
  -> DB ('(s, ByteStringIndex a):indexes) a
byteStringIndex _ f (DB indexes v) = unsafePerformIO $ do
  mm <- MMB.new
  pure $ DB (mm, f, indexes) v
{-# NOINLINE byteStringIndex #-}

--------------------------------------------------------------------------------

data Person = Person { name :: String, age :: Int } deriving (Generic, Show, Serialise)

personDB :: _
personDB
  = word32Index     #ageIndex  (fromIntegral . age)
  $ byteStringIndex #nameIndex (BC.pack . name)
  emptyDB

pickTest :: _
pickTest = pick #nameIndex personDB

insertTest :: _
insertTest = insert personDB (undefined :: Person) 50

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
