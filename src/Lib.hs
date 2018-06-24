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

import qualified Data.Serialize as S
import qualified Data.Serialize.Get as S
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

withDB :: S.Serialize a => FilePath -> ((a -> IO ()) -> IO ()) -> IO ()
withDB path f = do
  count <- newIORef (0 :: Word32)
  withFile path WriteMode $ \handle -> f $ \a -> do
    modifyIORef count (+1)
    B.hPut handle (S.encode a)
  withFile (path <> ".meta") WriteMode $ \handle -> do
    count' <- readIORef count
    B.hPut handle (S.encode count')

{-
newtype CompactedVector a = CompactedVector (Compact (V.Vector a))

(!) :: V.Unbox a => DB CompactedVector indexes a -> Offset a -> Maybe a
(!) (DB _ (CompactedVector as)) (Offset index)
  = getCompact as V.!? fromIntegral index

slice :: V.Unbox a => DB CompactedVector indexes a -> Offset a -> Limit a -> [a]
slice = undefined

--------------------------------------------------------------------------------

data Person = Person { name :: String, age :: Int }
  deriving (Generic, Show, S.Serialize)

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
  , atcocode      :: {-# UNPACK #-} !(Offset ScheduledStop)
  } deriving (Show, Generic, S.Serialize)

derivingUnbox "ScheduledStop"
  [t| ScheduledStop -> (Word32, Word32, Word32, Word32) |]
  [| \(ScheduledStop a b c (Offset d)) -> (a, b, c, d) |]
  [| \(a, b, c, d) -> (ScheduledStop a b c (Offset d)) |]

scheduledStopIndexes
  = word32Index #atcocodeIndex (getOffset . atcocode)
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
              , atcocode = Offset (fromIntegral x)
              }
            go (x + 1) write

  withDB "out.bin" (go 0)

readDB
  :: forall indexes a
  . S.Serialize a
  => Insert indexes a
  => Show a
  => V.Unbox a
  => FilePath
  -> Indexes indexes a
  -> IO (DB CompactedVector indexes a)
readDB path indexes = do
  meta <- B.readFile (path <> ".meta")
  case S.decode meta :: Either String Word32 of
    Left e -> error e
    Right count -> do
      mm <- MMW.new
      v <- V.new (fromIntegral count)
      fp <- openFile path ReadMode
      go v fp B.empty 0 (S.runGetPartial S.get)
      print "Freezing"
      v' <- V.unsafeFreeze v
      print "Compacting"
      v'' <- compact v'
      pure $ DB indexes (CompactedVector v'')
      where
        go v handle bs (!x) f = do
          if x `mod` 1000000 == 0
            then print x
            else pure ()
          case f bs of
            S.Fail e _  -> error e
            S.Partial k -> do
              bs <- B.hGet handle (1024 * 4)
              if B.null bs
                then pure ()
                else go v handle bs x f
            S.Done a bs -> do
              V.write v x a
              insertIndex indexes (fromIntegral x) a
              go v handle bs (x + 1) (S.runGetPartial S.get)

readTest = do
  db@(DB indexes _) <- readDBSZ "out.bin"
     $ word32Index #atcocodeIndex (getOffset . atcocode)
     $ word32Index #arrDepTimeIndex (\s -> arrivalTime s + departureTime s)
       unindexed
  print $ db ! (Offset 1000000)
  indexes <- lookup indexes #arrDepTimeIndex 2000
  print indexes
-}
