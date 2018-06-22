{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where

import Control.Concurrent
import Control.Monad
import Control.Monad.ST

import Codec.Serialise
import Codec.Serialise.IO

import Foreign.Ptr
import Foreign.Storable

import qualified Data.ByteString as B
import qualified Data.ByteString.Short as BS
import Data.IORef
import Data.Word
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as V
import Data.Vector.Unboxed.Deriving

import GHC.Compact

import System.IO
import System.IO.MMap

import GHC.Generics

{-
* Storable packed array for ScheduledStops, load in memory
* Serialise array for Stops (in memory or offset/limit into mmap buffer?)
* Field indices must be computed for both
* Accessable by array index for both
* TODO Use GC ranges after data is loaded!
-}

newtype Key a = Key Word32 deriving (Show, Generic, Serialise)

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
  fp <- openFile "out.cbor" WriteMode

  let go (!x)
        | x == 65000000 = pure ()
        | otherwise = do
            if x `mod` 1000000 == 0
              then print x
              else pure ()

            hPutSerialise fp $ ScheduledStop
              { stopType = 1
              , arrivalTime = fromIntegral x
              , departureTime = fromIntegral x
              , atcocode = Key (fromIntegral x)
              }
            go (x + 1)
  go 0

readOut :: IO ()
readOut = do
  v <- V.new 65000000
  fp <- openFile "out.cbor" ReadMode
  p <- (stToIO (deserialiseIncremental :: (ST s (IDecode s ScheduledStop))))
  eval v 0 0 fp "" p
  print "Freezing"
  v' <- V.unsafeFreeze v
  print "Compacting"
  v'' <- compact v'
  print "Done"
  print $ (getCompact v'' V.! 10000000)
  where
    eval v (!x) (!y) fp (!bs) p =  do
      if x `mod` 1000000 == 0
        then do
          print x
          print y
        else pure ()
      case p of
        Done next _ a -> do
          p <- (stToIO (deserialiseIncremental :: (ST s (IDecode s ScheduledStop))))
          V.write v x a
          eval v (x + 1) (y + departureTime a) fp next p
        Partial k -> do
          if B.length bs == 0
            then do
              bs <- B.hGet fp (1024 * 8)
              if B.length bs == 0
                then pure ()
                else eval v x y fp bs (Partial k)
            else do
              r <- stToIO (k $ Just bs)
              eval v x y fp "" r
        Fail _ _ e -> print e
