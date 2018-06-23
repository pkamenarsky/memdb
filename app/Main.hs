{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Multimap.ByteString as MM
import           Lib

testmmap :: IO ()
testmmap = do
  m <- MM.new
  MM.insert m "asd" 5
  MM.insert m "asd" 6
  MM.insert m "asd" 7
  r <- MM.lookup m "asd"
  print r

main :: IO ()
main = do
  readTest
