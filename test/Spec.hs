{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedLabels #-}

import          Control.Monad

import qualified Data.ByteString.Char8 as BC

import qualified Database.Immutable as DB
import qualified Database.Immutable.Read as DB
import qualified Database.Immutable.Write as DB

import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC

main :: IO ()
main = do
  QC.quickCheck $ QC.withMaxSuccess 1000 indexProp
  QC.quickCheck $ QC.withMaxSuccess 1000 lookupPropWord32
  QC.quickCheck $ QC.withMaxSuccess 1000 lookupPropString

safeIndex :: Int -> [a] -> Maybe a
safeIndex index as
  | index < 0         = Nothing
  | index >= length as = Nothing
  | otherwise         = Just (as !! index)

indexProp :: [Int] -> QC.Property
indexProp elements = QC.monadicIO $ do
  Right db <- QC.run
    $ DB.createDB
        binary
        (length elements)
        Nothing
        DB.unindexed
        :: _ (Either String (DB.DB _ Int))
  forM_ [-10..length elements + 10] $ \index -> do
    QC.assert (safeIndex index elements == db DB.! DB.Offset index)
  where
    binary = DB.fromList elements

lookupPropWord32' :: [Int] -> IO Bool
lookupPropWord32' elements = do
  Right db <- DB.createDB
        binary
        (length elements)
        Nothing
        $ DB.word32Index #index fromIntegral 
          DB.unindexed
        :: _ (Either String (DB.DB _ Int))

  fmap and $ forM elements $ \element -> do
    let actual   = DB.lookup db #index (fromIntegral element)
        expected = filter (== element) elements
    pure (actual == expected)

  where
    binary = DB.fromList elements

lookupPropWord32 :: [Int] -> QC.Property
lookupPropWord32 elements = QC.monadicIO $ do
  Right db <- QC.run
    $ DB.createDB
        binary
        (length elements)
        Nothing
        $ DB.word32Index #index fromIntegral 
          DB.unindexed
        :: _ (Either String (DB.DB _ Int))

  forM_ elements $ \element -> do
    let actual   = DB.lookup db #index (fromIntegral element)
        expected = filter (== element) elements
    QC.assert (actual == expected)

  where
    binary = DB.fromList elements

lookupPropString :: [String] -> QC.Property
lookupPropString elements = QC.monadicIO $ do
  Right db <- QC.run
    $ DB.createDB
        binary
        (length elements)
        Nothing
        $ DB.byteStringIndex #index BC.pack
          DB.unindexed
        :: _ (Either String (DB.DB _ String))

  forM_ elements $ \element -> do
    let actual   = DB.lookup db #index (BC.pack element)
        expected = filter (== element) elements
    QC.assert (actual == expected)

  where
    binary = DB.fromList elements
