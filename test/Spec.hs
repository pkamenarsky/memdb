{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedLabels #-}

import          Control.Monad

import qualified Data.ByteString.Char8 as BC

import qualified Database.Immutable as DB
import qualified Database.Immutable.Read as DB
import qualified Database.Immutable.Write as DB

import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC

import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Indexing" $ do
    it "works correctly"
      $ QC.withMaxSuccess 3000
      $ QC.property indexProp

  describe "Slicing" $ do
    it "works correctly"
      $ QC.withMaxSuccess 1000
      $ QC.property sliceProp

  describe "Word32 lookup" $ do
    it "works correctly with a random list"
      $ QC.withMaxSuccess 3000
      $ QC.property lookupPropWord32

    it "works correctly with a random list with duplicated elements"
      $ QC.withMaxSuccess 3000
      $ QC.property
      $ flip QC.forAll lookupPropWord32 $ do
          elements <- QC.vectorOf 5 QC.arbitrary
          QC.listOf (QC.elements elements)

  describe "ByteString lookup" $ do
    it "works correctly with a random list"
      $ QC.withMaxSuccess 3000
      $ QC.property lookupPropByteString

    it "works correctly with a random list with duplicated elements"
      $ QC.withMaxSuccess 3000
      $ QC.property
      $ flip QC.forAll lookupPropByteString $ do
          elements <- QC.vectorOf 5 QC.arbitrary
          QC.listOf (QC.elements elements)

safeIndex :: Int -> [a] -> Maybe a
safeIndex index as
  | index < 0         = Nothing
  | index >= length as = Nothing
  | otherwise         = Just (as !! index)

safeSlice :: Int -> Int -> [a] -> [a]
safeSlice offset limit as = take limit' $ drop offset' as
  where
    offset' = max 0 offset
    limit'  = max 0 (min (length as) (offset' + limit) - offset')

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
    QC.assert (safeIndex index elements == db DB.! DB.Id index)
  where
    binary = DB.fromList elements

sliceProp :: [Int] -> QC.Property
sliceProp elements = QC.monadicIO $ do
  Right db <- QC.run
    $ DB.createDB
        binary
        (length elements)
        Nothing
        DB.unindexed
        :: _ (Either String (DB.DB _ Int))
  forM_ [-10..length elements + 10] $ \index -> do
    forM_ [-10..length elements + 10] $ \limit -> do
      let actual   = DB.slice (DB.Id index) (DB.Limit limit) db
          expected = safeSlice index limit elements
      QC.assert (actual == expected)
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
    let actual   = DB.lookup #index (fromIntegral element) db 
        expected = filter (== element) elements
    QC.assert (actual == expected)

  where
    binary = DB.fromList elements

lookupPropByteString :: [String] -> QC.Property
lookupPropByteString elements = QC.monadicIO $ do
  Right db <- QC.run
    $ DB.createDB
        binary
        (length elements)
        Nothing
        $ DB.byteStringIndex #index BC.pack
          DB.unindexed
        :: _ (Either String (DB.DB _ String))

  forM_ elements $ \element -> do
    let actual   = DB.lookup #index (BC.pack element) db
        expected = filter (== BC.pack element) (map BC.pack elements)
    QC.assert (map BC.pack actual == expected)

  where
    binary = DB.fromList elements
