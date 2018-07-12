{-|
Module      : Database.Immutable
Copyright   : (c) Philip Kamenarsky, 2018
License     : MIT
Maintainer  : p.kamenarsky@gmail.com
Stability   : experimental
Portability : POSIX

This package contains a thin wrapper over a continous memory region combined
with an efficient reverse index implementation for fast and type-safe indexed
lookups.

It is aimed at storing, loading and querying big immutable datasets. Once
written, a database can not be modified further.

The underlying storage is pinned and thus ensures efficient garbage
collection without ever reading the structure contents, since no pointers
live inside the dataset that point outside it.

-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Database.Immutable
  (
  -- * Database
    DB

  -- * Index types
  , ByteStringIndex
  , Word32Index

  -- * Querying
  , (!)
  , slice
  , lookup
  ) where

import qualified Data.ByteString as B
import           Data.Either (either)
import qualified Data.Vector.Storable as V
import qualified Data.Serialize as S

import           Database.Immutable.Internal

import           System.IO.Unsafe (unsafePerformIO)

import           Prelude hiding (length, lookup)

-- | /O(1)/ Yield the database element at the specified position.
(!) :: S.Serialize a => DB indexes a -> Offset a -> Maybe a
(!) (DB _ contents offsets) (Offset index)
  | Just length' <- length
  , Just offset' <- offset = either (const Nothing) Just $ S.decode
      $ B.take (fromIntegral length')
      $ B.drop (fromIntegral offset') contents
  | otherwise = Nothing 
  where
    length = offsets V.!? index
    offset
      | index == 0 = Just 0
      | otherwise = offsets V.!? (index - 1)

unsafeIndex :: S.Serialize a => DB indexes a -> Offset a -> a
unsafeIndex (DB _ contents offsets) (Offset index)
  = either (error "unsafeIndex") id $ S.decode
      $ B.take (fromIntegral length)
      $ B.drop (fromIntegral offset) contents
  where
    length = offsets V.! index
    offset
      | index == 0 = 0
      | otherwise = offsets V.! (index - 1)

-- | /O(n)/ Yield a slice of the database. The database must contain
-- at least i+n elements.
slice :: S.Serialize a => DB indexes a -> Offset a -> Limit a -> [a]
slice db@(DB _ _ offsets) (Offset index) (Limit limit) = map
  ((db `unsafeIndex`) . Offset)
  [index'..max index' (min (V.length offsets) (index' + limit)) - 1]
  where
    index' = max index 0

-- | /O(n)/ Lookup by index, @n@ being the count of returned elements.
--
-- Example:
--
-- > lookup personsDB #nameIndex "Phil" -- Return all elements named "Phil"
lookup
  :: forall indexes s v a
  . S.Serialize a
  => LookupIndex indexes s v a
  => Name s             -- ^ Index name
  -> v                  -- ^ Index value
  -> DB indexes a       -- ^ Database
  -> [a]                -- ^ Resulting items
lookup name t db@(DB indexes _ _)
  = map ((db `unsafeIndex`) . Offset . fromIntegral) is
  where
    is = unsafePerformIO
       $ lookupIndex (Indexes' indexes :: Indexes' indexes a) name t
