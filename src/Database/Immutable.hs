{-|
Module      : Database.Immutable
Description : Efficient in memory indexed database
Copyright   : (c) Philip Kamenarsky, 2018
License     : BSD
Maintainer  : p.kamenarsky@gmail.com
Stability   : experimental
Portability : POSIX

This package contains a thin wrapper over 'Data.Vector' combined with an
efficient reverse index implementation for fast and type-safe indexed
lookups.

It is aimed at storing, loading and querying big immutable datasets. Once
written, a database can not be modified further.

Since databases are immutable, once loaded the underlying vectors are
frozen, fully evaluated and compacted. Compaction ensures efficient garbage
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

  -- * Querying
  , (!)
  , slice
  , slice'
  , lookup
  ) where

import qualified Data.Vector.Generic as GV

import           Database.Immutable.Internal

import           GHC.Compact
import           GHC.TypeLits

import           System.IO.Unsafe (unsafePerformIO)

import           Prelude hiding (lookup)

-- | /O(1)/ Yield the database element at the specified position.
(!) :: GV.Vector vector a => DB vector indexes a -> Offset a -> Maybe a
(!) (DB _ as) (Offset index) = getCompact as GV.!? fromIntegral index

-- | /O(1)/ Yield a slice of the underlying vector holding the database
-- elements, without copying it. The database must contain at least
-- i+n elements.
slice' :: GV.Vector vector a => DB vector indexes a -> Offset a -> Limit a -> vector a
slice' (DB _ as) (Offset index) (Limit limit)
  = GV.slice (fromIntegral index) (fromIntegral limit) (getCompact as)

-- | /O(n)/ Yield a slice of the database. The database must contain
-- at least i+n elements.
slice :: GV.Vector vector a => DB vector indexes a -> Offset a -> Limit a -> [a]
slice (DB _ as) (Offset index) (Limit limit) = GV.toList
  $ GV.slice (fromIntegral index) (fromIntegral limit) (getCompact as)

-- | /O(n)/ Lookup by index, @n@ being the count of returned elements.
--
-- Example:
--
-- > lookup personsDB #nameIndex "Phil" -- Return all elements named "Phil"
lookup
  :: GV.Vector vector a
  => LookupIndex indexes s v a
  => DB vector indexes a       -- ^ Database
  -> Name s                    -- ^ Index name
  -> v                         -- ^ Index value
  -> [a]                       -- ^ Resulting items
lookup (DB indexes as) name t = map (as' GV.!) (map fromIntegral is)
  where
    as' = getCompact as
    is  = unsafePerformIO $ lookupIndex indexes name t
