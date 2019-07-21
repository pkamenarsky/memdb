{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Immutable.Internal where

import qualified Data.ByteString as B
import qualified Data.Serialize as S
import           Data.Word
import qualified Data.Vector.Storable as V

import qualified Multimap.ByteString as MMB
import qualified Multimap.Word32 as MMW

import           GHC.OverloadedLabels
import           GHC.TypeLits

-- | Offset into the database.
newtype Id a = Id { getId :: Word32 }
  deriving (Eq, Ord, Show, S.Serialize)

-- | Zero 'Id'.
zeroId :: Id a
zeroId = Id 0

-- | Increment 'Id'.
incId :: Id a -> Id a
incId (Id i) = Id (i + 1)

coerceId :: Id a -> Id b
coerceId (Id i) = Id i

-- | Limit the number of elements read after an 'Id'.
newtype Limit a  = Limit { getLimit :: Word32 }
  deriving (Eq, Ord, Show, S.Serialize)

-- | Add a 'Limit' to an 'Id'.
addLimit :: Id a -> Limit a -> Id a
addLimit (Id a) (Limit b) = Id (a + b - 1)

-- | Subtract two indexes, returning a 'Limit'.
subIds :: Id a -> Id a -> Limit a
subIds (Id a) (Id b) = Limit (a - b)

-- | Type tying a typelevel 'Symbol' to a value.
--
-- 'Name' has an 'IsLabel' instance and can thus be created by
-- using the overloaded labels syntax (i.e. @#someName@) when the
-- @OverloadedLabels@ GHC extension is enabled.
data Name (a :: Symbol) = Name

instance l ~ l' => IsLabel (l :: Symbol) (Name l') where
  fromLabel = Name

-- | An immutable database containing elements of type @a@, each one
-- indexed according to an 'Indexes' description.
--
-- Import "Database.Immutable.Read" for reading boxed values and
-- "Database.Immutable.Read.Unboxed" for the unboxed variant.
data DB (indexes :: [(Symbol, *)]) a
  = DB (MapIndexes indexes a) B.ByteString (V.Vector Word32)

-- | 'Indexes' description. Currently, there are two supported index types:
-- 'Word32' and 'B.ByteString'. Both can be specified using the
-- utility functions 'word32Index' and 'byteStringIndex', respectively.
--
-- For example, one might define indexes over a datatype in the following
-- way:
--
-- @
-- data Person = Person
--   { name :: 'String'
--   , age  :: 'Int'
--   } deriving (Generic, 'S.Serialize')
--
-- personIndexes
--   = 'byteStringIndex' #nameIndex ('BC.pack' . name)
--   $ 'word32Index'     #ageIndex  ('fromIntegral' . age)
--     'unindexed'
-- @
--
-- Composite or computed indexes can be built by supplying an appropriate
-- function to 'Database.Immutable.Read.byteStringIndex' or
-- 'Database.Immutable.Read.word32Index', e.g:
--
-- @
-- personIndexes
--   = 'byteStringIndex' #nameAndAgePlusOneIndex
--       (\\p -> 'BC.pack' (name p '<>' 'show' (age p + 1)))
--     'unindexed'
-- @
data Indexes  (indexes :: [(Symbol, *)]) a = Indexes (IO (MapIndexes indexes a))
data Indexes' (indexes :: [(Symbol, *)]) a = Indexes' (MapIndexes indexes a)

--------------------------------------------------------------------------------

data ByteStringIndex a = ByteStringIndex
data Word32Index     a = Word32Index

type family Trd t where
  Trd (a, b, c) = c

type family MapIndexes (indexes :: [(Symbol, *)]) a :: * where
  MapIndexes '[] a = ()
  MapIndexes ('(s, ByteStringIndex b):xs) a = ( MMB.Multimap
                                              , a -> b
                                              , b -> B.ByteString
                                              , MapIndexes xs a
                                              )
  MapIndexes ('(s, Word32Index b):xs)     a = ( MMW.Multimap
                                              , a -> b
                                              , b -> Word32
                                              , MapIndexes xs a
                                              )

class LookupIndex indexes (s :: Symbol) t a | indexes s a -> t where
  lookupIndex :: Indexes' indexes a -> Name s -> t -> IO [Word32]

instance LookupIndex ('(s, Word32Index b):xs) s b a where
  lookupIndex (Indexes' (mm, _, f, _)) _ = MMW.lookup mm . f

instance LookupIndex ('(s, ByteStringIndex b):xs) s b a where
  lookupIndex (Indexes' (mm, _, f, _)) _ = MMB.lookup mm . f

instance {-# OVERLAPPABLE #-}
  ( MapIndexes (x:xs) a ~ (y, u, w, MapIndexes xs a)
  , LookupIndex xs s t a
  ) => LookupIndex (x:xs) s t a where
    lookupIndex (Indexes' (_, _, _, xs))
      = lookupIndex (Indexes' xs :: Indexes' xs a)

class InsertIndex indexes a where
  insertIndex :: Indexes' indexes a -> Word32 -> a -> IO ()

instance InsertIndex '[] a where
  insertIndex _ _ _ = pure ()

instance ( MapIndexes ('(s, Word32Index b) : xs) a ~ (MMW.Multimap, a -> b, b -> Word32, MapIndexes xs a)
         , InsertIndex xs a
         ) => InsertIndex ('(s, Word32Index b):xs) a where
  insertIndex (Indexes' (mm, p, f, xs)) index a = do
    MMW.insert mm (f $ p a) index
    insertIndex (Indexes' xs :: Indexes' xs a) index a

instance ( MapIndexes ('(s, ByteStringIndex b) : xs) a ~ (MMB.Multimap, a -> b, b -> B.ByteString, MapIndexes xs a)
         , InsertIndex xs a
         ) => InsertIndex ('(s, ByteStringIndex b):xs) a where
  insertIndex (Indexes' (mm, p, f, xs)) index a = do
    MMB.insert mm (f $ p a) index
    insertIndex (Indexes' xs :: Indexes' xs a) index a
