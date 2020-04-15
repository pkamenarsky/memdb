{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Immutable.Experimental where

import Data.Semigroup ((<>))
import Data.IORef (IORef, modifyIORef)

import qualified GHC.Generics as G
import qualified Generics.Eot as Eot

data R = Unresolved | Resolved

data Id t = Id t
data ForeignId (c :: R) t name a

data Person c = Person
  { pid :: Id Int
  , name :: String
  , friend :: Maybe (ForeignId c Int "pid" Person)          -- could be turned into Maybe (Person Resolved)
  , employer :: Maybe (ForeignId c String "owner" Employer) -- could be turned into Maybe (Employer Resolved)
  , pid2 :: Id String
  } deriving (G.Generic, Record)

data Employer c = Employer
  { owner :: Id String
  , address :: String
  , employees :: [ForeignId c Int "pid" Person]  -- could be turned into [Person]
  }

data TableMode = Insert | Memory | Lookup

type family Table (c :: TableMode) a where
  Table 'Insert a = [a 'Unresolved]
  Table 'Memory a = IORef [a 'Unresolved]

data CompanyTables c = CompanyTables
  { persons :: Table c Person  -- could be turned into (DB ... -> #name -> Person Resolved)
  , employers :: Table c Employer
  } deriving (G.Generic)

data Index c t table

data DBIndexes c = DBIndexes
  { personFriendCountIndex :: Index c Int Person     -- could be turned into (Person r -> Int)      and (DB ... -> Int -> [Person Resolved])
  , employerAddressIndex :: Index c String Employer  -- could be turned into (Employer r -> String) and (DB ... -> String -> [Employer Resolved])
  }

data DB tables indexes = DB
  { dbTables :: tables 'Memory
  , dbIndexes :: indexes
  }

insert :: DB tables indexes -> tables 'Insert -> IO ()
insert = undefined

--------------------------------------------------------------------------------

class GTables f tables where
  gInsert :: f (tables 'Memory) -> f (tables 'Insert) -> IO ()

-- instance GTables f => GTables (G.D1 c f) where
--   gInsert (G.M1 m) (G.M1 i) = gInsert m i

instance GTables (G.Rec0 c) tables where
  gInsert (G.K1 m) (G.K1 i) = modifyIORef m (i <>)

-- instance (G.Selector c, f ~ k p) => GTables (G.S1 c f) where
--   gInsert (G.M1 ref) (G.M1 a) = modifyIORef ref (a <>)

--------------------------------------------------------------------------------

data EId = EidInt String Int | EidString String String deriving Show

class GRecord a where
  gGatherIds :: [String] -> a -> [EId]

instance GRecord () where
  gGatherIds _ () = []

-- TODO: we can totally support sum types
instance GRecord a => GRecord (Either a Eot.Void) where
  gGatherIds fields (Left a) = gGatherIds fields a
  gGatherIds _ _ = undefined

instance GRecord as => GRecord (Id String, as) where
  gGatherIds (field:fields) ((Id v), as) = EidString field v:gGatherIds fields as

instance GRecord as => GRecord (Id Int, as) where
  gGatherIds (field:fields) ((Id v), as) = EidInt field v:gGatherIds fields as

instance {-# OVERLAPPABLE #-} GRecord as => GRecord (a, as) where
  gGatherIds (_:fields) (_, as) = gGatherIds fields as

class Record a where
  gatherIds :: a -> [EId]
  default gatherIds :: Eot.HasEot a => GRecord (Eot.Eot a) => a -> [EId]
  gatherIds a = gGatherIds fields (Eot.toEot a)
    where
      (Eot.Datatype _ (Eot.Constructor _ (Eot.Selectors fields):_)) = Eot.datatype (Eot.Proxy :: Eot.Proxy a)

person :: Person 'Unresolved
person = Person
  { pid = Id 5
  , name = "bla"
  , friend = Nothing
  , employer = Nothing
  , pid2 = Id "yeah"
  }

test = gatherIds person

companyTables :: DB CompanyTables ()
companyTables = undefined

testInsert = insert companyTables $ CompanyTables
  { persons = []
  , employers = []
  }
