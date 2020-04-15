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
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)

import qualified GHC.Generics as G
import qualified Generics.Eot as Eot

data R = Unresolved | Resolved

data Id t = Id t deriving Show
data ForeignId (c :: R) t name a = ForeignId deriving Show

data Person c = Person
  { pid :: Id Int
  , name :: String
  , friend :: Maybe (ForeignId c Int "pid" Person)          -- could be turned into Maybe (Person Resolved)
  , employer :: Maybe (ForeignId c String "owner" Employer) -- could be turned into Maybe (Employer Resolved)
  , pid2 :: Id String
  } deriving (Show, G.Generic, Record)

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
  } deriving (G.Generic, Tables)

data Index c t table

data DBIndexes c = DBIndexes
  { personFriendCountIndex :: Index c Int Person     -- could be turned into (Person r -> Int)      and (DB ... -> Int -> [Person Resolved])
  , employerAddressIndex :: Index c String Employer  -- could be turned into (Employer r -> String) and (DB ... -> String -> [Employer Resolved])
  }

data DB tables indexes = DB
  { dbTables :: tables 'Memory
  , dbIndexes :: indexes
  }

--------------------------------------------------------------------------------

class GTables mt it where
  gInsert :: mt -> it -> IO ()

instance GTables () () where
  gInsert () () = pure ()

instance (GTables mt mi) => GTables (Either mt Eot.Void) (Either mi Eot.Void) where
  gInsert (Left mt) (Left mi) = gInsert mt mi
  gInsert _ _ = undefined

instance (GTables mt mi) => GTables (IORef [a], mt) ([a], mi) where
  gInsert (ref, mt) (as, mi) = modifyIORef ref (as <>) >> gInsert mt mi

class Tables tables where
  insert :: tables 'Memory -> tables 'Insert -> IO ()
  default insert
    :: Eot.HasEot (tables 'Memory)
    => Eot.HasEot (tables 'Insert)
    => GTables (Eot.Eot (tables 'Memory)) (Eot.Eot (tables 'Insert))
    => tables 'Memory
    -> tables 'Insert
    -> IO ()
  insert mt mi = gInsert (Eot.toEot mt) (Eot.toEot mi)

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
  gGatherIds _ _ = undefined

instance GRecord as => GRecord (Id Int, as) where
  gGatherIds (field:fields) ((Id v), as) = EidInt field v:gGatherIds fields as
  gGatherIds _ _ = undefined

instance {-# OVERLAPPABLE #-} GRecord as => GRecord (a, as) where
  gGatherIds (_:fields) (_, as) = gGatherIds fields as
  gGatherIds _ _ = undefined

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

test :: [EId]
test = gatherIds person

companyTables :: CompanyTables 'Memory
companyTables = CompanyTables
  { persons = undefined
  , employers = undefined
  }

testInsert :: IO ()
testInsert = insert companyTables $ CompanyTables
  { persons = []
  , employers = []
  }

testMain :: IO ()
testMain = do
  personsRef <- newIORef []
  employersRef <- newIORef []

  let mt = CompanyTables personsRef employersRef

  insert mt $ CompanyTables
    { persons = [person, person]
    , employers = []
    }

  ps <- readIORef personsRef
  print ps
