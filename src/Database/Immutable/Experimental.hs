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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Immutable.Experimental where

import           Data.Semigroup ((<>))
import           Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Data.Map as M

import qualified GHC.Generics as G
import           GHC.TypeLits (KnownSymbol, AppendSymbol, Symbol, TypeError, ErrorMessage(..), symbolVal)
import qualified Generics.Eot as Eot

-- TODO: autoincrementing ids

data RecordId t = RecordId t deriving Show

type family Id (c :: TableMode) t where
  Id 'Unresolved t = RecordId t
  Id 'Lookup t = String

data ForeignId (c :: TableMode) (table :: Symbol) (field :: Symbol) = ForeignId deriving Show

data EId = EidInt String Int | EidString String String deriving Show

data Person c = Person
  { pid :: Id c Int                                     -- could be turned into (CompanyTables Memory -> Int -> ~(Person Resolved))
  , name :: String
  , friend :: Maybe (ForeignId c "persons2" "pid")       -- could be turned into Maybe ~(Person Resolved); NOTE: must be lazy!
  , employer :: Maybe (ForeignId c "employers" "owner") -- could be turned into Maybe ~(Employer Resolved)
  , pid2 :: Id c String
  } deriving (G.Generic)

deriving instance Show (Person 'Unresolved)
deriving instance Record (Person 'Unresolved)

lookup :: tables 'Memory -> (tables 'Lookup -> Id 'Lookup t) -> t -> a
lookup = undefined

lookupF
  :: 'Just (recordType, fields) ~ Lookup record (ExpandTables (Eot.Eot (tables 'Cannonical))) -- Look up record type from tables
  => tables 'Memory
  -> ForeignId c record field
  -> recordType
lookupF = undefined

-- lookup db (pid . persons) 4

data Employer c = Employer
  { owner :: Id c String
  , address :: String
  , employees :: [ForeignId c "persons" "pid"]  -- could be turned into [~(Person Resolved)]
  } deriving (G.Generic)

deriving instance Show (Employer 'Unresolved)
deriving instance Record (Employer 'Unresolved)

data TableMode = Unresolved | Lookup | Memory | Cannonical

data InternalTable a = InternalTable
  { itIds :: IORef (M.Map String (M.Map EId Int))
  , itRecords :: IORef [a]
  }

type family Table (c :: TableMode) a where
  Table 'Unresolved a = [a 'Unresolved]
  Table 'Memory a = InternalTable (a 'Unresolved)
  Table 'Cannonical a = a 'Unresolved
  Table 'Lookup a = a 'Lookup

data CompanyTables (c :: TableMode) = CompanyTables
  { persons :: Table c Person
  , employers :: Table c Employer
  } deriving (G.Generic, Tables)

data Index c t table

data DBIndexes c = DBIndexes
  { personFriendCountIndex :: Index c Int Person     -- could be turned into (Person r -> Int)      and (DB ... -> Int -> [Person Resolved])
  , employerAddressIndex :: Index c String Employer  -- could be turned into (Employer r -> String) and (DB ... -> String -> [Employer Resolved])
  }

data DB tables indexes = DB
  { dbTables :: tables 'Memory
  , dbIndexes :: indexes -- TODO: indexes shouldn't be part of the database; compute separately
  }

--------------------------------------------------------------------------------

type family ExpandRecord (parent :: Symbol) (record :: *) where
  ExpandRecord parent () = ()
  ExpandRecord parent (Either fields Eot.Void) = ExpandRecord parent fields
  ExpandRecord parent (Eot.Named name (RecordId a), fields)
    = (Eot.Named name a, ExpandRecord parent fields)
  ExpandRecord parent (Eot.Named name b, fields) = ExpandRecord parent fields

type family ExpandTables table where
  ExpandTables () = ()
  ExpandTables (Either records Eot.Void) = ExpandTables records
  ExpandTables (Eot.Named name record, records)
    = ((record, Eot.Named name (ExpandRecord name (Eot.Eot record))), ExpandTables records)

type family Lookup (a :: Symbol) (eot :: *) :: (Maybe *) where
  Lookup name () = TypeError ('Text "Can't lookup symbol in list")
  Lookup name (Eot.Named name a, as) = 'Just a
  Lookup name ((t, Eot.Named name a), as) = 'Just (t, a)
  Lookup name (a, as) = Lookup name as
  Lookup name a = TypeError ('Text "Can't lookup symbol in list")

type family LookupRecordFieldType (record :: Maybe Symbol) (field :: Symbol) (eot :: Maybe *) :: Maybe * where
  LookupRecordFieldType ('Just record) field ('Just eot)
    = LookupRecordFieldType 'Nothing field (Lookup record eot)
  LookupRecordFieldType 'Nothing field ('Just eot) = Lookup field eot
  LookupRecordFieldType a b c = TypeError ('Text "Can't find field in record")

type family Consistent eot where
  Consistent (Either eot Eot.Void) = Consistent eot

--------------------------------------------------------------------------------

class GTables mt it | mt -> it, it -> mt where
  gInsert :: mt -> it -> IO ()

instance GTables () () where
  gInsert () () = pure ()

instance (GTables mt mi) => GTables (Either mt Eot.Void) (Either mi Eot.Void) where
  gInsert (Left mt) (Left mi) = gInsert mt mi
  gInsert _ _ = undefined

-- TODO: consistency checks
-- * already existing ids
-- * referencing non-existent foreign ids
instance (GTables mt mi, Record a) => GTables (Eot.Named name (InternalTable a), mt) (Eot.Named name [a], mi) where
  gInsert (Eot.Named it, mt) (Eot.Named as, mi) = do
    sequence_
      [ modifyIORef (itIds it) undefined -- M.singleton recId M.empty
      | recId <- recIds
      ]
    gInsert mt mi
    where
      recIds =case as of
        (a:_) -> gatherIds a
        _ -> []

class Tables tables where
  insert :: tables 'Memory -> tables 'Unresolved -> IO ()
  default insert
    :: Eot.HasEot (tables 'Memory)
    => Eot.HasEot (tables 'Unresolved)
    => GTables (Eot.Eot (tables 'Memory)) (Eot.Eot (tables 'Unresolved))
    => tables 'Memory
    -> tables 'Unresolved
    -> IO ()
  insert mt mi = gInsert (Eot.toEot mt) (Eot.toEot mi)

--------------------------------------------------------------------------------

class GRecord a where
  gGatherIds :: a -> [EId]

instance GRecord () where
  gGatherIds () = []

-- TODO: we could totally support sum types

instance GRecord a => GRecord (Either a Eot.Void) where
  gGatherIds (Left a) = gGatherIds a
  gGatherIds _ = undefined

instance (GRecord as, KnownSymbol name) => GRecord (Eot.Named name (RecordId String), as) where
  gGatherIds (Eot.Named (RecordId v), as) = EidString (symbolVal (Eot.Proxy :: Eot.Proxy name)) v:gGatherIds as

instance (GRecord as, KnownSymbol name) => GRecord (Eot.Named name (RecordId Int), as) where
  gGatherIds (Eot.Named (RecordId v), as) = EidInt (symbolVal (Eot.Proxy :: Eot.Proxy name)) v:gGatherIds as

instance {-# OVERLAPPABLE #-} GRecord as => GRecord (a, as) where
  gGatherIds (_, as) = gGatherIds as

class Record a where
  gatherIds :: a -> [EId]
  default gatherIds :: Eot.HasEot a => GRecord (Eot.Eot a) => a -> [EId]
  gatherIds a = gGatherIds (Eot.toEot a)

person :: Person 'Unresolved
person = Person
  { pid = RecordId 5
  , name = "bla"
  , friend = Nothing
  , employer = Nothing
  , pid2 = RecordId "yeah"
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

mt :: CompanyTables 'Memory
mt = CompanyTables undefined undefined

ct :: CompanyTables 'Cannonical
ct = CompanyTables undefined undefined

testMain :: IO ()
testMain = do
  personsRef <- newIORef []
  employersRef <- newIORef []


  insert mt $ CompanyTables
    { persons = [person, person]
    , employers = []
    }

  -- ps <- readIORef personsRef
  -- print ps
  pure ()
