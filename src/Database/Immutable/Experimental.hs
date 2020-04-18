{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
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

-- table1
--  id
--  field1
--  field2 --+
--  field3   |
--           |
-- table2    |
--  id <-----+
--  field1
--  field2
--  field3

-- Data model
--   tableName ~> idName ~> idValue ~> index
--   tableName ~> [record]

--------------------------------------------------------------------------------

type family Fst (c :: (*, *)) :: * where
  Fst '(a, b) = a

type family Snd (c :: (*, *)) :: * where
  Snd '(a, b) = b

-- TODO: autoincrementing ids

data RecordMode = Resolved | Unresolved | LookupId

data RecordId t = RecordId t deriving Show

type family Id (tables :: TableMode -> *) (c :: RecordMode) t where
  Id tables 'Resolved t = t
  Id tables 'Unresolved t = RecordId t

data ForeignRecordId = ForeignRecordId deriving Show

type family ForeignId (tables :: TableMode -> *) (c :: RecordMode) (table :: Symbol) (field :: Symbol) where
  ForeignId tables 'Unresolved table field
    = LookupFieldType table field (ExpandTables (Eot.Eot (tables 'Cannonical)))
  ForeignId tables 'Resolved table field
    = LookupTableType table (ExpandTables (Eot.Eot (tables 'Cannonical)))

data EId = EidInt String Int | EidString String String deriving Show

data Person tables c = Person
  { pid :: Id tables c Int                                     -- could be turned into (CompanyTables Memory -> Int -> ~(Person Resolved))
  , name :: String
  , friend :: Maybe (ForeignId tables c "persons" "pid")       -- could be turned into Maybe ~(Person Resolved); NOTE: must be lazy!
  , employer :: Maybe (ForeignId tables c "employers" "owner") -- could be turned into Maybe ~(Employer Resolved)
  , pid2 :: Id tables c String
  } deriving (G.Generic)

deriving instance Show (Person CompanyTables 'Unresolved)
deriving instance Record (Person CompanyTables 'Unresolved)

-- lookup :: Id tables 'LookupId t -> t -> tables 'Memory -> a
-- lookup = undefined

--------------------------------------------------------------------------------

-- lookupForeign
--   :: (recordType, fields) ~ Lookup record (ExpandTables (Eot.Eot (tables 'Cannonical))) -- Look up record type from tables
--   => tables 'Memory
--   -> ForeignId tables c record field
--   -> recordType
-- lookupForeign = undefined

-- lookup db (pid . persons) 4

data Employer tables c = Employer
  { owner :: Id tables c String
  , address :: String
  , employees :: [ForeignId tables c "persons" "pid"]  -- could be turned into [~(Person Resolved)]
  } deriving (G.Generic)

deriving instance Show (Employer CompanyTables 'Unresolved)
deriving instance Record (Employer CompanyTables 'Unresolved)

data TableMode = Lookup | Memory | Insert | Cannonical

data InternalTable a = InternalTable
  { itIds :: IORef (M.Map String (M.Map EId Int))
  , itRecords :: IORef [a]
  }

data LookupFn a tables = forall t. LookupFn (RecordId t -> t -> a tables 'Resolved)

type family Table tables (c :: TableMode) a where
  Table tables 'Memory a = InternalTable (a tables 'Unresolved)
  Table tables 'Cannonical a = a tables 'Unresolved
  Table tables 'Insert a = [a tables 'Unresolved]
  Table tables 'Lookup a = LookupFn a tables

data CompanyTables (c :: TableMode) = CompanyTables
  { persons :: Table CompanyTables c Person
  , employers :: Table CompanyTables c Employer
  } deriving (G.Generic, Tables)

-- data Index c t table

-- data DBIndexes c = DBIndexes
--   { personFriendCountIndex :: Index c Int Person     -- could be turned into (Person r -> Int)      and (DB ... -> Int -> [Person Resolved])
--   , employerAddressIndex :: Index c String Employer  -- could be turned into (Employer r -> String) and (DB ... -> String -> [Employer Resolved])
--   }
-- 
-- data DB tables indexes = DB
--   { dbTables :: tables 'Memory
--   , dbIndexes :: indexes -- TODO: indexes shouldn't be part of the database; compute separately
--   }

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
  -- TODO: this expects a particular table format, e.g. Person table tableMode
  ExpandTables (Eot.Named name (table tables c), records)
    = ((table tables 'Resolved, Eot.Named name (ExpandRecord name (Eot.Eot (table tables c)))), ExpandTables records)

type family Lookup (a :: Symbol) (eot :: *) :: (*, *) where
  Lookup name () = TypeError ('Text "Can't lookup symbol in list")
  Lookup name (Eot.Named name a, as) = '((), a)
  Lookup name ((t, Eot.Named name a), as) = '(t, a)
  Lookup name (a, as) = Lookup name as
  Lookup name a = TypeError ('Text "Can't lookup symbol in list")

type family LookupTableType (table :: Symbol) (eot :: *) :: * where
  LookupTableType table eot = Fst (Lookup table eot)

type family LookupFieldType (table :: Symbol) (field :: Symbol) (eot :: *) :: * where
  LookupFieldType table field eot = Snd (Lookup field (Snd (Lookup table eot)))

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

person :: Person CompanyTables 'Unresolved
person = Person
  { pid = RecordId 5
  , name = "bla"
  , friend = Just 5
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
