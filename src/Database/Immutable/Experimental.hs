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
import           Generics.Eot (Eot, Void, Proxy)

-- TODO: autoincrementing ids
-- TODO: record <-> table naming

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

type family Fst a where
  Fst '(a, b) = a

type family Snd a where
  Snd '(a, b) = b

--------------------------------------------------------------------------------

data RecordMode = Resolved | Unresolved | LookupId

data RecordId t = RecordId t deriving Show

type family Id (tables :: TableMode -> *) (c :: RecordMode) t where
  Id tables 'Resolved t = t
  Id tables 'Unresolved t = RecordId t

data Lazy tables a = Lazy { resolve :: a tables 'Resolved }

type family ForeignId (tables :: TableMode -> *) (c :: RecordMode) (table :: Symbol) (field :: Symbol) where
  ForeignId tables 'Unresolved table field = LookupFieldType field (Snd (LookupTableType table (Eot (tables 'Cannonical))))
  ForeignId tables 'Resolved table field = Lazy tables (Fst (LookupTableType table (Eot (tables 'Cannonical))))

-- Expand ----------------------------------------------------------------------

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
    = ((record, Eot.Named name (ExpandRecord name (Eot record))), ExpandTables records)

type family Lookup (a :: Symbol) (eot :: *) :: (*, *) where
  Lookup name () = TypeError ('Text "Can't lookup symbol in list (())")
  Lookup name (Eot.Named name a, as) = '((), a)
  Lookup name ((t, Eot.Named name a), as) = '(t, a)
  Lookup name (a, as) = Lookup name as
  Lookup name a = TypeError ('Text "Can't lookup symbol in list (a)")

type family LookupTableType (table :: Symbol) (eot :: *) :: (((TableMode -> *) -> RecordMode -> *), *) where
  LookupTableType name (Either records Eot.Void) = LookupTableType name records
  LookupTableType name (Eot.Named name (record tables tableMode), records) = '(record, records)
  LookupTableType name (Eot.Named otherName (record tables tableMode), records) = LookupTableType name records
  LookupTableType name eot = TypeError ('Text "Can't lookup table type")

type family LookupFieldType (field :: Symbol) (eot :: *) :: * where
  LookupFieldType name (Either records Eot.Void) = LookupFieldType name records
  LookupFieldType name (Eot.Named name field, fields) = field
  LookupFieldType name (Eot.Named otherName field, fields) = LookupFieldType name fields
  LookupFieldType name eot = TypeError ('Text "Can't lookup field type")

-- Table -----------------------------------------------------------------------

data TableMode = Lookup | Memory | Insert | Cannonical

data LookupFn a tables = forall t. LookupFn (RecordId t -> t -> a tables 'Resolved)

data Break a

type family Table (tables :: TableMode -> *) (c :: TableMode) a where
  Table tables 'Cannonical a = a tables 'Resolved -- break recursion here

  Table tables 'Insert a = [a tables 'Unresolved]
  Table tables 'Lookup a = LookupFn a tables

--------------------------------------------------------------------------------

data Person tables c = Person
  { pid :: Id tables c Int                                     -- could be turned into (CompanyTables Memory -> Int -> ~(Person Resolved))
  , name :: String
  , friend :: Maybe (ForeignId tables c "persons" "pid")       -- could be turned into Maybe ~(Person Resolved); NOTE: must be lazy!
  , employer :: Maybe (ForeignId tables c "employers" "owner") -- could be turned into Maybe ~(Employer Resolved)
  , pid2 :: Id tables c String
  } deriving (G.Generic)

-- can't have tables be ExpandTables (Eot tables) here, since we can't derive Show etc
deriving instance Show (Person CompanyTables 'Unresolved)

data Employer tables c = Employer
  { owner :: Id tables c String
  , address :: String
  , employees :: [ForeignId tables c "persons" "pid"]  -- could be turned into [~(Person Resolved)]
  } deriving (G.Generic)

deriving instance Show (Employer CompanyTables 'Unresolved)

data CompanyTables (c :: TableMode) = CompanyTables
  { persons :: Table CompanyTables c Person
  , employers :: Table CompanyTables c Employer
  } deriving (G.Generic)

--------------------------------------------------------------------------------

personU :: Person CompanyTables 'Unresolved
personU = undefined

personR :: Person CompanyTables 'Resolved
personR = undefined
