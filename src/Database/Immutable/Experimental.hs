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

import qualified Data.ByteString as B
import           Data.Maybe (fromJust)
import qualified Data.Serialize as S
import           Data.Serialize (Serialize)
import           Data.Semigroup ((<>))
import           Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Data.Map as M

import qualified GHC.Generics as G
import           GHC.TypeLits (KnownSymbol, AppendSymbol, Symbol, TypeError, ErrorMessage(..), symbolVal)
import qualified Generics.Eot as Eot
import           Generics.Eot (Eot, HasEot, Named (Named), Void, Proxy (..), fromEot, toEot)

-- TODO: autoincrementing ids
-- TODO: k, v (instead of u, v)
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

data DB = DB (M.Map String (M.Map String (M.Map B.ByteString B.ByteString)))

data RecordMode = Resolved | Unresolved | forall table. LookupId table | Done

data RecordId t = Id t deriving (Show, G.Generic, Serialize)

type family Id (tables :: TableMode -> *) (c :: RecordMode) t where
  Id tables 'Done t = RecordId t
  Id tables 'Resolved t = RecordId t
  Id tables 'Unresolved t = RecordId t
  Id tables ('LookupId table) t = DB -> t -> Maybe (table tables 'Resolved)

data Lazy tables a = Lazy { get :: a tables 'Resolved }

data ForeignRecordId (table :: Symbol) (field :: Symbol) t = ForeignId { getFid :: t } deriving (Show, G.Generic, Serialize)

type family ForeignId (tables :: TableMode -> *) (c :: RecordMode) (table :: Symbol) (field :: Symbol) where
  -- ForeignId tables 'Done table field = TypeError ('Text "ForeignId: Done")
  ForeignId tables 'Unresolved table field = ForeignRecordId table field (LookupFieldType field (Snd (LookupTableType table (Eot (tables 'Cannonical)))))
  ForeignId tables 'Resolved table field = Lazy tables (Fst (LookupTableType table (Eot (tables 'Cannonical))))
  ForeignId tables ('LookupId table') table field = ()

-- LookupById ------------------------------------------------------------------

lookupRecord :: Serialize k => Serialize (v tables 'Unresolved) => Resolve tables v => DB -> String -> String -> k -> Maybe (v tables 'Resolved)
lookupRecord db@(DB tables) table field k = do
  t <- M.lookup table tables
  m <- M.lookup field t
  v <- M.lookup (S.runPut $ S.put k) m
  case S.runGet S.get v of
    Left e -> error e
    Right r -> pure $ resolve db r

class GLookupById r where
  gLookupById :: String -> r

instance GLookupById () where
  gLookupById _ = ()

instance GLookupById r => GLookupById (Either r Void) where
  gLookupById = Left . gLookupById

instance (GLookupById rs) => GLookupById (Named x r, rs) where
  gLookupById table = (undefined, gLookupById table)

instance {-# OVERLAPPING #-} (Serialize k, Serialize (table tables 'Unresolved), Resolve tables table, KnownSymbol field, GLookupById rs) =>
  GLookupById (Named field (DB -> k -> Maybe (table tables 'Resolved)), rs) where
    gLookupById table = (Named $ \db k -> (lookupRecord db table (symbolVal (Proxy :: Proxy field)) k), gLookupById table)

class LookupById tables (r :: (TableMode -> *) -> RecordMode -> *) where
  lookupById :: String -> r tables ('LookupId r)
  default lookupById
    :: HasEot (r tables ('LookupId r))
    => GLookupById (Eot (r tables ('LookupId r)))
    => String
    -> r tables ('LookupId r)
  lookupById = fromEot . gLookupById

-- Lookup ----------------------------------------------------------------------

class GLookupTables t where
  gLookupTables :: t

instance GLookupTables () where
  gLookupTables = ()

instance GLookupTables t => GLookupTables (Either t Void) where
  gLookupTables = Left gLookupTables

instance (KnownSymbol name, LookupById tables t, GLookupTables ts) => GLookupTables (Named name (t tables ('LookupId t)), ts) where
  gLookupTables = (Named $ lookupById (symbolVal (Proxy :: Proxy name)), gLookupTables)

class LookupTables (t :: TableMode -> *) where
  lookupTables :: t 'Lookup
  default lookupTables
    :: HasEot (t 'Lookup)
    => GLookupTables (Eot (t 'Lookup))
    => t 'Lookup
  lookupTables = fromEot gLookupTables

-- Resolve ---------------------------------------------------------------------

resolveField :: Serialize k => Serialize (v tables 'Unresolved) => Resolve tables v => DB -> String -> String -> ForeignRecordId table field k -> Lazy tables v
resolveField db@(DB tables) table field (ForeignId k) = fromJust $ do
  t <- M.lookup table tables
  m <- M.lookup field t
  v <- M.lookup (S.runPut $ S.put k) m
  case S.runGet S.get v of
    Left e -> error e
    Right r -> pure $ Lazy (resolve db r)

class GResolve u r where
  gResolve :: DB -> u -> r

instance GResolve () () where
  gResolve _ () = ()

instance GResolve u r => GResolve (Either u Void) (Either r Void) where
  gResolve db (Left u) = Left $ gResolve db u
  gResolve _ _ = undefined

instance (GResolve us rs) => GResolve (Named x u, us) (Named x u, rs) where
  gResolve db (u, us) = (u, gResolve db us)

instance (Serialize u, Serialize (r tables 'Unresolved), Resolve tables r, GResolve us rs, KnownSymbol table, KnownSymbol field) => GResolve (Named x (ForeignRecordId table field u), us) (Named x (Lazy tables r), rs) where
  gResolve db (Named u, us) = (Named $ resolveField db (symbolVal (Proxy :: Proxy table)) (symbolVal (Proxy :: Proxy field)) u, gResolve db us)

instance (Serialize u, Serialize (r tables 'Unresolved), Resolve tables r, Functor f, GResolve us rs, KnownSymbol table, KnownSymbol field) => GResolve (Named x (f (ForeignRecordId table field u)), us) (Named x (f (Lazy tables r)), rs) where
  gResolve db (Named u, us) =
    ( Named $ flip fmap u
        $ \fid -> resolveField db (symbolVal (Proxy :: Proxy table)) (symbolVal (Proxy :: Proxy field)) fid
    , gResolve db us
    )

class Resolve (tables :: TableMode -> *) u where
  resolve :: DB -> u tables 'Unresolved -> u tables 'Resolved
  default resolve
    :: HasEot (u tables 'Unresolved)
    => HasEot (u tables 'Resolved)
    => GResolve (Eot (u tables 'Unresolved)) (Eot (u tables 'Resolved))
    => DB
    -> u tables 'Unresolved
    -> u tables 'Resolved
  resolve db = fromEot . gResolve db . toEot

-- Insert ----------------------------------------------------------------------

data EId
  = forall t. Serialize t => EId String t
  | forall t. Serialize t => EForeignId String String t

class GGatherIds u where
  gGatherIds :: u -> [EId]

instance GGatherIds () where
  gGatherIds _ = []

instance GGatherIds u => GGatherIds (Either u Void) where
  gGatherIds (Left u) = gGatherIds u
  gGatherIds _ = undefined

instance GGatherIds us => GGatherIds (Named field u, us) where
  gGatherIds (_, us) = gGatherIds us

instance {-# OVERLAPPING #-} (KnownSymbol field, Serialize t, GGatherIds us) => GGatherIds (Named field (RecordId t), us) where
  gGatherIds (Named (Id t), us) = EId (symbolVal (Proxy :: Proxy field)) t:gGatherIds us

instance {-# OVERLAPPING #-} (KnownSymbol table, KnownSymbol field, Serialize t, GGatherIds us) => GGatherIds (Named field' (ForeignRecordId table field t), us) where
  gGatherIds (Named (ForeignId t), us) = EForeignId (symbolVal (Proxy :: Proxy table)) (symbolVal (Proxy :: Proxy field)) t:gGatherIds us

class GatherIds (tables :: TableMode -> *) u where
  gatherIds :: u tables 'Unresolved -> [EId]
  default gatherIds
    :: HasEot (u tables 'Unresolved)
    => GGatherIds (Eot (u tables 'Unresolved))
    => u tables 'Unresolved
    -> [EId]
  gatherIds = gGatherIds . toEot

-- Map -------------------------------------------------------------------------

type family IfJust (a :: Maybe *) (b :: *) :: * where
  IfJust ('Just a) b = a
  IfJust a b = b

type family MapEot (f :: * -> Maybe b) (eot :: *) :: b where
  MapEot f () = TypeError ('Text "MapEot: ()")
  MapEot f (Either a Void) = MapEot f a
  MapEot f (a, as) = IfJust (f a) (MapEot f as)

-- Expand ----------------------------------------------------------------------

type family ExpandRecord (parent :: Symbol) (record :: *) where
  ExpandRecord parent () = ()
  ExpandRecord parent (Either fields Eot.Void) = ExpandRecord parent fields
  ExpandRecord parent (Eot.Named name (RecordId a), fields)
    = (Eot.Named name a, ExpandRecord parent fields)
  ExpandRecord parent (Eot.Named otherName a, fields)
    = ExpandRecord parent fields

-- eot :: = Either
--  (Named "persons" (Person CompanyTables 'Resolved),
--   (Named "employers" (Employer CompanyTables 'Resolved), ()))
--  Void
type family LookupTableType (table :: Symbol) (eot :: *) :: (((TableMode -> *) -> RecordMode -> *), *) where
  LookupTableType name (Either records Eot.Void) = LookupTableType name records
  LookupTableType name (Eot.Named name (record tables recordMode), records) = '(record, ExpandRecord name (Eot (record tables 'Done)))
  LookupTableType name (Eot.Named otherName (record tables recordMode), records) = LookupTableType name records
  LookupTableType name eot = TypeError ('Text "Can't lookup table type")

type family LookupFieldType (field :: Symbol) (eot :: *) :: * where
  LookupFieldType name (Either records Eot.Void) = LookupFieldType name records
  LookupFieldType name (Eot.Named name field, fields) = field
  LookupFieldType name (Eot.Named otherName field, fields) = LookupFieldType name fields
  LookupFieldType name eot = TypeError ('Text "Can't lookup field type")

-- Table -----------------------------------------------------------------------

data TableMode = Lookup | Insert | Cannonical

type family Table (tables :: TableMode -> *) (c :: TableMode) a where
  Table tables 'Cannonical a = a tables 'Done

  Table tables 'Insert a = [a tables 'Unresolved]
  Table tables 'Lookup a = a tables ('LookupId a)

--------------------------------------------------------------------------------

data Person tables m = Person
  { pid :: Id tables m Int                                     -- could be turned into (CompanyTables Memory -> Int -> ~(Person Resolved))
  , name :: String
  , friend :: Maybe (ForeignId tables m "persons" "pid")       -- could be turned into Maybe ~(Person Resolved); NOTE: must be lazy!
  , employer :: Maybe (ForeignId tables m "employers" "owner") -- could be turned into Maybe ~(Employer Resolved)
  , pid2 :: Id tables m String
  } deriving (G.Generic, Resolve CompanyTables, LookupById CompanyTables, GatherIds CompanyTables)

deriving instance Show (Person CompanyTables 'Unresolved)
deriving instance Serialize (Person CompanyTables 'Unresolved)

data Employer tables m = Employer
  { owner :: Id tables m String
  , address :: String
  , employees :: [ForeignId tables m "persons" "pid"]  -- could be turned into [~(Person Resolved)]
  } deriving (G.Generic, Resolve CompanyTables, LookupById CompanyTables, GatherIds CompanyTables)

deriving instance Show (Employer CompanyTables 'Unresolved)
deriving instance Serialize (Employer CompanyTables 'Unresolved)

data CompanyTables m = CompanyTables
  { persons :: Table CompanyTables m Person
  , employers :: Table CompanyTables m Employer
  } deriving (G.Generic, LookupTables)

--------------------------------------------------------------------------------

personU :: Person CompanyTables 'Unresolved
personU = undefined

personR :: Person CompanyTables 'Resolved
personR = undefined

personR' :: Person CompanyTables 'Resolved
personR' = resolve undefined personU

personL :: Person CompanyTables ('LookupId Person)
personL = undefined

companyL :: CompanyTables 'Lookup
companyL = lookupTables
