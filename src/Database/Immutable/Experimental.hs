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
{-# LANGUAGE RankNTypes #-}
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

import           Prelude hiding (lookup)

-- TODO: autoincrementing ids
-- TODO: record <-> table naming

--------------------------------------------------------------------------------

type family Fst a where
  Fst '(a, b) = a

type family Snd a where
  Snd '(a, b) = b

--------------------------------------------------------------------------------

type TableName = String
type FieldName = String

data RecordMode = Resolved | Unresolved | forall table. LookupId table | Done

data RecordId t = Id t
  deriving (Show, G.Generic, Serialize)

data LookupFns tables table k = LookupFns
  { lookup :: k -> Maybe (table tables 'Resolved)
  }

type family Id (tables :: TableMode -> *) (recordMode :: RecordMode) t where
  Id tables 'Done t = RecordId t
  Id tables 'Resolved t = RecordId t
  Id tables 'Unresolved t = RecordId t
  Id tables ('LookupId table) t = LookupFns tables table t

data Lazy tables a = Lazy
  { get :: a tables 'Resolved
  }

instance Show (Lazy tables a) where
  show _ = "Lazy"

data ForeignRecordId (table :: Symbol) (field :: Symbol) t = ForeignId t
  deriving (Show, G.Generic, Serialize)

type family ForeignId (tables :: TableMode -> *) (recordMode :: RecordMode) (table :: Symbol) (field :: Symbol) where
  ForeignId tables 'Done table field = TypeError ('Text "ForeignId: Done")
  ForeignId tables 'Unresolved table field = ForeignRecordId
    table
    field
    (LookupFieldType field (Snd (LookupTableType table (Eot (tables 'Cannonical)))))
  ForeignId tables 'Resolved table field = Lazy
    tables
    (Fst (LookupTableType table (Eot (tables 'Cannonical))))
  ForeignId tables ('LookupId table') table field = ()

-- Backend ---------------------------------------------------------------------

type SerializedTable = (TableName, [([EId], B.ByteString)])

class Backend backend where
  type Snapshot backend

  withSnapshot
    :: backend
    -> (Snapshot backend -> a)
    -> IO a
  lookupRecord
    :: Serialize k
    => Serialize (v tables 'Unresolved)
    => Resolve tables v
  
    => backend
    -> Snapshot backend

    -> TableName
    -> FieldName
    -> k
    -> Maybe (v tables 'Resolved)
  resolveField
    :: Serialize k
    => Serialize (v tables 'Unresolved)
    => Resolve tables v
  
    => backend
    -> Snapshot backend

    -> TableName
    -> FieldName
    -> ForeignRecordId table field k
    -> Lazy tables v
  insertRecord
    :: backend
    -> [SerializedTable]
    -> IO ()

-- LookupById ------------------------------------------------------------------

class GLookupById r where
  gLookupById :: Backend db => db -> Snapshot db -> TableName -> r

instance GLookupById () where
  gLookupById _ _ _ = ()

instance GLookupById r => GLookupById (Either r Void) where
  gLookupById db snapshot = Left . gLookupById db snapshot

instance (GLookupById rs) => GLookupById (Named x r, rs) where
  gLookupById db snapshot table = (undefined, gLookupById db snapshot table)

instance {-# OVERLAPPING #-}
  ( Serialize k
  , Serialize (table tables 'Unresolved)

  , Resolve tables table
  , GLookupById rs

  , KnownSymbol field
  ) =>
  GLookupById (Named field (LookupFns tables table k), rs) where
    gLookupById db snapshot table
      = ( Named $ LookupFns
            { lookup = \k -> (lookupRecord db snapshot table (symbolVal (Proxy :: Proxy field)) k)
            }
        , gLookupById db snapshot table
        )

class LookupById tables (r :: (TableMode -> *) -> RecordMode -> *) where
  lookupById :: Backend db => db -> Snapshot db -> TableName -> r tables ('LookupId r)
  default lookupById
    :: HasEot (r tables ('LookupId r))
    => GLookupById (Eot (r tables ('LookupId r)))
    => Backend db
    => db
    -> Snapshot db
    -> TableName
    -> r tables ('LookupId r)
  lookupById db snapshot = fromEot . gLookupById db snapshot

-- Lookup ----------------------------------------------------------------------

class GLookupTables t where
  gLookupTables :: Backend db => db -> Snapshot db -> t

instance GLookupTables () where
  gLookupTables _ _ = ()

instance GLookupTables t => GLookupTables (Either t Void) where
  gLookupTables db = Left . gLookupTables db

instance
  ( LookupById tables t
  , GLookupTables ts

  , KnownSymbol name
  ) =>
  GLookupTables (Named name (t tables ('LookupId t)), ts) where
    gLookupTables db snapshot
      = ( Named $ lookupById db snapshot (symbolVal (Proxy :: Proxy name))
        , gLookupTables db snapshot
        )

class LookupTables (t :: TableMode -> *) where
  lookupTables :: Backend db => db -> Snapshot db -> t 'Lookup
  default lookupTables
    :: HasEot (t 'Lookup)
    => Backend db
    => GLookupTables (Eot (t 'Lookup))
    => db
    -> Snapshot db
    -> t 'Lookup
  lookupTables db = fromEot . gLookupTables db

-- Resolve ---------------------------------------------------------------------

class GResolve u r where
  gResolve :: Backend db => db -> Snapshot db -> u -> r

instance GResolve () () where
  gResolve _ _ () = ()

instance GResolve u r => GResolve (Either u Void) (Either r Void) where
  gResolve db snapshot (Left u) = Left $ gResolve db snapshot u 
  gResolve _ _ _ = undefined

instance (GResolve us rs) => GResolve (Named x u, us) (Named x u, rs) where
  gResolve db snapshot (u, us) = (u, gResolve db snapshot us)

instance
  ( Serialize u
  , Serialize (r tables 'Unresolved)

  , Resolve tables r
  , GResolve us rs

  , KnownSymbol table
  , KnownSymbol field
  ) =>
  GResolve (Named x (ForeignRecordId table field u), us) (Named x (Lazy tables r), rs) where
    gResolve db snapshot (Named u, us)
      = ( Named $ resolveField db snapshot (symbolVal (Proxy :: Proxy table)) (symbolVal (Proxy :: Proxy field)) u
        , gResolve db snapshot us
        )

instance
  ( Serialize u
  , Serialize (r tables 'Unresolved)

  , Resolve tables r
  , GResolve us rs

  , Functor f

  , KnownSymbol table
  , KnownSymbol field
  ) =>
  GResolve (Named x (f (ForeignRecordId table field u)), us) (Named x (f (Lazy tables r)), rs) where
    gResolve db snapshot (Named u, us) =
      ( Named $ flip fmap u
          $ \fid -> resolveField db snapshot (symbolVal (Proxy :: Proxy table)) (symbolVal (Proxy :: Proxy field)) fid
      , gResolve db snapshot us
      )

class Resolve (tables :: TableMode -> *) u where
  resolve :: Backend db => db -> Snapshot db -> u tables 'Unresolved -> u tables 'Resolved
  default resolve
    :: HasEot (u tables 'Unresolved)
    => HasEot (u tables 'Resolved)
    => GResolve (Eot (u tables 'Unresolved)) (Eot (u tables 'Resolved))
    => Backend db
    => db
    -> Snapshot db
    -> u tables 'Unresolved
    -> u tables 'Resolved
  resolve db snapshot = fromEot . gResolve db snapshot . toEot

-- GatherIds -------------------------------------------------------------------

data EId
  = forall t. Serialize t => EId FieldName t
  | forall t. Serialize t => EForeignId TableName FieldName t

class GGatherIds u where
  gGatherIds :: u -> [EId]

instance GGatherIds () where
  gGatherIds _ = []

instance GGatherIds u => GGatherIds (Either u Void) where
  gGatherIds (Left u) = gGatherIds u
  gGatherIds _ = undefined

instance GGatherIds us => GGatherIds (Named field u, us) where
  gGatherIds (_, us) = gGatherIds us

instance {-# OVERLAPPING #-}
  ( Serialize t

  , GGatherIds us

  , KnownSymbol field
  ) =>
  GGatherIds (Named field (RecordId t), us) where
    gGatherIds (Named (Id t), us) = EId (symbolVal (Proxy :: Proxy field)) t:gGatherIds us

instance {-# OVERLAPPING #-}
  ( Serialize t

  , GGatherIds us

  , KnownSymbol table
  , KnownSymbol field
  ) =>
  GGatherIds (Named field' (ForeignRecordId table field t), us) where
    gGatherIds (Named (ForeignId t), us)
      = EForeignId (symbolVal (Proxy :: Proxy table)) (symbolVal (Proxy :: Proxy field)) t:gGatherIds us

class GatherIds (tables :: TableMode -> *) u where
  gatherIds :: u tables 'Unresolved -> [EId]
  default gatherIds
    :: HasEot (u tables 'Unresolved)
    => GGatherIds (Eot (u tables 'Unresolved))
    => u tables 'Unresolved
    -> [EId]
  gatherIds = gGatherIds . toEot

-- Insert ----------------------------------------------------------------------

class GInsertTables t where
  gInsert :: Backend db => [SerializedTable] -> db -> t -> IO ()

instance GInsertTables () where
  gInsert srs db _ = insertRecord db srs

instance GInsertTables t => GInsertTables (Either t Void) where
  gInsert srs db (Left t) = gInsert srs db t
  gInsert _ _ _ = undefined

instance
  ( Serialize (r tables 'Unresolved)

  , GatherIds tables r
  , GInsertTables ts

  , KnownSymbol table
  ) =>
  GInsertTables (Named table [r tables 'Unresolved], ts) where
    -- TODO: consistency checks
    gInsert srs db (Named records, ts) = do
      gInsert ((symbolVal (Proxy :: Proxy table), srsTable):srs) db ts
      where
        srsTable = [ (gatherIds r, S.runPut (S.put r)) | r <- records ]

class InsertTables (t :: TableMode -> *) where
  insert :: Backend db => db -> t 'Insert -> IO ()
  default insert
    :: Backend db
    => HasEot (t 'Insert)
    => GInsertTables (Eot (t 'Insert))
    => db
    -> t 'Insert
    -> IO ()
  insert db = gInsert [] db . toEot

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
  LookupTableType name (Eot.Named name (record tables recordMode), records)
    = '(record, ExpandRecord name (Eot (record tables 'Done)))
  LookupTableType name (Eot.Named otherName (record tables recordMode), records)
    = LookupTableType name records
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
deriving instance Show (Person CompanyTables 'Resolved)
deriving instance Serialize (Person CompanyTables 'Unresolved)

data Employer tables m = Employer
  { owner :: Id tables m String
  , address :: String
  , employees :: [ForeignId tables m "persons" "pid"]  -- could be turned into [~(Person Resolved)]
  } deriving (G.Generic, Resolve CompanyTables, LookupById CompanyTables, GatherIds CompanyTables)

deriving instance Show (Employer CompanyTables 'Unresolved)
deriving instance Show (Employer CompanyTables 'Resolved)
deriving instance Serialize (Employer CompanyTables 'Unresolved)

data CompanyTables m = CompanyTables
  { persons :: Table CompanyTables m Person
  , employers :: Table CompanyTables m Employer
  } deriving (G.Generic, LookupTables, InsertTables)

--------------------------------------------------------------------------------

personU :: Person CompanyTables 'Unresolved
personU = Person
  { pid = Id 5
  , name = "bla"
  , friend = Just $ ForeignId 5 -- own best friend
  , employer = Just $ ForeignId "boss"
  , pid2 = Id "pid2"
  }

employerU :: Employer CompanyTables 'Unresolved
employerU = Employer
  { owner = Id "boss"
  , address = "thug mansion"
  , employees = [ForeignId 5]
  }

personR :: Person CompanyTables 'Resolved
personR = undefined

-- personR' :: Person CompanyTables 'Resolved
-- personR' = resolve undefined personU

personL :: Person CompanyTables ('LookupId Person)
personL = undefined

-- companyLookups :: CompanyTables 'Lookup
-- companyLookups = lookupTables

companyI :: CompanyTables 'Insert
companyI = CompanyTables
  { persons = [personU]
  , employers = [employerU]
  }

-- test = do
--   db <- newIORef (DB M.empty)
--   -- insert db companyI
-- 
--   dbf <- readIORef db
-- 
--   let Just p = (pid $ persons companyLookups) dbf 5
--   let Just p2 = (pid2 $ persons companyLookups) dbf "pid2"
-- 
--   print p
--   print $ fmap (show . get) $ employer p
--   print $ fmap (show . get) $ friend p
--   print p2
