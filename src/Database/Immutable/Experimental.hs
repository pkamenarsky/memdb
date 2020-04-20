{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
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

import           Control.DeepSeq (NFData (rnf))

import qualified Data.ByteString as B
import           Data.Foldable
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import           Data.Semigroup (Sum (Sum), Last (Last))
import qualified Data.Serialize as S
import           Data.Serialize (Serialize)
import           Data.Semigroup ((<>))
import qualified Data.Set as S
import           Data.Word (Word64)

import qualified GHC.Generics as G
import           GHC.TypeLits (KnownSymbol, Symbol, TypeError, ErrorMessage(..), symbolVal)
import qualified Generics.Eot as Eot
import           Generics.Eot (Eot, HasEot, Named (Named), Void, Proxy (..), fromEot, toEot)

import           Prelude hiding (id, lookup)

-- DONE: absolute/relative ids

-- TODO: batches
-- TODO: consistency checks (must be done on insert, want early abort/error reporting)
-- TODO: record <-> table naming
-- TODO: [leveldb] don't read table sizes while inserting, store sizes in MVar (db can't be opened multiple times)

--------------------------------------------------------------------------------

type family Fst a where
  Fst '(a, b) = a

type family Snd a where
  Snd '(a, b) = b

--------------------------------------------------------------------------------

type TableName = String
type FieldName = String

data RecordMode = Resolved | Unresolved | forall table. LookupId table | Done

data RecordId t where
  Id :: t -> RecordId t
  RelativeId :: Word64 -> RecordId Word64

deriving instance Show t => Show (RecordId t)

instance Serialize t => Serialize (RecordId t) where
  put (Id t) = S.put t
  put (RelativeId t) = S.put t

  get = Id <$> S.get

instance NFData t => NFData (RecordId t) where
  rnf (Id t) = rnf t
  rnf (RelativeId t) = rnf t

data LookupFns tables table k = LookupFns
  { lookup :: k -> Maybe (table tables 'Resolved)
  }

type family Id (tables :: TableMode -> *) (recordMode :: RecordMode) t where
  Id tables 'Done t = RecordId t
  Id tables 'Resolved t = t
  Id tables 'Unresolved t = RecordId t
  Id tables ('LookupId table) t = LookupFns tables table t

data Lazy tables a = Lazy
  { get :: a tables 'Resolved
  }

instance Show (Lazy tables a) where
  show _ = "Lazy"

data ForeignRecordId (table :: Symbol) (field :: Symbol) t where
  ForeignId :: t -> ForeignRecordId table field t
  ForeignRelativeId :: Word64 -> ForeignRecordId table field Word64

deriving instance Show t => Show (ForeignRecordId table field t)

instance Serialize t => Serialize (ForeignRecordId table field t) where
  put (ForeignId t) = S.put t
  put (ForeignRelativeId t) = S.put t

  get = ForeignId <$> S.get

instance NFData t => NFData (ForeignRecordId table field t) where
  rnf (ForeignId t) = rnf t
  rnf (ForeignRelativeId t) = rnf t

type family ForeignId (tables :: TableMode -> *) (recordMode :: RecordMode) (table :: Symbol) (field :: Symbol) where
  ForeignId tables 'Done table field = ()
  ForeignId tables 'Unresolved table field = ForeignRecordId
    table
    field
    (LookupFieldType field (Snd (LookupTableType table (Eot (tables 'Cannonical)))))
  ForeignId tables 'Resolved table field = Lazy
    tables
    (Fst (LookupTableType table (Eot (tables 'Cannonical))))
  ForeignId tables ('LookupId table') table field = ()

-- Backend ---------------------------------------------------------------------

data InsertOptions
  = OverwriteDuplicates
  | ErrorOnDuplicates

type SerializedTable = (TableName, [([ERecordId], B.ByteString)])

class Backend backend where
  type Snapshot backend

  -- TODO: always force, don't let backend decide
  withSnapshot
    :: NFData a
    => backend
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

  insertTables
    :: backend
    -> InsertOptions
    -> [EForeignId]
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

instance (GResolve us rs) => GResolve (Named x (RecordId u), us) (Named x u, rs) where
  gResolve db snapshot (Named (Id u), us) = (Named u, gResolve db snapshot us)
  gResolve db snapshot (Named (RelativeId u), us) = (Named u, gResolve db snapshot us)

instance
  ( Serialize u
  , Serialize (r tables 'Unresolved)

  , Resolve tables r
  , GResolve us rs

  , KnownSymbol table
  , KnownSymbol field
  ) =>
  GResolve (Named x (ForeignRecordId table field u), us) (Named x (Lazy tables r), rs) where
    gResolve db snapshot (Named (ForeignId k), us)
      = ( Named $ Lazy $ fromJust $ lookupRecord db snapshot (symbolVal (Proxy :: Proxy table)) (symbolVal (Proxy :: Proxy field)) k
        , gResolve db snapshot us
        )
    gResolve _ _ (Named (ForeignRelativeId _), _) = error "gResolve: ForeignRelativeId"

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
    gResolve db snapshot (Named k', us) =
      ( Named $ flip fmap k'
          $ \(ForeignId k) -> Lazy $ fromJust $ lookupRecord db snapshot (symbolVal (Proxy :: Proxy table)) (symbolVal (Proxy :: Proxy field)) k
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

data EForeignId
  = EForeignId TableName FieldName B.ByteString
  | EForeignRelativeId TableName FieldName Word64
  deriving Show

data EId
  = EId FieldName B.ByteString
  | ERelativeId FieldName Word64
  deriving Show

data ERecordId = EId' EId | EForeignId' EForeignId
  deriving Show

class GGatherIds u where
  gGatherIds :: u -> [ERecordId]

instance GGatherIds () where
  gGatherIds _ = []

instance GGatherIds u => GGatherIds (Either u Void) where
  gGatherIds (Left u) = gGatherIds u
  gGatherIds _ = undefined

instance GGatherIds us => GGatherIds (Named field u, us) where
  gGatherIds (_, us) = gGatherIds us

instance {-# OVERLAPPING #-}
  ( Serialize t
  , Show t

  , GGatherIds us

  , KnownSymbol field
  ) =>
  GGatherIds (Named field (RecordId t), us) where
    gGatherIds (Named (Id t), us) = EId' (EId (symbolVal (Proxy :: Proxy field)) (S.runPut (S.put t))):gGatherIds us
    gGatherIds (Named (RelativeId t), us) = EId' (ERelativeId (symbolVal (Proxy :: Proxy field)) t):gGatherIds us

instance {-# OVERLAPPING #-}
  ( Serialize t
  , Show t

  , GGatherIds us

  , KnownSymbol table
  , KnownSymbol field
  ) =>
  GGatherIds (Named field' (ForeignRecordId table field t), us) where
    gGatherIds (Named (ForeignId t), us)
      = EForeignId' (EForeignId (symbolVal (Proxy :: Proxy table)) (symbolVal (Proxy :: Proxy field)) (S.runPut (S.put t))):gGatherIds us
    gGatherIds (Named (ForeignRelativeId t), us)
      = EForeignId' (EForeignRelativeId (symbolVal (Proxy :: Proxy table)) (symbolVal (Proxy :: Proxy field)) t):gGatherIds us

instance {-# OVERLAPPING #-}
  ( Serialize t
  , Show t

  , GGatherIds us

  , Foldable f

  , KnownSymbol table
  , KnownSymbol field
  ) =>
  GGatherIds (Named field' (f (ForeignRecordId table field t)), us) where
    gGatherIds (Named f, us)
      = map (EForeignId' . EForeignId (symbolVal (Proxy :: Proxy table)) (symbolVal (Proxy :: Proxy field)) . S.runPut . S.put) (toList f) <> gGatherIds us

class GatherIds (tables :: TableMode -> *) u where
  gatherIds :: u tables 'Unresolved -> [ERecordId]
  default gatherIds
    :: HasEot (u tables 'Unresolved)
    => GGatherIds (Eot (u tables 'Unresolved))
    => u tables 'Unresolved
    -> [ERecordId]
  gatherIds = gGatherIds . toEot

-- Insert ----------------------------------------------------------------------

class GInsertTables t where
  gInsert :: Backend db => [SerializedTable] -> db -> InsertOptions -> t -> IO ()

instance GInsertTables () where
  gInsert srs db opts _ = do
    case opts of
      ErrorOnDuplicates
        | not (null duplicateIds) -> error "gInsert: duplicate ids"
      _ -> pure ()

    insertTables db opts missingFids srs
    where
      allEids :: [(TableName, ERecordId)]
      allEids = 
        [ (table, eid)
        | (table, records) <- srs
        , (eids, _) <- records
        , eid <- eids
        ]

      ids :: [(EId, (TableName, FieldName, Bool, B.ByteString))]
      ids = mconcat
        [ case eid of
            EId' id@(EId field k) -> [(id, (table, field, True,  S.runPut (S.put k)))]
            EId' id@(ERelativeId field k) -> [(id, (table, field, False, S.runPut (S.put k)))]
            _ -> []
        | (table, eid) <- allEids
        ]

      idsCount :: M.Map (TableName, FieldName, Bool, B.ByteString) (Sum Int, Last EId)
      idsCount = M.fromListWith (<>)
        [ (k, (Sum 1, Last id))
        | (id, k) <- ids
        ]

      duplicateIds :: [EId]
      duplicateIds =
        [ id
        | (Sum count, Last id) <- M.elems idsCount
        , count > 1
        ]

      fids :: [(EForeignId, (TableName, FieldName, Bool, B.ByteString))]
      fids = mconcat
        [ case eid of
            EForeignId' fid@(EForeignId table field k) -> [(fid, (table, field, True, S.runPut (S.put k)))]
            EForeignId' fid@(EForeignRelativeId table field k) -> [(fid, (table, field, False, S.runPut (S.put k)))]
            _ -> []
        | (_, eid) <- allEids
        ]

      missingFids :: [EForeignId]
      missingFids =
        [ fid
        | (fid, bs) <- fids
        , not (bs `S.member` idSet)
        ]
        where
          idSet = S.fromList (fmap snd ids)

instance GInsertTables t => GInsertTables (Either t Void) where
  gInsert srs db opts (Left t) = gInsert srs db opts t
  gInsert _ _ _ _ = undefined

instance
  ( Serialize (r tables 'Unresolved)

  , GatherIds tables r
  , GInsertTables ts

  , KnownSymbol table
  ) =>
  GInsertTables (Named table [r tables 'Unresolved], ts) where
    -- TODO: consistency checks; keysExist
    gInsert srs db opts (Named records, ts) = do
      gInsert ((symbolVal (Proxy :: Proxy table), srsTable):srs) db opts ts
      where
        srsTable = [ (gatherIds r, S.runPut (S.put r)) | r <- records ]

class InsertTables (t :: TableMode -> *) where
  insert :: Backend db => db -> InsertOptions -> t 'Batch -> IO ()
  default insert
    :: Backend db
    => HasEot (t 'Batch)
    => GInsertTables (Eot (t 'Batch))
    => db
    -> InsertOptions
    -> t 'Batch
    -> IO ()
  insert db opts = gInsert [] db opts . toEot

-- Expand ----------------------------------------------------------------------

type family ExpandRecord (parent :: Symbol) (record :: *) where
  ExpandRecord parent () = ()
  ExpandRecord parent (Either fields Eot.Void) = ExpandRecord parent fields
  ExpandRecord parent (Eot.Named name (RecordId a), fields) = (Eot.Named name a, ExpandRecord parent fields)
  ExpandRecord parent (a, fields) = ExpandRecord parent fields

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

data TableMode = Lookup | Batch | Cannonical

type family Table (tables :: TableMode -> *) (c :: TableMode) a where
  Table tables 'Cannonical a = a tables 'Done

  Table tables 'Batch a = [a tables 'Unresolved]
  Table tables 'Lookup a = a tables ('LookupId a)

--------------------------------------------------------------------------------

data Person tables m = Person
  { name :: String
  , friend :: Maybe (ForeignId tables m "persons" "pid")       -- could be turned into Maybe ~(Person Resolved); NOTE: must be lazy!
  , employer :: Maybe (ForeignId tables m "employers" "owner") -- could be turned into Maybe ~(Employer Resolved)
  , pid :: Id tables m Word64
  , pid2 :: Id tables m String
  } deriving (G.Generic, Resolve CompanyTables, LookupById CompanyTables, GatherIds CompanyTables)

deriving instance Show (Person CompanyTables 'Unresolved)
deriving instance Show (Person CompanyTables 'Resolved)
deriving instance Serialize (Person CompanyTables 'Unresolved)

data MaybeList a = MaybeList [Maybe a]
  deriving (Functor, Foldable, G.Generic, Serialize, Show)

data Employer tables m = Employer
  { address :: String
  , employees :: MaybeList (ForeignId tables m "persons" "pid")  -- could be turned into [~(Person Resolved)]
  , owner :: Id tables m String
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
  { pid = RelativeId 5
  , name = "bla"
  , friend = Just $ ForeignId 5 -- own best friend
  , employer = Just $ ForeignId "boss"
  , pid2 = Id "pid2"
  }

employerU :: Employer CompanyTables 'Unresolved
employerU = Employer
  { owner = Id "boss"
  , address = "thug mansion"
  , employees = MaybeList [Just $ ForeignId 5]
  }

personR :: Person CompanyTables 'Resolved
personR = undefined

-- personR' :: Person CompanyTables 'Resolved
-- personR' = resolve undefined personU

personL :: Person CompanyTables ('LookupId Person)
personL = undefined

-- companyLookups :: CompanyTables 'Lookup
-- companyLookups = lookupTables

companyI :: CompanyTables 'Batch
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
