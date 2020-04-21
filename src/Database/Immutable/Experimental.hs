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
import           Data.Foldable (Foldable, toList)
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

import           Prelude hiding (id, lookup, length)

-- DONE: absolute/relative ids
-- DONE: [leveldb] don't read table sizes while inserting, store sizes in MVar (db can't be opened multiple times)
-- DONE: consistency checks (must be done on insert, want early abort/error reporting)

-- TODO: batches
-- TODO: simplify lookupRecord (only operate on ByteStrings, keep Backend simple)

-- TODO: error -> MonadError
-- TODO: record <-> table naming

--------------------------------------------------------------------------------

serialize :: S.Serialize a => a -> B.ByteString
serialize = S.runPut . S.put

type family Fst a where
  Fst '(a, b) = a

type family Snd a where
  Snd '(a, b) = b

--------------------------------------------------------------------------------

type TableName = String
type FieldName = String

data RecordMode = Resolved | Unresolved | forall table. LookupField table | Done

data RecordId t where
  Id :: t -> RecordId t
  RelativeId :: Word64 -> RecordId Word64

deriving instance Show t => Show (RecordId t)

instance Serialize t => Serialize (RecordId t) where
  put (Id t) = S.put t
  put (RelativeId _) = error "put: RelativeId"

  get = Id <$> S.get

instance NFData t => NFData (RecordId t) where
  rnf (Id t) = rnf t
  rnf (RelativeId t) = rnf t

data LookupFns tables table k = LookupFns
  { lookup :: k -> Maybe (table tables 'Resolved)
  , elems :: [(k, table tables 'Resolved)]
  }

type family Id (tables :: TableMode -> *) (recordMode :: RecordMode) t where
  Id tables 'Done t = RecordId t
  Id tables 'Resolved t = t
  Id tables 'Unresolved t = RecordId t
  Id tables ('LookupField table) t = LookupFns tables table t

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
  put (ForeignRelativeId _) = error "put: ForeignRelativeId"

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
  ForeignId tables ('LookupField table') table field = ()

-- Backend ---------------------------------------------------------------------

data InsertOptions
  = OverwriteDuplicateIndexes
    -- ^ This will only overwrite the current indexes and point them at the new
    -- record; the old record will still remain in the database and be accessible
    -- when reading the batch it was added in.
  | ErrorOnDuplicateIndexes
    -- ^ Don't allow overwriting already existing indexes.

type SerializedTable = (TableName, [([EId], B.ByteString)])

class Backend backend where
  type Snapshot backend

  -- TODO: always force, don't let backend decide
  withSnapshot
    :: NFData a
    => backend
    -> (Snapshot backend -> a)
    -> IO a

  lookupRecord
    :: backend
    -> Snapshot backend

    -> TableName
    -> FieldName
    -> B.ByteString
    -> Maybe B.ByteString

  lookupElems
    :: backend
    -> Snapshot backend

    -> TableName
    -> FieldName
    -> [(B.ByteString, B.ByteString)]

  insertTables
    :: backend
    -> InsertOptions
    -> (OffsetMap ->
          ( [(TableName, FieldName, B.ByteString)]
          , [SerializedTable]
          )
       )
    -> IO ()

unsafeLookupRecord
  :: Serialize k
  => Serialize (v tables 'Unresolved)
  => Resolve tables v
  => Backend backend

  => backend
  -> Snapshot backend

  -> TableName
  -> FieldName
  -> k
  -> Maybe (v tables 'Resolved)
unsafeLookupRecord db snapshot table field k
  = case S.runGet S.get <$> lookupRecord db snapshot table field (serialize k) of
      Just (Right v) -> Just (resolve db snapshot v)
      _ -> Nothing

-- LookupField ------------------------------------------------------------------

class GLookupField r where
  gLookupById :: Backend db => db -> Snapshot db -> TableName -> r

instance GLookupField () where
  gLookupById _ _ _ = ()

instance GLookupField r => GLookupField (Either r Void) where
  gLookupById db snapshot = Left . gLookupById db snapshot

instance (GLookupField rs) => GLookupField (Named x r, rs) where
  gLookupById db snapshot table
    = ( error "Ordinary fields are undefined for lookups"
      , gLookupById db snapshot table
      )

instance {-# OVERLAPPING #-}
  ( Serialize k
  , Serialize (table tables 'Unresolved)

  , Resolve tables table
  , GLookupField rs

  , KnownSymbol field
  ) =>
  GLookupField (Named field (LookupFns tables table k), rs) where
    gLookupById db snapshot table
      = ( Named $ LookupFns
            { lookup = \k -> unsafeLookupRecord db snapshot table (symbolVal (Proxy :: Proxy field)) k
            , elems =
                [ (k', resolve db snapshot v')
                | (k, v) <- lookupElems db snapshot table (symbolVal (Proxy :: Proxy field))
                , Right k' <- [ S.runGet S.get k ]
                , Right v' <- [ S.runGet S.get v ]
                ]
            }
        , gLookupById db snapshot table
        )

class LookupField tables (r :: (TableMode -> *) -> RecordMode -> *) where
  lookupField :: Backend db => db -> Snapshot db -> TableName -> r tables ('LookupField r)
  default lookupField
    :: HasEot (r tables ('LookupField r))
    => GLookupField (Eot (r tables ('LookupField r)))
    => Backend db
    => db
    -> Snapshot db
    -> TableName
    -> r tables ('LookupField r)
  lookupField db snapshot = fromEot . gLookupById db snapshot

-- Lookup ----------------------------------------------------------------------

class GLookupFields t where
  gLookupFields :: Backend db => db -> Snapshot db -> t

instance GLookupFields () where
  gLookupFields _ _ = ()

instance GLookupFields t => GLookupFields (Either t Void) where
  gLookupFields db = Left . gLookupFields db

instance
  ( LookupField tables t
  , GLookupFields ts

  , KnownSymbol table
  ) =>
  GLookupFields (Named table (t tables ('LookupField t)), ts) where
    gLookupFields db snapshot
      = ( Named $ lookupField db snapshot (symbolVal (Proxy :: Proxy table))
        , gLookupFields db snapshot
        )

class LookupFields (t :: TableMode -> *) where
  lookupFields :: Backend db => db -> Snapshot db -> t 'LookupFields
  default lookupFields
    :: HasEot (t 'LookupFields)
    => Backend db
    => GLookupFields (Eot (t 'LookupFields))
    => db
    -> Snapshot db
    -> t 'LookupFields
  lookupFields db = fromEot . gLookupFields db

-- Resolve ---------------------------------------------------------------------

class GResolve u r where
  gResolve :: Backend db => db -> Snapshot db -> u -> r

instance GResolve () () where
  gResolve _ _ () = ()

instance GResolve Void Void where
  gResolve _ _ _ = undefined

instance (GResolve u r, GResolve t s) => GResolve (Either u t) (Either r s) where
  gResolve db snapshot (Left u) = Left $ gResolve db snapshot u 
  gResolve db snapshot (Right u) = Right $ gResolve db snapshot u 

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
      = ( Named $ Lazy $ fromJust $ unsafeLookupRecord
            db
            snapshot
            (symbolVal (Proxy :: Proxy table))
            (symbolVal (Proxy :: Proxy field))
            k
        , gResolve db snapshot us
        )
    gResolve _ _ (Named (ForeignRelativeId _), _) = error "gResolve: ForeignRelativeId"

instance
  ( Serialize (r tables 'Unresolved)

  , Resolve tables r
  , GResolve us rs
  ) =>
  GResolve (Named x (r tables 'Unresolved), us) (Named x (r tables 'Resolved), rs) where
    gResolve db snapshot (Named u, us)
      = ( Named $ resolve db snapshot u
        , gResolve db snapshot us
        )

instance
  ( Serialize (r tables 'Unresolved)

  , Functor f

  , Resolve tables r
  , GResolve us rs
  ) =>
  GResolve (Named x (f (r tables 'Unresolved)), us) (Named x (f (r tables 'Resolved)), rs) where
    gResolve db snapshot (Named u, us)
      = ( Named $ fmap (resolve db snapshot) u
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
    gResolve db snapshot (Named k', us) =
      ( Named $ flip fmap k' $ \(ForeignId k) ->
          Lazy $ fromJust $ unsafeLookupRecord
            db
            snapshot
            (symbolVal (Proxy :: Proxy table))
            (symbolVal (Proxy :: Proxy field))
            k
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

type OffsetMap = M.Map String Word64

absolutizeId
  :: Serialize t
  => Show t

  => OffsetMap
  -> TableName
  -> FieldName
  -> RecordId t
  -> (EId, RecordId t)
absolutizeId _ _ field (Id t) = (EId field t, Id t)
absolutizeId offsets table field (RelativeId t)
  = case M.lookup table offsets of
      Just offset -> 
        ( EAbsolutizedId field (offset + t)
        , Id (offset + t)
        )
      Nothing -> 
        ( EAbsolutizedId field t
        , Id t
        )

absolutizeForeignId
  :: forall table field t. Serialize t
  => Show t
  => KnownSymbol table
  => KnownSymbol field

  => OffsetMap
  -> ForeignRecordId table field t
  -> (EId, ForeignRecordId table field t)
absolutizeForeignId _ (ForeignId t)
  = ( EForeignId (symbolVal (Proxy :: Proxy table)) (symbolVal (Proxy :: Proxy field)) t
    , ForeignId t
    )
absolutizeForeignId offsets (ForeignRelativeId t)
  = case M.lookup (symbolVal (Proxy :: Proxy table)) offsets of
      Just offset ->
        ( EForeignAbsolutizedId (symbolVal (Proxy :: Proxy table)) (symbolVal (Proxy :: Proxy field)) (offset + t)
        , ForeignId (offset + t)
        )
      Nothing ->
        ( EForeignAbsolutizedId (symbolVal (Proxy :: Proxy table)) (symbolVal (Proxy :: Proxy field)) t
        , ForeignId t
        )

data EId
  = forall t. (Show t, Serialize t) => EId FieldName t
  | EAbsolutizedId FieldName Word64
  | forall t. (Show t, Serialize t) => EForeignId TableName FieldName t
  | EForeignAbsolutizedId TableName FieldName Word64

deriving instance Show EId

class GGatherIds u where
  gGatherIds :: TableName -> OffsetMap -> u -> ([EId], u)

instance GGatherIds () where
  gGatherIds _ _ () = ([], ())

instance GGatherIds Void where
  gGatherIds _ _ _ = undefined

instance (GGatherIds u, GGatherIds v) => GGatherIds (Either u v) where
  gGatherIds table offsets (Left u) = Left <$> (gGatherIds table offsets u)
  gGatherIds table offsets (Right v) = Right <$> (gGatherIds table offsets v)

instance GGatherIds us => GGatherIds (Named field u, us) where
  gGatherIds table offsets (u, us) = (,) <$> pure u <*> gGatherIds table offsets us

instance {-# OVERLAPPING #-}
  ( Serialize t
  , Show t

  , GGatherIds us

  , KnownSymbol field
  ) =>
  GGatherIds (Named field (RecordId t), us) where
    gGatherIds table offsets (Named id, us) = (eid:eids, (Named id', rs))
      where
        (eids, rs) = gGatherIds table offsets us
        (eid, id') = absolutizeId offsets table (symbolVal (Proxy :: Proxy field)) id

instance {-# OVERLAPPING #-}
  ( Serialize t
  , Show t

  , GGatherIds us

  , KnownSymbol table
  , KnownSymbol field
  ) =>
  GGatherIds (Named field' (ForeignRecordId table field t), us) where
    gGatherIds table offsets (Named fid, us) = (eid:eids, (Named fid', rs))
      where
        (eids, rs) = gGatherIds table offsets us
        (eid, fid') = absolutizeForeignId offsets fid

instance {-# OVERLAPPING #-}
  ( Serialize t
  , Show t

  , GGatherIds us

  , Foldable f
  , Functor f

  , KnownSymbol table
  , KnownSymbol field
  ) =>
  GGatherIds (Named field' (f (ForeignRecordId table field t)), us) where
    gGatherIds table offsets (Named f, us) = (fids <> eids, (Named f', rs))
      where
        (eids, rs) = gGatherIds table offsets us
        f' = fmap (snd . absolutizeForeignId offsets) f
        fids = fmap (fst . absolutizeForeignId offsets) (toList f)

class GatherIds (tables :: TableMode -> *) u where
  gatherIds :: TableName -> OffsetMap -> u tables 'Unresolved -> ([EId], u tables 'Unresolved)
  default gatherIds
    :: HasEot (u tables 'Unresolved)
    => GGatherIds (Eot (u tables 'Unresolved))
    => TableName
    -> OffsetMap
    -> u tables 'Unresolved
    -> ([EId], u tables 'Unresolved)
  gatherIds table offsets u = (eids, fromEot r)
    where
      (eids, r) = gGatherIds table offsets (toEot u)

-- Insert tables ---------------------------------------------------------------

class GInsertTables t where
  gInsert
    :: InsertOptions
    -> t
    -> [SerializedTable]
    -> OffsetMap
    -> ( [(TableName, FieldName, B.ByteString)] -- ^ Missing absolute foreign ids
       , [SerializedTable]                      -- ^ Absolutized, serialized tables
       )

instance GInsertTables () where
  gInsert opts () srs _
    | not (null missingRelFids) = error $ "Consistency violation (gInsert): missing relative foreign ids: " <> show (ids, fids)
    | otherwise = case opts of
        ErrorOnDuplicateIndexes
          | not (null duplicateIds) -> error "Consistency violation (gInsert): duplicate ids"
        _ -> (missingAbsFids, srs)
    where
      allEids :: [(TableName, EId)]
      allEids = 
        [ (table, eid)
        | (table, records) <- srs
        , (eids, _) <- records
        , eid <- eids
        ]

      ids :: [(EId, (TableName, FieldName, Bool, B.ByteString))]
      ids = mconcat
        [ case eid of
            id@(EId field k) -> [(id, (table, field, True, serialize k))]
            id@(EAbsolutizedId field k) -> [(id, (table, field, False, serialize k))]
            _ -> []
        | (table, eid) <- allEids
        ]

      idsCount :: M.Map (TableName, FieldName, Bool, B.ByteString) (Sum Int, Last EId)
      idsCount = M.fromListWith (<>)
        [ (k, (Sum 1, Last id))
        | (id, k) <- ids
        ]

      duplicateIds :: [EId]
      duplicateIds = [ id | (Sum count, Last id) <- M.elems idsCount, count > 1 ]

      fids :: [(EId, (TableName, FieldName, Bool, B.ByteString))]
      fids = mconcat
        [ case eid of
            eid'@(EForeignId table field k) -> [(eid', (table, field, True, serialize k))]
            eid'@(EForeignAbsolutizedId table field k) -> [(eid', (table, field, False, serialize k))]
            _ -> []
        | (_, eid) <- allEids
        ]

      missingFids :: [(TableName, FieldName, Bool, B.ByteString)]
      missingFids = S.toList $ S.fromList (fmap snd fids) `S.difference` S.fromList (fmap snd ids)

      missingAbsFids = [ (table, field, k) | (table, field, True, k) <- missingFids ]
      missingRelFids = [ fid | fid@(_, _, False, _) <- missingFids ]

instance GInsertTables t => GInsertTables (Either t Void) where
  gInsert opts (Left t) srs offsets = gInsert opts t srs offsets 
  gInsert _ _ _ _ = undefined

instance
  ( Serialize (r tables 'Unresolved)

  , GatherIds tables r
  , GInsertTables ts

  , KnownSymbol table
  ) =>
  GInsertTables (Named table [r tables 'Unresolved], ts) where
    gInsert opts (Named records, ts) srs offsets 
      = gInsert opts ts ((table, srsTable):srs) offsets 
      where
        table = symbolVal (Proxy :: Proxy table)
        srsTable = [ serialize <$> gatherIds table offsets r | r <- records ]

class InsertTables (t :: TableMode -> *) where
  -- | Consistency checks:
  --
  -- * All relative foreign ids must point to records inside the batch
  -- * Absolute foreign ids not pointing to records inside the batch must point to
  --   records already in the backend
  -- * When `ErrorOnDuplicateIndexes` is specified, no duplicate indexes are allowed
  --   (both in the batch and across the database)
  insert :: Backend db => db -> InsertOptions -> t 'Batch -> IO ()
  default insert
    :: Backend db
    => HasEot (t 'Batch)
    => GInsertTables (Eot (t 'Batch))
    => db
    -> InsertOptions
    -> t 'Batch
    -> IO ()
  insert db opts batch = insertTables db opts (gInsert opts (toEot batch) [])

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

data TableMode = LookupFields | Batch | Cannonical

type family Table (tables :: TableMode -> *) (c :: TableMode) table where
  Table tables 'Cannonical table = table tables 'Done

  Table tables 'Batch table = [table tables 'Unresolved]
  Table tables 'LookupFields table = table tables ('LookupField table)

--------------------------------------------------------------------------------

data Person tables m = Person
  { name :: String
  , friend :: Maybe (ForeignId tables m "persons" "pid")       -- could be turned into Maybe ~(Person Resolved); NOTE: must be lazy!
  , employer :: Maybe (ForeignId tables m "employers" "owner") -- could be turned into Maybe ~(Employer Resolved)
  , pid :: Id tables m Word64
  , pid2 :: Id tables m String
  , other :: Employer tables m
  } deriving (G.Generic, Resolve CompanyTables, LookupField CompanyTables, GatherIds CompanyTables)

deriving instance Show (Person CompanyTables 'Unresolved)
deriving instance Show (Person CompanyTables 'Resolved)
deriving instance Serialize (Person CompanyTables 'Unresolved)

data MaybeList a = MaybeList [Maybe a]
  deriving (Functor, Foldable, G.Generic, Serialize, Show)

data Employer tables m = Employer
  { address :: String
  , employees :: MaybeList (ForeignId tables m "persons" "pid")  -- could be turned into [~(Person Resolved)]
  , owner :: Id tables m String
  } deriving (G.Generic, Resolve CompanyTables, LookupField CompanyTables, GatherIds CompanyTables)

deriving instance Show (Employer CompanyTables 'Unresolved)
deriving instance Show (Employer CompanyTables 'Resolved)
deriving instance Serialize (Employer CompanyTables 'Unresolved)

data CompanyTables m = CompanyTables
  { persons :: Table CompanyTables m Person
  , employers :: Table CompanyTables m Employer
  } deriving (G.Generic, LookupFields, InsertTables)

--------------------------------------------------------------------------------

personU :: Person CompanyTables 'Unresolved
personU = Person
  { pid = RelativeId 0
  , name = "bla"
  , friend = Just $ ForeignRelativeId 0 -- own best friend
  , employer = Just $ ForeignId "boss"
  , pid2 = Id "pid2"
  , other = Employer
      { owner = Id "boss2"
      , address = "yeyea"
      , employees = MaybeList []
      }
  }

employerU :: Employer CompanyTables 'Unresolved
employerU = Employer
  { owner = Id "boss"
  , address = "thug mansion"
  , employees = MaybeList [Just $ ForeignRelativeId 0]
  }

personR :: Person CompanyTables 'Resolved
personR = undefined

-- personR' :: Person CompanyTables 'Resolved
-- personR' = resolve undefined personU

personL :: Person CompanyTables ('LookupField Person)
personL = undefined

-- companyLookups :: CompanyTables 'Lookup
-- companyLookups = lookupFields

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
