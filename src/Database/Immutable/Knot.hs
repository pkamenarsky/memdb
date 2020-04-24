{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Immutable.Knot where

import           Control.DeepSeq (NFData (rnf))

import qualified Data.ByteString as B
import           Data.Foldable (Foldable, toList)
import qualified Data.Map as M
import qualified Data.Map.Monoidal as MM
import           Data.Maybe (fromJust, fromMaybe)
import           Data.Semigroup (Semigroup, Sum (Sum), Last (Last))
import qualified Data.Serialize as S
import           Data.Serialize (Serialize)
import           Data.Semigroup ((<>))
import           Data.Traversable (sequenceA)
import           Data.Validation (Validation (..), bindValidation)
import qualified Data.Set as S
import           Data.Word (Word64)

import qualified GHC.Generics as G
import           GHC.TypeLits (KnownSymbol, Symbol, TypeError, ErrorMessage(..), symbolVal)
import qualified Generics.Eot as Eot
import           Generics.Eot (Eot, HasEot, Named (Named), Void, Proxy (..), fromEot, toEot)

import           Unsafe.Coerce (unsafeCoerce)

import           Prelude hiding (lookup, length)
import           Debug.Trace

-- TODO: test: resolve with same ids always resolves to last record

-- TODO: Resolve ForeignRelativeId, etc
-- TODO: nest tables

-- TODO: Map -> HashMap

-- TODO: resolve with internal batch ids, then go to db
-- TODO: ResolveError

-- TODO: ReadBatches returns only 'Unresolved tables, use ResolveTables to resolve

-- TODO: LookupFns on table level? no danger of undefined fields...

-- TODO: Absolute / Relative ids on type level

-- TODO: do we need ErrorOnDuplicateIndexes? standard behaviour is just to overwrite

-- TODO: can we derive multiple classes at once? or combine everything into a single class? however, not all classes are desirable every time (i.e. nested records don't need InsertTables, etc)

-- TODO: 3 concepts:
--   * in place editing, no concept of batch reading, batches only for insert consistency
--   * append only, delete happens by mapping & filtering
--   * reactive LookupFns
--     * keys subscriptions easy
--     * range subscriptions also, if restricted to e.g. between and startsWith
--   * can we unify? at least the Table concept seems transferable; maybe split into combination of Backend classes?

-- TODO: split Backend typeclass into lookup and insert

-- TODO: error -> MonadError
-- TODO: record <-> table naming

-- DONE: absolute/relative ids
-- DONE: [leveldb] don't read table sizes while inserting, store sizes in MVar (db can't be opened multiple times)
-- DONE: consistency checks (must be done on insert, want early abort/error reporting)
-- DONE: simplify lookupRecord (only operate on ByteStrings, keep Backend simple)
-- DONE: batches
-- DONE: insert -> insertTables
-- DONE: ModifyTables

--------------------------------------------------------------------------------

serialize :: S.Serialize a => a -> B.ByteString
serialize = S.runPut . S.put

unsafeParse :: S.Serialize a => B.ByteString -> a
unsafeParse = either (error "unsafeParse") id . S.runGet S.get

type family Fst a where
  Fst '(a, b) = a

type family Snd a where
  Snd '(a, b) = b

--------------------------------------------------------------------------------

type TableName = String
type FieldName = String

data RecordMode = Resolved | Unresolved | Done

newtype RecordId t = Id t
  deriving (Show, Serialize)

type family Id (tables :: TableMode -> *) (recordMode :: RecordMode) t where
  Id tables 'Done t = RecordId t
  Id tables 'Resolved t = t
  Id tables 'Unresolved t = RecordId t

data Lazy tables a = Lazy
  { get :: a tables 'Resolved
  }

instance Show (Lazy tables a) where
  show _ = "Lazy"

newtype ForeignRecordId (table :: Symbol) (field :: Symbol) t = ForeignId t
  deriving (Show, Serialize, NFData)

type family ForeignId (tables :: TableMode -> *) (recordMode :: RecordMode) (table :: Symbol) (field :: Symbol) where
  ForeignId tables 'Done table field = ()
  ForeignId tables 'Unresolved table field = ForeignRecordId
    table
    field
    (LookupFieldType field (Snd (LookupTableType table (Eot (tables 'Cannonical)))))
  ForeignId tables 'Resolved table field = Lazy
    tables
    (Fst (LookupTableType table (Eot (tables 'Cannonical))))

-- Resolve ---------------------------------------------------------------------

newtype ResolveError = ResolveError [EId]
  deriving Show

instance Monoid ResolveError where
  mempty = ResolveError []
  mappend = (<>)

instance Semigroup ResolveError where
  ResolveError es <> ResolveError gs = ResolveError (es <> gs)

class GResolve u r where
  gResolve
    :: (TableName -> FieldName -> B.ByteString -> Validation ResolveError Dynamic)
    -> u
    -> Validation ResolveError r

instance GResolve () () where
  gResolve _ () = Success ()

instance GResolve Void Void where
  gResolve _ _ = undefined

instance (GResolve u r, GResolve t s) => GResolve (Either u t) (Either r s) where
  gResolve rsvMap (Left u) = Left <$> gResolve rsvMap u 
  gResolve rsvMap (Right u) = Right <$> gResolve rsvMap u 

instance (GResolve us rs) => GResolve (Named x u, us) (Named x u, rs) where
  gResolve rsvMap (u, us) = (u,) <$> gResolve rsvMap us

instance (GResolve us rs) => GResolve (Named x (RecordId u), us) (Named x u, rs) where
  gResolve rsvMap (Named (Id u), us) = (Named u,) <$> gResolve rsvMap us

instance
  ( Serialize u
  , Serialize (r tables 'Unresolved)
  , Show u

  , Resolve (tables :: TableMode -> *) r
  , GResolve us rs

  , KnownSymbol table
  , KnownSymbol field
  ) =>
  GResolve (Named x (ForeignRecordId table field u), us) (Named x (Lazy tables r), rs) where
    gResolve rsvMap (Named (ForeignId k), us)
      = (,) <$> rsv <*> gResolve rsvMap us
      where
        table = symbolVal (Proxy :: Proxy table)
        field = symbolVal (Proxy :: Proxy field)

        rsv = fmap fromDynamic (rsvMap table field (serialize k)) `bindValidation` \rsvRecord -> fmap (Named . Lazy) $ resolve rsvMap rsvRecord

instance
  ( Serialize u
  , Serialize (r tables 'Unresolved)
  , Show u

  , Resolve tables r
  , GResolve us rs

  , Functor f
  , Traversable f

  , KnownSymbol table
  , KnownSymbol field
  ) =>
  GResolve (Named x (f (ForeignRecordId table field u)), us) (Named x (f (Lazy tables r)), rs) where
    gResolve rsvMap (Named fid, us)
      = (,) <$> rsv fid <*> gResolve rsvMap us
      where
        table = symbolVal (Proxy :: Proxy table)
        field = symbolVal (Proxy :: Proxy field)

        lookup :: Serialize t => Show t => ForeignRecordId table field t -> Validation ResolveError Dynamic
        lookup (ForeignId k) = rsvMap table field (serialize k)

        rsv :: Resolve tables r => f (ForeignRecordId table field u) -> Validation ResolveError (Named x (f (Lazy tables r)))
        rsv = fmap (Named . fmap Lazy) . sequenceA . fmap ((`bindValidation` resolve rsvMap) . fmap fromDynamic . lookup)

instance
  ( Serialize (r tables 'Unresolved)

  , Resolve tables r
  , GResolve us rs
  ) =>
  GResolve (Named x (r tables 'Unresolved), us) (Named x (r tables 'Resolved), rs) where
    gResolve rsvMap (Named u, us)
      = (,) <$> fmap Named (resolve rsvMap u)
            <*> gResolve rsvMap us

class Resolve (tables :: TableMode -> *) u where
  resolve
    :: (TableName -> FieldName -> B.ByteString -> Validation ResolveError Dynamic)
    -> u tables 'Unresolved
    -> Validation ResolveError (u tables 'Resolved)
  default resolve
    :: HasEot (u tables 'Unresolved)
    => HasEot (u tables 'Resolved)
    => GResolve (Eot (u tables 'Unresolved)) (Eot (u tables 'Resolved))

    => (TableName -> FieldName -> B.ByteString -> Validation ResolveError Dynamic)
    -> u tables 'Unresolved
    -> Validation ResolveError (u tables 'Resolved)
  resolve rsvMap = fmap fromEot . gResolve rsvMap . toEot

-- ResolveTables ---------------------------------------------------------------

class GResolveTables u t where
  gResolveTables
    :: (TableName -> FieldName -> B.ByteString -> Validation ResolveError Dynamic)
    -> u
    -> Validation ResolveError t

instance GResolveTables () () where
  gResolveTables _ () = Success ()

instance GResolveTables u t => GResolveTables (Either u Void) (Either t Void) where
  gResolveTables rsvMap (Left u) = Left <$> gResolveTables rsvMap u
  gResolveTables _ _ = undefined

instance
  ( GResolveTables us ts
  , Resolve tables t 
  ) => GResolveTables (Named table [t tables 'Unresolved], us) (Named table [t tables 'Resolved], ts) where
    gResolveTables rsvMap (Named ts, us) =
      (,) <$> fmap Named (traverse (resolve rsvMap) ts)
          <*> gResolveTables rsvMap us

class ResolveTables t where
  resolveTables
    :: (TableName -> FieldName -> B.ByteString -> Validation ResolveError Dynamic)
    -> t ('Batch 'Unresolved)
    -> Validation ResolveError (t ('Batch 'Resolved))
  default resolveTables
    :: HasEot (t ('Batch 'Unresolved))
    => HasEot (t ('Batch 'Resolved))
    => GResolveTables (Eot (t ('Batch 'Unresolved))) (Eot (t ('Batch 'Resolved)))
    => GatherTableIds t

    => (TableName -> FieldName -> B.ByteString -> Validation ResolveError Dynamic)
    -> t ('Batch 'Unresolved)
    -> Validation ResolveError (t ('Batch 'Resolved))
  resolveTables extRsvMap u = fromEot <$> gResolveTables rsv (toEot u)
    where
      eidMap = gatherTableIds u

      rsvRecord table field value = M.lookup (table, field, value) eidMap

      rsv table field value = case rsvRecord table field (serialize value) of
        Nothing -> extRsvMap table field value
        Just [record] -> Success record
        _ -> error "resolveTables: Repeating ids"

-- GatherIds -------------------------------------------------------------------

data EId
  = forall t. (Show t, Serialize t) => EId FieldName t
  | forall t. (Show t, Serialize t) => EForeignId TableName FieldName t

deriving instance Show EId

class GGatherIds u where
  gGatherIds :: u -> [EId]

instance GGatherIds () where
  gGatherIds () = []

instance GGatherIds Void where
  gGatherIds _ = undefined

instance (GGatherIds u, GGatherIds v) => GGatherIds (Either u v) where
  gGatherIds (Left u) = gGatherIds u
  gGatherIds (Right v) = gGatherIds v

instance GGatherIds us => GGatherIds (Named field u, us) where
  gGatherIds (_, us) = gGatherIds us

instance {-# OVERLAPPING #-}
  ( Serialize t
  , Show t

  , GGatherIds us

  , KnownSymbol field
  ) =>
  GGatherIds (Named field (RecordId t), us) where
    gGatherIds (Named (Id k), us) = EId (symbolVal (Proxy :: Proxy field)) k:gGatherIds us

instance {-# OVERLAPPING #-}
  ( Serialize t
  , Show t

  , GGatherIds us

  , KnownSymbol table
  , KnownSymbol field
  ) =>
  GGatherIds (Named field' (ForeignRecordId table field t), us) where
    gGatherIds (Named (ForeignId k), us) = EForeignId (symbolVal (Proxy :: Proxy table)) (symbolVal (Proxy :: Proxy field)) k:gGatherIds us

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
    gGatherIds (Named f, us) = eids <> gGatherIds us
      where
        eids =
          [ EForeignId (symbolVal (Proxy :: Proxy table)) (symbolVal (Proxy :: Proxy field)) k
          | ForeignId k <- toList f
          ]

class GatherIds (tables :: TableMode -> *) u where
  gatherIds :: u tables 'Unresolved -> [EId]
  default gatherIds
    :: HasEot (u tables 'Unresolved)
    => GGatherIds (Eot (u tables 'Unresolved))

    => u tables 'Unresolved
    -> [EId]
  gatherIds = gGatherIds . toEot

-- GatherTableIds --------------------------------------------------------------

newtype Dynamic = Dynamic ()

toDynamic :: a -> Dynamic
toDynamic = unsafeCoerce

fromDynamic :: Dynamic -> a
fromDynamic = unsafeCoerce

class GGatherTableIds t where
  gGatherTableIds :: t -> M.Map (TableName, FieldName, B.ByteString) [Dynamic]

instance GGatherTableIds () where
  gGatherTableIds () = M.empty

instance GGatherTableIds Void where
  gGatherTableIds _ = undefined

instance (GGatherTableIds t, GGatherTableIds u) => GGatherTableIds (Either t u) where
  gGatherTableIds (Left t) = gGatherTableIds t
  gGatherTableIds (Right u) = gGatherTableIds u

instance ( GGatherTableIds ts
         , GatherIds tables r
         , KnownSymbol table
         ) => GGatherTableIds (Named table [r tables 'Unresolved], ts) where
  gGatherTableIds (Named records, ts) = M.unionWith (<>) eidMap (gGatherTableIds ts)
    where
      eidMap = M.fromListWith (<>)
        [ ((symbolVal (Proxy :: Proxy table), field, serialize k), [toDynamic record])
        | record <- records
        , EId field k <- gatherIds record
        ]

class GatherTableIds t where
  gatherTableIds :: t ('Batch 'Unresolved) -> M.Map (TableName, FieldName, B.ByteString) [Dynamic]
  default gatherTableIds
    :: HasEot (t ('Batch 'Unresolved))
    => GGatherTableIds (Eot (t ('Batch 'Unresolved)))
    => t ('Batch 'Unresolved)
    -> M.Map (TableName, FieldName, B.ByteString) [Dynamic]
  gatherTableIds = gGatherTableIds . toEot

-- Expand ----------------------------------------------------------------------

type family ExpandRecord (parent :: Symbol) (record :: *) where
  ExpandRecord parent () = ()
  ExpandRecord parent (Either fields Eot.Void) = ExpandRecord parent fields
  ExpandRecord parent (Eot.Named name (RecordId a), fields) = (Eot.Named name a, ExpandRecord parent fields)
  ExpandRecord parent (a, fields) = ExpandRecord parent fields

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

data TableMode = LookupFields | forall r. Batch r | Cannonical | Modify

type family Table (tables :: TableMode -> *) (c :: TableMode) table where
  Table tables 'Cannonical table = table tables 'Done

  Table tables ('Batch r) table = [table tables r]
  Table tables 'Modify table = (table tables 'Unresolved -> Maybe (table tables 'Unresolved))
