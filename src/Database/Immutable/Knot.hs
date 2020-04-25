{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Immutable.Knot
  ( KnitRecord
  , KnitTables
  , Mode (..)

  , Table
  , Lazy (..)

  , Id
  , ForeignId

  , RecordId (..)
  , ForeignRecordId (..)

  , ResolveError (..)

  , knit
  )where

import           Control.DeepSeq (NFData)

import           Data.Foldable (Foldable, toList)
import qualified Data.Map as M
import           Data.Maybe (catMaybes)
import           Data.Semigroup ((<>))

import           GHC.Generics (Generic)
import           GHC.TypeLits (KnownSymbol, Symbol, TypeError, ErrorMessage(..), symbolVal)
import qualified Generics.Eot as Eot
import           Generics.Eot (Eot, HasEot, Named (Named), Void, Proxy (..), fromEot, toEot)

import           Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------

type family Fst a where
  Fst '(a, b) = a

type family Snd a where
  Snd '(a, b) = b

--------------------------------------------------------------------------------

type TableName = String
type FieldName = String
type FieldValue = String

data Mode = Resolved | Unresolved | Done

newtype RecordId t = Id t
  deriving (Show, Eq, Ord, Num, Generic, NFData)

type family Id (tables :: Mode -> *) (recordMode :: Mode) t where
  Id tables 'Done t = RecordId t
  Id tables 'Resolved t = t
  Id tables 'Unresolved t = RecordId t

data Lazy tables a = Lazy
  { get :: a tables 'Resolved
  }

instance Show (Lazy tables a) where
  show _ = "Lazy"

newtype ForeignRecordId (table :: Symbol) (field :: Symbol) t = ForeignId t
  deriving (Show, Eq, Ord, Num, Generic, NFData)

type family ForeignId (tables :: Mode -> *) (recordMode :: Mode) (table :: Symbol) (field :: Symbol) where
  ForeignId tables 'Done table field = ()
  ForeignId tables 'Unresolved table field = ForeignRecordId
    table
    field
    (LookupFieldType field (Snd (LookupTableType table (Eot (tables 'Unresolved)))))
  ForeignId tables 'Resolved table field = Lazy
    tables
    (Fst (LookupTableType table (Eot (tables 'Resolved))))

-- GatherIds -------------------------------------------------------------------

data EId
  = forall t. Show t => EId TableName FieldName t Dynamic
  | forall t. Show t => EForeignId TableName FieldName t

deriving instance Show EId

newtype Dynamic = Dynamic ()

instance Show Dynamic where
  show _ = "Dynamic"

toDynamic :: a -> Dynamic
toDynamic = Dynamic . unsafeCoerce

fromDynamic :: Dynamic -> a
fromDynamic (Dynamic v) = unsafeCoerce v

--------------------------------------------------------------------------------

class GGatherIds u where
  gGatherIds :: TableName -> Dynamic -> u -> [EId]

instance GGatherIds () where
  gGatherIds _ _ () = []

instance GGatherIds Void where
  gGatherIds _ _ _ = undefined

instance (GGatherIds u, GGatherIds v) => GGatherIds (Either u v) where
  gGatherIds table record (Left u) = gGatherIds table record u
  gGatherIds table record (Right v) = gGatherIds table record v

instance GGatherIds us => GGatherIds (Named field u, us) where
  gGatherIds table record (_, us) = gGatherIds table record us

instance {-# OVERLAPPING #-}
  ( Show t
  , GGatherIds us
  , KnownSymbol field
  ) =>
  GGatherIds (Named field (RecordId t), us) where
    gGatherIds table record (Named (Id k), us)
      = EId table (symbolVal (Proxy :: Proxy field)) k record:gGatherIds table record us

instance {-# OVERLAPPING #-}
  ( Show t

  , GGatherIds us

  , KnownSymbol table
  , KnownSymbol field
  ) =>
  GGatherIds (Named field' (ForeignRecordId table field t), us) where
    gGatherIds table record (Named (ForeignId k), us)
      = EForeignId (symbolVal (Proxy :: Proxy table)) (symbolVal (Proxy :: Proxy field)) k:gGatherIds table record us

instance {-# OVERLAPPING #-}
  ( Show t

  , GGatherIds us

  , Foldable f

  , KnownSymbol field
  ) =>
  GGatherIds (Named field (f (RecordId t)), us) where
    gGatherIds table record (Named f, us) = eids <> gGatherIds table record us
      where
        eids =
          [ EId table (symbolVal (Proxy :: Proxy field)) k record
          | Id k <- toList f
          ]

instance {-# OVERLAPPING #-}
  ( Show t

  , GGatherIds us

  , Foldable f

  , KnownSymbol table
  , KnownSymbol field
  ) =>
  GGatherIds (Named field' (f (ForeignRecordId table field t)), us) where
    gGatherIds table record (Named f, us) = eids <> gGatherIds table record us
      where
        eids =
          [ EForeignId (symbolVal (Proxy :: Proxy table)) (symbolVal (Proxy :: Proxy field)) k
          | ForeignId k <- toList f
          ]

-- GatherTableIds --------------------------------------------------------------

class GGatherTableIds t where
  gGatherTableIds :: t -> [EId]

instance GGatherTableIds () where
  gGatherTableIds () = []

instance GGatherTableIds Void where
  gGatherTableIds _ = undefined

instance (GGatherTableIds t, GGatherTableIds u) => GGatherTableIds (Either t u) where
  gGatherTableIds (Left t) = gGatherTableIds t
  gGatherTableIds (Right u) = gGatherTableIds u

instance ( GGatherTableIds ts
         , KnitRecord tables r
         , KnownSymbol table
         ) => GGatherTableIds (Named table [r tables 'Unresolved], ts) where
  gGatherTableIds (Named records, ts) = eids <> gGatherTableIds ts
    where
      eids = mconcat
        [ gatherIds (symbolVal (Proxy :: Proxy table)) (toDynamic record) record
        | record <- records
        ]

-- Resolve ---------------------------------------------------------------------

data ResolveError
  = MissingIds [(TableName, FieldName, FieldValue)]
  | RepeatingIds [(TableName, FieldName, FieldValue)]
  deriving Show

class GResolve u r where
  gResolve
    :: (TableName -> FieldName -> FieldValue -> Dynamic)
    -> u
    -> r

instance GResolve () () where
  gResolve _ () = ()

instance GResolve Void Void where
  gResolve _ _ = undefined

instance (GResolve u r, GResolve t s) => GResolve (Either u t) (Either r s) where
  gResolve rsvMap (Left u) = Left $ gResolve rsvMap u 
  gResolve rsvMap (Right u) = Right $ gResolve rsvMap u 

instance (GResolve us rs) => GResolve (Named x u, us) (Named x u, rs) where
  gResolve rsvMap (u, us) = (u, gResolve rsvMap us)

instance (GResolve us rs) => GResolve (Named x (RecordId u), us) (Named x u, rs) where
  gResolve rsvMap (Named (Id u), us) = (Named u, gResolve rsvMap us)

instance (GResolve us rs, Functor f) => GResolve (Named x (f (RecordId u)), us) (Named x (f u), rs) where
  gResolve rsvMap (Named u', us) = (Named $ fmap (\(Id u) -> u) u', gResolve rsvMap us)

instance
  ( Show u

  , KnitRecord tables r
  , GResolve us rs

  , KnownSymbol table
  , KnownSymbol field
  ) =>
  GResolve (Named x (ForeignRecordId table field u), us) (Named x (Lazy tables r), rs) where
    gResolve rsvMap (Named (ForeignId k), us)
      = ( Named $ Lazy $ resolve rsvMap (fromDynamic $ rsvMap table field (show k))
        , gResolve rsvMap us
        )
      where
        table = symbolVal (Proxy :: Proxy table)
        field = symbolVal (Proxy :: Proxy field)

instance
  ( Show u

  , KnitRecord tables r
  , GResolve us rs

  , Functor f

  , KnownSymbol table
  , KnownSymbol field
  ) =>
  GResolve (Named x (f (ForeignRecordId table field u)), us) (Named x (f (Lazy tables r)), rs) where
    gResolve rsvMap (Named f, us)
      = ( Named $ flip fmap f $ \(ForeignId k) -> Lazy $ resolve rsvMap (fromDynamic $ rsvMap table field (show k))
        , gResolve rsvMap us
        )
      where
        table = symbolVal (Proxy :: Proxy table)
        field = symbolVal (Proxy :: Proxy field)

instance
  ( KnitRecord tables r
  , GResolve us rs
  ) =>
  GResolve (Named x (r tables 'Unresolved), us) (Named x (r tables 'Resolved), rs) where
    gResolve rsvMap (Named u, us) = (Named $ resolve rsvMap u, gResolve rsvMap us)

instance
  ( KnitRecord tables r
  , GResolve us rs
  , Functor f
  ) =>
  GResolve (Named x (f (r tables 'Unresolved)), us) (Named x (f (r tables 'Resolved)), rs) where
    gResolve rsvMap (Named u, us) = (Named $ fmap (resolve rsvMap) u, gResolve rsvMap us)

-- ResolveTables ---------------------------------------------------------------

class GResolveTables u t where
  gResolveTables :: (TableName -> FieldName -> FieldValue -> Dynamic) -> u -> t

instance GResolveTables () () where
  gResolveTables _ () = ()

instance GResolveTables u t => GResolveTables (Either u Void) (Either t Void) where
  gResolveTables rsvMap (Left u) = Left $ gResolveTables rsvMap u
  gResolveTables _ _ = undefined

instance
  ( GResolveTables us ts
  , KnitRecord tables t 
  ) => GResolveTables (Named table [t tables 'Unresolved], us) (Named table [t tables 'Resolved], ts) where
    gResolveTables rsvMap (Named ts, us)
      = (Named $ map (resolve rsvMap) ts, gResolveTables rsvMap us)

-- KnitRecord ------------------------------------------------------------------

class KnitRecord (tables :: Mode -> *) u where
  resolve
    :: (TableName -> FieldName -> FieldValue -> Dynamic)
    -> u tables 'Unresolved
    -> u tables 'Resolved
  default resolve
    :: HasEot (u tables 'Unresolved)
    => HasEot (u tables 'Resolved)
    => GResolve (Eot (u tables 'Unresolved)) (Eot (u tables 'Resolved))

    => (TableName -> FieldName -> FieldValue -> Dynamic)
    -> u tables 'Unresolved
    -> u tables 'Resolved
  resolve rsvMap = fromEot . gResolve rsvMap . toEot

  gatherIds :: TableName -> Dynamic -> u tables 'Unresolved -> [EId]
  default gatherIds
    :: HasEot (u tables 'Unresolved)
    => GGatherIds (Eot (u tables 'Unresolved))

    => TableName
    -> Dynamic
    -> u tables 'Unresolved
    -> [EId]
  gatherIds table record = gGatherIds table record . toEot

-- KnitTables ------------------------------------------------------------------

class KnitTables t where
  resolveTables
    :: (TableName -> FieldName -> FieldValue -> Dynamic)
    -> t 'Unresolved
    -> Either ResolveError (t 'Resolved)
  default resolveTables
    :: HasEot (t 'Unresolved)
    => HasEot (t 'Resolved)
    => GResolveTables (Eot (t 'Unresolved)) (Eot (t 'Resolved))
    => KnitTables t

    => (TableName -> FieldName -> FieldValue -> Dynamic)
    -> t 'Unresolved
    -> Either ResolveError (t 'Resolved)
  resolveTables extRsvMap u
    | not (null repeatingIds) = Left $ RepeatingIds repeatingIds
    | [] <- missingIds = Right $ fromEot $ gResolveTables rsv (toEot u)
    | otherwise = Left $ MissingIds missingIds
    where
      eids = gatherTableIds u

      eidMap = M.fromListWith (<>)
        [ ((table, field, show k), [record])
        | EId table field k record <- eids
        ]

      repeatingIds = mconcat
        [ if length records > 1
            then [(table, field, k)]
            else []
        | ((table, field, k), records) <- M.toList eidMap
        ]

      missingIds = catMaybes
        [ case M.lookup (table, field, show k) eidMap of
            Nothing -> Just (table, field, show k)
            Just _ -> Nothing
        | EForeignId table field k <- eids
        ]

      rsvRecord table field value = M.lookup (table, field, value) eidMap

      rsv table field value = case rsvRecord table field value of
        Nothing -> extRsvMap table field value
        Just [record] -> record
        _ -> error "resolveTables: repeating ids (this is a bug, the consistency check should have caught this)"

  gatherTableIds :: t 'Unresolved -> [EId]
  default gatherTableIds
    :: HasEot (t 'Unresolved)
    => GGatherTableIds (Eot (t 'Unresolved))
    => t 'Unresolved
    -> [EId]
  gatherTableIds = gGatherTableIds . toEot

-- Expand ----------------------------------------------------------------------

type family ExpandRecord (parent :: Symbol) (record :: *) where
  ExpandRecord parent () = ()
  ExpandRecord parent (Either fields Eot.Void) = ExpandRecord parent fields
  ExpandRecord parent (Eot.Named name (RecordId a), fields) = (Eot.Named name a, ExpandRecord parent fields)
  ExpandRecord parent (Eot.Named name (f (RecordId a)), fields) = (Eot.Named name (f a), ExpandRecord parent fields)
  ExpandRecord parent (a, fields) = ExpandRecord parent fields

type family LookupTableType (table :: Symbol) (eot :: *) :: (((Mode -> *) -> Mode -> *), *) where
  LookupTableType name (Either records Eot.Void) = LookupTableType name records
  LookupTableType name (Eot.Named name [record tables recordMode], records)
    = '(record, ExpandRecord name (Eot (record tables 'Done)))
  LookupTableType name (Eot.Named otherName [record tables recordMode], records)
    = LookupTableType name records

  LookupTableType name eot = TypeError ('Text "Can't lookup table type")

type family LookupFieldType (field :: Symbol) (eot :: *) :: * where
  LookupFieldType name (Either records Eot.Void) = LookupFieldType name records
  LookupFieldType name (Eot.Named name field, fields) = field
  LookupFieldType name (Eot.Named otherName field, fields) = LookupFieldType name fields
  LookupFieldType name eot = TypeError ('Text "Can't lookup field type")

-- Table -----------------------------------------------------------------------

type family Table (tables :: Mode -> *) (c :: Mode) table where
  Table tables r table = [table tables r]

--------------------------------------------------------------------------------

knit :: KnitTables t => t 'Unresolved -> Either ResolveError (t 'Resolved)
knit = resolveTables
  (error "knit: inconsistent record (this is a bug, the consistency check should have caught this")
