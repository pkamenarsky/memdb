{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
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

module Database.Immutable.ExperimentalWishlist () where

import Data.Proxy
import GHC.TypeLits

-- Define ----------------------------------------------------------------------

data DB t

data Id m t
data Fid m (table :: Symbol) (field :: Symbol)

data Person m = Person
  { pid :: Id m Int
  , address :: Id m String
  , name :: String
  , friend :: Maybe (Fid m "persons" "pid")
  , employer :: Maybe (Fid m "employers" "address")
  , pid2 :: Id m String
  }

data Table m t

data CompanyTables m = CompanyTables
  { persons :: Table m Person
  , employers :: Table m Person
  }

-- Lookup ----------------------------------------------------------------------

data PersonLookup = PersonLookup
  { pid :: Int
  , address :: String
  , name :: String
  , friend :: Maybe PersonLookup
  , employer :: Maybe PersonLookup
  , pid2 :: String
  }

data LookupId (field :: Symbol) t

data PersonLookupId = PersonLookupId
  { pid :: LookupId "pid" Int
  , address :: LookupId "address" String
  , name :: String
  , friend :: Maybe ()
  , employer :: Maybe ()
  , pid2 :: LookupId "pid2" String
  }

data CompanyTablesLookup = CompanyTablesLookup
  { persons :: forall t field. (PersonLookupId -> LookupId field t) -> t -> Maybe PersonLookup
  , employers :: forall t field. (PersonLookupId -> LookupId field t) -> t -> Maybe PersonLookup
  }

mkLookups :: DB CompanyTables -> CompanyTablesLookup
mkLookups = undefined

-- Insert ----------------------------------------------------------------------

data PersonInsert = PersonInsert
  { pid :: Int
  , address :: String
  , name :: String
  , friend :: Maybe Int
  , employer :: Maybe String
  , pid2 :: String
  }

data CompanyTablesInsert = CompanyTablesInsert
  { persons :: [PersonInsert]
  , employers :: [PersonInsert]
  }

insert :: DB CompanyTables -> CompanyTablesInsert -> IO ()
insert = undefined

-- Project ---------------------------------------------------------------------

data CompanyTablesProject = CompanyTablesProject
  { persons :: PersonInsert -> Maybe PersonInsert
  , employers :: PersonInsert -> Maybe PersonInsert
  }

project :: DB CompanyTables -> CompanyTablesInsert -> IO ()
project = undefined
