{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Database.Immutable.KnotTest where

import           Data.Serialize (Serialize)
import           Data.Word (Word64)

import qualified GHC.Generics as G

import Database.Immutable.Knot

data Person tables m = Person
  { name :: String
  , friend :: Maybe (ForeignId tables m "persons" "pid")
  , employer :: Maybe (ForeignId tables m "employers" "owner")
  , pid :: Id tables m Word64
  , pid2 :: Id tables m String
  , other :: Employer tables m
  } deriving (G.Generic, KnitRecord CompanyTables)

deriving instance Show (Person CompanyTables 'Unresolved)
deriving instance Show (Person CompanyTables 'Resolved)

data MaybeList a = MaybeList [Maybe a]
  deriving (Functor, Foldable, G.Generic, Serialize, Show)

data Employer tables m = Employer
  { address :: String
  , employees :: MaybeList (ForeignId tables m "persons" "pid")
  , owner :: Id tables m String
  } deriving (G.Generic, KnitRecord CompanyTables)

deriving instance Show (Employer CompanyTables 'Unresolved)
deriving instance Show (Employer CompanyTables 'Resolved)

data CompanyTables m = CompanyTables
  { persons :: Table CompanyTables m Person
  , employers :: Table CompanyTables m Employer
  } deriving (G.Generic, KnitTables)

deriving instance Show (CompanyTables 'Resolved)

--------------------------------------------------------------------------------

personU :: Person CompanyTables 'Unresolved
personU = Person
  { pid = Id 0
  , name = "bla"
  , friend = Just $ ForeignId 0 -- own best friend
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
  , employees = MaybeList [Just $ ForeignId 0]
  }

personR :: Person CompanyTables 'Resolved
personR = undefined

companyU :: CompanyTables 'Unresolved
companyU = CompanyTables
  { persons = [personU]
  , employers = [employerU]
  }

companyR = knit companyU

r = fmap (fmap (fmap get . employer) . persons) companyR
