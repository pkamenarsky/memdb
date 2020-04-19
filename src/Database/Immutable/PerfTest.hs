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

module Database.Immutable.PerfTest where

import Database.Immutable.Experimental
import qualified Data.Serialize as S

import qualified GHC.Generics as G

data Person2 tables m = Person
  { pid :: Id tables m Int                                     -- could be turned into (CompanyTables Memory -> Int -> ~(Person Resolved))
  , name :: String
  , friend :: Maybe (ForeignId tables m "persons" "pid")       -- could be turned into Maybe ~(Person Resolved); NOTE: must be lazy!
  , employer :: Maybe (ForeignId tables m "employers" "owner") -- could be turned into Maybe ~(Employer Resolved)
  , pid2 :: Id tables m String
  } deriving (G.Generic, Resolve CompanyTables2, LookupById CompanyTables2, GatherIds CompanyTables2)

deriving instance Show (Person2 CompanyTables2 'Unresolved)
deriving instance Show (Person2 CompanyTables2 'Resolved)
deriving instance S.Serialize (Person2 CompanyTables2 'Unresolved)

data Person3 tables m = Person3
  { pid3 :: Id tables m Int                                     -- could be turned into (CompanyTables Memory -> Int -> ~(Person Resolved))
  , name3 :: String
  , friend3 :: Maybe (ForeignId tables m "persons" "pid")       -- could be turned into Maybe ~(Person Resolved); NOTE: must be lazy!
  , employer3 :: Maybe (ForeignId tables m "employers" "owner") -- could be turned into Maybe ~(Employer Resolved)
  , pid2_3 :: Id tables m String
  } deriving (G.Generic, Resolve CompanyTables2, LookupById CompanyTables2, GatherIds CompanyTables2)

deriving instance Show (Person3 CompanyTables2 'Unresolved)
deriving instance Show (Person3 CompanyTables2 'Resolved)
deriving instance S.Serialize (Person3 CompanyTables2 'Unresolved)

data Person4 tables m = Person4
  { pid4 :: Id tables m Int                                     -- could be turned into (CompanyTables Memory -> Int -> ~(Person Resolved))
  , name4 :: String
  , friend4 :: Maybe (ForeignId tables m "persons" "pid")       -- could be turned into Maybe ~(Person Resolved); NOTE: must be lazy!
  , employer4 :: Maybe (ForeignId tables m "employers" "owner") -- could be turned into Maybe ~(Employer Resolved)
  , pid2_4 :: Id tables m String
  } deriving (G.Generic, Resolve CompanyTables2, LookupById CompanyTables2, GatherIds CompanyTables2)

deriving instance Show (Person4 CompanyTables2 'Unresolved)
deriving instance Show (Person4 CompanyTables2 'Resolved)
deriving instance S.Serialize (Person4 CompanyTables2 'Unresolved)

data Person5 tables m = Person5
  { pid5 :: Id tables m Int                                     -- could be turned into (CompanyTables Memory -> Int -> ~(Person Resolved))
  , name5 :: String
  , friend5 :: Maybe (ForeignId tables m "persons" "pid")       -- could be turned into Maybe ~(Person Resolved); NOTE: must be lazy!
  , employer5 :: Maybe (ForeignId tables m "employers" "owner") -- could be turned into Maybe ~(Employer Resolved)
  , pid2_5 :: Id tables m String
  } deriving (G.Generic, Resolve CompanyTables2, LookupById CompanyTables2, GatherIds CompanyTables2)

deriving instance Show (Person5 CompanyTables2 'Unresolved)
deriving instance Show (Person5 CompanyTables2 'Resolved)
deriving instance S.Serialize (Person5 CompanyTables2 'Unresolved)

data Person6 tables m = Person6
  { pid6 :: Id tables m Int                                     -- could be turned into (CompanyTables Memory -> Int -> ~(Person Resolved))
  , name6 :: String
  , friend6 :: Maybe (ForeignId tables m "persons" "pid")       -- could be turned into Maybe ~(Person Resolved); NOTE: must be lazy!
  , employer6 :: Maybe (ForeignId tables m "employers" "owner") -- could be turned into Maybe ~(Employer Resolved)
  , pid2_6 :: Id tables m String
  } deriving (G.Generic, Resolve CompanyTables2, LookupById CompanyTables2, GatherIds CompanyTables2)

deriving instance Show (Person6 CompanyTables2 'Unresolved)
deriving instance Show (Person6 CompanyTables2 'Resolved)
deriving instance S.Serialize (Person6 CompanyTables2 'Unresolved)

data Person7 tables m = Person7
  { pid7 :: Id tables m Int                                     -- could be turned into (CompanyTables Memory -> Int -> ~(Person Resolved))
  , name7 :: String
  , friend7 :: Maybe (ForeignId tables m "persons" "pid")       -- could be turned into Maybe ~(Person Resolved); NOTE: must be lazy!
  , employer7 :: Maybe (ForeignId tables m "employers" "owner") -- could be turned into Maybe ~(Employer Resolved)
  , pid2_7 :: Id tables m String
  } deriving (G.Generic, Resolve CompanyTables2, LookupById CompanyTables2, GatherIds CompanyTables2)

deriving instance Show (Person7 CompanyTables2 'Unresolved)
deriving instance Show (Person7 CompanyTables2 'Resolved)
deriving instance S.Serialize (Person7 CompanyTables2 'Unresolved)

data Person8 tables m = Person8
  { pid8 :: Id tables m Int                                     -- could be turned into (CompanyTables Memory -> Int -> ~(Person Resolved))
  , name8 :: String
  , friend8 :: Maybe (ForeignId tables m "persons" "pid")       -- could be turned into Maybe ~(Person Resolved); NOTE: must be lazy!
  , employer8 :: Maybe (ForeignId tables m "employers" "owner") -- could be turned into Maybe ~(Employer Resolved)
  , pid2_8 :: Id tables m String
  } deriving (G.Generic, Resolve CompanyTables2, LookupById CompanyTables2, GatherIds CompanyTables2)

deriving instance Show (Person8 CompanyTables2 'Unresolved)
deriving instance Show (Person8 CompanyTables2 'Resolved)
deriving instance S.Serialize (Person8 CompanyTables2 'Unresolved)

data Person9 tables m = Person9
  { pid9 :: Id tables m Int                                     -- could be turned into (CompanyTables Memory -> Int -> ~(Person Resolved))
  , name9 :: String
  , friend9 :: Maybe (ForeignId tables m "persons" "pid")       -- could be turned into Maybe ~(Person Resolved); NOTE: must be lazy!
  , employer9 :: Maybe (ForeignId tables m "employers" "owner") -- could be turned into Maybe ~(Employer Resolved)
  , pid2_9 :: Id tables m String
  } deriving (G.Generic, Resolve CompanyTables2, LookupById CompanyTables2, GatherIds CompanyTables2)

deriving instance Show (Person9 CompanyTables2 'Unresolved)
deriving instance Show (Person9 CompanyTables2 'Resolved)
deriving instance S.Serialize (Person9 CompanyTables2 'Unresolved)

data Employer2 tables m = Employer
  { owner :: Id tables m String
  , address :: String
  , employees :: [ForeignId tables m "persons" "pid"]  -- could be turned into [~(Person Resolved)]
  } deriving (G.Generic, Resolve CompanyTables2, LookupById CompanyTables2, GatherIds CompanyTables2)

deriving instance Show (Employer2 CompanyTables2 'Unresolved)
deriving instance Show (Employer2 CompanyTables2 'Resolved)
deriving instance S.Serialize (Employer2 CompanyTables2 'Unresolved)

data CompanyTables2 m = CompanyTables
  { persons :: Table CompanyTables2 m Person2
  , persons3 :: Table CompanyTables2 m Person3
  , persons4 :: Table CompanyTables2 m Person4
  , persons5 :: Table CompanyTables2 m Person5
  , persons6 :: Table CompanyTables2 m Person6
  , persons7 :: Table CompanyTables2 m Person7
  , persons8 :: Table CompanyTables2 m Person8
  , persons9 :: Table CompanyTables2 m Person9
  , employers :: Table CompanyTables2 m Employer2
  } deriving (G.Generic, LookupTables, InsertTables)
