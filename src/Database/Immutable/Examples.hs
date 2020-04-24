{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Database.Immutable.Examples where

import           GHC.Generics

import           Database.Immutable.Knot

-- Stolen from https://hackage.haskell.org/package/tie-knot-0.2/docs/Data-Knot.html

data Person tables m = Person
  { name  :: Id tables m String
  , loves :: [ForeignId tables m "persons" "name"]
  } deriving (Generic, KnitRecord Model)

deriving instance Show (Person Model 'Unresolved)
deriving instance Show (Person Model 'Resolved)

data Model m = Model
  { persons :: Table Model m Person
  } deriving (Generic, KnitTables)

deriving instance Show (Model ('Batch 'Resolved))

--------------------------------------------------------------------------------

model :: Model ('Batch 'Unresolved)
model = Model
  [ Person (Id "Alice") [ ForeignId "Bob", ForeignId "cat" ]
  , Person (Id "Bob") [ ForeignId "Alice" ]

  -- you may disagree, but the cat thinks of itself as Person
  , Person (Id "cat") [ ForeignId "cat" ]
  ]

knitModel :: Model ('Batch 'Resolved)
knitModel = case knit model of
  Right resolved -> resolved
  Left e -> error (show e)

whoLovesX :: String -> [String]
whoLovesX x =
  [ lovingName
  | Person lovingName lovedPersons <- persons knitModel
  , lovedPerson <- lovedPersons
  , name (get lovedPerson) == x
  ]
