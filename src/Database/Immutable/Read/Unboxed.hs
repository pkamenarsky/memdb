module Database.Immutable.Read.Unboxed
  (
  -- * Database
    DB
  ) where

import qualified Data.Vector.Unboxed as V

import qualified Database.Immutable as DB

-- | Specialized version of 'DB.DB' using unboxed vectors as the
-- underlying storage.
type DB = DB.DB V.Vector
