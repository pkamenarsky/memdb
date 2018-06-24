{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Database.Immutable.Internal where

import qualified Data.Serialize as S
import           Data.Word

import           GHC.Generics

newtype Offset a = Offset { getOffset :: Word32 }
  deriving (Show, Generic, S.Serialize)

newtype Limit a  = Limit { getLimit :: Word32 }
  deriving (Show, Generic, S.Serialize)

