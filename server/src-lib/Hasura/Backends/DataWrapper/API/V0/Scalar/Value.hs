{-# LANGUAGE DeriveAnyClass #-}

module Hasura.Backends.DataWrapper.API.V0.Scalar.Value
  ( Value (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Hasura.Incremental (Cacheable)
import Hasura.Prelude

--------------------------------------------------------------------------------

data Value
  = String Text
  | Number Double
  | Boolean Bool
  | Null
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)
