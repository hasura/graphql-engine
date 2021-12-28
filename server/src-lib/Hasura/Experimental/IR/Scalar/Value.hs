{-# LANGUAGE DeriveAnyClass #-}

module Hasura.Experimental.IR.Scalar.Value
  ( Value (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Hasura.Incremental (Cacheable)
import Hasura.Prelude

--------------------------------------------------------------------------------

-- | Literal scalar values that can appear as leaf nodes in expressions
--
-- NOTE(jkachmar): This type shouldn't _need_ ser/de instances, but they're
-- imposed by the 'Backend' class.
data Value
  = String Text
  | Number Double
  | Boolean Bool
  | Null
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)
