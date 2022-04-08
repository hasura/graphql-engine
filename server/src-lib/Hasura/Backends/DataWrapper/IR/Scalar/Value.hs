{-# LANGUAGE DeriveAnyClass #-}

module Hasura.Backends.DataWrapper.IR.Scalar.Value
  ( Value (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific
import Hasura.Backends.DataWrapper.API qualified as API
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Witch

--------------------------------------------------------------------------------

-- | Literal scalar values that can appear as leaf nodes in expressions
--
-- NOTE: This type shouldn't _need_ ser/de instances, but they're imposed by
-- the 'Backend' class.
data Value
  = String Text
  | Number Scientific
  | Boolean Bool
  | Null
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)

instance From API.Value Value where
  from = \case
    API.String txt -> String txt
    API.Number x -> Number x
    API.Boolean p -> Boolean p
    API.Null -> Null

instance From Value API.Value where
  from = \case
    String txt -> API.String txt
    Number x -> API.Number x
    Boolean p -> API.Boolean p
    Null -> API.Null
