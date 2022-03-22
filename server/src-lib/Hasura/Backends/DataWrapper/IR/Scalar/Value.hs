{-# LANGUAGE DeriveAnyClass #-}

module Hasura.Backends.DataWrapper.IR.Scalar.Value
  ( Value (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
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
  | Number Double
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
