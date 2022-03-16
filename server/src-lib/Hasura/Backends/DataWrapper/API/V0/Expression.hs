{-# LANGUAGE DeriveAnyClass #-}

--
module Hasura.Backends.DataWrapper.API.V0.Expression
  ( Expression (..),
    Operator (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Hasura.Backends.DataWrapper.API.V0.Column qualified as API.V0
import Hasura.Backends.DataWrapper.API.V0.Scalar.Value qualified as API.V0.Scalar
import Hasura.Incremental (Cacheable)
import Hasura.Prelude

--------------------------------------------------------------------------------

-- | A serializable representation of query expressions.
data Expression
  = -- | A constant 'Scalar.Value'.
    Literal API.V0.Scalar.Value
  | In Expression (HashSet API.V0.Scalar.Value)
  | And [Expression]
  | Or [Expression]
  | Not Expression
  | IsNull Expression
  | IsNotNull Expression
  | Column API.V0.ColumnName
  | Equal Expression Expression
  | NotEqual Expression Expression
  | ApplyOperator Operator Expression Expression
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)

--------------------------------------------------------------------------------

-- | A serializable representation of query operators.
data Operator
  = LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)
