{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.Backends.DataConnector.API.V0.Expression
  ( Expression (..),
    BinaryComparisonOperator (..),
    BinaryArrayComparisonOperator (..),
    UnaryComparisonOperator (..),
    ComparisonValue (..),
  )
where

import Autodocodec.Extended
import Autodocodec.OpenAPI ()
import Control.DeepSeq (NFData)
import Control.Lens.TH (makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.Column qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Scalar.Value qualified as API.V0.Scalar
import Prelude

--------------------------------------------------------------------------------

-- | A serializable representation of binary comparison operators.
data BinaryComparisonOperator
  = LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  | Equal
  deriving stock (Data, Eq, Generic, Ord, Show, Enum, Bounded)
  deriving anyclass (Hashable, NFData)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec BinaryComparisonOperator

instance HasCodec BinaryComparisonOperator where
  codec =
    named "BinaryComparisonOperator" $
      disjointStringConstCodec
        [ (LessThan, "less_than"),
          (LessThanOrEqual, "less_than_or_equal"),
          (GreaterThan, "greater_than"),
          (GreaterThanOrEqual, "greater_than_or_equal"),
          (Equal, "equal")
        ]

-- | A serializable representation of binary array comparison operators.
data BinaryArrayComparisonOperator
  = In
  deriving stock (Data, Eq, Generic, Ord, Show, Enum, Bounded)
  deriving anyclass (Hashable, NFData)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec BinaryArrayComparisonOperator

instance HasCodec BinaryArrayComparisonOperator where
  codec =
    named "BinaryArrayComparisonOperator" $
      disjointStringConstCodec
        [ (In, "in")
        ]

-- | A serializable representation of unary comparison operators.
data UnaryComparisonOperator
  = IsNull
  deriving stock (Data, Eq, Generic, Ord, Show, Enum, Bounded)
  deriving anyclass (Hashable, NFData)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec UnaryComparisonOperator

instance HasCodec UnaryComparisonOperator where
  codec =
    named "UnaryComparisonOperator" $
      disjointStringConstCodec
        [ (IsNull, "is_null")
        ]

-- | A serializable representation of query expressions.
data Expression
  = And (ValueWrapper "expressions" [Expression])
  | Or (ValueWrapper "expressions" [Expression])
  | Not (ValueWrapper "expression" Expression)
  | ApplyBinaryComparisonOperator (ValueWrapper3 "operator" BinaryComparisonOperator "column" API.V0.ColumnName "value" ComparisonValue)
  | ApplyBinaryArrayComparisonOperator (ValueWrapper3 "operator" BinaryArrayComparisonOperator "column" API.V0.ColumnName "values" [ComparisonValue])
  | ApplyUnaryComparisonOperator (ValueWrapper2 "operator" UnaryComparisonOperator "column" API.V0.ColumnName)
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Hashable, NFData)

-- | A serializable representation of comparison values used in comparisons inside 'Expression's.
data ComparisonValue
  = -- | Allows a comparison to a column on the current table
    -- TODO: joins in Expressions and then comparisons to other tables involved in those joins
    AnotherColumn (ValueWrapper "column" API.V0.ColumnName)
  | ScalarValue (ValueWrapper "value" API.V0.Scalar.Value)
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Hashable, NFData)

$(makePrisms ''ComparisonValue)
$(makePrisms ''Expression)

instance HasCodec Expression where
  codec =
    named "Expression" $
      sumTypeCodec
        [ TypeAlternative "AndExpression" "and" _And,
          TypeAlternative "OrExpression" "or" _Or,
          TypeAlternative "NotExpression" "not" _Not,
          TypeAlternative "ApplyBinaryComparisonOperator" "binary_op" _ApplyBinaryComparisonOperator,
          TypeAlternative "ApplyBinaryArrayComparisonExpression" "binary_arr_op" _ApplyBinaryArrayComparisonOperator,
          TypeAlternative "ApplyUnaryComparisonOperator" "unary_op" _ApplyUnaryComparisonOperator
        ]

deriving via Autodocodec Expression instance FromJSON Expression

deriving via Autodocodec Expression instance ToJSON Expression

deriving via Autodocodec Expression instance ToSchema Expression

instance HasCodec ComparisonValue where
  codec =
    named "ComparisonValue" $
      sumTypeCodec
        [ TypeAlternative "AnotherColumnComparison" "column" _AnotherColumn,
          TypeAlternative "ScalarValueComparison" "scalar" _ScalarValue
        ]

deriving via Autodocodec ComparisonValue instance FromJSON ComparisonValue

deriving via Autodocodec ComparisonValue instance ToJSON ComparisonValue

deriving via Autodocodec ComparisonValue instance ToSchema ComparisonValue
