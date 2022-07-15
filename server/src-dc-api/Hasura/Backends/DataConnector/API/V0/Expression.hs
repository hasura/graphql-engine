{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.Backends.DataConnector.API.V0.Expression
  ( Expression (..),
    BinaryComparisonOperator (..),
    BinaryArrayComparisonOperator (..),
    UnaryComparisonOperator (..),
    ComparisonColumn (..),
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
import Data.Text (Text)
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.Column qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Relationships qualified as API.V0
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
  | CustomBinaryComparisonOperator {getCustomBinaryComparisonOperator :: Text}
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Hashable, NFData)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec BinaryComparisonOperator

instance HasCodec BinaryComparisonOperator where
  codec =
    named "BinaryComparisonOperator" $
      matchChoiceCodec
        ( disjointStringConstCodec
            [ (LessThan, "less_than"),
              (LessThanOrEqual, "less_than_or_equal"),
              (GreaterThan, "greater_than"),
              (GreaterThanOrEqual, "greater_than_or_equal"),
              (Equal, "equal")
            ]
        )
        (dimapCodec CustomBinaryComparisonOperator getCustomBinaryComparisonOperator textCodec)
        $ \case
          op@CustomBinaryComparisonOperator {} -> Right op
          op -> Left op

-- | A serializable representation of binary array comparison operators.
data BinaryArrayComparisonOperator
  = In
  | CustomBinaryArrayComparisonOperator {getCustomBinaryArrayComparisonOperator :: Text}
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Hashable, NFData)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec BinaryArrayComparisonOperator

instance HasCodec BinaryArrayComparisonOperator where
  codec =
    named "BinaryArrayComparisonOperator" $
      matchChoiceCodec
        ( disjointStringConstCodec
            [ (In, "in")
            ]
        )
        (dimapCodec CustomBinaryArrayComparisonOperator getCustomBinaryArrayComparisonOperator textCodec)
        $ \case
          op@CustomBinaryArrayComparisonOperator {} -> Right op
          op -> Left op

-- | A serializable representation of unary comparison operators.
data UnaryComparisonOperator
  = IsNull
  | CustomUnaryComparisonOperator {getCustomUnaryComparisonOperator :: Text}
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Hashable, NFData)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec UnaryComparisonOperator

instance HasCodec UnaryComparisonOperator where
  codec =
    named "UnaryComparisonOperator" $
      matchChoiceCodec
        ( disjointStringConstCodec
            [ (IsNull, "is_null")
            ]
        )
        (dimapCodec CustomUnaryComparisonOperator getCustomUnaryComparisonOperator textCodec)
        $ \case
          op@CustomUnaryComparisonOperator {} -> Right op
          op -> Left op

-- | A serializable representation of query expressions.
data Expression
  = And (ValueWrapper "expressions" [Expression])
  | Or (ValueWrapper "expressions" [Expression])
  | Not (ValueWrapper "expression" Expression)
  | ApplyBinaryComparisonOperator (ValueWrapper3 "operator" BinaryComparisonOperator "column" ComparisonColumn "value" ComparisonValue)
  | ApplyBinaryArrayComparisonOperator (ValueWrapper3 "operator" BinaryArrayComparisonOperator "column" ComparisonColumn "values" [API.V0.Scalar.Value])
  | ApplyUnaryComparisonOperator (ValueWrapper2 "operator" UnaryComparisonOperator "column" ComparisonColumn)
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Hashable, NFData)

-- | Specifies a particular column to use in a comparison via its path and name
data ComparisonColumn = ComparisonColumn
  { -- | The path of relationships from the current query table to the table that contains the column
    _ccPath :: [API.V0.RelationshipName],
    -- | The name of the column
    _ccName :: API.V0.ColumnName
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ComparisonColumn
  deriving anyclass (Hashable, NFData)

instance HasCodec ComparisonColumn where
  codec =
    object "ComparisonColumn" $
      ComparisonColumn
        <$> requiredField "path" "The relationship path from the current query table to the table that contains the specified column. Empty array means the current query table." .= _ccPath
        <*> requiredField "name" "The name of the column" .= _ccName

-- | A serializable representation of comparison values used in comparisons inside 'Expression's.
data ComparisonValue
  = -- | Allows a comparison to a column on the current table or another table
    AnotherColumn (ValueWrapper "column" ComparisonColumn)
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
