{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}

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
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import Data.Tuple.Extra
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
        ( stringConstCodec
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
        ( stringConstCodec
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
        ( stringConstCodec
            [ (IsNull, "is_null")
            ]
        )
        (dimapCodec CustomUnaryComparisonOperator getCustomUnaryComparisonOperator textCodec)
        $ \case
          op@CustomUnaryComparisonOperator {} -> Right op
          op -> Left op

-- | A serializable representation of query expressions.
data Expression
  = And [Expression]
  | Or [Expression]
  | Not Expression
  | ApplyBinaryComparisonOperator BinaryComparisonOperator ComparisonColumn ComparisonValue
  | ApplyBinaryArrayComparisonOperator BinaryArrayComparisonOperator ComparisonColumn [API.V0.Scalar.Value]
  | ApplyUnaryComparisonOperator UnaryComparisonOperator ComparisonColumn
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Hashable, NFData)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec Expression

instance HasCodec Expression where
  codec =
    named "Expression" $
      object "Expression" $
        discriminatedUnionCodec "type" enc dec
    where
      expressionsCodec = requiredField' "expressions"
      expressionCodec = requiredField' "expression"
      binaryOperatorCodec =
        (,,)
          <$> requiredField' "operator" .= fst3
          <*> requiredField' "column" .= snd3
          <*> requiredField' "value" .= thd3
      binaryArrayOperatorCodec =
        (,,)
          <$> requiredField' "operator" .= fst3
          <*> requiredField' "column" .= snd3
          <*> requiredField' "values" .= thd3
      unaryOperatorCodec =
        (,)
          <$> requiredField' "operator" .= fst
          <*> requiredField' "column" .= snd
      enc = \case
        And expressions -> ("and", mapToEncoder expressions expressionsCodec)
        Or expressions -> ("or", mapToEncoder expressions expressionsCodec)
        Not expression -> ("not", mapToEncoder expression expressionCodec)
        ApplyBinaryComparisonOperator o c v ->
          ("binary_op", mapToEncoder (o, c, v) binaryOperatorCodec)
        ApplyBinaryArrayComparisonOperator o c vs ->
          ("binary_arr_op", mapToEncoder (o, c, vs) binaryArrayOperatorCodec)
        ApplyUnaryComparisonOperator o c ->
          ("unary_op", mapToEncoder (o, c) unaryOperatorCodec)
      dec =
        HashMap.fromList
          [ ("and", ("AndExpression", mapToDecoder And expressionsCodec)),
            ("or", ("OrExpression", mapToDecoder Or expressionsCodec)),
            ("not", ("NotExpression", mapToDecoder Not expressionCodec)),
            ( "binary_op",
              ( "ApplyBinaryComparisonOperator",
                mapToDecoder (uncurry3 ApplyBinaryComparisonOperator) binaryOperatorCodec
              )
            ),
            ( "binary_arr_op",
              ( "ApplyBinaryArrayComparisonOperator",
                mapToDecoder (uncurry3 ApplyBinaryArrayComparisonOperator) binaryArrayOperatorCodec
              )
            ),
            ( "unary_op",
              ( "ApplyUnaryComparisonOperator",
                mapToDecoder (uncurry ApplyUnaryComparisonOperator) unaryOperatorCodec
              )
            )
          ]

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
    AnotherColumn ComparisonColumn
  | ScalarValue API.V0.Scalar.Value
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Hashable, NFData)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ComparisonValue

instance HasCodec ComparisonValue where
  codec =
    object "ComparisonValue" $
      discriminatedUnionCodec "type" enc dec
    where
      columnCodec = requiredField' "column"
      scalarValueCodec = requiredField' "value"
      enc = \case
        AnotherColumn c -> ("column", mapToEncoder c columnCodec)
        ScalarValue v -> ("scalar", mapToEncoder v scalarValueCodec)
      dec =
        HashMap.fromList
          [ ("column", ("AnotherColumnComparison", mapToDecoder AnotherColumn columnCodec)),
            ("scalar", ("ScalarValueComparison", mapToDecoder ScalarValue scalarValueCodec))
          ]
