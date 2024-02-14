{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.Backends.DataConnector.API.V0.Expression
  ( Expression (..),
    ExistsInTable (..),
    BinaryComparisonOperator (..),
    BinaryArrayComparisonOperator (..),
    UnaryComparisonOperator (..),
    ComparisonColumn (..),
    ccPath,
    ccName,
    ccColumnType,
    ccRedactionExpression,
    ColumnPath (..),
    ComparisonValue (..),
    TargetRedactionExpressions (..),
    RedactionExpressionName (..),
    RedactionExpression (..),
  )
where

import Autodocodec.Extended
import Autodocodec.OpenAPI ()
import Control.DeepSeq (NFData)
import Control.Lens (makeLenses, (^.), _1, _2, _3, _4)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey, Value)
import Data.Data (Data)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.OpenApi (ToSchema)
import Data.Set (Set)
import Data.Text (Text)
import Data.Tuple.Extra
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.Column qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Relationships qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Scalar qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Table qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Target qualified as API.V0
import Prelude

--------------------------------------------------------------------------------

-- | A serializable representation of binary comparison operators.
data BinaryComparisonOperator
  = LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  | Equal
  | CustomBinaryComparisonOperator Text
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
        \case
          op@CustomBinaryComparisonOperator {} -> Right op
          op -> Left op
    where
      getCustomBinaryComparisonOperator = \case
        CustomBinaryComparisonOperator op -> op
        _ -> error "Not a custom binary operator when expected"

-- | A serializable representation of binary array comparison operators.
data BinaryArrayComparisonOperator
  = In
  | CustomBinaryArrayComparisonOperator Text
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
        \case
          op@CustomBinaryArrayComparisonOperator {} -> Right op
          op -> Left op
    where
      getCustomBinaryArrayComparisonOperator = \case
        CustomBinaryArrayComparisonOperator op -> op
        _ -> error "Not a custom binary array operator when expected"

-- | A serializable representation of unary comparison operators.
data UnaryComparisonOperator
  = IsNull
  | CustomUnaryComparisonOperator Text
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
        \case
          op@CustomUnaryComparisonOperator {} -> Right op
          op -> Left op
    where
      getCustomUnaryComparisonOperator = \case
        CustomUnaryComparisonOperator op -> op
        _ -> error "Not a custom unary operator when expected"

-- | A serializable representation of query filter expressions.
data Expression
  = -- | A logical AND fold
    And (Set Expression)
  | -- | A logical OR fold
    Or (Set Expression)
  | -- | A logical NOT function
    Not Expression
  | -- | There must exist a row in the table specified by 'ExistsInTable' that
    -- satisfies the 'Expression'
    Exists ExistsInTable Expression
  | -- | Apply a 'BinaryComparisonOperator' that compares a column to a 'ComparisonValue';
    -- the result of this application will return "true" or "false" depending on the
    -- 'BinaryComparisonOperator' that's being applied.
    ApplyBinaryComparisonOperator BinaryComparisonOperator ComparisonColumn ComparisonValue
  | -- | Apply a 'BinaryArrayComparisonOperator' that evaluates a column with the
    -- 'BinaryArrayComparisonOperator' against an array of 'ComparisonValue's.
    -- The result of this application will return "true" or "false" depending
    -- on the 'BinaryArrayComparisonOperator' that's being applied.
    ApplyBinaryArrayComparisonOperator BinaryArrayComparisonOperator ComparisonColumn [Value] API.V0.ScalarType
  | -- | Apply a 'UnaryComparisonOperator' that evaluates a column with the
    -- 'UnaryComparisonOperator'; the result of this application will return "true" or
    -- "false" depending on the 'UnaryComparisonOperator' that's being applied.
    ApplyUnaryComparisonOperator UnaryComparisonOperator ComparisonColumn
  deriving stock (Eq, Generic, Ord, Show)
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
      existsCodec =
        (,)
          <$> requiredField' "in_table" .= fst
          <*> requiredField' "where" .= snd
      binaryOperatorCodec =
        (,,)
          <$> requiredField' "operator" .= fst3
          <*> requiredField' "column" .= snd3
          <*> requiredField' "value" .= thd3
      binaryArrayOperatorCodec =
        (,,,)
          <$> requiredField' "operator" .= (^. _1)
          <*> requiredField' "column" .= (^. _2)
          <*> requiredField' "values" .= (^. _3)
          <*> requiredField' "value_type" .= (^. _4)
      unaryOperatorCodec =
        (,)
          <$> requiredField' "operator" .= fst
          <*> requiredField' "column" .= snd
      enc = \case
        And expressions -> ("and", mapToEncoder expressions expressionsCodec)
        Or expressions -> ("or", mapToEncoder expressions expressionsCodec)
        Not expression -> ("not", mapToEncoder expression expressionCodec)
        Exists inTable where' ->
          ("exists", mapToEncoder (inTable, where') existsCodec)
        ApplyBinaryComparisonOperator o c v ->
          ("binary_op", mapToEncoder (o, c, v) binaryOperatorCodec)
        ApplyBinaryArrayComparisonOperator o c vs scalarType ->
          ("binary_arr_op", mapToEncoder (o, c, vs, scalarType) binaryArrayOperatorCodec)
        ApplyUnaryComparisonOperator o c ->
          ("unary_op", mapToEncoder (o, c) unaryOperatorCodec)
      dec =
        HashMap.fromList
          [ ("and", ("AndExpression", mapToDecoder And expressionsCodec)),
            ("or", ("OrExpression", mapToDecoder Or expressionsCodec)),
            ("not", ("NotExpression", mapToDecoder Not expressionCodec)),
            ( "exists",
              ( "ExistsExpression",
                mapToDecoder (uncurry Exists) existsCodec
              )
            ),
            ( "binary_op",
              ( "ApplyBinaryComparisonOperator",
                mapToDecoder (uncurry3 ApplyBinaryComparisonOperator) binaryOperatorCodec
              )
            ),
            ( "binary_arr_op",
              ( "ApplyBinaryArrayComparisonOperator",
                mapToDecoder (uncurry4 ApplyBinaryArrayComparisonOperator) binaryArrayOperatorCodec
              )
            ),
            ( "unary_op",
              ( "ApplyUnaryComparisonOperator",
                mapToDecoder (uncurry ApplyUnaryComparisonOperator) unaryOperatorCodec
              )
            )
          ]
      uncurry4 fn (a, b, c, d) = fn a b c d

-- | Which table should be subqueried to satisfy the 'Exists' expression
data ExistsInTable
  = -- | The table is the one found by navigating the specified relationship
    -- from the current table
    RelatedTable API.V0.RelationshipName
  | -- | The table is completely unrelated to the current table (ie no join
    -- between the current table and the specified table should be performed
    -- and the whole of the specified table would be subqueried)
    UnrelatedTable API.V0.TableName
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Hashable, NFData)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ExistsInTable

instance HasCodec ExistsInTable where
  codec = named "ExistsInTable" . object "ExistsInTable" $ discriminatedUnionCodec "type" enc dec
    where
      relatedTableCodec = requiredField' "relationship"
      unrelatedTableCodec = requiredField' "table"
      enc = \case
        RelatedTable relationship -> ("related", mapToEncoder relationship relatedTableCodec)
        UnrelatedTable tableName -> ("unrelated", mapToEncoder tableName unrelatedTableCodec)
      dec =
        HashMap.fromList
          [ ("related", ("RelatedTable", mapToDecoder RelatedTable relatedTableCodec)),
            ("unrelated", ("UnrelatedTable", mapToDecoder UnrelatedTable unrelatedTableCodec))
          ]

-- | Specifies a particular column to use in a comparison via its path and name
data ComparisonColumn = ComparisonColumn
  { -- | The path to the table that contains the specified column.
    _ccPath :: ColumnPath,
    -- | The name of the column
    _ccName :: API.V0.ColumnSelector,
    -- | The scalar type of the column
    _ccColumnType :: API.V0.ScalarType,
    -- | If present, the name of the redaction expression to evaluate.
    -- If the expression is false, the column value must be nulled out before being compared to.
    _ccRedactionExpression :: Maybe RedactionExpressionName
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ComparisonColumn
  deriving anyclass (Hashable, NFData)

instance HasCodec ComparisonColumn where
  codec =
    object "ComparisonColumn" $
      ComparisonColumn
        <$> optionalFieldWithOmittedDefault "path" CurrentTable "The path to the table that contains the specified column. Missing or empty array means the current table. [\"$\"] means the query table. No other values are supported at this time." .= _ccPath
        <*> requiredField "name" "The name of the column" .= _ccName
        <*> requiredField "column_type" "The scalar type of the column" .= _ccColumnType
        <*> optionalFieldOrNull "redaction_expression" "If present, the name of the redaction expression to evaluate. If the expression is false, the column value must be nulled out before being compared to." .= _ccRedactionExpression

-- | Describes what table a column is located on. This may either be the "current" table
-- (which would be query table, or the table specified by the closest ancestor 'Exists'
-- expression), or the query table (meaning the table being queried by the 'Query' which
-- the current 'Expression' is from)
--
-- This currently encodes to @[]@ or @["$"]@ in JSON. This format has been chosen to ensure
-- that if we want to extend the pathing to allow navigation of table relationships by
-- turning this type into a list of path components, we can do that without breaking the
-- JSON format. The JSON format also aligns with how HGE encodes this concept in @_ceq@ etc
-- operators in the permissions system.
data ColumnPath
  = CurrentTable
  | QueryTable
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ColumnPath
  deriving anyclass (Hashable, NFData)

instance HasCodec ColumnPath where
  codec = bimapCodec decode encode codec
    where
      decode :: [Text] -> Either String ColumnPath
      decode = \case
        [] -> Right CurrentTable
        ["$"] -> Right QueryTable
        _otherwise -> Left "Invalid ColumnPath"

      encode :: ColumnPath -> [Text]
      encode = \case
        CurrentTable -> []
        QueryTable -> ["$"]

-- | A serializable representation of comparison values used in comparisons inside 'Expression's.
data ComparisonValue
  = -- | Allows a comparison to a column on the current table or another table
    AnotherColumnComparison ComparisonColumn
  | ScalarValueComparison API.V0.ScalarValue
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable, NFData)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ComparisonValue

instance HasCodec ComparisonValue where
  codec =
    object "ComparisonValue" $
      discriminatedUnionCodec "type" enc dec
    where
      columnCodec = requiredField' "column"
      enc = \case
        AnotherColumnComparison c -> ("column", mapToEncoder c columnCodec)
        ScalarValueComparison scalarValue -> ("scalar", mapToEncoder scalarValue objectCodec)
      dec =
        HashMap.fromList
          [ ("column", ("AnotherColumnComparison", mapToDecoder AnotherColumnComparison columnCodec)),
            ("scalar", ("ScalarValueComparison", mapToDecoder ScalarValueComparison objectCodec))
          ]

data TargetRedactionExpressions = TargetRedactionExpressions
  { _treTarget :: API.V0.TargetName,
    _treExpressions :: HashMap RedactionExpressionName RedactionExpression
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec TargetRedactionExpressions

instance HasCodec TargetRedactionExpressions where
  codec =
    object "TargetRedactionExpressions" $
      TargetRedactionExpressions
        <$> requiredField "target" "The target entity with whom the redaction expressions are to be used with" .= _treTarget
        <*> requiredField "expressions" "The named redaction expressions associated with the target" .= _treExpressions

newtype RedactionExpressionName = RedactionExpressionName {unRedactionExpressionName :: Text}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving newtype (ToJSONKey, FromJSONKey)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec RedactionExpressionName

instance HasCodec RedactionExpressionName where
  codec = named "RedactionExpressionName" $ dimapCodec RedactionExpressionName unRedactionExpressionName textCodec

newtype RedactionExpression = RedactionExpression {unRedactionExpression :: Expression}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec RedactionExpression

instance HasCodec RedactionExpression where
  codec = dimapCodec RedactionExpression unRedactionExpression codec

$(makeLenses 'ComparisonColumn)
