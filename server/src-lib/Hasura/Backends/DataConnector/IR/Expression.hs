{-# LANGUAGE DeriveAnyClass #-}

module Hasura.Backends.DataConnector.IR.Expression
  ( Expression (..),
    BinaryComparisonOperator (..),
    BinaryArrayComparisonOperator (..),
    UnaryComparisonOperator (..),
    ComparisonValue (..),
  )
where

--------------------------------------------------------------------------------

import Autodocodec.Extended (ValueWrapper (..), ValueWrapper2 (..), ValueWrapper3 (..))
import Data.Aeson (FromJSON, ToJSON)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.IR.Column qualified as IR.C
import Hasura.Backends.DataConnector.IR.Scalar.Value qualified as IR.S
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Witch qualified

--------------------------------------------------------------------------------

-- | A "concrete" expression type for datasource-agnostic queries (as opposed
-- to our existing polymorphic intermediate representation).
--
-- This type should be seen as an intermediate phase of the processing pipeline
-- which provides a high-level interface that the GraphQL Engine can use to
-- inspect, manipulate, optimize, etc. before sending off to an agent that will
-- be responsible for performing query generation/execution.
--
-- This type should ascribe clear semantics to its sub-expressions; when this
-- is not possible, it should clearly defer to the semantics of some reference
-- datasource with clearer documentation.
--
-- e.g. https://www.postgresql.org/docs/13/sql-expressions.html
data Expression
  = -- | A logical @AND@ fold.
    --
    -- cf. https://www.postgresql.org/docs/13/functions-logical.html
    And [Expression]
  | -- | A logical @OR@ fold.
    --
    -- cf. https://www.postgresql.org/docs/13/functions-logical.html
    Or [Expression]
  | -- | A logical @NOT@ function.
    --
    -- cf. https://www.postgresql.org/docs/13/functions-logical.html
    Not Expression
  | -- | Apply a 'BinaryComparisonOperator' that compares a column to a 'ComparisonValue';
    -- the result of this application will return "true" or "false" depending on the
    -- 'BinaryComparisonOperator' that's being applied.
    ApplyBinaryComparisonOperator BinaryComparisonOperator IR.C.Name ComparisonValue
  | -- | Apply a 'BinaryArrayComparisonOperator' that evaluates a column with the
    -- 'BinaryArrayComparisonOperator' against an array of 'ComparisonValue's.
    -- The result of this application will return "true" or "false" depending
    -- on the 'BinaryArrayComparisonOperator' that's being applied.
    ApplyBinaryArrayComparisonOperator BinaryArrayComparisonOperator IR.C.Name [ComparisonValue]
  | -- | Apply a 'UnaryComparisonOperator' that evaluates a column with the
    -- 'UnaryComparisonOperator'; the result of this application will return "true" or
    -- "false" depending on the 'UnaryComparisonOperator' that's being applied.
    ApplyUnaryComparisonOperator UnaryComparisonOperator IR.C.Name
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)

instance Witch.From Expression API.Expression where
  from = \case
    And exprs -> API.And . ValueWrapper $ Witch.from <$> exprs
    Or exprs -> API.Or . ValueWrapper $ Witch.from <$> exprs
    Not expr -> API.Not . ValueWrapper $ Witch.from expr
    ApplyBinaryComparisonOperator op column value ->
      API.ApplyBinaryComparisonOperator $ ValueWrapper3 (Witch.from op) (Witch.from column) (Witch.from value)
    ApplyUnaryComparisonOperator op column ->
      API.ApplyUnaryComparisonOperator $ ValueWrapper2 (Witch.from op) (Witch.from column)
    ApplyBinaryArrayComparisonOperator op column values ->
      API.ApplyBinaryArrayComparisonOperator $ ValueWrapper3 (Witch.from op) (Witch.from column) (Witch.from <$> values)

instance Witch.From API.Expression Expression where
  from = \case
    API.And (ValueWrapper exprs) -> And $ Witch.from <$> exprs
    API.Or (ValueWrapper exprs) -> Or $ Witch.from <$> exprs
    API.Not (ValueWrapper expr) -> Not $ Witch.from expr
    API.ApplyBinaryComparisonOperator (ValueWrapper3 op column value) ->
      ApplyBinaryComparisonOperator (Witch.from op) (Witch.from column) (Witch.from value)
    API.ApplyBinaryArrayComparisonOperator (ValueWrapper3 op column values) ->
      ApplyBinaryArrayComparisonOperator (Witch.from op) (Witch.from column) (Witch.from <$> values)
    API.ApplyUnaryComparisonOperator (ValueWrapper2 op column) ->
      ApplyUnaryComparisonOperator (Witch.from op) (Witch.from column)

--------------------------------------------------------------------------------

-- | Operators which are typically applied to two 'Expression's (via the
-- 'ApplyOperator' sub-'Expression') to perform a boolean comparison.
--
-- cf. https://www.postgresql.org/docs/13/functions-comparison.html
--
-- XXX(jkachmar): Comparison operations are tricky business!
--
-- We should define the semantics of these comparisons in a way that is clear
-- and carefully considered.
data BinaryComparisonOperator
  = LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  | Equal
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)

instance Witch.From API.BinaryComparisonOperator BinaryComparisonOperator where
  from API.LessThan = LessThan
  from API.LessThanOrEqual = LessThanOrEqual
  from API.GreaterThan = GreaterThan
  from API.GreaterThanOrEqual = GreaterThanOrEqual
  from API.Equal = Equal

instance Witch.From BinaryComparisonOperator API.BinaryComparisonOperator where
  from LessThan = API.LessThan
  from LessThanOrEqual = API.LessThanOrEqual
  from GreaterThan = API.GreaterThan
  from GreaterThanOrEqual = API.GreaterThanOrEqual
  from Equal = API.Equal

data UnaryComparisonOperator
  = IsNull
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)

instance Witch.From API.UnaryComparisonOperator UnaryComparisonOperator where
  from API.IsNull = IsNull

instance Witch.From UnaryComparisonOperator API.UnaryComparisonOperator where
  from IsNull = API.IsNull

data BinaryArrayComparisonOperator
  = In
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)

instance Witch.From API.BinaryArrayComparisonOperator BinaryArrayComparisonOperator where
  from API.In = In

instance Witch.From BinaryArrayComparisonOperator API.BinaryArrayComparisonOperator where
  from In = API.In

data ComparisonValue
  = AnotherColumn IR.C.Name
  | ScalarValue IR.S.Value
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)

instance Witch.From ComparisonValue API.ComparisonValue where
  from (AnotherColumn column) = API.AnotherColumn $ ValueWrapper (Witch.from column)
  from (ScalarValue value) = API.ScalarValue . ValueWrapper $ Witch.from value

instance Witch.From API.ComparisonValue ComparisonValue where
  from (API.AnotherColumn (ValueWrapper column)) = AnotherColumn (Witch.from column)
  from (API.ScalarValue (ValueWrapper value)) = ScalarValue $ Witch.from value
