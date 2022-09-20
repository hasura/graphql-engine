{-# LANGUAGE DeriveAnyClass #-}

module Hasura.Backends.DataConnector.IR.Expression
  ( Expression (..),
    ExistsInTable (..),
    BinaryComparisonOperator (..),
    BinaryArrayComparisonOperator (..),
    UnaryComparisonOperator (..),
    ComparisonColumn (..),
    ColumnPath (..),
    ComparisonValue (..),
  )
where

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
import Data.Aeson (FromJSON, ToJSON, Value)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.IR.Column qualified as IR.C
import Hasura.Backends.DataConnector.IR.Relationships qualified as IR.R
import Hasura.Backends.DataConnector.IR.Table qualified as IR.T
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
    ApplyBinaryArrayComparisonOperator BinaryArrayComparisonOperator ComparisonColumn [Value]
  | -- | Apply a 'UnaryComparisonOperator' that evaluates a column with the
    -- 'UnaryComparisonOperator'; the result of this application will return "true" or
    -- "false" depending on the 'UnaryComparisonOperator' that's being applied.
    ApplyUnaryComparisonOperator UnaryComparisonOperator ComparisonColumn
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)

instance Witch.From Expression API.Expression where
  from = \case
    And exprs -> API.And $ Witch.from <$> exprs
    Or exprs -> API.Or $ Witch.from <$> exprs
    Not expr -> API.Not $ Witch.from expr
    Exists inTable expr ->
      API.Exists (Witch.from inTable) (Witch.from expr)
    ApplyBinaryComparisonOperator op column value ->
      API.ApplyBinaryComparisonOperator (Witch.from op) (Witch.from column) (Witch.from value)
    ApplyUnaryComparisonOperator op column ->
      API.ApplyUnaryComparisonOperator (Witch.from op) (Witch.from column)
    ApplyBinaryArrayComparisonOperator op column values ->
      API.ApplyBinaryArrayComparisonOperator (Witch.from op) (Witch.from column) (Witch.from <$> values)

instance Witch.From API.Expression Expression where
  from = \case
    API.And exprs -> And $ Witch.from <$> exprs
    API.Or exprs -> Or $ Witch.from <$> exprs
    API.Not expr -> Not $ Witch.from expr
    API.Exists inTable expr ->
      Exists (Witch.from inTable) (Witch.from expr)
    API.ApplyBinaryComparisonOperator op column value ->
      ApplyBinaryComparisonOperator (Witch.from op) (Witch.from column) (Witch.from value)
    API.ApplyBinaryArrayComparisonOperator op column values ->
      ApplyBinaryArrayComparisonOperator (Witch.from op) (Witch.from column) (Witch.from <$> values)
    API.ApplyUnaryComparisonOperator op column ->
      ApplyUnaryComparisonOperator (Witch.from op) (Witch.from column)

-- | Which table should be subqueried to satisfy the 'Exists' expression
data ExistsInTable
  = -- | The table is the one found by navigating the specified relationship
    -- from the current table
    RelatedTable IR.R.RelationshipName
  | -- | The table is completely unrelated to the current table (ie no join
    -- between the current table and the specified table should be performed
    -- and the whole of the specified table would be subqueried)
    UnrelatedTable IR.T.Name
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)

instance Witch.From ExistsInTable API.ExistsInTable where
  from = \case
    RelatedTable relationshipName -> API.RelatedTable (Witch.from relationshipName)
    UnrelatedTable tableName -> API.UnrelatedTable (Witch.from tableName)

instance Witch.From API.ExistsInTable ExistsInTable where
  from = \case
    API.RelatedTable relationshipName -> RelatedTable (Witch.from relationshipName)
    API.UnrelatedTable tableName -> UnrelatedTable (Witch.from tableName)

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
  | CustomBinaryComparisonOperator Text
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)

instance Witch.From API.BinaryComparisonOperator BinaryComparisonOperator where
  from API.LessThan = LessThan
  from API.LessThanOrEqual = LessThanOrEqual
  from API.GreaterThan = GreaterThan
  from API.GreaterThanOrEqual = GreaterThanOrEqual
  from API.Equal = Equal
  from (API.CustomBinaryComparisonOperator name) = CustomBinaryComparisonOperator name

instance Witch.From BinaryComparisonOperator API.BinaryComparisonOperator where
  from LessThan = API.LessThan
  from LessThanOrEqual = API.LessThanOrEqual
  from GreaterThan = API.GreaterThan
  from GreaterThanOrEqual = API.GreaterThanOrEqual
  from Equal = API.Equal
  from (CustomBinaryComparisonOperator name) = API.CustomBinaryComparisonOperator name

data UnaryComparisonOperator
  = IsNull
  | CustomUnaryComparisonOperator Text
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)

instance Witch.From API.UnaryComparisonOperator UnaryComparisonOperator where
  from API.IsNull = IsNull
  from (API.CustomUnaryComparisonOperator name) = CustomUnaryComparisonOperator name

instance Witch.From UnaryComparisonOperator API.UnaryComparisonOperator where
  from IsNull = API.IsNull
  from (CustomUnaryComparisonOperator name) = API.CustomUnaryComparisonOperator name

data BinaryArrayComparisonOperator
  = In
  | CustomBinaryArrayComparisonOperator Text
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)

instance Witch.From API.BinaryArrayComparisonOperator BinaryArrayComparisonOperator where
  from API.In = In
  from (API.CustomBinaryArrayComparisonOperator name) = CustomBinaryArrayComparisonOperator name

instance Witch.From BinaryArrayComparisonOperator API.BinaryArrayComparisonOperator where
  from In = API.In
  from (CustomBinaryArrayComparisonOperator name) = API.CustomBinaryArrayComparisonOperator name

data ComparisonColumn = ComparisonColumn
  { _ccPath :: ColumnPath,
    _ccName :: IR.C.Name
  }
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)

instance Witch.From ComparisonColumn API.ComparisonColumn where
  from ComparisonColumn {..} =
    API.ComparisonColumn
      { _ccPath = Witch.from _ccPath,
        _ccName = Witch.from _ccName
      }

instance Witch.From API.ComparisonColumn ComparisonColumn where
  from API.ComparisonColumn {..} =
    ComparisonColumn
      { _ccPath = Witch.from _ccPath,
        _ccName = Witch.from _ccName
      }

data ColumnPath
  = CurrentTable
  | QueryTable
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)

instance Witch.From ColumnPath API.ColumnPath where
  from CurrentTable = API.CurrentTable
  from QueryTable = API.QueryTable

instance Witch.From API.ColumnPath ColumnPath where
  from API.CurrentTable = CurrentTable
  from API.QueryTable = QueryTable

data ComparisonValue
  = AnotherColumn ComparisonColumn
  | ScalarValue Value
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)

instance Witch.From ComparisonValue API.ComparisonValue where
  from (AnotherColumn column) = API.AnotherColumn $ Witch.from column
  from (ScalarValue value) = API.ScalarValue value

instance Witch.From API.ComparisonValue ComparisonValue where
  from (API.AnotherColumn column) = AnotherColumn (Witch.from column)
  from (API.ScalarValue value) = ScalarValue value
