{-# LANGUAGE DeriveAnyClass #-}

module Hasura.Backends.DataWrapper.IR.Expression
  ( Expression (..),
    Operator (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.HashSet qualified as S
import Hasura.Backends.DataWrapper.API qualified as API
import Hasura.Backends.DataWrapper.IR.Column qualified as Column (Name)
import Hasura.Backends.DataWrapper.IR.Scalar.Value qualified as Scalar (Value)
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Witch

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
  = -- | A constant 'Scalar.Value'.
    Literal Scalar.Value
  | -- | A construct for making multiple comparisons between groups of
    -- 'Scalar.Value's.
    --
    -- The right-hand side is a collection of unique 'Scalar.Value's; the
    -- result is "true" if the result of the left-hand 'Expression' is equal to
    -- any of these 'Scalar.Value's.
    --
    -- cf. https://www.postgresql.org/docs/13/functions-comparisons.html#FUNCTIONS-COMPARISONS-IN-SCALAR
    --
    -- XXX(jkachmar): It's unclear that there's any benefit from using a
    -- 'HashSet' for the RHS collection of 'Scalar.Value's.
    --
    -- Consider switching this to a 'Set' after the typeclass methods which use
    -- this type have been implemented and we have an opportunity to see how
    -- its used in practice.
    In Expression (HashSet Scalar.Value)
  | -- | A logical @AND@ operator.
    --
    -- cf. https://www.postgresql.org/docs/13/functions-logical.html
    And [Expression]
  | -- | A logical @OR@ operator.
    --
    -- cf. https://www.postgresql.org/docs/13/functions-logical.html
    Or [Expression]
  | -- | A logical @NOT@ operator.
    --
    -- cf. https://www.postgresql.org/docs/13/functions-logical.html
    Not Expression
  | -- | A comparison predicate which returns "true" if an expression evaluates
    -- to 'Scalar.Null'.
    IsNull Expression
  | -- | A comparison predicate which returns "true" if an expression does not
    -- evaluate to 'Scalar.Null'.
    IsNotNull Expression
  | -- | The textual name associated with some "column" of data within a
    -- datasource.
    --
    -- XXX(jkachmar): It's unclear whether "column" is the right descriptor for
    -- this construct; what we want here seems closer to an "identifier".
    --
    -- cf. https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS
    Column Column.Name
  | -- | An equality operation which returns "true" if two expressions evaluate
    -- to equivalent forms.
    --
    -- cf. https://www.postgresql.org/docs/13/functions-comparison.html
    --
    -- XXX(jkachmar): Consider making this a part of 'Operator'.
    --
    -- XXX(jkachmar): Equality of expressions is tricky business!
    --
    -- We should define the semantics of expression equality in a way that is
    -- clear and carefully considered.
    Equal Expression Expression
  | -- | An inequality operation which returns "true" if two expressions do not
    -- evaluate to equivalent forms.
    --
    -- cf. https://www.postgresql.org/docs/13/functions-comparison.html
    --
    -- XXX(jkachmar): Consider making this a part of 'Operator', or eliminating
    -- 'NotEqual' as an explicit case of 'Expression' and only ever construct
    -- it as @Not (Equal x y)@.
    --
    -- XXX(jkachmar): Inequality of expressions is tricky business!
    --
    -- We should define the semantics of expression inequality in a way that is
    -- clear and carefully considered.
    NotEqual Expression Expression
  | -- | Apply a comparison 'Operator' to two expressions; the result of this
    -- application will return "true" or "false" depending on the 'Operator'
    -- that's being applied.
    --
    -- XXX(jkachmar): Consider renaming 'Operator' to @ComparisonOperator@ and
    -- this sub-expression to @ApplyComparisonOperator@ for clarity.
    ApplyOperator Operator Expression Expression
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)

instance From API.Expression Expression where
  from = \case
    API.Literal value -> Literal $ from value
    API.In expr values -> In (from expr) (S.map from values)
    API.And exprs -> And $ map from exprs
    API.Or exprs -> Or $ map from exprs
    API.Not expr -> Not $ from expr
    API.IsNull expr -> IsNull $ from expr
    API.IsNotNull expr -> IsNotNull $ from expr
    API.Column name -> Column $ from name
    API.Equal expr1 expr2 -> Equal (from expr1) (from expr2)
    API.NotEqual expr1 expr2 -> NotEqual (from expr1) (from expr2)
    API.ApplyOperator op expr1 expr2 -> ApplyOperator (from op) (from expr1) (from expr2)

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
data Operator
  = LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)

instance From API.Operator Operator where
  from API.LessThan = LessThan
  from API.LessThanOrEqual = LessThanOrEqual
  from API.GreaterThan = GreaterThan
  from API.GreaterThanOrEqual = GreaterThanOrEqual
