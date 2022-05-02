{-# LANGUAGE DeriveAnyClass #-}

module Hasura.Backends.DataConnector.IR.Expression
  ( Expression (..),
    Operator (..),
  )
where

--------------------------------------------------------------------------------

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
  = -- | A constant 'Scalar.Value'.
    Literal IR.S.Value
  | -- | A literal array of 'Scalar.Value's.
    --
    -- NOTE: This constructor is necessary for purposes of parsing
    -- into the RQL IR but should never be exposed by the GDW API.
    Array [IR.S.Value]
  | -- | A construct for making multiple comparisons between groups of
    -- 'Scalar.Value's.
    --
    -- The right-hand side is a collection of unique 'Scalar.Value's; the
    -- result is "true" if the result of the left-hand 'Expression' is equal to
    -- any of these 'Scalar.Value's.
    --
    -- cf. https://www.postgresql.org/docs/13/functions-comparisons.html#FUNCTIONS-COMPARISONS-IN-SCALAR
    --
    -- Consider switching this to a 'Set' after the typeclass methods which use
    -- this type have been implemented and we have an opportunity to see how
    -- its used in practice.
    In Expression [IR.S.Value]
  | -- | A logical @AND@ fold.
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
  | -- | A comparison predicate which returns "true" if an expression evaluates
    -- to 'Scalar.Null'.
    IsNull Expression
  | -- | The textual name associated with some "column" of data within a
    -- datasource.
    --
    -- XXX(jkachmar): It's unclear whether "column" is the right descriptor for
    -- this construct; what we want here seems closer to an "identifier".
    --
    -- cf. https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS
    Column IR.C.Name
  | -- | Apply a comparison 'Operator' to two expressions; the result of this
    -- application will return "true" or "false" depending on the 'Operator'
    -- that's being applied.
    --
    -- XXX(jkachmar): Consider renaming 'Operator' to @ComparisonOperator@ and
    -- this sub-expression to @ApplyComparisonOperator@ for clarity.
    ApplyOperator Operator Expression Expression
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)

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
  | Equal
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)

instance Witch.From API.Operator Operator where
  from API.LessThan = LessThan
  from API.LessThanOrEqual = LessThanOrEqual
  from API.GreaterThan = GreaterThan
  from API.GreaterThanOrEqual = GreaterThanOrEqual
  from API.Equal = Equal

instance Witch.From Operator API.Operator where
  from LessThan = API.LessThan
  from LessThanOrEqual = API.LessThanOrEqual
  from GreaterThan = API.GreaterThan
  from GreaterThanOrEqual = API.GreaterThanOrEqual
  from Equal = API.Equal
