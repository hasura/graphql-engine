{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.IR.Value
  ( UnpreparedValue (..),
    Provenance (..),
    ValueWithOrigin (..),
    openValueOrigin,
    mkParameter,
  )
where

import Hasura.GraphQL.Parser.Variable
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.SQL.Backend
import Hasura.Session (SessionVariable)

-- | Where did this variable come from?
data Provenance
  = -- | We don't know / it isn't important
    Unknown
  | -- | A GraphQL variable (e.g. a query parameter)
    FromGraphQL VariableInfo
  | -- | An internal source (e.g. a native query argument)
    FromInternal Text
  deriving stock (Eq, Show)

data UnpreparedValue (b :: BackendType)
  = -- | A SQL value that can be parameterized over.
    UVParameter Provenance (ColumnValue b)
  | -- | A literal SQL expression that /cannot/ be parameterized over.
    UVLiteral (SQLExpression b)
  | -- | The entire session variables JSON object.
    UVSession
  | -- | A single session variable.
    UVSessionVar (SessionVarType b) SessionVariable

deriving instance
  ( Backend b,
    Eq (ColumnValue b)
  ) =>
  Eq (UnpreparedValue b)

deriving instance
  ( Backend b,
    Show (ColumnValue b)
  ) =>
  Show (UnpreparedValue b)

-- | This indicates whether a variable value came from a certain GraphQL variable
data ValueWithOrigin a
  = ValueWithOrigin VariableInfo a
  | ValueNoOrigin a
  deriving (Functor)

openValueOrigin :: ValueWithOrigin a -> a
openValueOrigin (ValueWithOrigin _ a) = a
openValueOrigin (ValueNoOrigin a) = a

mkParameter :: ValueWithOrigin (ColumnValue b) -> UnpreparedValue b
mkParameter (ValueWithOrigin valInfo columnValue) =
  UVParameter (FromGraphQL valInfo) columnValue
mkParameter (ValueNoOrigin columnValue) =
  UVParameter Unknown columnValue
