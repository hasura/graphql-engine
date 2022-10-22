{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.IR.Value
  ( UnpreparedValue (..),
    ValueWithOrigin (..),
    openValueOrigin,
    mkParameter,
  )
where

import Hasura.GraphQL.Parser.Schema (VariableInfo)
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.SQL.Backend
import Hasura.Session (SessionVariable)

data UnpreparedValue (b :: BackendType)
  = -- | A SQL value that can be parameterized over.
    UVParameter
      (Maybe VariableInfo)
      -- ^ The GraphQL variable this value came from, if any.
      (ColumnValue b)
  | -- | A literal SQL expression that /cannot/ be parameterized over.
    UVLiteral (SQLExpression b)
  | -- | The entire session variables JSON object.
    UVSession
  | -- | A single session variable.
    UVSessionVar (SessionVarType b) SessionVariable

deriving instance
  ( Backend b,
    Eq (ColumnValue b),
    Eq (ScalarValue b)
  ) =>
  Eq (UnpreparedValue b)

deriving instance
  ( Backend b,
    Show (ColumnValue b),
    Show (ScalarValue b)
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
  UVParameter (Just valInfo) columnValue
mkParameter (ValueNoOrigin columnValue) =
  UVParameter Nothing columnValue
