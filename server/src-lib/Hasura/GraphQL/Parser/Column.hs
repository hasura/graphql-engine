{-# LANGUAGE UndecidableInstances #-}

module Hasura.GraphQL.Parser.Column
  ( UnpreparedValue (..),
    ValueWithOrigin (..),
    openValueOrigin,
    peelWithOrigin,
    mkParameter,
  )
where

import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Parser.Internal.TypeChecking
import Hasura.GraphQL.Parser.Internal.Types
import Hasura.GraphQL.Parser.Schema
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column hiding
  ( EnumValue (..),
    EnumValueInfo (..),
  )
import Hasura.SQL.Backend
import Hasura.Session (SessionVariable)
import Language.GraphQL.Draft.Syntax qualified as G

-- -------------------------------------------------------------------------------------------------

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

-- TODO: figure out what the purpose of this method is.
peelWithOrigin :: MonadParse m => Parser 'Both m a -> Parser 'Both m (ValueWithOrigin a)
peelWithOrigin parser =
  parser
    { pParser = \case
        GraphQLValue (G.VVariable var@Variable {vInfo, vValue}) -> do
          -- Check types c.f. 5.8.5 of the June 2018 GraphQL spec
          typeCheck False (toGraphQLType $ pType parser) var
          ValueWithOrigin vInfo <$> pParser parser (absurd <$> vValue)
        value -> ValueNoOrigin <$> pParser parser value
    }
