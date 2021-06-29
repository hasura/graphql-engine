-- | This module defines all functions that convert between different
-- representations of values in the schema; most commonly: GraphQL literals,
-- JSON values, and 'InputValue', a type that provides an abstraction above both
-- of those.

module Hasura.GraphQL.Parser.Internal.Convert where

import           Hasura.Prelude

import qualified Data.Aeson                                  as A
import qualified Data.HashMap.Strict.Extended                as M

import           Data.Int                                    (Int64)
import           Data.Scientific                             (toBoundedInteger)
import           Data.Text.Extended
import           Language.GraphQL.Draft.Syntax               hiding (Definition)

import           Hasura.GraphQL.Parser.Class.Parse
import           Hasura.GraphQL.Parser.Internal.TypeChecking
import           Hasura.GraphQL.Parser.Schema


valueToJSON :: MonadParse m => GType -> InputValue Variable -> m A.Value
valueToJSON expected = peelVariable expected >=> valueToJSON'
  where
    valueToJSON' = \case
      JSONValue    j -> pure j
      GraphQLValue g -> graphQLToJSON g
    graphQLToJSON = \case
      VNull               -> pure A.Null
      VInt i              -> pure $ A.toJSON i
      VFloat f            -> pure $ A.toJSON f
      VString t           -> pure $ A.toJSON t
      VBoolean b          -> pure $ A.toJSON b
      VEnum (EnumValue n) -> pure $ A.toJSON n
      VList values        -> A.toJSON <$> traverse graphQLToJSON values
      VObject objects     -> A.toJSON <$> traverse graphQLToJSON objects
      VVariable variable  -> valueToJSON' $ absurd <$> vValue variable

jsonToGraphQL :: (MonadError Text m) => A.Value -> m (Value Void)
jsonToGraphQL = \case
  A.Null        -> pure VNull
  A.Bool val    -> pure $ VBoolean val
  A.String val  -> pure $ VString val
  A.Number val  -> case toBoundedInteger val of
    Just intVal -> pure $ VInt $ fromIntegral @Int64 intVal
    Nothing     -> pure $ VFloat val
  A.Array vals  -> VList <$> traverse jsonToGraphQL (toList vals)
  A.Object vals -> VObject . M.fromList <$> for (M.toList vals) \(key, val) -> do
    graphQLName <- mkName key `onNothing`
      throwError ("variable value contains an object with key "
                   <> key <<> ", which is not a legal GraphQL name"
                 )
    (graphQLName,) <$> jsonToGraphQL val
