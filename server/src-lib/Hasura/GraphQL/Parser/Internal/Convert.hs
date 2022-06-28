{-# LANGUAGE ViewPatterns #-}

-- | This module defines all functions that convert between different
-- representations of values in the schema; most commonly: GraphQL literals,
-- JSON values, and 'InputValue', a type that provides an abstraction above both
-- of those.
module Hasura.GraphQL.Parser.Internal.Convert
  ( jsonToGraphQL,
    valueToJSON,
  )
where

import Data.Aeson qualified as A
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.HashMap.Strict.Extended qualified as M
import Data.Int (Int64)
import Data.Scientific (toBoundedInteger)
import Data.Text.Extended
import Hasura.GraphQL.Parser.Class.Parse
import Hasura.GraphQL.Parser.Internal.TypeChecking
import Hasura.GraphQL.Parser.Variable
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax qualified as G

valueToJSON :: MonadParse m => G.GType -> InputValue Variable -> m A.Value
valueToJSON expectedType inputVal = do
  peeledVal <- peelVariable expectedType inputVal
  pure $ valueToJSON' peeledVal
  where
    valueToJSON' :: InputValue Variable -> A.Value
    valueToJSON' = \case
      JSONValue j -> j
      GraphQLValue g -> graphQLToJSON g

    graphQLToJSON :: G.Value Variable -> A.Value
    graphQLToJSON = \case
      G.VNull -> A.Null
      G.VInt i -> A.toJSON i
      G.VFloat f -> A.toJSON f
      G.VString t -> A.toJSON t
      G.VBoolean b -> A.toJSON b
      G.VEnum (G.EnumValue n) -> A.toJSON n
      G.VList values -> A.toJSON $ graphQLToJSON <$> values
      G.VObject objects -> A.toJSON $ graphQLToJSON <$> objects
      G.VVariable variable -> valueToJSON' $ absurd <$> vValue variable

jsonToGraphQL :: MonadError Text m => A.Value -> m (G.Value Void)
jsonToGraphQL = \case
  A.Null -> pure G.VNull
  A.Bool val -> pure $ G.VBoolean val
  A.String val -> pure $ G.VString val
  A.Number val -> case toBoundedInteger val of
    Just intVal -> pure $ G.VInt $ fromIntegral @Int64 intVal
    Nothing -> pure $ G.VFloat val
  A.Array vals -> G.VList <$> traverse jsonToGraphQL (toList vals)
  A.Object vals ->
    G.VObject . M.fromList <$> for (KM.toList vals) \(K.toText -> key, val) -> do
      graphQLName <-
        G.mkName key
          `onNothing` throwError
            ( "variable value contains an object with key "
                <> key <<> ", which is not a legal GraphQL name"
            )
      (graphQLName,) <$> jsonToGraphQL val
