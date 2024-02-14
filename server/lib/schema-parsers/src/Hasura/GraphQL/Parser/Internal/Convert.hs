-- | This module defines all functions that convert between different
-- representations of values in the schema; most commonly: GraphQL literals,
-- JSON values, and 'InputValue', a type that provides an abstraction above both
-- of those.
module Hasura.GraphQL.Parser.Internal.Convert
  ( jsonToGraphQL,
    valueToJSON,
    graphQLToJSON,
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.HashMap.Strict qualified as HashMap
import Data.Int (Int64)
import Data.Scientific (toBoundedInteger)
import Data.Traversable (for)
import Data.Vector qualified as V
import Data.Void (Void, absurd)
import Hasura.Base.ErrorMessage
import Hasura.Base.ToErrorValue
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Parser.Internal.TypeChecking
import Hasura.GraphQL.Parser.Variable
import Language.GraphQL.Draft.Syntax qualified as G

-- Disable custom prelude warnings in preparation for extracting this module into a separate package.
{-# ANN module ("HLint: ignore Use onNothing" :: String) #-}

valueToJSON :: (MonadParse m) => G.GType -> InputValue Variable -> m J.Value
valueToJSON expectedType inputVal = do
  peeledVal <- peelVariable expectedType inputVal
  pure $ valueToJSON' peeledVal
  where
    valueToJSON' :: InputValue Variable -> J.Value
    valueToJSON' = \case
      JSONValue j -> j
      GraphQLValue g -> graphQLToJSON g

graphQLToJSON :: G.Value Variable -> J.Value
graphQLToJSON = \case
  G.VNull -> J.Null
  G.VInt i -> J.toJSON i
  G.VFloat f -> J.toJSON f
  G.VString t -> J.toJSON t
  G.VBoolean b -> J.toJSON b
  G.VEnum (G.EnumValue n) -> J.toJSON n
  G.VList values -> J.toJSON $ graphQLToJSON <$> values
  G.VObject objects -> J.toJSON $ graphQLToJSON <$> objects
  G.VVariable variable -> case vValue variable of
    -- TODO: Absent values can be encoded as nulls, but this arguably breaks GraphQL spec June 2018, section 2.9.5.
    Nothing -> J.Null
    Just (JSONValue j) -> j
    Just (GraphQLValue g) -> graphQLToJSON (absurd <$> g)

jsonToGraphQL :: J.Value -> Either ErrorMessage (G.Value Void)
jsonToGraphQL = \case
  J.Null -> Right G.VNull
  J.Bool val -> Right $ G.VBoolean val
  J.String val -> Right $ G.VString val
  J.Number val -> case toBoundedInteger val of
    Just intVal -> Right $ G.VInt $ fromIntegral @Int64 intVal
    Nothing -> Right $ G.VFloat val
  J.Array vals -> G.VList <$> traverse jsonToGraphQL (V.toList vals)
  J.Object vals ->
    G.VObject . HashMap.fromList <$> for (KM.toList vals) \(key, val) -> do
      graphQLName <- maybe (invalidName key) Right $ G.mkName (K.toText key)
      (graphQLName,) <$> jsonToGraphQL val
  where
    invalidName key =
      Left $ "variable value contains an object with key " <> toErrorValue key <> ", which is not a legal GraphQL name"
