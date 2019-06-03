{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}

-- | Input object types.

module Hasura.RQL.DDL.Remote.Input
  ( substituteVariables
  , remoteArgumentsToMap
  , SubstituteError(..)
  , RemoteArguments(..)
  ) where

import           Data.Aeson (FromJSON(..),ToJSON(..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import           Data.Foldable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Scientific (floatingOrInteger)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Validation
import qualified Language.GraphQL.Draft.Syntax as G
import           Language.Haskell.TH.Syntax (Lift)
import           Prelude

--------------------------------------------------------------------------------
-- Type definitions

newtype RemoteArguments =
  RemoteArguments
    { getRemoteArguments :: [G.ObjectFieldG G.Value]
    } deriving (Show, Eq, Lift)

instance ToJSON RemoteArguments where
  toJSON (RemoteArguments fields) = fieldsToObject fields

instance FromJSON RemoteArguments where
  parseJSON = parseRemoteArguments

-- | An error substituting variables into the argument list.
data SubstituteError
  = ValueNotProvided !G.Variable
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Operations

-- | Substitute values in the argument list.
substituteVariables ::
     Map G.Variable G.ValueConst
  -- ^ Values to use.
  -> [G.ObjectFieldG G.Value]
  -- ^ A template.
  -> Validation [SubstituteError] [G.ObjectFieldG G.ValueConst]
substituteVariables values = traverse (traverse go)
  where
    go =
      \case
        G.VVariable variable ->
          case M.lookup variable values of
            Nothing -> Failure [ValueNotProvided variable]
            Just valueConst -> pure valueConst
        G.VInt int32 -> pure (G.VCInt int32)
        G.VFloat double -> pure (G.VCFloat double)
        G.VString stringValue -> pure (G.VCString stringValue)
        G.VBoolean bool -> pure (G.VCBoolean bool)
        G.VNull -> pure G.VCNull
        G.VEnum enumValue -> pure (G.VCEnum enumValue)
        G.VList (G.ListValueG listValue) ->
          fmap (G.VCList . G.ListValueG) (traverse go listValue)
        G.VObject (G.ObjectValueG objectValue) ->
          fmap (G.VCObject . G.ObjectValueG) (traverse (traverse go) objectValue)

-- | Make a map out of remote arguments.
remoteArgumentsToMap :: RemoteArguments -> HashMap G.Name G.Value
remoteArgumentsToMap =
  HM.fromList .
  map (\field -> (G._ofName field, G._ofValue field)) .
  getRemoteArguments

--------------------------------------------------------------------------------
-- Parsing GraphQL input arguments from JSON

parseRemoteArguments :: A.Value -> A.Parser RemoteArguments
parseRemoteArguments j =
  case j of
    A.Object hashMap -> fmap RemoteArguments (parseObjectFields hashMap)
    _ -> fail "Remote arguments should be an object of keys."

parseObjectFields :: HashMap Text A.Value -> A.Parser [G.ObjectFieldG G.Value]
parseObjectFields hashMap =
  traverse
    (\(key, value) -> do
       name <- parseJSON (A.String key)
       parsedValue <- parseValue value
       pure G.ObjectFieldG {_ofName = name, _ofValue = parsedValue})
    (HM.toList hashMap)

parseValue :: A.Value -> A.Parser G.Value
parseValue =
  \case
    A.Object object ->
      fmap (G.VObject . G.ObjectValueG) (parseObjectFields object)
    A.Array array ->
      fmap (G.VList . G.ListValueG . toList) (traverse parseValue array)
    A.String text ->
      case T.uncons text of
        Just ('$', rest)
          | T.null rest -> fail "Invalid variable name."
          | otherwise -> pure (G.VVariable (G.Variable (G.Name rest)))
        _ -> pure (G.VString (G.StringValue text))
    A.Number !scientific ->
      pure (either G.VFloat G.VInt (floatingOrInteger scientific))
    A.Bool !bool -> pure (G.VBoolean bool)
    A.Null -> pure G.VNull

fieldsToObject :: [G.ObjectFieldG G.Value] -> A.Value
fieldsToObject =
  A.Object .
  HM.fromList .
  map (\(G.ObjectFieldG {_ofName=G.Name name, _ofValue}) -> (name, gValueToValue _ofValue))

gValueToValue :: G.Value -> A.Value
gValueToValue =
  \case
    (G.VVariable (G.Variable v)) -> toJSON ("$" <> v)
    (G.VInt i) -> toJSON i
    (G.VFloat f) -> toJSON f
    (G.VString (G.StringValue s)) -> toJSON s
    (G.VBoolean b) -> toJSON b
    G.VNull -> A.Null
    (G.VEnum s) -> toJSON s
    (G.VList (G.ListValueG list)) -> toJSON (map gValueToValue list)
    (G.VObject (G.ObjectValueG xs)) -> fieldsToObject xs
