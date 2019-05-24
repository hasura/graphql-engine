{-# LANGUAGE DisambiguateRecordFields #-}

-- | Input object types.

module Hasura.GraphQL.Remote.Input
  ( substituteVariables
  , SubstituteError(..)
  , RemoteArguments(..)
  ) where

import           Data.Aeson (FromJSON(..),ToJSON(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Foldable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Scientific (floatingOrInteger)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Validation
import qualified Language.GraphQL.Draft.Syntax as GraphQL
import           Language.Haskell.TH.Syntax (Lift)
import           Prelude

--------------------------------------------------------------------------------
-- Type definitions

newtype RemoteArguments =
  RemoteArguments
    { getRemoteArguments :: [GraphQL.ObjectFieldG GraphQL.Value]
    } deriving (Show, Eq, Lift)

instance ToJSON RemoteArguments where
  toJSON = error "TODO: ToJSON: RemoteArguments"

instance FromJSON RemoteArguments where
  parseJSON = parseRemoteArguments

-- | An error substituting variables into the argument list.
data SubstituteError
  = ValueNotProvided !GraphQL.Variable
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Operations

-- | Substitute values in the argument list.
substituteVariables ::
     Map GraphQL.Variable GraphQL.ValueConst
  -- ^ Values to use.
  -> [GraphQL.ObjectFieldG GraphQL.Value]
  -- ^ A template.
  -> Validation [SubstituteError] [GraphQL.ObjectFieldG GraphQL.ValueConst]
substituteVariables values = traverse (traverse go)
  where
    go =
      \case
        GraphQL.VVariable variable ->
          case M.lookup variable values of
            Nothing -> Failure [ValueNotProvided variable]
            Just valueConst -> pure valueConst
        GraphQL.VInt int32 -> pure (GraphQL.VCInt int32)
        GraphQL.VFloat double -> pure (GraphQL.VCFloat double)
        GraphQL.VString stringValue -> pure (GraphQL.VCString stringValue)
        GraphQL.VBoolean bool -> pure (GraphQL.VCBoolean bool)
        GraphQL.VNull -> pure GraphQL.VCNull
        GraphQL.VEnum enumValue -> pure (GraphQL.VCEnum enumValue)
        GraphQL.VList (GraphQL.ListValueG listValue) ->
          fmap (GraphQL.VCList . GraphQL.ListValueG) (traverse go listValue)
        GraphQL.VObject (GraphQL.ObjectValueG objectValue) ->
          fmap (GraphQL.VCObject . GraphQL.ObjectValueG) (traverse (traverse go) objectValue)

--------------------------------------------------------------------------------
-- Parsing GraphQL input arguments from JSON

parseRemoteArguments :: Aeson.Value -> Aeson.Parser RemoteArguments
parseRemoteArguments j =
  case j of
    Aeson.Object hashMap -> fmap RemoteArguments (parseObjectFields hashMap)
    _ -> fail "Remote arguments should be an object of keys."

parseObjectFields :: HashMap Text Aeson.Value -> Aeson.Parser [GraphQL.ObjectFieldG GraphQL.Value]
parseObjectFields hashMap =
  traverse
    (\(key, value) -> do
       name <- parseJSON (Aeson.String key)
       parsedValue <- parseValue value
       pure GraphQL.ObjectFieldG {_ofName = name, _ofValue = parsedValue})
    (HM.toList hashMap)

parseValue :: Aeson.Value -> Aeson.Parser GraphQL.Value
parseValue =
  \case
    Aeson.Object object ->
      fmap (GraphQL.VObject . GraphQL.ObjectValueG) (parseObjectFields object)
    Aeson.Array array ->
      fmap (GraphQL.VList . GraphQL.ListValueG . toList) (traverse parseValue array)
    Aeson.String text ->
      case T.uncons text of
        Just ('$', rest)
          | T.null rest -> fail "Invalid variable name."
          | otherwise -> pure (GraphQL.VVariable (GraphQL.Variable (GraphQL.Name rest)))
        _ -> pure (GraphQL.VString (GraphQL.StringValue text))
    Aeson.Number !scientific ->
      pure (either GraphQL.VFloat GraphQL.VInt (floatingOrInteger scientific))
    Aeson.Bool !bool -> pure (GraphQL.VBoolean bool)
    Aeson.Null -> pure GraphQL.VNull
