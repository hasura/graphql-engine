{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
-- |

module Hasura.RQL.DDL.Remote.Types where

import           Hasura.Prelude
import           Hasura.RQL.DDL.Remote.Input
import           Hasura.RQL.Instances ()
import           Hasura.RQL.Types.Common
import           Hasura.SQL.Types

import           Data.Aeson as A
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Aeson.Types (Parser)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Scientific
import           Data.Set (Set)
import           Data.Text (Text)
import qualified Database.PG.Query as Q
import           Instances.TH.Lift ()
import qualified Language.GraphQL.Draft.Syntax as G
import           Language.Haskell.TH.Syntax (Lift)

data RemoteField =
  RemoteField
    { rmfRemoteRelationship :: !RemoteRelationship
    , rmfGType :: !G.GType
    , rmfParamMap :: !(HashMap G.Name InpValInfo)
    }
  deriving (Show, Eq, Lift)

data RemoteRelationship =
  RemoteRelationship
    { rtrName :: RemoteRelationshipName
    , rtrTable :: QualifiedTable
    , rtrRemoteSchema :: RemoteSchemaName
    , rtrRemoteField :: G.Name
    , rtrHasuraFields :: Set FieldName
    , rtrRemoteArguments :: RemoteArguments
    }
  deriving (Show, Eq, Lift)

newtype RemoteSchemaName
  = RemoteSchemaName
  { unRemoteSchemaName :: Text}
  deriving (Show, Eq, Lift, Hashable, ToJSON, ToJSONKey, FromJSON, Q.ToPrepArg, Q.FromCol)

newtype RemoteRelationshipName
  = RemoteRelationshipName
  { unRemoteRelationshipName :: Text}
  deriving (Show, Eq, Lift, Hashable, ToJSON, ToJSONKey, FromJSON, Q.ToPrepArg, Q.FromCol)

$(deriveJSON (aesonDrop 3 snakeCase) ''RemoteRelationship)

--------------------------------------------------------------------------------
-- Custom JSON roundtrip instances for RemoteField and down

instance ToJSON RemoteField where
  toJSON RemoteField {..} =
    object
      [ "remote_relationship" .= toJSON rmfRemoteRelationship
      , "g_type" .= toJsonGType rmfGType
      , "param_map" .= fmap toJsonInpValInfo rmfParamMap
      ]

instance FromJSON RemoteField where
  parseJSON value = do
    hmap <- parseJSON value
    rmfRemoteRelationship <- hmap .: "remote_relationship"
    rmfGType <- hmap .: "g_type" >>= parseJsonGType
    rmfParamMap <- hmap .: "param_map" >>= traverse parseJsonInpValInfo
    pure RemoteField {..}

-- | Parse a GType, using Either as an auxilliary type.
parseJsonGType :: Value -> Parser G.GType
parseJsonGType value = do
  oneof <- parseJSON value
  case oneof of
    Left (nullability, namedType) ->
      pure (G.TypeNamed (G.Nullability nullability) namedType)
    Right (nullability, listTypeValue) -> do
      listType <- fmap G.ListType (parseJsonGType listTypeValue)
      pure (G.TypeList (G.Nullability nullability) listType)

-- | Convert to JSON, using Either as an auxilliary type.
toJsonGType :: G.GType -> Value
toJsonGType gtype =
  toJSON
    (case gtype of
       G.TypeNamed (G.Nullability nullability) namedType ->
         Left (nullability, namedType)
       G.TypeList (G.Nullability nullability) (G.ListType listType) ->
         Right (nullability, listType))

parseJsonInpValInfo :: Value -> Parser InpValInfo
parseJsonInpValInfo value = do
  hashmap <- parseJSON value
  _iviDesc <- hashmap .: "desc"
  _iviName <- hashmap .: "name"
  _iviDefVal <-
    hashmap .: "def_val" >>= maybe (pure Nothing) (fmap Just . parseValueConst)
  _iviType <- hashmap .: "type" >>= parseJsonGType
  pure InpValInfo {..}

toJsonInpValInfo :: InpValInfo -> Value
toJsonInpValInfo InpValInfo {..} =
  object
    [ "desc" .= _iviDesc
    , "name" .= _iviName
    , "def_val" .= fmap gValueConstToValue _iviDefVal
    , "type" .= _iviType
    ]

gValueConstToValue :: G.ValueConst -> A.Value
gValueConstToValue =
  \case
    (G.VCInt i) -> toJSON i
    (G.VCFloat f) -> toJSON f
    (G.VCString (G.StringValue s)) -> toJSON s
    (G.VCBoolean b) -> toJSON b
    G.VCNull -> A.Null
    (G.VCEnum s) -> toJSON s
    (G.VCList (G.ListValueG list)) -> toJSON (map gValueConstToValue list)
    (G.VCObject (G.ObjectValueG xs)) -> constFieldsToObject xs

constFieldsToObject :: [G.ObjectFieldG G.ValueConst] -> A.Value
constFieldsToObject =
  A.Object .
  HM.fromList .
  map
    (\(G.ObjectFieldG {_ofName = G.Name name, _ofValue}) ->
       (name, gValueConstToValue _ofValue))

parseValueConst :: A.Value -> Parser G.ValueConst
parseValueConst =
  \case
    A.Object hashmap ->
      fmap (G.VCObject . G.ObjectValueG) (parseObjectFields hashmap)
    A.Array array ->
      fmap (G.VCList . G.ListValueG . toList) (traverse parseValueConst array)
    A.String text -> pure (G.VCString (G.StringValue text))
    A.Number !number ->
      pure (either G.VCFloat G.VCInt (floatingOrInteger number))
    A.Bool !predicate -> pure (G.VCBoolean predicate)
    A.Null -> pure G.VCNull

parseObjectFields :: HashMap Text A.Value -> Parser [G.ObjectFieldG G.ValueConst]
parseObjectFields hashMap =
  traverse
    (\(key, value) -> do
       name <- parseJSON (A.String key)
       parsedValue <- parseValueConst value
       pure G.ObjectFieldG {_ofName = name, _ofValue = parsedValue})
    (HM.toList hashMap)
