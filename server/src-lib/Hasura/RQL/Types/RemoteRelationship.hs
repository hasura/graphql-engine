{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE RecordWildCards          #-}

-- |

module Hasura.RQL.Types.RemoteRelationship where

import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Hasura.Prelude
import           Hasura.RQL.Instances          ()
import           Hasura.RQL.Types.Common
import           Hasura.SQL.Types

import           Data.Aeson                    as A
import qualified Data.Aeson.Types              as AT
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HM
import           Data.List.NonEmpty            (NonEmpty (..))
import           Data.Scientific
import           Data.Set                      (Set)

import qualified Data.Text                     as T
import           Data.Validation
import qualified Database.PG.Query             as Q
import           Instances.TH.Lift             ()
import qualified Language.GraphQL.Draft.Syntax as G
import           Language.Haskell.TH.Syntax    (Lift)

import           Hasura.RQL.Types.RemoteSchema

data RemoteField =
  RemoteField
    { rmfRemoteRelationship :: !RemoteRelationship
    , rmfGType              :: !G.GType
    , rmfParamMap           :: !(HashMap G.Name InpValInfo)
    }
  deriving (Show, Eq, Lift)

data RemoteRelationship =
  RemoteRelationship
    { rtrName         :: RemoteRelationshipName
    , rtrTable        :: QualifiedTable
    , rtrHasuraFields :: Set FieldName -- change to PGCol
    , rtrRemoteSchema :: RemoteSchemaName
    , rtrRemoteFields :: NonEmpty FieldCall
    }  deriving (Show, Eq, Lift)

-- Parsing GraphQL input arguments from JSON

parseObjectFieldsToGValue :: HashMap Text A.Value -> AT.Parser [G.ObjectFieldG G.Value]
parseObjectFieldsToGValue hashMap =
  traverse
    (\(key, value) -> do
       name <- parseJSON (A.String key)
       parsedValue <- parseValue value
       pure G.ObjectFieldG {_ofName = name, _ofValue = parsedValue})
    (HM.toList hashMap)

parseValue :: A.Value -> AT.Parser G.Value
parseValue =
  \case
    A.Object obj ->
      fmap (G.VObject . G.ObjectValueG) (parseObjectFieldsToGValue obj)
    A.Array array ->
      fmap (G.VList . G.ListValueG . toList) (traverse parseValue array)
    A.String text ->
      case T.uncons text of
        Just ('$', rest)
          | T.null rest -> fail "Invalid variable name."
          | otherwise -> pure (G.VVariable (G.Variable (G.Name rest)))
        _ -> pure (G.VString (G.StringValue text))
    A.Number !scientificNum ->
      pure (either G.VFloat G.VInt (floatingOrInteger scientificNum))
    A.Bool !boolean -> pure (G.VBoolean boolean)
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

parseRemoteArguments :: A.Value -> AT.Parser RemoteArguments
parseRemoteArguments j =
  case j of
    A.Object hashMap -> fmap RemoteArguments (parseObjectFieldsToGValue hashMap)
    _                -> fail "Remote arguments should be an object of keys."

newtype RemoteArguments =
  RemoteArguments
    { getRemoteArguments :: [G.ObjectFieldG G.Value]
    } deriving (Show, Eq, Lift)

instance ToJSON RemoteArguments where
  toJSON (RemoteArguments fields) = fieldsToObject fields

instance FromJSON RemoteArguments where
  parseJSON = parseRemoteArguments

data FieldCall =
  FieldCall
    { fcName      :: !G.Name
    , fcArguments :: !RemoteArguments
    }
  deriving (Show, Eq, Lift, Generic)

newtype RemoteRelationshipName
  = RemoteRelationshipName
  { unRemoteRelationshipName :: Text}
  deriving (Show, Eq, Lift, Hashable, ToJSON, ToJSONKey, FromJSON, Q.ToPrepArg, Q.FromCol)

data DeleteRemoteRelationship =
  DeleteRemoteRelationship
    { drrTable :: QualifiedTable
    , drrName  :: RemoteRelationshipName
    }  deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''DeleteRemoteRelationship)

--------------------------------------------------------------------------------
-- Custom JSON roundtrip instances for RemoteField and down

instance ToJSON RemoteRelationship where
  toJSON RemoteRelationship {..} =
    object
      [ "name" .= rtrName
      , "table" .= rtrTable
      , "hasura_fields" .= rtrHasuraFields
      , "remote_schema" .= rtrRemoteSchema
      , "remote_field" .= remoteFieldsJson rtrRemoteFields
      ]

instance FromJSON RemoteRelationship where
  parseJSON value = do
    o <- parseJSON value
    rtrName <- o .: "name"
    rtrTable <- o .: "table"
    rtrHasuraFields <- o .: "hasura_fields"
    rtrRemoteSchema <- o .: "remote_schema"
    rtrRemoteFields <- o .: "remote_field" >>= parseRemoteFields
    pure RemoteRelationship {..}

parseRemoteFields :: Value -> AT.Parser (NonEmpty FieldCall)
parseRemoteFields v =
  case v of
    Object hashmap ->
      case HM.toList hashmap of
        [(fieldNameText, callValue)] -> do
          fieldName <- parseJSON (A.String fieldNameText)
          callObject <- parseJSON callValue
          arguments <- callObject .: "arguments"
          maybeSubField <- callObject .:? "field"
          subFields <-
            case maybeSubField of
              Nothing -> pure []
              Just fieldValue -> do
                remoteFields <- parseRemoteFields fieldValue
                pure (toList remoteFields)
          pure
            (FieldCall {fcName = fieldName, fcArguments = arguments} :|
             subFields)
        _ -> fail "Only one field allowed, not multiple."
    _ ->
      fail
        "Remote fields should be an object that starts\
              \ with the name of a field e.g. person: ..."

remoteFieldsJson :: NonEmpty FieldCall -> Value
remoteFieldsJson (field :| subfields) =
  object
    [ nameText (fcName field) .=
      object
        (concat
           [ ["arguments" .= fcArguments field]
           , case subfields of
               [] -> []
               subfield:subsubfields ->
                 ["field" .= remoteFieldsJson (subfield :| subsubfields)]
           ])
    ]
  where
    nameText (G.Name t) = t

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
parseJsonGType :: Value -> AT.Parser G.GType
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

parseJsonInpValInfo :: Value -> AT.Parser InpValInfo
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

parseValueConst :: A.Value -> AT.Parser G.ValueConst
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

parseObjectFields :: HashMap Text A.Value -> AT.Parser [G.ObjectFieldG G.ValueConst]
parseObjectFields hashMap =
  traverse
    (\(key, value) -> do
       name <- parseJSON (A.String key)
       parsedValue <- parseValueConst value
       pure G.ObjectFieldG {_ofName = name, _ofValue = parsedValue})
    (HM.toList hashMap)


-- | An error substituting variables into the argument list.
data SubstituteError
  = ValueNotProvided !G.Variable
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Operations

-- | Substitute values in the argument list.
substituteVariables ::
     HashMap G.Variable G.ValueConst
  -- ^ Values to use.
  -> [G.ObjectFieldG G.Value]
  -- ^ A template.
  -> Validation [SubstituteError] [G.ObjectFieldG G.ValueConst]
substituteVariables values = traverse (traverse go)
  where
    go =
      \case
        G.VVariable variable ->
          case HM.lookup variable values of
            Nothing         -> Failure [ValueNotProvided variable]
            Just valueConst -> pure valueConst
        G.VInt int32 -> pure (G.VCInt int32)
        G.VFloat double -> pure (G.VCFloat double)
        G.VString stringValue -> pure (G.VCString stringValue)
        G.VBoolean boolean -> pure (G.VCBoolean boolean)
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

