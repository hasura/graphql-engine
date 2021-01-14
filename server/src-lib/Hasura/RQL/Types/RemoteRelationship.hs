{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Hasura.RQL.Types.RemoteRelationship
  ( RemoteRelationshipName(..)
  , remoteRelationshipNameToText
  , fromRemoteRelationship
  , RemoteFields(..)
  , RemoteFieldInfo(..)
  , RemoteRelationship(..)
  , RemoteRelationshipDef(..)
  , rrdRemoteSchema
  , rrdHasuraFields
  , rrdRemoteField
  , FieldCall(..)
  , RemoteArguments(..)
  , DeleteRemoteRelationship(..)
  , graphQLValueToJSON
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                as HM
import qualified Data.Text                          as T
import qualified Database.PG.Query                  as Q
import qualified Language.GraphQL.Draft.Syntax      as G

import           Control.Lens                       (makeLenses)
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Scientific
import           Data.Set                           (Set)
import           Data.Text.Extended
import           Data.Text.NonEmpty

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Incremental                 (Cacheable)
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.SQL.Backend


newtype RemoteRelationshipName
  = RemoteRelationshipName
  { unRemoteRelationshipName :: NonEmptyText}
  deriving ( Show, Eq, Ord, Hashable, ToJSON, ToJSONKey, FromJSON
           , Q.ToPrepArg, Q.FromCol, ToTxt, Cacheable, NFData, Arbitrary
           )

remoteRelationshipNameToText :: RemoteRelationshipName -> Text
remoteRelationshipNameToText = unNonEmptyText . unRemoteRelationshipName

fromRemoteRelationship :: RemoteRelationshipName -> FieldName
fromRemoteRelationship = FieldName . remoteRelationshipNameToText

-- | Resolved remote relationship
data RemoteFieldInfo (b :: BackendType)
  = RemoteFieldInfo
  { _rfiName             :: !RemoteRelationshipName
    -- ^ Field name to which we'll map the remote in hasura; this becomes part
    -- of the hasura schema.
  , _rfiParamMap         :: !(HashMap G.Name G.InputValueDefinition)
  -- ^ Input arguments to the remote field info; The '_rfiParamMap' will only
  --   include the arguments to the remote field that is being joined. The
  --   names of the arguments here are modified, it will be in the format of
  --   <Original Field Name>_remote_rel_<hasura table schema>_<hasura table name><remote relationship name>
  , _rfiHasuraFields     :: !(HashSet (ColumnInfo b))
  -- ^ Hasura fields used to join the remote schema node
  , _rfiRemoteFields     :: !RemoteFields
  , _rfiRemoteSchema     :: !RemoteSchemaInfo
  , _rfiSchemaIntrospect :: RemoteSchemaIntrospection
  -- ^ The introspection data is used to make parsers for the arguments and the selection set
  , _rfiRemoteSchemaName :: !RemoteSchemaName
  -- ^ Name of the remote schema, that's used for joining
  } deriving (Generic)
deriving instance Backend b => Eq (RemoteFieldInfo b)
instance Backend b => Cacheable (RemoteFieldInfo b)

graphQLValueToJSON :: G.Value Void -> Value
graphQLValueToJSON = \case
  G.VNull                 -> Null
  G.VInt i                -> toJSON i
  G.VFloat f              -> toJSON f
  G.VString t             -> toJSON t
  G.VBoolean b            -> toJSON b
  G.VEnum (G.EnumValue n) -> toJSON n
  G.VList values          -> toJSON $ graphQLValueToJSON <$> values
  G.VObject objects       -> toJSON $ graphQLValueToJSON <$> objects

instance Backend b => ToJSON (RemoteFieldInfo b) where
  toJSON RemoteFieldInfo{..} = object
    [ "name" .= _rfiName
    , "param_map" .= fmap toJsonInpValInfo _rfiParamMap
    , "hasura_fields" .= _rfiHasuraFields
    , "remote_fields" .= _rfiRemoteFields
    , "remote_schema" .= _rfiRemoteSchema
    ]
    where
      toJsonInpValInfo (G.InputValueDefinition desc name type' defVal _directives)  =
        object
          [ "desc" .= desc
          , "name" .= name
          , "def_val" .= fmap graphQLValueToJSON defVal
          , "type" .= type'
          ]

-- | For some 'FieldCall', for instance, associates a field argument name with
-- either a list of either scalar values or some 'G.Variable' we are closed
-- over (brought into scope, e.g. in 'rtrHasuraFields'.
newtype RemoteArguments =
  RemoteArguments
    { getRemoteArguments :: HashMap G.Name (G.Value G.Name)
    } deriving (Show, Eq, Cacheable, NFData)

instance ToJSON RemoteArguments where
  toJSON (RemoteArguments fields) = fieldsToObject fields
    where
      fieldsToObject =
        Object .
        HM.fromList .
        map
          (\(name, val) ->
             (G.unName name, gValueToValue val)) .
        HM.toList

      gValueToValue =
        \case
          G.VVariable v -> toJSON ("$" <> G.unName v)
          G.VInt i      -> toJSON i
          G.VFloat f    -> toJSON f
          G.VString s   -> toJSON s
          G.VBoolean b  -> toJSON b
          G.VNull       -> Null
          G.VEnum s     -> toJSON s
          G.VList list  -> toJSON (map gValueToValue list)
          G.VObject obj -> fieldsToObject obj

instance FromJSON RemoteArguments where
  parseJSON = \case
    Object hashMap -> fmap RemoteArguments (parseObjectFieldsToGValue hashMap)
    _              -> fail "Remote arguments should be an object of keys."
    where
      -- Parsing GraphQL input arguments from JSON
      parseObjectFieldsToGValue hashMap = do
        bleh <-
          traverse
          (\(key, value) -> do
              name <- G.mkName key `onNothing` fail (T.unpack key <> " is an invalid key name")
              parsedValue <- parseValueAsGValue value
              pure (name,parsedValue))
             (HM.toList hashMap)
        pure $ HM.fromList bleh

      parseValueAsGValue =
        \case
          Object obj ->
            fmap G.VObject (parseObjectFieldsToGValue obj)
          Array array ->
            fmap (G.VList . toList) (traverse parseValueAsGValue array)
          String text ->
            case T.uncons text of
              Just ('$', rest)
                | T.null rest -> fail "Invalid variable name."
                | otherwise ->
                    case G.mkName rest of
                      Nothing    -> fail "Invalid variable name."
                      Just name' -> pure $ G.VVariable name'
              _ -> pure (G.VString text)
          Number !scientificNum ->
            pure (either (\(_::Float) -> G.VFloat scientificNum) G.VInt (floatingOrInteger scientificNum))
          Bool !boolean -> pure (G.VBoolean boolean)
          Null -> pure G.VNull

-- | Associates a field name with the arguments it will be passed in the query.
--
-- https://graphql.github.io/graphql-spec/June2018/#sec-Language.Arguments
--
-- TODO (from master) we don't seem to support empty RemoteArguments (like 'hello'), but this seems arbitrary:
data FieldCall =
  FieldCall
    { fcName      :: !G.Name
    , fcArguments :: !RemoteArguments
    } deriving (Show, Eq, Generic)
instance NFData FieldCall
instance Cacheable FieldCall

newtype RemoteFields = RemoteFields {unRemoteFields :: NonEmpty FieldCall}
  deriving (Show, Eq, Generic)
instance NFData RemoteFields
instance Cacheable RemoteFields

instance FromJSON RemoteFields where
  parseJSON = fmap RemoteFields . parseRemoteFields
    where
      parseRemoteFields v =
        case v of
          Object hashmap ->
            case HM.toList hashmap of
              [(fieldNameText, callValue)] -> do
                fieldName <- parseJSON (String fieldNameText)
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

instance ToJSON RemoteFields where
  toJSON (RemoteFields fields) = remoteFieldsJson fields
    where
      remoteFieldsJson (field :| subfields) =
        object
          [ G.unName (fcName field) .=
            object
              (concat
                 [ ["arguments" .= fcArguments field]
                 , case subfields of
                     [] -> []
                     subfield:subsubfields ->
                       ["field" .= remoteFieldsJson (subfield :| subsubfields)]
                 ]
              )
          ]

-- | Metadata type for remote relationship
data RemoteRelationship =
  RemoteRelationship
    { rtrName         :: !RemoteRelationshipName
    -- ^ Field name to which we'll map the remote in hasura; this becomes part
    -- of the hasura schema.
    , rtrSource       :: !SourceName
    , rtrTable        :: !QualifiedTable
    -- ^ (SourceName, QualifiedTable) determines the table on which the relationship
    -- is defined
    , rtrHasuraFields :: !(Set FieldName) -- TODO (from master)? change to PGCol
    -- ^ The hasura fields from 'rtrTable' that will be in scope when resolving
    -- the remote objects in 'rtrRemoteField'.
    , rtrRemoteSchema :: !RemoteSchemaName
    -- ^ Identifier for this mapping.
    , rtrRemoteField  :: !RemoteFields
    }  deriving (Show, Eq, Generic)
instance NFData RemoteRelationship
instance Cacheable RemoteRelationship
$(deriveToJSON (aesonDrop 3 snakeCase) ''RemoteRelationship)

instance FromJSON RemoteRelationship where
  parseJSON = withObject "Object" $ \o ->
    RemoteRelationship
      <$> o .: "name"
      <*> o .:? "source" .!= defaultSource
      <*> o .: "table"
      <*> o .: "hasura_fields"
      <*> o .: "remote_schema"
      <*> o .: "remote_field"

data RemoteRelationshipDef
  = RemoteRelationshipDef
  { _rrdRemoteSchema :: !RemoteSchemaName
  , _rrdHasuraFields :: !(Set FieldName)
  , _rrdRemoteField  :: !RemoteFields
  } deriving (Show, Eq, Generic)
instance Cacheable RemoteRelationshipDef
$(deriveJSON (aesonDrop 4 snakeCase) ''RemoteRelationshipDef)
$(makeLenses ''RemoteRelationshipDef)

data DeleteRemoteRelationship
  = DeleteRemoteRelationship
  { drrSource :: !SourceName
  , drrTable  :: !QualifiedTable
  , drrName   :: !RemoteRelationshipName
  } deriving (Show, Eq)
instance FromJSON DeleteRemoteRelationship where
  parseJSON = withObject "Object" $ \o ->
    DeleteRemoteRelationship
      <$> o .:? "source" .!= defaultSource
      <*> o .: "table"
      <*> o .: "name"

$(deriveToJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''DeleteRemoteRelationship)
