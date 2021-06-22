module Hasura.RQL.Types.RemoteRelationship
  ( RemoteRelationshipName(..)
  , remoteRelationshipNameToText
  , fromRemoteRelationship
  , RemoteFields(..)
  , RemoteFieldInfo(..)
  , RemoteSchemaFieldInfo(..)
  , RemoteRelationship(..)
  , RemoteSchemaRelationshipDef(..)
  , RemoteSourceRelationshipType (..)
  , RemoteSourceRelationshipDef(..)
  , RemoteSourceRelationshipInfo(..)
  , RemoteRelationshipDef(..)
  , rrdRemoteSchemaName
  , rrdHasuraFields
  , rrdRemoteField
  , rtrDefinition
  , rtrName
  , rtrSource
  , rtrTable
  , _RemoteSchemaRelDef
  , FieldCall(..)
  , RemoteArguments(..)
  , DeleteRemoteRelationship(..)
  , graphQLValueToJSON
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import qualified Database.PG.Query             as Q
import qualified Language.GraphQL.Draft.Syntax as G

import           Control.Lens                  (makeLenses, makePrisms)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bifunctor                (bimap)
import           Data.Scientific
import           Data.Text.Extended
import           Data.Text.NonEmpty

import           Hasura.Incremental            (Cacheable)
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Instances    ()
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.SQL.AnyBackend         (AnyBackend)
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

data RemoteSourceRelationshipInfo src tgt
  = RemoteSourceRelationshipInfo
  { _rsriName         :: !RemoteRelationshipName
  , _rsriType         :: !RemoteSourceRelationshipType
  , _rsriSource       :: !SourceName
  , _rsriSourceConfig :: !(SourceConfig tgt)
  , _rsriTable        :: !(TableName tgt) -- this is parsed from `Value`
  , _rsriMapping      :: !(HM.HashMap FieldName (ColumnInfo src, ScalarType tgt, Column tgt))
  }
  deriving stock Generic

deriving instance (Backend src, Backend tgt) => Eq (RemoteSourceRelationshipInfo src tgt)
instance (Backend src, Backend tgt) => Cacheable (RemoteSourceRelationshipInfo src tgt)

data RemoteFieldInfo (b :: BackendType)
  = RFISchema !(RemoteSchemaFieldInfo b)
  | RFISource !(AnyBackend (RemoteSourceRelationshipInfo b))
  deriving (Generic)
deriving instance Backend b => Eq (RemoteFieldInfo b)
instance Backend b => Cacheable (RemoteFieldInfo b)

instance Backend b => ToJSON (RemoteFieldInfo b) where
  toJSON =
    \case
      RFISchema schema -> toJSON schema
      RFISource _      -> toJSON ()

-- | Resolved remote relationship
data RemoteSchemaFieldInfo (b :: BackendType)
  = RemoteSchemaFieldInfo
  { _rfiName                  :: !RemoteRelationshipName
    -- ^ Field name to which we'll map the remote in hasura; this becomes part
    -- of the hasura schema.
  , _rfiParamMap              :: !(HashMap G.Name RemoteSchemaInputValueDefinition)
  -- ^ Input arguments to the remote field info; The '_rfiParamMap' will only
  --   include the arguments to the remote field that is being joined. The
  --   names of the arguments here are modified, it will be in the format of
  --   <Original Field Name>_remote_rel_<hasura table schema>_<hasura table name><remote relationship name>
  , _rfiHasuraFields          :: !(HashSet (ColumnInfo b))
  -- ^ Hasura fields used to join the remote schema node
  , _rfiRemoteFields          :: !RemoteFields
  , _rfiRemoteSchema          :: !RemoteSchemaInfo
  , _rfiInputValueDefinitions :: ![G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition]
  -- ^ The new input value definitions created for this remote field
  , _rfiRemoteSchemaName      :: !RemoteSchemaName
  -- ^ Name of the remote schema, that's used for joining
  , _rfiTable                 :: !(TableName b, SourceName)
  -- ^ Name of the table and its source
  } deriving (Generic)
deriving instance Backend b => Eq (RemoteSchemaFieldInfo b)
deriving instance Backend b => Show (RemoteSchemaFieldInfo b)
instance Backend b => Cacheable (RemoteSchemaFieldInfo b)

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

instance Backend b => ToJSON (RemoteSchemaFieldInfo b) where
  toJSON RemoteSchemaFieldInfo{..} = object
    [ "name" .= _rfiName
    , "param_map" .= fmap toJsonInpValInfo _rfiParamMap
    , "hasura_fields" .= _rfiHasuraFields
    , "remote_fields" .= _rfiRemoteFields
    , "remote_schema" .= _rfiRemoteSchema
    ]
    where
      toJsonInpValInfo (RemoteSchemaInputValueDefinition (G.InputValueDefinition desc name type' defVal _directives) _preset) =
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
    } deriving (Show, Eq, Generic, Cacheable, NFData)

instance Hashable RemoteArguments

instance ToJSON RemoteArguments where
  toJSON (RemoteArguments fields) = fieldsToObject fields
    where
      fieldsToObject =
        Object . HM.fromList . map (bimap G.unName gValueToValue) . HM.toList

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
      parseObjectFieldsToGValue hashMap =
        HM.fromList <$>
        traverse
          (\(key, value) -> do
              name <- G.mkName key `onNothing` fail (T.unpack key <> " is an invalid key name")
              parsedValue <- parseValueAsGValue value
              pure (name,parsedValue))
             (HM.toList hashMap)

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
data FieldCall
  = FieldCall
  { fcName      :: !G.Name
  , fcArguments :: !RemoteArguments
  } deriving (Show, Eq, Generic)
instance NFData FieldCall
instance Cacheable FieldCall
instance Hashable FieldCall

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
              (("arguments" .= fcArguments field) : (case subfields of
                                                       [] -> []
                                                       subfield:subsubfields ->
                                                         ["field" .= remoteFieldsJson (subfield :| subsubfields)])
              )
          ]

data RemoteSourceRelationshipType = ArrayRelationship | ObjectRelationship
  deriving stock (Generic, Show, Eq)

instance NFData RemoteSourceRelationshipType
instance Cacheable RemoteSourceRelationshipType
instance ToJSON RemoteSourceRelationshipType where
  toJSON =
    \case
      ArrayRelationship  -> String "array_relationship"
      ObjectRelationship -> String "object_relationship"
instance FromJSON RemoteSourceRelationshipType where
  parseJSON =
    \case
      (String "array_relationship")  -> pure ArrayRelationship
      (String "object_relationship") -> pure ObjectRelationship
      _                              -> fail "Expected \"array_relationship\" or \"object_relationship\"."

data RemoteSourceRelationshipDef =
  RemoteSourceRelationshipDef
    { rsrRelationshipType :: !RemoteSourceRelationshipType
    , rsrFieldMapping     :: !(HashMap FieldName FieldName)
    , rsrSource           :: !SourceName
    , rsrTable            :: !Value
    } deriving stock (Generic, Show, Eq)
instance NFData    RemoteSourceRelationshipDef
instance Cacheable RemoteSourceRelationshipDef

instance ToJSON RemoteSourceRelationshipDef where
  toJSON = genericToJSON hasuraJSON

instance FromJSON RemoteSourceRelationshipDef where
  parseJSON = withObject "RemoteSourceRelationship" $ \o ->
    RemoteSourceRelationshipDef
      <$> o .: "relationship_type"
      <*> o .: "field_mapping"
      <*> o .: "source"
      <*> o .: "table"

-- | This is the relationship type we store in our metadata.
data RemoteSchemaRelationshipDef
  = RemoteSchemaRelationshipDef
  { _rrdRemoteSchemaName :: !RemoteSchemaName
  , _rrdHasuraFields     :: !(HashSet FieldName)
  , _rrdRemoteField      :: !RemoteFields
  } deriving stock (Show, Eq, Generic)
instance NFData RemoteSchemaRelationshipDef
instance Cacheable RemoteSchemaRelationshipDef
$(deriveJSON hasuraJSON ''RemoteSchemaRelationshipDef)
$(makeLenses ''RemoteSchemaRelationshipDef)

data RemoteRelationshipDef
  = RemoteSchemaRelDef !RemoteSchemaRelationshipDef
  | RemoteSourceRelDef !RemoteSourceRelationshipDef
  deriving stock (Show, Eq, Generic)
instance NFData RemoteRelationshipDef
instance Cacheable RemoteRelationshipDef
$(makePrisms ''RemoteRelationshipDef)

instance FromJSON RemoteRelationshipDef where
  parseJSON j = j & withObject "RemoteRelationshipDef" \o -> do
    (mSource :: Maybe Value) <- o .:? "remote_source"
    (mSchema :: Maybe Value) <- o .:? "remote_schema"
    case (mSource, mSchema) of
      (Nothing, Nothing) -> fail "expected one of \"remote_source\" or \"remote_schema\""
      (Just _, Just _)   -> fail "expected only one of \"remote_source\" or \"remote_schema\""
      (Just source, _)   -> RemoteSourceRelDef <$> parseJSON source
      (_, Just schema)   ->
        case schema of
          Object _ -> RemoteSchemaRelDef <$> parseJSON schema
          _        -> do -- old parser format
            fmap RemoteSchemaRelDef $ RemoteSchemaRelationshipDef
              <$> o .: "remote_schema"
              <*> o .: "hasura_fields"
              <*> o .: "remote_field"

instance ToJSON RemoteRelationshipDef where
  toJSON = \case
    RemoteSourceRelDef source -> object [ "remote_source" .= toJSON source ]
    RemoteSchemaRelDef schema -> object [ "remote_schema" .= toJSON schema ]

-- | Metadata type for remote relationship
data RemoteRelationship b =
  RemoteRelationship
    { _rtrName       :: !RemoteRelationshipName
    -- ^ Field name to which we'll map the remote in hasura; this becomes part
    -- of the hasura schema.
    , _rtrSource     :: !SourceName
    , _rtrTable      :: !(TableName b)
    -- ^ (SourceName, QualifiedTable) determines the table on which the relationship
    -- is defined
    , _rtrDefinition :: !RemoteRelationshipDef
    }  deriving (Generic)
deriving instance (Backend b) => Show (RemoteRelationship b)
deriving instance (Backend b) => Eq   (RemoteRelationship b)
instance (Backend b) => NFData    (RemoteRelationship b)
instance (Backend b) => Cacheable (RemoteRelationship b)

instance (Backend b) => ToJSON (RemoteRelationship b) where
  toJSON = genericToJSON hasuraJSON

instance (Backend b) => FromJSON (RemoteRelationship b) where
  parseJSON = withObject "RemoteRelationship" $ \o -> do
    hasuraFields <- o .: "hasura_fields"
    name         <- o .: "remote_schema" <|> o .: "remote_schema_name"
    remoteField  <- o .: "remote_field"
    RemoteRelationship
      <$> o .: "name"
      <*> o .:? "source" .!= defaultSource
      <*> o .: "table"
      <*> pure (RemoteSchemaRelDef $ RemoteSchemaRelationshipDef name hasuraFields remoteField)
$(makeLenses ''RemoteRelationship)

data DeleteRemoteRelationship (b :: BackendType)
  = DeleteRemoteRelationship
  { drrSource :: !SourceName
  , drrTable  :: !(TableName b)
  , drrName   :: !RemoteRelationshipName
  } deriving stock (Generic)
deriving instance (Backend b) => Show (DeleteRemoteRelationship b)
deriving instance (Backend b) => Eq   (DeleteRemoteRelationship b)

instance Backend b => FromJSON (DeleteRemoteRelationship b) where
  parseJSON = withObject "Object" $ \o ->
    DeleteRemoteRelationship
      <$> o .:? "source" .!= defaultSource
      <*> o .: "table"
      <*> o .: "name"

instance Backend b => ToJSON (DeleteRemoteRelationship b) where
  toJSON = genericToJSON hasuraJSON{omitNothingFields=True}
