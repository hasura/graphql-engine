module Hasura.RQL.Types.Relationships.FromSource where

import Control.Lens (makeLenses, makePrisms)
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types (prependFailure)
import Data.Bifunctor (bimap)
import Data.HashMap.Strict qualified as HM
import Data.Scientific
import Data.Text qualified as T
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Instances ()
import Hasura.RQL.Types.RemoteSchema
import Hasura.SQL.AnyBackend (AnyBackend)
import Hasura.SQL.Backend
import Language.GraphQL.Draft.Syntax qualified as G

--------------------------------------------------------------------------------
-- metadata

-- | A remote relationship from a source targets either another source, or a
-- (remote) schema.
-- In the case of a remote schema, we keep a 'RRFormat' info, which keeps track
-- of versioning information in the serialization.
--
-- FIXME: move this to Hasura/Metadata
data FromSourceRelationshipDef
  = FromSourceToSchemaRelDef !RRFormat !FromSourceToSchemaRelationshipDef
  | FromSourceToSourceRelDef !FromSourceToSourceRelationshipDef
  deriving stock (Show, Eq, Generic)

instance NFData FromSourceRelationshipDef

instance Cacheable FromSourceRelationshipDef

instance FromJSON FromSourceRelationshipDef where
  parseJSON j =
    j & withObject "FromSourceRelationshipDef" \o -> do
      (mSource :: Maybe Value) <- o .:? "remote_source"
      (mSchema :: Maybe Value) <- o .:? "remote_schema"
      case (mSource, mSchema) of
        (Nothing, Nothing) -> fail "expected one of \"remote_source\" or \"remote_schema\""
        (Just _, Just _) -> fail "expected only one of \"remote_source\" or \"remote_schema\""
        (Just source, _) -> FromSourceToSourceRelDef <$> parseJSON source
        (_, Just schema) ->
          case schema of
            Object _ -> FromSourceToSchemaRelDef RFSchemaAndSource <$> parseJSON schema
            _ -> do
              -- old parser format
              fmap (FromSourceToSchemaRelDef RFRemoteSchemaOnly) $
                FromSourceToSchemaRelationshipDef
                  <$> o .: "remote_schema"
                  <*> o .: "hasura_fields"
                  <*> o .: "remote_field"

instance ToJSON FromSourceRelationshipDef where
  toJSON = \case
    FromSourceToSourceRelDef source -> object ["remote_source" .= toJSON source]
    FromSourceToSchemaRelDef fmt schema@FromSourceToSchemaRelationshipDef {..} ->
      case fmt of
        RFRemoteSchemaOnly ->
          object
            [ "remote_schema" .= toJSON _fstrrdRemoteSchemaName,
              "hasura_fields" .= toJSON _fstrrdHasuraFields,
              "remote_field" .= toJSON _fstrrdRemoteField
            ]
        RFSchemaAndSource ->
          object ["remote_schema" .= toJSON schema]

-- | Remote relationship serialization format
--
-- When we introduced remote relationships from source to source, we re-used the
-- existing metadata API, meaning that the two cases are handled by the same API
-- calls. When decoding remote relationship information, we keep track of
-- whether it was encoded with the old format (remote schema only) or with the
-- new format (schema and source). That way, we can re-encode it the same way.
data RRFormat = RFRemoteSchemaOnly | RFSchemaAndSource
  deriving stock (Show, Eq, Generic)

instance NFData RRFormat

instance Cacheable RRFormat

-- | Metadata representation of a relationship from source to source.
--
-- This representation has to be backend-agnostic, as it will be parsed before
-- the source cache is built, meaning we can't decide how something backend
-- specific should be resolved based solely on the source name. But the table
-- name is specific to the targeted backend...There are two solutions to this
-- problem:
--  - we can either include an additional field in the serialization that tells
--    us the "kind" of the backend; but that requies an additional field that is
--    technically not required, and that could potentially be inconsistent
--  - or we can do the same thing that we do for source to source relationships:
--    we store an unparsed JSON value as far as the metadata goes, and we parse
--    it when building the schema cache, when we know the kind of the source
--    from its name
--
-- We chose the latter.
--
-- FIXME: move this to Hasura/Metadata
data FromSourceToSourceRelationshipDef = FromSourceToSourceRelationshipDef
  { _fstsrdRelationshipType :: !RelType,
    _fstsrdFieldMapping :: !(HashMap FieldName FieldName),
    _fstsrdSource :: !SourceName,
    _fstsrdTable :: !Value
  }
  deriving stock (Generic, Show, Eq)

instance NFData FromSourceToSourceRelationshipDef

instance Cacheable FromSourceToSourceRelationshipDef

instance ToJSON FromSourceToSourceRelationshipDef where
  toJSON = genericToJSON hasuraJSON

instance FromJSON FromSourceToSourceRelationshipDef where
  parseJSON = withObject "RemoteSourceRelationship" $ \o ->
    FromSourceToSourceRelationshipDef
      <$> o .: "relationship_type"
      <*> o .: "field_mapping"
      <*> o .: "source"
      <*> o .: "table"

-- | Metadata representation of a relationship from source to schema.
-- FIXME: move this to Hasura/Metadata
data FromSourceToSchemaRelationshipDef = FromSourceToSchemaRelationshipDef
  { -- | Identifier for this mapping.
    _fstrrdRemoteSchemaName :: !RemoteSchemaName,
    -- | The hasura fields from '_rtrTable' that will be in scope when resolving
    -- the remote objects in 'rtrRemoteField'. Supports columns and computed fields.
    _fstrrdHasuraFields :: !(HashSet FieldName),
    _fstrrdRemoteField :: !RemoteFields
  }
  deriving stock (Show, Eq, Generic)

instance NFData FromSourceToSchemaRelationshipDef

instance Cacheable FromSourceToSchemaRelationshipDef

-- | Targeted field in a remote schema relationship.
-- TODO: explain about subfields and why this is a container
newtype RemoteFields = RemoteFields {unRemoteFields :: NonEmpty FieldCall}
  deriving (Show, Eq, Generic)

instance NFData RemoteFields

instance Cacheable RemoteFields

instance FromJSON RemoteFields where
  parseJSON = prependFailure details . fmap RemoteFields . parseRemoteFields
    where
      details = "Remote fields are represented by an object that maps each field name to its arguments."
      parseRemoteFields = withObject "RemoteFields" \hashmap -> case HM.toList hashmap of
        [(fieldNameText, callValue)] -> do
          fieldName <- parseJSON (String fieldNameText)
          callObject <- parseJSON callValue
          arguments <- callObject .: "arguments"
          maybeSubField <- callObject .:? "field"
          subFields <-
            fromMaybe [] <$> for maybeSubField \fieldValue -> do
              remoteFields <- parseRemoteFields fieldValue
              pure (toList remoteFields)
          pure $ FieldCall {fcName = fieldName, fcArguments = arguments} :| subFields
        [] -> fail "Expecting one single mapping, received none."
        _ -> fail "Expecting one single mapping, received too many."

instance ToJSON RemoteFields where
  toJSON (RemoteFields fields) = remoteFieldsJson fields
    where
      remoteFieldsJson (field :| subfields) =
        object
          [ G.unName (fcName field)
              .= object
                ( catMaybes
                    [ Just $ "arguments" .= fcArguments field,
                      nonEmpty subfields <&> \sf -> "field" .= remoteFieldsJson sf
                    ]
                )
          ]

-- | Associates a field name with the arguments it will be passed in the query.
--
-- https://graphql.github.io/graphql-spec/June2018/#sec-Language.Arguments
data FieldCall = FieldCall
  { fcName :: !G.Name,
    fcArguments :: !RemoteArguments
  }
  deriving (Show, Eq, Generic)

instance NFData FieldCall

instance Cacheable FieldCall

instance Hashable FieldCall

-- | Arguments to a remote GraphQL fields, represented as a mapping from name to
-- GraphQL Value. Said values can be variable names, in which case they'll be
-- referring to values we're closed over.
-- TODO: expand on this
newtype RemoteArguments = RemoteArguments
  { getRemoteArguments :: HashMap G.Name (G.Value G.Name)
  }
  deriving (Show, Eq, Generic, Cacheable, NFData)

instance Hashable RemoteArguments

instance FromJSON RemoteArguments where
  parseJSON = prependFailure details . fmap RemoteArguments . withObject "RemoteArguments" parseObjectFieldsToGValue
    where
      details = "Remote arguments are represented by an object that maps each argument name to its value."

      parseObjectFieldsToGValue hashMap =
        HM.fromList <$> for (HM.toList hashMap) \(key, value) -> do
          name <- G.mkName key `onNothing` fail (T.unpack key <> " is an invalid key name")
          parsedValue <- parseValueAsGValue value
          pure (name, parsedValue)

      parseValueAsGValue = \case
        Object obj ->
          G.VObject <$> parseObjectFieldsToGValue obj
        Array array ->
          G.VList . toList <$> traverse parseValueAsGValue array
        String text ->
          case T.uncons text of
            Just ('$', rest)
              | T.null rest -> fail $ "Empty variable name"
              | otherwise -> case G.mkName rest of
                Nothing -> fail $ "Invalid variable name '" <> T.unpack rest <> "'"
                Just name' -> pure $ G.VVariable name'
            _ -> pure (G.VString text)
        Number !scientificNum ->
          pure $ case floatingOrInteger scientificNum of
            -- this number couldn't be interpreted as an integer
            Left (_ :: Float) -> G.VFloat scientificNum
            -- this number was successfully interpreted as an integer
            Right n -> G.VInt n
        Bool !boolean ->
          pure $ G.VBoolean boolean
        Null ->
          pure G.VNull

instance ToJSON RemoteArguments where
  toJSON (RemoteArguments fields) = fieldsToObject fields
    where
      fieldsToObject =
        Object . HM.fromList . map (bimap G.unName gValueToValue) . HM.toList

      gValueToValue =
        \case
          G.VVariable v -> toJSON ("$" <> G.unName v)
          G.VInt i -> toJSON i
          G.VFloat f -> toJSON f
          G.VString s -> toJSON s
          G.VBoolean b -> toJSON b
          G.VNull -> Null
          G.VEnum s -> toJSON s
          G.VList list -> toJSON (map gValueToValue list)
          G.VObject obj -> fieldsToObject obj

--------------------------------------------------------------------------------
-- schema cache

-- | Resolved remote relationship, as stored in the schema cache.
data RemoteFieldInfo (b :: BackendType)
  = RFISchema !(RemoteSchemaFieldInfo b)
  | RFISource !(AnyBackend (RemoteSourceFieldInfo b))
  deriving (Generic)

deriving instance Backend b => Eq (RemoteFieldInfo b)

instance Backend b => Cacheable (RemoteFieldInfo b)

instance Backend b => ToJSON (RemoteFieldInfo b) where
  toJSON =
    \case
      RFISchema schema -> toJSON schema
      RFISource _ -> toJSON ()

-- | Schema cache information for a table field targeting a remote source.
data RemoteSourceFieldInfo src tgt = RemoteSourceFieldInfo
  { _rsfiName :: !RelName,
    _rsfiType :: !RelType,
    _rsfiSource :: !SourceName,
    _rsfiSourceConfig :: !(SourceConfig tgt),
    _rsfiTable :: !(TableName tgt), -- this is parsed from `Value`
    _rsfiMapping :: !(HM.HashMap FieldName (ColumnInfo src, ScalarType tgt, Column tgt))
  }
  deriving stock (Generic)

deriving instance (Backend src, Backend tgt) => Eq (RemoteSourceFieldInfo src tgt)

instance (Backend src, Backend tgt) => Cacheable (RemoteSourceFieldInfo src tgt)

-- | Schema cache information for a table field targeting a remote schema.
data RemoteSchemaFieldInfo (b :: BackendType) = RemoteSchemaFieldInfo
  { -- | Field name to which we'll map the remote in hasura; this becomes part
    --   of the hasura schema.
    _rrfiName :: !RelName,
    -- | Input arguments to the remote field info; The '_rfiParamMap' will only
    --   include the arguments to the remote field that is being joined. The
    --   names of the arguments here are modified, it will be in the format of
    --   <Original Field Name>_remote_rel_<hasura table schema>_<hasura table name><remote relationship name>
    _rrfiParamMap :: !(HashMap G.Name RemoteSchemaInputValueDefinition),
    -- | Hasura fields used to join the remote schema node
    _rrfiHasuraFields :: !(HashSet (DBJoinField b)),
    _rrfiRemoteFields :: !RemoteFields,
    _rrfiRemoteSchema :: !RemoteSchemaInfo,
    -- | The new input value definitions created for this remote field
    _rrfiInputValueDefinitions :: ![G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition],
    -- | Name of the remote schema, that's used for joining
    _rrfiRemoteSchemaName :: !RemoteSchemaName,
    -- | Name of the table and its source
    _rrfiTable :: !(TableName b, SourceName)
  }
  deriving (Generic)

deriving instance Backend b => Eq (RemoteSchemaFieldInfo b)

deriving instance Backend b => Show (RemoteSchemaFieldInfo b)

instance Backend b => Cacheable (RemoteSchemaFieldInfo b)

instance Backend b => ToJSON (RemoteSchemaFieldInfo b) where
  toJSON RemoteSchemaFieldInfo {..} =
    object
      [ "name" .= _rrfiName,
        "param_map" .= fmap toJsonInpValInfo _rrfiParamMap,
        "hasura_fields" .= _rrfiHasuraFields,
        "remote_fields" .= _rrfiRemoteFields,
        "remote_schema" .= _rrfiRemoteSchema
      ]
    where
      toJsonInpValInfo (RemoteSchemaInputValueDefinition (G.InputValueDefinition desc name type' defVal _directives) _preset) =
        object
          [ "desc" .= desc,
            "name" .= name,
            "def_val" .= fmap graphQLValueToJSON defVal,
            "type" .= type'
          ]

-- FIXME: deduplicate this
graphQLValueToJSON :: G.Value Void -> Value
graphQLValueToJSON = \case
  G.VNull -> Null
  G.VInt i -> toJSON i
  G.VFloat f -> toJSON f
  G.VString t -> toJSON t
  G.VBoolean b -> toJSON b
  G.VEnum (G.EnumValue n) -> toJSON n
  G.VList values -> toJSON $ graphQLValueToJSON <$> values
  G.VObject objects -> toJSON $ graphQLValueToJSON <$> objects

-- | Information about the field on the LHS of a join against a remote schema.
data DBJoinField (b :: BackendType)
  = JoinColumn !(ColumnInfo b)
  | JoinComputedField !(ScalarComputedField b)
  deriving (Generic)

deriving instance Backend b => Eq (DBJoinField b)

deriving instance Backend b => Show (DBJoinField b)

instance Backend b => Cacheable (DBJoinField b)

instance Backend b => Hashable (DBJoinField b)

instance (Backend b) => ToJSON (DBJoinField b) where
  toJSON = \case
    JoinColumn columnInfo -> toJSON columnInfo
    JoinComputedField computedField -> toJSON computedField

dbJoinFieldToName :: forall b. (Backend b) => DBJoinField b -> FieldName
dbJoinFieldToName = \case
  JoinColumn columnInfo -> fromCol @b $ pgiColumn $ columnInfo
  JoinComputedField computedFieldInfo -> fromComputedField $ _scfName computedFieldInfo

-- | Information about a computed field appearing on the LHS of a remote join.
-- FIXME: why do we need all of this?
data ScalarComputedField (b :: BackendType) = ScalarComputedField
  { _scfXField :: !(XComputedField b),
    _scfName :: !ComputedFieldName,
    _scfFunction :: !(FunctionName b),
    _scfTableArgument :: !FunctionTableArgument,
    _scfSessionArgument :: !(Maybe FunctionSessionArgument),
    _scfType :: !(ScalarType b)
  }
  deriving (Generic)

deriving instance Backend b => Eq (ScalarComputedField b)

deriving instance Backend b => Show (ScalarComputedField b)

instance Backend b => Cacheable (ScalarComputedField b)

instance Backend b => Hashable (ScalarComputedField b)

instance Backend b => ToJSON (ScalarComputedField b) where
  toJSON ScalarComputedField {..} =
    object
      [ "name" .= _scfName,
        "function" .= _scfFunction,
        "table_argument" .= _scfTableArgument,
        "session_argument" .= _scfSessionArgument,
        "type" .= _scfType
      ]

--------------------------------------------------------------------------------
-- metadata api

-- | This type represents a remote relationship throughout our metadata API.
-- It is expected as an argument to commnands that create or modify
-- such relationships, and when communicating with the legacy catalog.
data FromSourceRelationship b = FromSourceRelationship
  { -- | Field name to which we'll map the remote in hasura; this becomes part
    -- of the hasura schema.
    _fsrName :: !RelName,
    _fsrSource :: !SourceName,
    -- | (SourceName, QualifiedTable) determines the table on which the relationship
    -- is defined
    _fsrTable :: !(TableName b),
    _fsrDefinition :: !FromSourceRelationshipDef
  }
  deriving (Generic)

instance (Backend b) => ToJSON (FromSourceRelationship b) where
  toJSON = genericToJSON hasuraJSON

instance (Backend b) => FromJSON (FromSourceRelationship b) where
  parseJSON = withObject "RemoteRelationship" $ \o ->
    FromSourceRelationship
      <$> o .: "name"
      <*> o .:? "source" .!= defaultSource
      <*> o .: "table"
      <*> parseJSON (Object o)

--------------------------------------------------------------------------------
-- template haskell generation

$(makePrisms ''FromSourceRelationshipDef)
$(makeLenses ''FromSourceToSourceRelationshipDef)
$(makeLenses ''FromSourceRelationship)
$(makeLenses ''FromSourceToSchemaRelationshipDef)
$(deriveJSON hasuraJSON ''FromSourceToSchemaRelationshipDef)
