{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.DDL.RemoteRelationship
  ( CreateFromSourceRelationship (..),
    runCreateRemoteRelationship,
    execDeleteRemoteRelationship,
    runDeleteRemoteRelationship,
    runUpdateRemoteRelationship,
    DeleteFromSourceRelationship (..),
    dropRemoteRelationshipInMetadata,
    PartiallyResolvedSource (..),
    buildRemoteFieldInfo,
    CreateRemoteSchemaRemoteRelationship (..),
    runCreateRemoteSchemaRemoteRelationship,
    runUpdateRemoteSchemaRemoteRelationship,
    DeleteRemoteSchemaRemoteRelationship (..),
    runDeleteRemoteSchemaRemoteRelationship,
    getRemoteSchemaEntityJoinColumns,
  )
where

import Control.Lens (at, non, to, (^?))
import Data.Aeson (FromJSON (..), ToJSON (..), (.!=), (.:), (.:?), (.=))
import Data.Aeson qualified as J
import Data.Aeson.KeyMap qualified as KM
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Sequence qualified as Seq
import Data.Text.Extended ((<<>), (<>>))
import Hasura.Base.Error
  ( Code (NotExists, NotFound, NotSupported, RemoteSchemaError),
    QErr,
    QErrM,
    runAesonParser,
    throw400,
    throw500,
  )
import Hasura.EncJSON (EncJSON)
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.Relationships.Remote
import Hasura.RQL.Types.Relationships.ToSource
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.SchemaCacheTypes
import Hasura.RQL.Types.Source
import Hasura.RemoteSchema.Metadata
import Hasura.RemoteSchema.SchemaCache
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Table.Cache
import Hasura.Table.Metadata (tmRemoteRelationships)
import Language.GraphQL.Draft.Syntax qualified as G

--------------------------------------------------------------------------------
-- Create or update relationship from source

-- | Argument to the @_create_remote_relationship@ and
-- @_update_remote_relationship@ families of metadata commands.
--
-- For historical reason, this type is also used to represent a db-to-rs schema
-- in the metadata.
data CreateFromSourceRelationship (b :: BackendType) = CreateFromSourceRelationship
  { _crrSource :: SourceName,
    _crrTable :: TableName b,
    _crrName :: RelName,
    _crrDefinition :: RemoteRelationshipDefinition
  }

deriving stock instance (Eq (TableName b)) => Eq (CreateFromSourceRelationship b)

deriving stock instance (Show (TableName b)) => Show (CreateFromSourceRelationship b)

instance (Backend b) => FromJSON (CreateFromSourceRelationship b) where
  parseJSON = J.withObject "CreateFromSourceRelationship" $ \o -> do
    _crrSource <- o .:? "source" .!= defaultSource
    _crrTable <- o .: "table"
    _crrName <- o .: "name"
    -- In the old format, the definition is inlined; in the new format, the
    -- definition is in the "definition" object, and we don't allow legacy
    -- fields to appear under it.
    remoteSchema :: Maybe J.Value <- o .:? "remote_schema"
    definition <- o .:? "definition"
    _crrDefinition <- case (remoteSchema, definition) of
      -- old format
      (Just {}, Nothing) -> parseRemoteRelationshipDefinition RRPLegacy $ J.Object o
      -- new format
      (Nothing, Just def) -> parseRemoteRelationshipDefinition RRPStrict def
      -- both or neither
      _ -> fail "create_remote_relationship expects exactly one of: remote_schema, definition"
    pure $ CreateFromSourceRelationship {..}

instance (Backend b) => ToJSON (CreateFromSourceRelationship b) where
  toJSON CreateFromSourceRelationship {..} =
    -- We need to introspect the definition, to know whether we need to inline
    -- it, or if it needs to be in a distinct "definition" object.
    J.object $ case _crrDefinition of
      -- old format
      RelationshipToSchema RRFOldDBToRemoteSchema _ ->
        case J.toJSON _crrDefinition of
          -- The result of this serialization will be an empty list if this
          -- conversion fails (which it should _never_ do), in which case those
          -- fields will be omitted from the serialized JSON. This could only
          -- happen if the ToJSON instance of RemoteRelationshipDefinition were
          -- changed to return something that isn't an object.
          J.Object obj -> commonFields <> KM.toList obj
          _ -> []
      -- new format
      _ -> ("definition" .= _crrDefinition) : commonFields
    where
      commonFields =
        [ "source" .= _crrSource,
          "table" .= _crrTable,
          "name" .= _crrName
        ]

runCreateRemoteRelationship ::
  forall b m.
  (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b) =>
  CreateFromSourceRelationship b ->
  m EncJSON
runCreateRemoteRelationship CreateFromSourceRelationship {..} = do
  void $ askTableInfo @b _crrSource _crrTable
  let metadataObj =
        MOSourceObjId _crrSource
          $ AB.mkAnyBackend
          $ SMOTableObj @b _crrTable
          $ MTORemoteRelationship _crrName
  buildSchemaCacheFor metadataObj
    $ MetadataModifier
    $ tableMetadataSetter @b _crrSource _crrTable
    . tmRemoteRelationships
    %~ InsOrdHashMap.insert _crrName (RemoteRelationship _crrName _crrDefinition)
  pure successMsg

runUpdateRemoteRelationship ::
  forall b m.
  (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b) =>
  CreateFromSourceRelationship b ->
  m EncJSON
runUpdateRemoteRelationship CreateFromSourceRelationship {..} = do
  fieldInfoMap <- askTableFieldInfoMap @b _crrSource _crrTable
  let metadataObj =
        MOSourceObjId _crrSource
          $ AB.mkAnyBackend
          $ SMOTableObj @b _crrTable
          $ MTORemoteRelationship _crrName
  void $ askRemoteRel fieldInfoMap _crrName
  buildSchemaCacheFor metadataObj
    $ MetadataModifier
    $ tableMetadataSetter @b _crrSource _crrTable
    . tmRemoteRelationships
    %~ InsOrdHashMap.insert _crrName (RemoteRelationship _crrName _crrDefinition)
  pure successMsg

--------------------------------------------------------------------------------
-- Drop relationship from source

-- | Argument to the @_drop_remote_relationship@ family of metadata commands.
data DeleteFromSourceRelationship (b :: BackendType) = DeleteFromSourceRelationship
  { _drrSource :: SourceName,
    _drrTable :: TableName b,
    _drrName :: RelName
  }

instance (Backend b) => FromJSON (DeleteFromSourceRelationship b) where
  parseJSON = J.withObject "DeleteFromSourceRelationship" $ \o ->
    DeleteFromSourceRelationship
      <$> o
      .:? "source"
      .!= defaultSource
      <*> o
      .: "table"
      <*> o
      .: "name"

execDeleteRemoteRelationship ::
  forall b m.
  (BackendMetadata b, MonadError QErr m, CacheRWM m) =>
  DeleteFromSourceRelationship b ->
  m (MetadataObjId, MetadataModifier)
execDeleteRemoteRelationship (DeleteFromSourceRelationship source table relName) = do
  fieldInfoMap <- askTableFieldInfoMap @b source table
  void $ askRemoteRel fieldInfoMap relName

  let metadataObjId :: MetadataObjId
      metadataObjId =
        MOSourceObjId source
          $ AB.mkAnyBackend
          $ SMOTableObj @b table
          $ MTORemoteRelationship relName

      metadataModifier :: MetadataModifier
      metadataModifier =
        MetadataModifier
          $ tableMetadataSetter @b source table
          %~ dropRemoteRelationshipInMetadata relName

  pure (metadataObjId, metadataModifier)

runDeleteRemoteRelationship ::
  forall b m.
  (BackendMetadata b, MonadError QErr m, CacheRWM m, MetadataM m) =>
  DeleteFromSourceRelationship b ->
  m EncJSON
runDeleteRemoteRelationship command = do
  (metadataObj, metadataModifier) <- execDeleteRemoteRelationship command

  buildSchemaCacheFor metadataObj metadataModifier
  pure successMsg

--------------------------------------------------------------------------------
-- Create relationship from remote schema

data CreateRemoteSchemaRemoteRelationship = CreateRemoteSchemaRemoteRelationship
  { _crsrrRemoteSchema :: RemoteSchemaName,
    _crsrrType :: G.Name,
    _crsrrName :: RelName,
    _crsrrDefinition :: RemoteRelationshipDefinition
  }
  deriving (Generic)

instance FromJSON CreateRemoteSchemaRemoteRelationship where
  parseJSON = J.withObject "CreateRemoteSchemaRemoteRelationship" $ \o ->
    CreateRemoteSchemaRemoteRelationship
      <$> o
      .: "remote_schema"
      <*> o
      .: "type_name"
      <*> o
      .: "name"
      <*> (o .: "definition" >>= parseRemoteRelationshipDefinition RRPStrict)

instance J.ToJSON CreateRemoteSchemaRemoteRelationship where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

runCreateRemoteSchemaRemoteRelationship ::
  forall m.
  (MonadError QErr m, CacheRWM m, MetadataM m) =>
  CreateRemoteSchemaRemoteRelationship ->
  m EncJSON
runCreateRemoteSchemaRemoteRelationship CreateRemoteSchemaRemoteRelationship {..} = do
  let metadataObj =
        MORemoteSchemaRemoteRelationship _crsrrRemoteSchema _crsrrType _crsrrName
  buildSchemaCacheFor metadataObj
    $ MetadataModifier
    $ metaRemoteSchemas
    . ix _crsrrRemoteSchema
    . rsmRemoteRelationships
    . at _crsrrType
    . non (RemoteSchemaTypeRelationships _crsrrType mempty)
    . rstrsRelationships
    %~ InsOrdHashMap.insert _crsrrName (RemoteRelationship _crsrrName _crsrrDefinition)
  pure successMsg

runUpdateRemoteSchemaRemoteRelationship ::
  forall m.
  (MonadError QErr m, CacheRWM m, MetadataM m) =>
  CreateRemoteSchemaRemoteRelationship ->
  m EncJSON
runUpdateRemoteSchemaRemoteRelationship crss@CreateRemoteSchemaRemoteRelationship {..} = do
  schemaCache <- askSchemaCache
  let remoteRelationship =
        schemaCache
          ^? to scRemoteSchemas
            . ix _crsrrRemoteSchema
            . rscRemoteRelationships
            . ix _crsrrType
            . ix _crsrrName
  void
    $ onNothing remoteRelationship
    $ throw400 NotExists
    $ "no relationship defined on remote schema "
    <>> _crsrrRemoteSchema
    <<> " with name "
    <>> _crsrrName
  runCreateRemoteSchemaRemoteRelationship crss

--------------------------------------------------------------------------------
-- Drop relationship from remote schema

-- | Argument to the @_drop_remote_relationship@ family of metadata commands.
data DeleteRemoteSchemaRemoteRelationship = DeleteRemoteSchemaRemoteRelationship
  { _drsrrRemoteSchema :: RemoteSchemaName,
    _drsrrTypeName :: G.Name,
    _drsrrName :: RelName
  }

instance FromJSON DeleteRemoteSchemaRemoteRelationship where
  parseJSON = J.withObject "DeleteRemoteSchemaRemoteRelationship" $ \o ->
    DeleteRemoteSchemaRemoteRelationship
      <$> o
      .: "remote_schema"
      <*> o
      .: "type_name"
      <*> o
      .: "name"

runDeleteRemoteSchemaRemoteRelationship ::
  forall m.
  (MonadError QErr m, CacheRWM m, MetadataM m) =>
  DeleteRemoteSchemaRemoteRelationship ->
  m EncJSON
runDeleteRemoteSchemaRemoteRelationship DeleteRemoteSchemaRemoteRelationship {..} = do
  let relName = _drsrrName
      metadataObj =
        MORemoteSchemaRemoteRelationship _drsrrRemoteSchema _drsrrTypeName relName
  buildSchemaCacheFor metadataObj
    $ MetadataModifier
    $ metaRemoteSchemas
    . ix _drsrrRemoteSchema
    . rsmRemoteRelationships
    . ix _drsrrTypeName
    . rstrsRelationships
    %~ InsOrdHashMap.delete relName
  pure successMsg

--------------------------------------------------------------------------------
-- Schema cache building (TODO: move this elsewere!)

-- | Internal intermediary step.
--
-- We build the output of sources in two steps:
--   1. we first resolve sources, and collect the core info of their tables
--   2. we then build the entire output from the collection of partially resolved sources
--
-- We need this split to be able to resolve cross-source relationships: to process one source's
-- remote relationship, we need to know about the target source's tables core info.
--
-- This data structure is used as an argument to @AnyBackend@ in the backend-agnostic intermediary
-- collection, and used here to build remote field info.
data PartiallyResolvedSource b = PartiallyResolvedSource
  { _prsSourceMetadata :: SourceMetadata b,
    _prsConfig :: SourceConfig b,
    _prsIntrospection :: DBObjectsIntrospection b,
    _tableCoreInfoMap :: HashMap (TableName b) (TableCoreInfoG b (StructuredColumnInfo b) (ColumnInfo b)),
    _eventTriggerInfoMap :: HashMap (TableName b) (EventTriggerInfoMap b)
  }
  deriving (Eq)

-- | Builds the schema cache representation of a remote relationship
-- TODO: this is not actually called by the remote relationship DDL API and is only used as part of
-- the schema cache process. Should this be moved elsewhere?
buildRemoteFieldInfo ::
  (QErrM m) =>
  -- | The entity on which the remote relationship is defined
  LHSIdentifier ->
  -- | join fields provided by the LHS entity
  HashMap.HashMap FieldName lhsJoinField ->
  -- | definition of remote relationship
  RemoteRelationship ->
  -- | Required context to process cross boundary relationships
  HashMap SourceName (AB.AnyBackend PartiallyResolvedSource) ->
  -- | Required context to process cross boundary relationships
  PartiallyResolvedRemoteSchemaMap ->
  -- | returns
  --   1. schema cache representation of the remote relationships
  --   2. the dependencies on the RHS of the join. The dependencies
  --      on the LHS entities has to be handled by the calling function
  m (RemoteFieldInfo lhsJoinField, Seq SchemaDependency)
buildRemoteFieldInfo lhsIdentifier lhsJoinFields RemoteRelationship {..} allSources remoteSchemaMap =
  case _rrDefinition of
    RelationshipToSource ToSourceRelationshipDef {..} -> do
      targetTables <-
        HashMap.lookup _tsrdSource allSources
          `onNothing` throw400 NotFound ("source not found: " <>> _tsrdSource)
      AB.dispatchAnyBackendWithTwoConstraints @Backend @BackendMetadata targetTables \(partiallyResolvedSource :: PartiallyResolvedSource b') -> do
        let PartiallyResolvedSource _ sourceConfig _ targetTablesInfo _ = partiallyResolvedSource
        unless (supportsBeingRemoteRelationshipTarget @b' sourceConfig)
          $ throw400 NotSupported ("source " <> sourceNameToText _tsrdSource <> " does not support being used as the target of a remote relationship")

        (targetTable :: TableName b') <- runAesonParser J.parseJSON _tsrdTable
        targetColumns <-
          fmap _tciFieldInfoMap
            $ onNothing (HashMap.lookup targetTable targetTablesInfo)
            $ throw400 NotExists
            $ "table "
            <> targetTable
            <<> " does not exist in source: "
            <> sourceNameToText _tsrdSource
        -- TODO: rhs fields should also ideally be DBJoinFields
        columnPairs <- for (HashMap.toList _tsrdFieldMapping) \(srcFieldName, tgtFieldName) -> do
          lhsJoinField <- askFieldInfo lhsJoinFields srcFieldName
          tgtField <- toScalarColumnInfo <$> askFieldInfo targetColumns tgtFieldName
          tgtFieldScalarColumn <- tgtField `onNothing` throw500 ("Target field " <> tgtFieldName <<> "is not a scalar column")
          pure (srcFieldName, lhsJoinField, tgtFieldScalarColumn)
        columnMapping <- for columnPairs \(srcFieldName, srcColumn, tgtColumn) -> do
          tgtScalar <- case ciType tgtColumn of
            ColumnScalar scalarType -> pure scalarType
            ColumnEnumReference _ -> throw400 NotSupported "relationships to enum fields are not supported yet"
          pure (srcFieldName, (srcColumn, tgtScalar, ciColumn tgtColumn))
        let rsri =
              RemoteSourceFieldInfo _rrName _tsrdRelationshipType _tsrdSource sourceConfig targetTable
                $ fmap (\(_, tgtType, tgtColumn) -> (tgtType, tgtColumn))
                $ HashMap.fromList columnMapping
            rhsDependencies =
              SchemaDependency (SOSourceObj _tsrdSource $ AB.mkAnyBackend $ SOITable @b' targetTable) DRTable
                : flip map columnPairs \(_, _srcColumn, tgtColumn) ->
                  SchemaDependency
                    ( SOSourceObj _tsrdSource
                        $ AB.mkAnyBackend
                        $ SOITableObj @b' targetTable
                        $ TOCol @b'
                        $ ciColumn tgtColumn
                    )
                    DRRemoteRelationship
            requiredLHSJoinFields = fmap (\(srcField, _, _) -> srcField) $ HashMap.fromList columnMapping
        pure (RemoteFieldInfo requiredLHSJoinFields $ RFISource $ AB.mkAnyBackend @b' rsri, Seq.fromList rhsDependencies)
    RelationshipToSchema _ remoteRelationship@ToSchemaRelationshipDef {..} -> do
      RemoteSchemaCtx {..} <-
        onNothing (HashMap.lookup _trrdRemoteSchema remoteSchemaMap)
          $ throw400 RemoteSchemaError
          $ "remote schema with name "
          <> _trrdRemoteSchema
          <<> " not found"
      (requiredLHSJoinFields, remoteField) <-
        validateToSchemaRelationship remoteRelationship lhsIdentifier _rrName (_rscInfo, _rscIntroOriginal) lhsJoinFields
          `onLeft` (throw400 RemoteSchemaError . errorToText)
      let rhsDependency = SchemaDependency (SORemoteSchema _trrdRemoteSchema) DRRemoteSchema
      pure (RemoteFieldInfo requiredLHSJoinFields $ RFISchema remoteField, Seq.singleton $ rhsDependency)

getRemoteSchemaEntityJoinColumns ::
  (MonadError QErr m) =>
  RemoteSchemaName ->
  RemoteSchemaIntrospection ->
  G.Name ->
  m (HashMap FieldName G.Name)
getRemoteSchemaEntityJoinColumns remoteSchemaName introspection typeName = do
  typeDefinition <-
    onNothing (lookupType introspection typeName)
      $ throw400 NotFound ("no type named " <> typeName <<> " defined in remote schema " <>> remoteSchemaName)
  case typeDefinition of
    G.TypeDefinitionObject objectDefinition ->
      pure
        $ HashMap.fromList
        $ do
          fieldDefinition <- G._otdFieldsDefinition objectDefinition
          guard $ null $ G._fldArgumentsDefinition fieldDefinition
          pure (FieldName $ G.unName $ G._fldName fieldDefinition, G._fldName fieldDefinition)
    _ -> throw400 NotSupported "remote relationships on a remote schema can only be defined on object types"
