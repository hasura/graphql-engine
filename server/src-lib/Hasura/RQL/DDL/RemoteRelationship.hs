module Hasura.RQL.DDL.RemoteRelationship
  ( CreateFromSourceRelationship (..),
    runCreateRemoteRelationship,
    runDeleteRemoteRelationship,
    runUpdateRemoteRelationship,
    DeleteFromSourceRelationship (..),
    dropRemoteRelationshipInMetadata,
    PartiallyResolvedSource (..),
    buildRemoteFieldInfo,
  )
where

import Data.Aeson
import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.HashSet qualified as S
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.RQL.DDL.RemoteRelationship.Validate
import Hasura.RQL.Types
import Hasura.SQL.AnyBackend (mkAnyBackend)
import Hasura.SQL.AnyBackend qualified as AB

--------------------------------------------------------------------------------
-- Create or update relationship

-- | Argument to the @_create_remote_relationship@ and
-- @_update_remote_relationship@ families of metadata commands.
--
-- For historical reason, this type is also used to represent a db-to-rs schema
-- in the metadata.
data CreateFromSourceRelationship (b :: BackendType) = CreateFromSourceRelationship
  { _crrSource :: SourceName,
    _crrTable :: TableName b,
    _crrDefinition :: RemoteRelationship
  }
  deriving (Generic)

instance Backend b => FromJSON (CreateFromSourceRelationship b) where
  parseJSON = withObject "CreateFromSourceRelationship" $ \o ->
    CreateFromSourceRelationship
      <$> o .:? "source" .!= defaultSource
      <*> o .: "table"
      <*> parseJSON (Object o)

instance (Backend b) => ToJSON (CreateFromSourceRelationship b) where
  toJSON = genericToJSON hasuraJSON

runCreateRemoteRelationship ::
  forall b m.
  (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b) =>
  CreateFromSourceRelationship b ->
  m EncJSON
runCreateRemoteRelationship CreateFromSourceRelationship {..} = do
  void $ askTabInfo @b _crrSource _crrTable
  let relName = _rrName _crrDefinition
      metadataObj =
        MOSourceObjId _crrSource $
          AB.mkAnyBackend $
            SMOTableObj @b _crrTable $
              MTORemoteRelationship relName
  buildSchemaCacheFor metadataObj $
    MetadataModifier $
      tableMetadataSetter @b _crrSource _crrTable . tmRemoteRelationships
        %~ OMap.insert relName _crrDefinition
  pure successMsg

runUpdateRemoteRelationship ::
  forall b m.
  (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b) =>
  CreateFromSourceRelationship b ->
  m EncJSON
runUpdateRemoteRelationship CreateFromSourceRelationship {..} = do
  fieldInfoMap <- askFieldInfoMap @b _crrSource _crrTable
  let relName = _rrName _crrDefinition
      metadataObj =
        MOSourceObjId _crrSource $
          AB.mkAnyBackend $
            SMOTableObj @b _crrTable $
              MTORemoteRelationship relName
  void $ askRemoteRel fieldInfoMap relName
  buildSchemaCacheFor metadataObj $
    MetadataModifier $
      tableMetadataSetter @b _crrSource _crrTable . tmRemoteRelationships
        %~ OMap.insert relName _crrDefinition
  pure successMsg

--------------------------------------------------------------------------------
-- Drop relationship

-- | Argument to the @_drop_remote_relationship@ family of metadata commands.
data DeleteFromSourceRelationship (b :: BackendType) = DeleteFromSourceRelationship
  { _drrSource :: SourceName,
    _drrTable :: TableName b,
    _drrName :: RelName
  }

instance Backend b => FromJSON (DeleteFromSourceRelationship b) where
  parseJSON = withObject "DeleteFromSourceRelationship" $ \o ->
    DeleteFromSourceRelationship
      <$> o .:? "source" .!= defaultSource
      <*> o .: "table"
      <*> o .: "name"

runDeleteRemoteRelationship ::
  forall b m.
  (BackendMetadata b, MonadError QErr m, CacheRWM m, MetadataM m) =>
  DeleteFromSourceRelationship b ->
  m EncJSON
runDeleteRemoteRelationship (DeleteFromSourceRelationship source table relName) = do
  fieldInfoMap <- askFieldInfoMap @b source table
  void $ askRemoteRel fieldInfoMap relName
  let metadataObj =
        MOSourceObjId source $
          AB.mkAnyBackend $
            SMOTableObj @b table $
              MTORemoteRelationship relName
  buildSchemaCacheFor metadataObj $
    MetadataModifier $
      tableMetadataSetter @b source table %~ dropRemoteRelationshipInMetadata relName
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
  { _prsSourceMetadata :: !(SourceMetadata b),
    _resolvedSource :: !(ResolvedSource b),
    _tableCoreInfoMap :: !(HashMap (TableName b) (TableCoreInfoG b (ColumnInfo b) (ColumnInfo b)))
  }

-- TODO: this is not actually called by the remote relationship DDL API and is only used as part of
-- the schema cache process. Should this be moved elsewhere?
buildRemoteFieldInfo ::
  forall m b.
  (Backend b, QErrM m) =>
  SourceName ->
  TableName b ->
  FieldInfoMap (FieldInfo b) ->
  RemoteRelationship ->
  HashMap SourceName (AB.AnyBackend PartiallyResolvedSource) ->
  RemoteSchemaMap ->
  m (RemoteFieldInfo b, [SchemaDependency])
buildRemoteFieldInfo sourceSource sourceTable fields RemoteRelationship {..} allSources remoteSchemaMap =
  case _rrDefinition of
    RelationshipToSource ToSourceRelationshipDef {..} -> do
      targetTables <-
        Map.lookup _tsrdSource allSources
          `onNothing` throw400 NotFound ("source not found: " <>> _tsrdSource)
      AB.dispatchAnyBackend @Backend targetTables \(partiallyResolvedSource :: PartiallyResolvedSource b') -> do
        let PartiallyResolvedSource _ targetSourceInfo targetTablesInfo = partiallyResolvedSource
        (targetTable :: TableName b') <- runAesonParser J.parseJSON _tsrdTable
        targetColumns <-
          fmap _tciFieldInfoMap $
            onNothing (Map.lookup targetTable targetTablesInfo) $ throwTableDoesNotExist @b' targetTable
        columnPairs <- for (Map.toList _tsrdFieldMapping) \(srcFieldName, tgtFieldName) -> do
          srcField <- askFieldInfo fields srcFieldName
          tgtField <- askFieldInfo targetColumns tgtFieldName
          srcColumn <- case srcField of
            FIColumn column -> pure column
            _ -> throw400 NotSupported "relationships from non-columns are not supported yet"
          pure (srcFieldName, srcColumn, tgtField)
        mapping <- for columnPairs \(srcFieldName, srcColumn, tgtColumn) -> do
          tgtScalar <- case pgiType tgtColumn of
            ColumnScalar scalarType -> pure scalarType
            ColumnEnumReference _ -> throw400 NotSupported "relationships to enum fields are not supported yet"
          pure (srcFieldName, (srcColumn, tgtScalar, pgiColumn tgtColumn))
        let sourceConfig = _rsConfig targetSourceInfo
            sourceCustomization = _rsCustomization targetSourceInfo
            rsri = RemoteSourceFieldInfo _rrName _tsrdRelationshipType _tsrdSource sourceConfig sourceCustomization targetTable $ Map.fromList mapping
            tableDependencies =
              [ SchemaDependency (SOSourceObj sourceSource $ AB.mkAnyBackend $ SOITable @b sourceTable) DRTable,
                SchemaDependency (SOSourceObj _tsrdSource $ AB.mkAnyBackend $ SOITable @b' targetTable) DRTable
              ]
            columnDependencies = flip concatMap columnPairs \(_, srcColumn, tgtColumn) ->
              [ SchemaDependency (SOSourceObj sourceSource $ AB.mkAnyBackend $ SOITableObj @b sourceTable $ TOCol @b $ pgiColumn srcColumn) DRRemoteRelationship,
                SchemaDependency (SOSourceObj _tsrdSource $ AB.mkAnyBackend $ SOITableObj @b' targetTable $ TOCol @b' $ pgiColumn tgtColumn) DRRemoteRelationship
              ]
        pure (RFISource $ mkAnyBackend @b' rsri, tableDependencies <> columnDependencies)
    RelationshipToSchema _ remoteRelationship@ToSchemaRelationshipDef {..} -> do
      RemoteSchemaCtx {..} <-
        onNothing (Map.lookup _trrdRemoteSchema remoteSchemaMap) $
          throw400 RemoteSchemaError $ "remote schema with name " <> _trrdRemoteSchema <<> " not found"
      remoteField <-
        validateSourceToSchemaRelationship remoteRelationship sourceTable _rrName sourceSource (_rscInfo, _rscIntroOriginal) fields
          `onLeft` (throw400 RemoteSchemaError . errorToText)
      let tableDep = SchemaDependency (SOSourceObj sourceSource $ AB.mkAnyBackend $ SOITable @b sourceTable) DRTable
          remoteSchemaDep = SchemaDependency (SORemoteSchema _trrdRemoteSchema) DRRemoteSchema
          fieldsDep =
            S.toList (_rrfiHasuraFields remoteField) <&> \case
              JoinColumn column _ ->
                -- TODO: shouldn't this be DRColumn??
                mkColDep @b DRRemoteRelationship sourceSource sourceTable column
              JoinComputedField computedFieldInfo ->
                mkComputedFieldDep @b DRRemoteRelationship sourceSource sourceTable $ _scfName computedFieldInfo
          schemaDependencies = (tableDep : remoteSchemaDep : fieldsDep)
      pure (RFISchema remoteField, schemaDependencies)
