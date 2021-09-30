module Hasura.RQL.DDL.RemoteRelationship where

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
import Hasura.RQL.Types.Relationships.FromSchema
import Hasura.SQL.AnyBackend (mkAnyBackend)
import Hasura.SQL.AnyBackend qualified as AB

runCreateRemoteRelationship ::
  forall b m.
  (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b) =>
  FromSourceRelationship b ->
  m EncJSON
runCreateRemoteRelationship FromSourceRelationship {..} = do
  void $ askTabInfo @b _fsrSource _fsrTable
  let metadataObj =
        MOSourceObjId _fsrSource $
          AB.mkAnyBackend $
            SMOTableObj @b _fsrTable $
              MTORemoteRelationship _fsrName
      metadata = SourceRelationshipMetadata _fsrName _fsrDefinition
  buildSchemaCacheFor metadataObj $
    MetadataModifier $
      tableMetadataSetter @b _fsrSource _fsrTable . tmRemoteRelationships
        %~ OMap.insert _fsrName metadata
  pure successMsg

runUpdateRemoteRelationship ::
  forall b m.
  (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b) =>
  FromSourceRelationship b ->
  m EncJSON
runUpdateRemoteRelationship FromSourceRelationship {..} = do
  fieldInfoMap <- askFieldInfoMap @b _fsrSource _fsrTable
  void $ askRemoteRel fieldInfoMap _fsrName
  let metadataObj =
        MOSourceObjId _fsrSource $
          AB.mkAnyBackend $
            SMOTableObj @b _fsrTable $
              MTORemoteRelationship _fsrName
      metadata = SourceRelationshipMetadata _fsrName _fsrDefinition
  buildSchemaCacheFor metadataObj $
    MetadataModifier $
      tableMetadataSetter @b _fsrSource _fsrTable . tmRemoteRelationships
        %~ OMap.insert _fsrName metadata
  pure successMsg

data DeleteRemoteRelationship (b :: BackendType) = DeleteRemoteRelationship
  { _drrSource :: !SourceName,
    _drrTable :: !(TableName b),
    _drrName :: !RelName
  }

instance Backend b => FromJSON (DeleteRemoteRelationship b) where
  parseJSON = withObject "DeleteRemoteRelationship" $ \o ->
    DeleteRemoteRelationship
      <$> o .:? "source" .!= defaultSource
      <*> o .: "table"
      <*> o .: "name"

runDeleteRemoteRelationship ::
  forall b m.
  (BackendMetadata b, MonadError QErr m, CacheRWM m, MetadataM m) =>
  DeleteRemoteRelationship b ->
  m EncJSON
runDeleteRemoteRelationship (DeleteRemoteRelationship source table relName) = do
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

dropRemoteRelationshipInMetadata ::
  RelName ->
  TableMetadata b ->
  TableMetadata b
dropRemoteRelationshipInMetadata name =
  tmRemoteRelationships %~ OMap.delete name

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
  FromSourceRelationship b ->
  HashMap SourceName (AB.AnyBackend PartiallyResolvedSource) ->
  RemoteSchemaMap ->
  m (RemoteFieldInfo b, [SchemaDependency])
buildRemoteFieldInfo sourceSource sourceTable fields FromSourceRelationship {..} allSources remoteSchemaMap =
  case _fsrDefinition of
    FromSourceToSourceRelDef FromSourceToSourceRelationshipDef {..} -> do
      targetTables <-
        Map.lookup _fstsrdSource allSources
          `onNothing` throw400 NotFound ("source not found: " <>> _fstsrdSource)
      AB.dispatchAnyBackend @Backend targetTables \(partiallyResolvedSource :: PartiallyResolvedSource b') -> do
        let PartiallyResolvedSource _ targetSourceInfo targetTablesInfo = partiallyResolvedSource
        (targetTable :: TableName b') <- runAesonParser J.parseJSON _fstsrdTable
        targetColumns <-
          fmap _tciFieldInfoMap $
            onNothing (Map.lookup targetTable targetTablesInfo) $ throwTableDoesNotExist @b' targetTable
        columnPairs <- for (Map.toList _fstsrdFieldMapping) \(srcFieldName, tgtFieldName) -> do
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
            rsri = RemoteSourceFieldInfo _fsrName _fstsrdRelationshipType _fstsrdSource sourceConfig targetTable $ Map.fromList mapping
            tableDependencies =
              [ SchemaDependency (SOSourceObj sourceSource $ AB.mkAnyBackend $ SOITable @b sourceTable) DRTable,
                SchemaDependency (SOSourceObj _fstsrdSource $ AB.mkAnyBackend $ SOITable @b' targetTable) DRTable
              ]
            columnDependencies = flip concatMap columnPairs \(_, srcColumn, tgtColumn) ->
              [ SchemaDependency (SOSourceObj sourceSource $ AB.mkAnyBackend $ SOITableObj @b sourceTable $ TOCol @b $ pgiColumn srcColumn) DRRemoteRelationship,
                SchemaDependency (SOSourceObj _fstsrdSource $ AB.mkAnyBackend $ SOITableObj @b' targetTable $ TOCol @b' $ pgiColumn tgtColumn) DRRemoteRelationship
              ]
        pure (RFISource $ mkAnyBackend @b' rsri, tableDependencies <> columnDependencies)
    FromSourceToSchemaRelDef _ remoteRelationship@FromSourceToSchemaRelationshipDef {..} -> do
      RemoteSchemaCtx {..} <-
        onNothing (Map.lookup _fstrrdRemoteSchemaName remoteSchemaMap) $
          throw400 RemoteSchemaError $ "remote schema with name " <> _fstrrdRemoteSchemaName <<> " not found"
      remoteField <-
        validateSourceToSchemaRelationship remoteRelationship _fsrTable _fsrName _fsrSource (_rscInfo, _rscIntroOriginal) fields
          `onLeft` (throw400 RemoteSchemaError . errorToText)
      let tableDep = SchemaDependency (SOSourceObj _fsrSource $ AB.mkAnyBackend $ SOITable @b _fsrTable) DRTable
          remoteSchemaDep = SchemaDependency (SORemoteSchema _fstrrdRemoteSchemaName) DRRemoteSchema
          fieldsDep =
            S.toList (_rrfiHasuraFields remoteField) <&> \case
              JoinColumn columnInfo ->
                -- TODO: shouldn't this be DRColumn??
                mkColDep @b DRRemoteRelationship _fsrSource _fsrTable $ pgiColumn columnInfo
              JoinComputedField computedFieldInfo ->
                mkComputedFieldDep @b DRRemoteRelationship _fsrSource _fsrTable $ _scfName computedFieldInfo
          schemaDependencies = (tableDep : remoteSchemaDep : fieldsDep)
      pure (RFISchema remoteField, schemaDependencies)

buildFromSchemaRelationship ::
  MonadError QErr m =>
  FromSchemaRelationshipDef ->
  HashMap SourceName (AB.AnyBackend PartiallyResolvedSource) ->
  m (AB.AnyBackend ResolvedFromSchemaRelationship)
buildFromSchemaRelationship definition allSources = case definition of
  FromSchemaToSourceRelDef FromSchemaToSourceRelationshipDef {..} -> do
    targetTables <-
      Map.lookup _frtsrdSource allSources
        `onNothing` throw400 NotFound ("source not found: " <>> _frtsrdSource)
    AB.traverseBackend @Backend targetTables \(partiallyResolvedSource :: PartiallyResolvedSource b) -> do
      let PartiallyResolvedSource _ targetSourceInfo targetTablesInfo = partiallyResolvedSource
          sourceConfig = _rsConfig targetSourceInfo
      targetTable <- runAesonParser J.parseJSON _frtsrdTable
      targetColumns <-
        fmap _tciFieldInfoMap (Map.lookup targetTable targetTablesInfo)
          `onNothing` throwTableDoesNotExist @b targetTable
      mapping <- for _frtsrdFieldMapping \tgtFieldName -> do
        tgtColumn <- askFieldInfo targetColumns tgtFieldName
        tgtScalar <- case pgiType tgtColumn of
          ColumnScalar scalarType -> pure scalarType
          ColumnEnumReference _ -> throw400 NotSupported "relationships to enum fields are not supported yet"
        pure (tgtScalar, pgiColumn tgtColumn)
      pure $
        ResolvedFromSchemaToSourceRel $
          ResolvedFromSchemaToSourceRelationship
            { _rfrtsdrLHSTypeName = _frtsrdLHSTypeName,
              _rfrtsdrRelationshipType = _frtsrdRelationshipType,
              _rfrtsdrSource = _frtsrdSource,
              _rfrtsdrSourceConfig = sourceConfig,
              _rfrtsdrTable = targetTable,
              _rfrtsdrFieldMapping = mapping
            }
