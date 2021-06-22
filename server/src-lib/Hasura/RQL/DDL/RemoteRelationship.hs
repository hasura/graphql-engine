module Hasura.RQL.DDL.RemoteRelationship
  ( runCreateRemoteRelationship
  , runDeleteRemoteRelationship
  , runUpdateRemoteRelationship
  , dropRemoteRelationshipInMetadata
  , SourcePartialInfo(..)
  , buildRemoteFieldInfo
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                                 as J
import qualified Data.HashMap.Strict                        as Map
import qualified Data.HashMap.Strict.InsOrd                 as OMap
import qualified Data.HashSet                               as S

import           Data.Text.Extended

import qualified Hasura.SQL.AnyBackend                      as AB

import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.RQL.DDL.RemoteRelationship.Validate
import           Hasura.RQL.Types
import           Hasura.SQL.AnyBackend                      (mkAnyBackend)


runCreateRemoteRelationship
  :: forall b m
   . (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b)
  => RemoteRelationship b
  -> m EncJSON
runCreateRemoteRelationship RemoteRelationship {..} =
  case _rtrDefinition of
    RemoteSourceRelDef _ -> error "TODO"
    def@(RemoteSchemaRelDef _) -> do
      void $ askTabInfo @b _rtrSource _rtrTable
      let metadataObj = MOSourceObjId _rtrSource
                        $ AB.mkAnyBackend
                        $ SMOTableObj @b _rtrTable
                        $ MTORemoteRelationship _rtrName
          metadata = RemoteRelationshipMetadata _rtrName def
      buildSchemaCacheFor metadataObj
        $ MetadataModifier
        $ tableMetadataSetter @b _rtrSource _rtrTable.tmRemoteRelationships
          %~ OMap.insert _rtrName metadata
      pure successMsg

runUpdateRemoteRelationship
  :: forall b m
   . (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b)
  => RemoteRelationship b
  -> m EncJSON
runUpdateRemoteRelationship RemoteRelationship {..} =
  case _rtrDefinition of
    RemoteSourceRelDef _ -> error "TODO"
    def@(RemoteSchemaRelDef _) -> do
      fieldInfoMap <- askFieldInfoMap @b _rtrSource _rtrTable
      void $ askRemoteRel fieldInfoMap _rtrName
      let metadataObj = MOSourceObjId _rtrSource
                          $ AB.mkAnyBackend
                          $ SMOTableObj @b _rtrTable
                          $ MTORemoteRelationship _rtrName
          metadata = RemoteRelationshipMetadata _rtrName def
      buildSchemaCacheFor metadataObj
        $ MetadataModifier
        $ tableMetadataSetter @b _rtrSource _rtrTable.tmRemoteRelationships
          %~ OMap.insert _rtrName metadata
      pure successMsg

runDeleteRemoteRelationship
  :: forall b m
   . (BackendMetadata b, MonadError QErr m, CacheRWM m, MetadataM m)
  => DeleteRemoteRelationship b
  -> m EncJSON
runDeleteRemoteRelationship (DeleteRemoteRelationship source table relName)= do
  fieldInfoMap <- askFieldInfoMap @b source table
  void $ askRemoteRel fieldInfoMap relName
  let metadataObj = MOSourceObjId source
                      $ AB.mkAnyBackend
                      $ SMOTableObj @b table
                      $ MTORemoteRelationship relName
  buildSchemaCacheFor metadataObj
    $ MetadataModifier
    $ tableMetadataSetter @b source table %~ dropRemoteRelationshipInMetadata relName
  pure successMsg

dropRemoteRelationshipInMetadata
  :: RemoteRelationshipName -> TableMetadata b -> TableMetadata b
dropRemoteRelationshipInMetadata name =
  tmRemoteRelationships %~ OMap.delete name


-- FIXME: move me somewhere that makes sense perhaps?
newtype SourcePartialInfo b = SPI (ResolvedSource b, TableCoreInfoMap b)

buildRemoteFieldInfo
  :: forall m b
   . (Backend b, QErrM m)
  => HashMap SourceName (AB.AnyBackend SourcePartialInfo)
  -> RemoteRelationship b
  -> FieldInfoMap (ColumnInfo b)
  -> RemoteSchemaMap
  -> m (RemoteFieldInfo b, [SchemaDependency])
buildRemoteFieldInfo allSources rr allColumns remoteSchemaMap =
  case _rtrDefinition rr of
    RemoteSourceRelDef RemoteSourceRelationshipDef{..} -> do
      targetTables <- Map.lookup rsrSource allSources
        `onNothing` (throw400 NotFound $ "source not found: " <>> rsrSource)
      AB.dispatchAnyBackend @Backend targetTables \(SPI (targetSourceInfo, TCIM targetTablesInfo) :: SourcePartialInfo b') -> do
        (table :: TableName b') <- runAesonParser J.parseJSON rsrTable
        targetColumns <- fmap _tciFieldInfoMap $
          onNothing ( Map.lookup table targetTablesInfo) $ throwTableDoesNotExist @b' table
        mapping <- for (Map.toList rsrFieldMapping) \(srcFieldName, tgtFieldName) -> do
          srcColumn <- askFieldInfo allColumns srcFieldName
          tgtColumn <- askFieldInfo targetColumns tgtFieldName
          tgtScalar <- case pgiType tgtColumn of
            ColumnScalar scalarType -> pure scalarType
            ColumnEnumReference _   -> throw400 RemoteSchemaError "FIXME LOL target column is an enum"
          pure (srcFieldName, (srcColumn, tgtScalar, pgiColumn tgtColumn))
        let sourceConfig = _rsConfig targetSourceInfo
            rsri = RemoteSourceRelationshipInfo (_rtrName rr) rsrRelationshipType rsrSource sourceConfig table $ Map.fromList mapping
        pure (RFISource $ mkAnyBackend @b' rsri, [])
    RemoteSchemaRelDef remoteRelationship -> do
      let remoteSchemaName = _rrdRemoteSchema remoteRelationship
      (RemoteSchemaCtx _name introspectionResult remoteSchemaInfo _ _ _permissions) <-
        onNothing (Map.lookup remoteSchemaName remoteSchemaMap)
          $ throw400 RemoteSchemaError $ "remote schema with name " <> remoteSchemaName <<> " not found"
      let table  = _rtrTable rr
          name   = _rtrName rr
          source = _rtrSource rr
      eitherRemoteField <- runExceptT
        $ validateRemoteSchemaRelationship remoteRelationship table name source (remoteSchemaInfo, introspectionResult)
        $ Map.elems allColumns
      remoteField <- onLeft eitherRemoteField $ throw400 RemoteSchemaError . errorToText
      let schemaDependencies =
            let tableDep = SchemaDependency
                             (SOSourceObj source
                                $ AB.mkAnyBackend
                                $ SOITable @b table)
                             DRTable
                columnsDep =
                  map
                    (flip SchemaDependency DRRemoteRelationship
                       . SOSourceObj source
                       . AB.mkAnyBackend
                       . SOITableObj @b table
                       . TOCol @b
                       . pgiColumn)
                    $ S.toList $ _rfiHasuraFields remoteField
                remoteSchemaDep =
                  SchemaDependency (SORemoteSchema remoteSchemaName) DRRemoteSchema
             in (tableDep : remoteSchemaDep : columnsDep)

      pure (RFISchema remoteField, schemaDependencies)
