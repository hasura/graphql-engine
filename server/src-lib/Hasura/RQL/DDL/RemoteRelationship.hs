module Hasura.RQL.DDL.RemoteRelationship
  ( runCreateRemoteRelationship
  , runDeleteRemoteRelationship
  , runUpdateRemoteRelationship
  , dropRemoteRelationshipInMetadata
  , buildRemoteFieldInfo
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                        as Map
import qualified Data.HashMap.Strict.InsOrd                 as OMap
import qualified Data.HashSet                               as S

import           Data.Text.Extended

import qualified Hasura.SQL.AnyBackend                      as AB

import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.RQL.DDL.RemoteRelationship.Validate
import           Hasura.RQL.Types


runCreateRemoteRelationship
  :: forall b m
   . (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b)
  => RemoteRelationship b
  -> m EncJSON
runCreateRemoteRelationship RemoteRelationship{..} = do
  void $ askTabInfo @b rtrSource rtrTable
  let metadataObj = MOSourceObjId rtrSource
                    $ AB.mkAnyBackend
                    $ SMOTableObj @b rtrTable
                    $ MTORemoteRelationship rtrName
      metadata = RemoteRelationshipMetadata rtrName $
        RemoteRelationshipDef rtrRemoteSchema rtrHasuraFields rtrRemoteField
  buildSchemaCacheFor metadataObj
    $ MetadataModifier
    $ tableMetadataSetter @b rtrSource rtrTable.tmRemoteRelationships
      %~ OMap.insert rtrName metadata
  pure successMsg

runUpdateRemoteRelationship
  :: forall b m
   . (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b)
  => RemoteRelationship b
  -> m EncJSON
runUpdateRemoteRelationship RemoteRelationship{..} = do
  fieldInfoMap <- askFieldInfoMap @b rtrSource rtrTable
  void $ askRemoteRel fieldInfoMap rtrName
  let metadataObj = MOSourceObjId rtrSource
                      $ AB.mkAnyBackend
                      $ SMOTableObj @b rtrTable
                      $ MTORemoteRelationship rtrName
      metadata = RemoteRelationshipMetadata rtrName $
        RemoteRelationshipDef rtrRemoteSchema rtrHasuraFields rtrRemoteField
  buildSchemaCacheFor metadataObj
    $ MetadataModifier
    $ tableMetadataSetter @b rtrSource rtrTable.tmRemoteRelationships
      %~ OMap.insert rtrName metadata
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

buildRemoteFieldInfo
  :: forall m b
   . (Backend b, QErrM m)
  => RemoteRelationship b
  -> FieldInfoMap (FieldInfo b)
  -> RemoteSchemaMap
  -> m (RemoteFieldInfo b, [SchemaDependency])
buildRemoteFieldInfo remoteRelationship
                          fields
                          remoteSchemaMap = do
  let remoteSchemaName = rtrRemoteSchema remoteRelationship
  (RemoteSchemaCtx _name introspectionResult remoteSchemaInfo _ _ _permissions) <-
    onNothing (Map.lookup remoteSchemaName remoteSchemaMap)
      $ throw400 RemoteSchemaError $ "remote schema with name " <> remoteSchemaName <<> " not found"
  eitherRemoteField <- runExceptT $
    validateRemoteRelationship remoteRelationship (remoteSchemaInfo, introspectionResult) fields
  remoteField <- onLeft eitherRemoteField $ throw400 RemoteSchemaError . errorToText
  let table = rtrTable remoteRelationship
      source = rtrSource remoteRelationship
      schemaDependencies =
        let tableDep = SchemaDependency
                         (SOSourceObj source
                            $ AB.mkAnyBackend
                            $ SOITable @b table)
                         DRTable
            fieldsDep = S.toList (_rfiHasuraFields remoteField) <&> \case
              JoinColumn columnInfo ->
                mkColDep @b DRRemoteRelationship source table $ pgiColumn columnInfo
              JoinComputedField computedFieldInfo ->
                mkComputedFieldDep @b DRRemoteRelationship source table $ _scfName computedFieldInfo
            remoteSchemaDep =
              SchemaDependency (SORemoteSchema remoteSchemaName) DRRemoteSchema
         in (tableDep : remoteSchemaDep : fieldsDep)

  pure (remoteField, schemaDependencies)
