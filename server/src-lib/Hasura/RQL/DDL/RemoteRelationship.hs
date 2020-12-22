{-# LANGUAGE RecordWildCards #-}
module Hasura.RQL.DDL.RemoteRelationship
  ( runCreateRemoteRelationship
  , runDeleteRemoteRelationship
  , runUpdateRemoteRelationship
  , resolveRemoteRelationship
  , dropRemoteRelationshipInMetadata
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                        as Map
import qualified Data.HashMap.Strict.InsOrd                 as OMap
import qualified Data.HashSet                               as HS

import           Data.Text.Extended

import           Hasura.EncJSON
import           Hasura.RQL.DDL.RemoteRelationship.Validate
import           Hasura.RQL.Types

runCreateRemoteRelationship
  :: (MonadError QErr m, CacheRWM m, MetadataM m) => RemoteRelationship -> m EncJSON
runCreateRemoteRelationship RemoteRelationship{..} = do
  void $ askTabInfo rtrTable
  let metadataObj = MOTableObj rtrTable $ MTORemoteRelationship rtrName
      metadata = RemoteRelationshipMetadata rtrName $
        RemoteRelationshipDef rtrRemoteSchema rtrHasuraFields rtrRemoteField
  buildSchemaCacheFor metadataObj
    $ MetadataModifier
    $ metaTables.ix rtrTable.tmRemoteRelationships
      %~ OMap.insert rtrName metadata
  pure successMsg

resolveRemoteRelationship
  :: QErrM m
  => RemoteRelationship
  -> [ColumnInfo 'Postgres]
  -> RemoteSchemaMap
  -> m (RemoteFieldInfo 'Postgres, [SchemaDependency])
resolveRemoteRelationship remoteRelationship
                          pgColumns
                          remoteSchemaMap = do
  let remoteSchemaName = rtrRemoteSchema remoteRelationship
  (RemoteSchemaCtx _name introspectionResult remoteSchemaInfo _ _ _permissions) <-
    onNothing (Map.lookup remoteSchemaName remoteSchemaMap)
      $ throw400 RemoteSchemaError $ "remote schema with name " <> remoteSchemaName <<> " not found"
  eitherRemoteField <- runExceptT $
    validateRemoteRelationship remoteRelationship (remoteSchemaInfo, introspectionResult) pgColumns
  remoteField <- onLeft eitherRemoteField $ throw400 RemoteSchemaError . errorToText
  let table = rtrTable remoteRelationship
      schemaDependencies =
        let tableDep = SchemaDependency (SOTable table) DRTable
            columnsDep =
              map
                (flip SchemaDependency DRRemoteRelationship . SOTableObj table . TOCol . pgiColumn)
                $ HS.toList $ _rfiHasuraFields remoteField
            remoteSchemaDep =
              SchemaDependency (SORemoteSchema remoteSchemaName) DRRemoteSchema
         in (tableDep : remoteSchemaDep : columnsDep)

  pure (remoteField, schemaDependencies)

runUpdateRemoteRelationship :: (MonadError QErr m, CacheRWM m, MetadataM m) => RemoteRelationship -> m EncJSON
runUpdateRemoteRelationship RemoteRelationship{..} = do
  fieldInfoMap <- askFieldInfoMap rtrTable
  void $ askRemoteRel fieldInfoMap rtrName
  let metadataObj = MOTableObj rtrTable $ MTORemoteRelationship rtrName
      metadata = RemoteRelationshipMetadata rtrName $
        RemoteRelationshipDef rtrRemoteSchema rtrHasuraFields rtrRemoteField
  buildSchemaCacheFor metadataObj
    $ MetadataModifier
    $ metaTables.ix rtrTable.tmRemoteRelationships
      %~ OMap.insert rtrName metadata
  pure successMsg

runDeleteRemoteRelationship
  :: (MonadError QErr m, CacheRWM m, MetadataM m) => DeleteRemoteRelationship -> m EncJSON
runDeleteRemoteRelationship (DeleteRemoteRelationship table relName)= do
  fieldInfoMap <- askFieldInfoMap table
  void $ askRemoteRel fieldInfoMap relName
  let metadataObj = MOTableObj table $ MTORemoteRelationship relName
  buildSchemaCacheFor metadataObj
    $ MetadataModifier
    $ metaTables.ix table %~ dropRemoteRelationshipInMetadata relName
  pure successMsg

dropRemoteRelationshipInMetadata
  :: RemoteRelationshipName -> TableMetadata -> TableMetadata
dropRemoteRelationshipInMetadata name =
  tmRemoteRelationships %~ OMap.delete name
