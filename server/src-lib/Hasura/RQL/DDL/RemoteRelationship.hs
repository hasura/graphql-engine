{-# LANGUAGE RecordWildCards #-}
module Hasura.RQL.DDL.RemoteRelationship
  ( runCreateRemoteRelationship
  , runDeleteRemoteRelationship
  , runUpdateRemoteRelationship
  , resolveRemoteRelationship
  , dropRemoteRelationshipInMetadata
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.InsOrd                 as OMap
import qualified Data.HashSet                               as HS

import           Hasura.EncJSON
import           Hasura.RQL.DDL.RemoteRelationship.Validate
import           Hasura.RQL.Types

runCreateRemoteRelationship
  :: (MonadError QErr m, CacheRWM m, MetadataM m) => RemoteRelationship -> m EncJSON
runCreateRemoteRelationship RemoteRelationship{..} = do
  void $ askTabInfo rtrSource rtrTable
  let metadataObj = MOSourceObjId rtrSource $
                    SMOTableObj rtrTable $ MTORemoteRelationship rtrName
      metadata = RemoteRelationshipMetadata rtrName $
        RemoteRelationshipDef rtrRemoteSchema rtrHasuraFields rtrRemoteField
  buildSchemaCacheFor metadataObj
    $ MetadataModifier
    $ tableMetadataSetter rtrSource rtrTable.tmRemoteRelationships
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
  eitherRemoteField <- runExceptT $
    validateRemoteRelationship remoteRelationship remoteSchemaMap pgColumns
  remoteField <- onLeft eitherRemoteField $ throw400 RemoteSchemaError . errorToText
  let table = rtrTable remoteRelationship
      source = rtrSource remoteRelationship
      schemaDependencies =
        let tableDep = SchemaDependency (SOSourceObj source $ SOITable table) DRTable
            columnsDep =
              map
                (flip SchemaDependency DRRemoteRelationship . SOSourceObj source . SOITableObj table . TOCol . pgiColumn)
                $ HS.toList $ _rfiHasuraFields remoteField
            remoteSchemaDep =
              SchemaDependency (SORemoteSchema $ rtrRemoteSchema remoteRelationship) DRRemoteSchema
         in (tableDep : remoteSchemaDep : columnsDep)

  pure (remoteField, schemaDependencies)

runUpdateRemoteRelationship :: (MonadError QErr m, CacheRWM m, MetadataM m) => RemoteRelationship -> m EncJSON
runUpdateRemoteRelationship RemoteRelationship{..} = do
  fieldInfoMap <- askFieldInfoMap rtrSource rtrTable
  void $ askRemoteRel fieldInfoMap rtrName
  let metadataObj = MOSourceObjId rtrSource $
                    SMOTableObj rtrTable $ MTORemoteRelationship rtrName
      metadata = RemoteRelationshipMetadata rtrName $
        RemoteRelationshipDef rtrRemoteSchema rtrHasuraFields rtrRemoteField
  buildSchemaCacheFor metadataObj
    $ MetadataModifier
    $ tableMetadataSetter rtrSource rtrTable.tmRemoteRelationships
      %~ OMap.insert rtrName metadata
  pure successMsg

runDeleteRemoteRelationship
  :: (MonadError QErr m, CacheRWM m, MetadataM m) => DeleteRemoteRelationship -> m EncJSON
runDeleteRemoteRelationship (DeleteRemoteRelationship source table relName)= do
  fieldInfoMap <- askFieldInfoMap source table
  void $ askRemoteRel fieldInfoMap relName
  let metadataObj = MOSourceObjId source $
                    SMOTableObj table $ MTORemoteRelationship relName
  buildSchemaCacheFor metadataObj
    $ MetadataModifier
    $ tableMetadataSetter source table %~ dropRemoteRelationshipInMetadata relName
  pure successMsg

dropRemoteRelationshipInMetadata
  :: RemoteRelationshipName -> TableMetadata -> TableMetadata
dropRemoteRelationshipInMetadata name =
  tmRemoteRelationships %~ OMap.delete name
