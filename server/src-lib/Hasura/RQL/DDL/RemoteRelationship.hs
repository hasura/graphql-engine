{-# LANGUAGE RecordWildCards #-}
module Hasura.RQL.DDL.RemoteRelationship
  ( runCreateRemoteRelationship
  , runDeleteRemoteRelationship
  , runUpdateRemoteRelationship
  , dropRemoteRelationshipInMetadata
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.InsOrd as OMap

import           Hasura.EncJSON
import           Hasura.RQL.Types

runCreateRemoteRelationship
  :: (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b) => RemoteRelationship b -> m EncJSON
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

runUpdateRemoteRelationship :: (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b) => RemoteRelationship b -> m EncJSON
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
  :: RemoteRelationshipName -> TableMetadata b -> TableMetadata b
dropRemoteRelationshipInMetadata name =
  tmRemoteRelationships %~ OMap.delete name
