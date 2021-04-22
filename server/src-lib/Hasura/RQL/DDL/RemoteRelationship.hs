module Hasura.RQL.DDL.RemoteRelationship
  ( runCreateRemoteRelationship
  , runDeleteRemoteRelationship
  , runUpdateRemoteRelationship
  , dropRemoteRelationshipInMetadata
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.InsOrd as OMap

import qualified Hasura.SQL.AnyBackend      as AB

import           Hasura.EncJSON
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
