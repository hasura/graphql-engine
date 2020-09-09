{-# LANGUAGE RecordWildCards #-}
module Hasura.RQL.DDL.RemoteRelationship
  ( runCreateRemoteRelationship
  , runDeleteRemoteRelationship
  , runUpdateRemoteRelationship
  , updateRemoteRelInCatalog
  , persistRemoteRelationship
  , resolveRemoteRelationship
  -- , delRemoteRelFromCatalog
  , dropRemoteRelationshipInMetadata
  , getRemoteRelDefFromCatalog
  ) where

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.RemoteRelationship.Validate
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Column                    ()
import           Hasura.SQL.Types

import           Instances.TH.Lift                          ()

import qualified Data.HashMap.Strict                        as HM
import qualified Data.HashSet                               as HS
import qualified Database.PG.Query                          as Q

runCreateRemoteRelationship
  :: (MonadTx m, CacheRWM m) => RemoteRelationship -> m EncJSON
runCreateRemoteRelationship RemoteRelationship{..} = do
  -- Few checks
  void $ askTabInfo rtrSource rtrTable
  -- liftTx $ persistRemoteRelationship remoteRelationship
  let metadataObj = MOSourceObjId rtrSource $ SMOTableObj rtrTable $
                    MTORemoteRelationship rtrName
      metadata = RemoteRelationshipMeta rtrName $
        RemoteRelationshipDef rtrRemoteSchema rtrHasuraFields rtrRemoteField
  buildSchemaCacheFor metadataObj
    $ MetadataModifier
    $ tableMetadataSetter rtrSource rtrTable.tmRemoteRelationships
      %~ HM.insert rtrName metadata
  pure successMsg

resolveRemoteRelationship
  :: QErrM m
  => RemoteRelationship
  -> [PGColumnInfo]
  -> RemoteSchemaMap
  -> m (RemoteFieldInfo, [SchemaDependency])
resolveRemoteRelationship remoteRelationship
                          pgColumns
                          remoteSchemaMap = do
  eitherRemoteField <- runExceptT $
    validateRemoteRelationship remoteRelationship remoteSchemaMap pgColumns
  remoteField <- either (throw400 RemoteSchemaError . errorToText) pure $ eitherRemoteField
  let table = rtrTable remoteRelationship
      source = rtrSource remoteRelationship
      schemaDependencies =
        let tableDep = SchemaDependency (SOSourceObj source $ SOITable table) DRTable
            columnsDep =
              map
                (\column ->
                   SchemaDependency
                     (SOSourceObj source $ SOITableObj table $ TOCol column)
                     DRRemoteRelationship ) $
              map pgiColumn $ HS.toList $ _rfiHasuraFields remoteField
            remoteSchemaDep =
              SchemaDependency (SORemoteSchema $ rtrRemoteSchema remoteRelationship) DRRemoteSchema
         in (tableDep : remoteSchemaDep : columnsDep)

  pure (remoteField, schemaDependencies)

runUpdateRemoteRelationship :: (MonadTx m, CacheRWM m) => RemoteRelationship -> m EncJSON
runUpdateRemoteRelationship RemoteRelationship{..} = do
  fieldInfoMap <- askFieldInfoMap rtrTable
  void $ askRemoteRel fieldInfoMap rtrName
  -- liftTx $ updateRemoteRelInCatalog remoteRelationship
  let metadataObj = MOSourceObjId rtrSource $ SMOTableObj rtrTable $
                    MTORemoteRelationship rtrName
      metadata = RemoteRelationshipMeta rtrName $
        RemoteRelationshipDef rtrRemoteSchema rtrHasuraFields rtrRemoteField
  buildSchemaCacheFor metadataObj
    $ MetadataModifier
    $ tableMetadataSetter rtrSource rtrTable.tmRemoteRelationships
      %~ HM.insert rtrName metadata
  pure successMsg

mkRemoteRelationshipDef :: RemoteRelationship -> RemoteRelationshipDef
mkRemoteRelationshipDef RemoteRelationship {..} =
  RemoteRelationshipDef rtrRemoteSchema rtrHasuraFields rtrRemoteField

persistRemoteRelationship :: RemoteRelationship -> Q.TxE QErr ()
persistRemoteRelationship remoteRelationship =
  Q.unitQE defaultTxErrorHandler [Q.sql|
       INSERT INTO hdb_catalog.hdb_remote_relationship
       (remote_relationship_name, table_schema, table_name, definition)
       VALUES ($1, $2, $3, $4::jsonb)
  |] (rtrName remoteRelationship, schemaName, tableName, Q.AltJ definition) True
  where
    QualifiedObject schemaName tableName = rtrTable remoteRelationship
    definition = mkRemoteRelationshipDef remoteRelationship

updateRemoteRelInCatalog
  :: RemoteRelationship -> Q.TxE QErr ()
updateRemoteRelInCatalog remoteRelationship =
  Q.unitQE defaultTxErrorHandler [Q.sql|
    UPDATE hdb_catalog.hdb_remote_relationship
    SET definition = $4::jsonb
    WHERE remote_relationship_name = $1 AND table_schema = $2 AND table_name = $3
  |] (rtrName remoteRelationship, schemaName, tableName, Q.AltJ definition) True
  where
    QualifiedObject schemaName tableName = rtrTable remoteRelationship
    definition = mkRemoteRelationshipDef remoteRelationship

runDeleteRemoteRelationship
  :: (MonadTx m, CacheRWM m) => DeleteRemoteRelationship -> m EncJSON
runDeleteRemoteRelationship (DeleteRemoteRelationship source table relName)= do
  fieldInfoMap <- askFieldInfoMap table
  void $ askRemoteRel fieldInfoMap relName
  -- liftTx $ delRemoteRelFromCatalog table relName
  let metadataObj = MOSourceObjId source $ SMOTableObj table $
                    MTORemoteRelationship relName
  buildSchemaCacheFor metadataObj
    $ MetadataModifier
    $ tableMetadataSetter source table %~ dropRemoteRelationshipInMetadata relName
  pure successMsg

dropRemoteRelationshipInMetadata
  :: RemoteRelationshipName -> TableMetadata -> TableMetadata
dropRemoteRelationshipInMetadata name =
  tmRemoteRelationships %~ HM.delete name

-- delRemoteRelFromCatalog
--   :: QualifiedTable -> RemoteRelationshipName -> Q.TxE QErr ()
-- delRemoteRelFromCatalog (QualifiedObject sn tn) (RemoteRelationshipName relName) =
--   Q.unitQE defaultTxErrorHandler [Q.sql|
--            DELETE FROM
--                   hdb_catalog.hdb_remote_relationship
--            WHERE table_schema =  $1
--              AND table_name = $2
--              AND remote_relationship_name = $3
--                 |] (sn, tn, relName) True

getRemoteRelDefFromCatalog :: RemoteRelationshipName -> QualifiedTable ->  Q.TxE QErr RemoteRelationshipDef
getRemoteRelDefFromCatalog relName (QualifiedObject schemaName tableName) = do
  (Q.AltJ defn) <- (runIdentity . Q.getRow) <$> Q.withQE defaultTxErrorHandler
    [Q.sql|
     SELECT definition::json
     FROM hdb_catalog.hdb_remote_relationship
     WHERE remote_relationship_name = $1 and table_schema = $2 and table_name = $3
     |] (relName,schemaName,tableName) False
  return defn
