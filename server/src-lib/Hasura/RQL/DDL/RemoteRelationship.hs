{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Hasura.RQL.DDL.RemoteRelationship
  ( runCreateRemoteRelationship
  , runDeleteRemoteRelationship
  , runUpdateRemoteRelationship
  , updateRemoteRelInCatalog
  , persistRemoteRelationship
  , resolveRemoteRelationship
  , delRemoteRelFromCatalog
  , getRemoteRelDefFromCatalog
  ) where

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.RQL.Types.RemoteRelationship
import           Hasura.RQL.Types.SchemaCacheTypes
import           Hasura.RQL.Types.Column                    ()
import           Hasura.SQL.Types
import           Hasura.RQL.DDL.RemoteRelationship.Validate

import           Instances.TH.Lift                          ()

import qualified Database.PG.Query                          as Q
import qualified Data.HashMap.Strict                        as Map
import qualified Data.HashSet                                   as HS

runCreateRemoteRelationship
  :: (MonadTx m, CacheRWM m) => RemoteRelationship -> m EncJSON
runCreateRemoteRelationship remoteRelationship = do
  void $ askTabInfo $ rtrTable remoteRelationship
  liftTx $ persistRemoteRelationship remoteRelationship
  buildSchemaCacheFor $ MOTableObj table $ MTORemoteRelationship $ rtrName remoteRelationship
  pure successMsg
  where
    table = rtrTable remoteRelationship

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
      schemaDependencies =
        let tableDep = SchemaDependency (SOTable table) DRTable
            columnsDep =
              map
                (\column ->
                   SchemaDependency
                     (SOTableObj table $ TOCol column)
                     DRRemoteRelationship ) $
              map pgiColumn $ HS.toList $ _rfiHasuraFields remoteField
            remoteSchemaDep =
              SchemaDependency (SORemoteSchema $ rtrRemoteSchema remoteRelationship) DRRemoteSchema
         in (tableDep : remoteSchemaDep : columnsDep)

  pure (remoteField, schemaDependencies)

runUpdateRemoteRelationship :: (MonadTx m, CacheRWM m) => RemoteRelationship -> m EncJSON
runUpdateRemoteRelationship remoteRelationship = do
  fieldInfoMap <- askFieldInfoMap table
  void $ askRemoteRel fieldInfoMap (rtrName remoteRelationship)
  liftTx $ updateRemoteRelInCatalog remoteRelationship
  buildSchemaCacheFor $ MOTableObj table $ MTORemoteRelationship $ rtrName remoteRelationship
  pure successMsg
  where
    table = rtrTable remoteRelationship

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

runDeleteRemoteRelationship ::
     (MonadTx m, CacheRWM m) => DeleteRemoteRelationship -> m EncJSON
runDeleteRemoteRelationship (DeleteRemoteRelationship table relName)= do
  fieldInfoMap <- askFieldInfoMap table
  void $ askRemoteRel fieldInfoMap relName
  liftTx $ delRemoteRelFromCatalog table relName
  buildSchemaCacheFor $ MOTableObj table $ MTORemoteRelationship relName
  pure successMsg

delRemoteRelFromCatalog
  :: QualifiedTable -> RemoteRelationshipName -> Q.TxE QErr ()
delRemoteRelFromCatalog (QualifiedObject sn tn) (RemoteRelationshipName relName) =
  Q.unitQE defaultTxErrorHandler [Q.sql|
           DELETE FROM
                  hdb_catalog.hdb_remote_relationship
           WHERE table_schema =  $1
             AND table_name = $2
             AND remote_relationship_name = $3
                |] (sn, tn, relName) True

getRemoteRelDefFromCatalog :: RemoteRelationshipName -> QualifiedTable ->  Q.TxE QErr RemoteRelationshipDef
getRemoteRelDefFromCatalog relName (QualifiedObject schemaName tableName) = do
  (Q.AltJ defn) <- (runIdentity . Q.getRow) <$> Q.withQE defaultTxErrorHandler
    [Q.sql|
     SELECT definition::json
     FROM hdb_catalog.hdb_remote_relationship
     WHERE remote_relationship_name = $1 and table_schema = $2 and table_name = $3
     |] (relName,schemaName,tableName) False
  return defn
