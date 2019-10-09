{-# LANGUAGE RecordWildCards #-}

module Hasura.RQL.DDL.RemoteRelationship
  ( runCreateRemoteRelationship
  )
where

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.RemoteRelationship.Validate
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Data.Aeson                                 as J
import qualified Data.HashMap.Strict                        as Map
import qualified Data.Set                                   as Set
import qualified Database.PG.Query                          as Q

runCreateRemoteRelationship ::
     (MonadTx m, CacheRWM m, UserInfoM m) => RemoteRelationship -> m EncJSON
runCreateRemoteRelationship remoteRelationship = do
  adminOnly
  remoteField <- runCreateRemoteRelationshipP1 remoteRelationship
  runCreateRemoteRelationshipP2 remoteField
  pure successMsg

runCreateRemoteRelationshipP1 ::
     (MonadTx m, CacheRM m) => RemoteRelationship -> m RemoteField
runCreateRemoteRelationshipP1 remoteRel@RemoteRelationship {..} = do
  sc <- askSchemaCache
  case Map.lookup rrRemoteSchema (scRemoteSchemas sc) of
    Just rsCtx -> do
      tableInfo <-
        onNothing (Map.lookup rrTable $ scTables sc) $
        throw400 NotFound "table not found"
      let remoteFieldE =
            validateRelationship remoteRel (rscGCtx rsCtx) tableInfo
      case remoteFieldE of
        Left err          -> fromValidationError err
        Right remoteField -> pure remoteField
    Nothing ->
      throw400
        NotFound
        ("no such remote schema: " <> remoteSchemaNameToTxt rrRemoteSchema)

runCreateRemoteRelationshipP2 ::
     (MonadTx m, CacheRWM m) => RemoteField -> m EncJSON
runCreateRemoteRelationshipP2 remoteField = do
  liftTx (persistRemoteRelationship (rfRemoteRelationship remoteField))
  runCreateRemoteRelationshipP2Setup remoteField
  pure successMsg

persistRemoteRelationship
  :: RemoteRelationship -> Q.TxE QErr ()
persistRemoteRelationship remoteRelationship =
  Q.unitQE defaultTxErrorHandler [Q.sql|
  INSERT INTO hdb_catalog.hdb_remote_relationship
  (name, table_schema, table_name, remote_schema, configuration)
  VALUES ($1, $2, $3, $4, $5 :: jsonb)
  |]
  (let QualifiedObject schema_name table_name = rrTable remoteRelationship
   in (rrName remoteRelationship
      ,schema_name
      ,table_name
      ,rrRemoteSchema remoteRelationship
      ,Q.JSONB (J.toJSON remoteRelationship)))
  True

runCreateRemoteRelationshipP2Setup ::
     (MonadTx m, CacheRWM m) => RemoteField -> m ()
runCreateRemoteRelationshipP2Setup remoteField = do
  addRemoteRelToCache remoteField schemaDependencies
  where
    schemaDependencies =
      let table = rrTable $ rfRemoteRelationship remoteField
          joinVariables = extractVariables (rfRemoteRelationship remoteField)
          columns = map (PGCol . getFieldNameTxt) $ Set.toList joinVariables
          remoteSchemaName = rrRemoteSchema $ rfRemoteRelationship remoteField
          tableDep = SchemaDependency (SOTable table) DRTable
          columnsDep =
            flip map columns $ \column ->
              SchemaDependency
                (SOTableObj table $ TOCol column)
                DRRemoteRelationship
          remoteSchemaDep =
            SchemaDependency
              (SORemoteSchema remoteSchemaName)
              DRRemoteRelationship
       in (tableDep : remoteSchemaDep : columnsDep)
