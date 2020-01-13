module Hasura.RQL.DDL.RemoteSchema
  ( runAddRemoteSchema
  , runRemoveRemoteSchema
  , removeRemoteSchemaFromCatalog
  , runReloadRemoteSchema
  , addRemoteSchemaP1
  , addRemoteSchemaP2Setup
  , addRemoteSchemaP2
  ) where

import           Hasura.EncJSON
import           Hasura.Prelude

import qualified Data.Aeson                  as J
import qualified Data.HashMap.Strict         as Map
import qualified Database.PG.Query           as Q

import           Hasura.GraphQL.RemoteServer
import           Hasura.RQL.Types
import           Hasura.SQL.Types

runAddRemoteSchema
  :: ( QErrM m
     , CacheRWM m
     , MonadTx m
     , MonadIO m
     , HasHttpManager m
     )
  => AddRemoteSchemaQuery -> m EncJSON
runAddRemoteSchema q = do
  addRemoteSchemaP1 name
  addRemoteSchemaP2 q
  buildSchemaCacheFor $ MORemoteSchema name
  pure successMsg
  where
    name = _arsqName q

addRemoteSchemaP1
  :: (QErrM m, CacheRM m)
  => RemoteSchemaName -> m ()
addRemoteSchemaP1 name = do
  remoteSchemaMap <- scRemoteSchemas <$> askSchemaCache
  onJust (Map.lookup name remoteSchemaMap) $ const $
    throw400 AlreadyExists $ "remote schema with name "
    <> name <<> " already exists"

addRemoteSchemaP2Setup
  :: (QErrM m, MonadIO m, HasHttpManager m)
  => AddRemoteSchemaQuery -> m RemoteSchemaCtx
addRemoteSchemaP2Setup (AddRemoteSchemaQuery name def _) = do
  httpMgr <- askHttpManager
  rsi <- validateRemoteSchemaDef def
  gCtx <- fetchRemoteSchema httpMgr name rsi
  pure $ RemoteSchemaCtx name gCtx rsi

addRemoteSchemaP2 :: (MonadTx m, MonadIO m, HasHttpManager m) => AddRemoteSchemaQuery -> m ()
addRemoteSchemaP2 q = do
  void $ addRemoteSchemaP2Setup q
  liftTx $ addRemoteSchemaToCatalog q

runRemoveRemoteSchema
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m)
  => RemoteSchemaNameQuery -> m EncJSON
runRemoveRemoteSchema (RemoteSchemaNameQuery rsn) = do
  removeRemoteSchemaP1 rsn
  liftTx $ removeRemoteSchemaFromCatalog rsn
  withNewInconsistentObjsCheck buildSchemaCache
  pure successMsg

removeRemoteSchemaP1
  :: (UserInfoM m, QErrM m, CacheRM m)
  => RemoteSchemaName -> m ()
removeRemoteSchemaP1 rsn = do
  sc <- askSchemaCache
  let rmSchemas = scRemoteSchemas sc
  void $ onNothing (Map.lookup rsn rmSchemas) $
    throw400 NotExists "no such remote schema"

runReloadRemoteSchema
  :: (QErrM m, CacheRWM m)
  => RemoteSchemaNameQuery -> m EncJSON
runReloadRemoteSchema (RemoteSchemaNameQuery name) = do
  rmSchemas <- scRemoteSchemas <$> askSchemaCache
  void $ onNothing (Map.lookup name rmSchemas) $
    throw400 NotExists $ "remote schema with name " <> name <<> " does not exist"

  invalidateCachedRemoteSchema name
  withNewInconsistentObjsCheck buildSchemaCache
  pure successMsg

addRemoteSchemaToCatalog
  :: AddRemoteSchemaQuery
  -> Q.TxE QErr ()
addRemoteSchemaToCatalog (AddRemoteSchemaQuery name def comment) =
  Q.unitQE defaultTxErrorHandler [Q.sql|
    INSERT into hdb_catalog.remote_schemas
      (name, definition, comment)
      VALUES ($1, $2, $3)
  |] (name, Q.AltJ $ J.toJSON def, comment) True

removeRemoteSchemaFromCatalog :: RemoteSchemaName -> Q.TxE QErr ()
removeRemoteSchemaFromCatalog name =
  Q.unitQE defaultTxErrorHandler [Q.sql|
    DELETE FROM hdb_catalog.remote_schemas
      WHERE name = $1
  |] (Identity name) True
