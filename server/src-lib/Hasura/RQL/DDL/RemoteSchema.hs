module Hasura.RQL.DDL.RemoteSchema
  ( runAddRemoteSchema
  , runRemoveRemoteSchema
  , removeRemoteSchemaFromCatalog
  , runReloadRemoteSchema
  , buildGCtxMap
  , fetchRemoteSchemas
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

import qualified Hasura.GraphQL.Schema       as GS

runAddRemoteSchema
  :: ( QErrM m, UserInfoM m
     , CacheRWM m, MonadTx m
     , MonadIO m, HasHttpManager m
     )
  => AddRemoteSchemaQuery -> m EncJSON
runAddRemoteSchema q = do
  addRemoteSchemaP1 name >> addRemoteSchemaP2 q
  where
    name = _arsqName q

addRemoteSchemaP1
  :: (QErrM m, UserInfoM m, CacheRM m)
  => RemoteSchemaName -> m ()
addRemoteSchemaP1 name = do
  adminOnly
  remoteSchemaMap <- scRemoteSchemas <$> askSchemaCache
  onJust (Map.lookup name remoteSchemaMap) $ const $
    throw400 AlreadyExists $ "remote schema with name "
    <> name <<> " already exists"

addRemoteSchemaP2Setup
  :: (QErrM m, CacheRWM m, MonadIO m, HasHttpManager m)
  => AddRemoteSchemaQuery -> m RemoteSchemaCtx
addRemoteSchemaP2Setup q = do
  httpMgr <- askHttpManager
  rsi <- validateRemoteSchemaDef def
  gCtx <- fetchRemoteSchema httpMgr name rsi
  let rsCtx = RemoteSchemaCtx name gCtx rsi
  addRemoteSchemaToCache rsCtx
  return rsCtx
  where
    AddRemoteSchemaQuery name def _ = q

addRemoteSchemaP2
  :: ( QErrM m
     , CacheRWM m
     , MonadTx m
     , MonadIO m, HasHttpManager m
     )
  => AddRemoteSchemaQuery
  -> m EncJSON
addRemoteSchemaP2 q = do
  void $ addRemoteSchemaP2Setup q
  liftTx $ addRemoteSchemaToCatalog q
  return successMsg

runRemoveRemoteSchema
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m)
  => RemoteSchemaNameQuery -> m EncJSON
runRemoveRemoteSchema (RemoteSchemaNameQuery rsn)= do
  removeRemoteSchemaP1 rsn
  removeRemoteSchemaP2 rsn

removeRemoteSchemaP1
  :: (UserInfoM m, QErrM m, CacheRM m)
  => RemoteSchemaName -> m ()
removeRemoteSchemaP1 rsn = do
  adminOnly
  sc <- askSchemaCache
  let rmSchemas = scRemoteSchemas sc
  void $ onNothing (Map.lookup rsn rmSchemas) $
    throw400 NotExists "no such remote schema"

removeRemoteSchemaP2
  :: ( CacheRWM m
     , MonadTx m
     )
  => RemoteSchemaName
  -> m EncJSON
removeRemoteSchemaP2 rsn = do
  delRemoteSchemaFromCache rsn
  liftTx $ removeRemoteSchemaFromCatalog rsn
  return successMsg

runReloadRemoteSchema
  :: ( QErrM m, UserInfoM m , CacheRWM m
     , MonadIO m, HasHttpManager m
     )
  => RemoteSchemaNameQuery -> m EncJSON
runReloadRemoteSchema (RemoteSchemaNameQuery name) = do
  adminOnly
  rmSchemas <- scRemoteSchemas <$> askSchemaCache
  rsi <- fmap rscInfo $ onNothing (Map.lookup name rmSchemas) $
         throw400 NotExists $ "remote schema with name "
         <> name <<> " does not exist"
  httpMgr <- askHttpManager
  gCtx <- fetchRemoteSchema httpMgr name rsi
  delRemoteSchemaFromCache name
  addRemoteSchemaToCache $ RemoteSchemaCtx name gCtx rsi
  return successMsg

-- | build GraphQL schema
buildGCtxMap
  :: (QErrM m, CacheRWM m) => m ()
buildGCtxMap = do
  -- build GraphQL Context with Hasura schema
  GS.buildGCtxMapPG
  sc <- askSchemaCache
  let gCtxMap = scGCtxMap sc
  -- Stitch remote schemas
  (mergedGCtxMap, defGCtx) <- mergeSchemas (scRemoteSchemas sc) gCtxMap
  writeSchemaCache sc { scGCtxMap = mergedGCtxMap
                      , scDefaultRemoteGCtx = defGCtx
                      }

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


fetchRemoteSchemas :: Q.TxE QErr [AddRemoteSchemaQuery]
fetchRemoteSchemas =
  map fromRow <$> Q.listQE defaultTxErrorHandler
    [Q.sql|
     SELECT name, definition, comment
       FROM hdb_catalog.remote_schemas
     |] () True
  where
    fromRow (n, Q.AltJ def, comm) = AddRemoteSchemaQuery n def comm
