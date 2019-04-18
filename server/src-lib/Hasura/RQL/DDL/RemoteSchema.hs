module Hasura.RQL.DDL.RemoteSchema
  ( runAddRemoteSchema
  , addRemoteSchemaToCache
  , resolveRemoteSchemas
  , runRemoveRemoteSchema
  , removeRemoteSchemaFromCache
  , removeRemoteSchemaFromCatalog
  , refreshGCtxMapInSchema
  , fetchRemoteSchemas
  , addRemoteSchemaP1
  , addRemoteSchemaP2
  ) where

import           Hasura.EncJSON
import           Hasura.Prelude

import qualified Data.Aeson                  as J
import qualified Data.HashMap.Strict         as Map
import qualified Database.PG.Query           as Q
import qualified Network.HTTP.Client         as HTTP

import           Hasura.GraphQL.RemoteServer
import           Hasura.RQL.Types

import qualified Hasura.GraphQL.Schema       as GS

runAddRemoteSchema
  :: ( QErrM m, UserInfoM m
     , CacheRWM m, MonadTx m
     , MonadIO m, HasHttpManager m
     )
  => AddRemoteSchemaQuery -> m EncJSON
runAddRemoteSchema q = do
  addRemoteSchemaP1 q >>= addRemoteSchemaP2 q

addRemoteSchemaP1
  :: ( QErrM m, UserInfoM m
     , MonadIO m, HasHttpManager m
     )
  => AddRemoteSchemaQuery -> m RemoteSchemaInfo
addRemoteSchemaP1 q = do
  adminOnly
  httpMgr <- askHttpManager
  rsi <- validateRemoteSchemaDef def
  -- TODO:- Maintain a cache of remote schema with it's GCtx
  void $ fetchRemoteSchema httpMgr name rsi
  return rsi
  where
    AddRemoteSchemaQuery name def _ = q

addRemoteSchemaP2
  :: ( QErrM m
     , CacheRWM m
     , MonadTx m
     )
  => AddRemoteSchemaQuery
  -> RemoteSchemaInfo
  -> m EncJSON
addRemoteSchemaP2 q rsi = do
  addRemoteSchemaToCache name rsi
  liftTx $ addRemoteSchemaToCatalog q
  return successMsg
  where
    name = _arsqName q

addRemoteSchemaToCache
  :: CacheRWM m
  => RemoteSchemaName
  -> RemoteSchemaInfo
  -> m ()
addRemoteSchemaToCache name rmDef = do
  sc <- askSchemaCache
  let resolvers = scRemoteResolvers sc
  writeSchemaCache sc
    {scRemoteResolvers = Map.insert name rmDef resolvers}

refreshGCtxMapInSchema
  :: (CacheRWM m, MonadIO m, MonadError QErr m, HasHttpManager m)
  => m ()
refreshGCtxMapInSchema = do
  sc <- askSchemaCache
  gCtxMap <- GS.mkGCtxMap (scTables sc) (scFunctions sc)
  httpMgr <- askHttpManager
  (mergedGCtxMap, defGCtx) <-
    mergeSchemas (scRemoteResolvers sc) gCtxMap httpMgr
  writeSchemaCache sc { scGCtxMap = mergedGCtxMap
                      , scDefaultRemoteGCtx = defGCtx }

runRemoveRemoteSchema
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m)
  => RemoveRemoteSchemaQuery -> m EncJSON
runRemoveRemoteSchema (RemoveRemoteSchemaQuery rsn)= do
  removeRemoteSchemaP1 rsn
  removeRemoteSchemaP2 rsn

removeRemoteSchemaP1
  :: (UserInfoM m, QErrM m, CacheRM m)
  => RemoteSchemaName -> m ()
removeRemoteSchemaP1 rsn = do
  adminOnly
  sc <- askSchemaCache
  let resolvers = scRemoteResolvers sc
  case Map.lookup rsn resolvers of
    Just _  -> return ()
    Nothing -> throw400 NotExists "no such remote schema"

removeRemoteSchemaP2
  :: ( CacheRWM m
     , MonadTx m
     )
  => RemoteSchemaName
  -> m EncJSON
removeRemoteSchemaP2 rsn = do
  removeRemoteSchemaFromCache rsn
  liftTx $ removeRemoteSchemaFromCatalog rsn
  return successMsg

removeRemoteSchemaFromCache
  :: CacheRWM m => RemoteSchemaName -> m ()
removeRemoteSchemaFromCache rsn = do
  sc <- askSchemaCache
  let resolvers = scRemoteResolvers sc
  writeSchemaCache sc {scRemoteResolvers = Map.delete rsn resolvers}

resolveRemoteSchemas
  :: ( MonadError QErr m
     , MonadIO m
     )
  => SchemaCache -> HTTP.Manager -> m SchemaCache
resolveRemoteSchemas sc httpMgr = do
  (mergedGCtxMap, defGCtx) <-
    mergeSchemas (scRemoteResolvers sc) gCtxMap httpMgr
  return $ sc { scGCtxMap = mergedGCtxMap
              , scDefaultRemoteGCtx = defGCtx
              }
  where
    gCtxMap = scGCtxMap sc

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
