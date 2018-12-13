module Hasura.RQL.DDL.RemoteSchema
  ( runAddRemoteSchema
  , runRemoveRemoteSchema
  , writeRemoteSchemasToCache
  , refreshGCtxMapInSchema
  , fetchRemoteSchemas
  , addRemoteSchemaP2
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                  as J
import qualified Data.ByteString.Lazy        as BL
import qualified Data.HashMap.Strict         as Map
import qualified Database.PG.Query           as Q

import           Hasura.GraphQL.RemoteServer
import           Hasura.RQL.Types

import qualified Hasura.GraphQL.Schema       as GS

runAddRemoteSchema
  :: ( QErrM m, UserInfoM m, CacheRWM m, MonadTx m
     , MonadIO m
     , HasHttpManager m
     )
  => AddRemoteSchemaQuery -> m RespBody
runAddRemoteSchema q = do
  adminOnly
  addRemoteSchemaP2 q

addRemoteSchemaP2
  :: ( QErrM m
     , CacheRWM m
     , MonadTx m
     , MonadIO m
     , HasHttpManager m
     )
  => AddRemoteSchemaQuery
  -> m BL.ByteString
addRemoteSchemaP2 q@(AddRemoteSchemaQuery name def _) = do
  rsi <- validateRemoteSchemaDef def
  manager <- askHttpManager
  sc <- askSchemaCache
  let defRemoteGCtx = scDefaultRemoteGCtx sc
  remoteGCtx <- fetchRemoteSchema manager name rsi
  newDefGCtx <- mergeGCtx defRemoteGCtx $ convRemoteGCtx remoteGCtx
  newHsraGCtxMap <- GS.mkGCtxMap (scTables sc)
  newGCtxMap <- mergeRemoteSchema newHsraGCtxMap newDefGCtx
  liftTx $ addRemoteSchemaToCatalog q
  addRemoteSchemaToCache newGCtxMap newDefGCtx name rsi
  return successMsg

addRemoteSchemaToCache
  :: CacheRWM m
  => GS.GCtxMap
  -> GS.GCtx
  -> RemoteSchemaName
  -> RemoteSchemaInfo
  -> m ()
addRemoteSchemaToCache gCtxMap defGCtx name rmDef = do
  sc <- askSchemaCache
  let resolvers = scRemoteResolvers sc
  writeSchemaCache sc { scRemoteResolvers = Map.insert name rmDef resolvers
                      , scGCtxMap = gCtxMap
                      , scDefaultRemoteGCtx = defGCtx
                      }

writeRemoteSchemasToCache
  :: CacheRWM m
  => GS.GCtxMap -> RemoteSchemaMap -> m ()
writeRemoteSchemasToCache gCtxMap resolvers = do
  sc <- askSchemaCache
  writeSchemaCache sc { scRemoteResolvers = resolvers
                      , scGCtxMap = gCtxMap
                      }

refreshGCtxMapInSchema
  :: (CacheRWM m, MonadIO m, MonadError QErr m, HasHttpManager m)
  => m ()
refreshGCtxMapInSchema = do
  sc <- askSchemaCache
  gCtxMap <- GS.mkGCtxMap (scTables sc)
  httpMgr <- askHttpManager
  (mergedGCtxMap, defGCtx) <-
    mergeSchemas (scRemoteResolvers sc) gCtxMap httpMgr
  writeSchemaCache sc { scGCtxMap = mergedGCtxMap
                      , scDefaultRemoteGCtx = defGCtx }

runRemoveRemoteSchema
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m, MonadIO m, HasHttpManager m)
  => RemoveRemoteSchemaQuery -> m RespBody
runRemoveRemoteSchema q =
  removeRemoteSchemaP1 q >>= removeRemoteSchemaP2

removeRemoteSchemaP1
  :: (UserInfoM m, QErrM m)
  => RemoveRemoteSchemaQuery -> m RemoveRemoteSchemaQuery
removeRemoteSchemaP1 q = adminOnly >> return q

removeRemoteSchemaP2
  :: ( QErrM m
     , CacheRWM m
     , MonadTx m
     , MonadIO m
     , HasHttpManager m
     )
  => RemoveRemoteSchemaQuery
  -> m BL.ByteString
removeRemoteSchemaP2 (RemoveRemoteSchemaQuery name) = do
  mSchema <- liftTx $ fetchRemoteSchemaDef name
  _ <- liftMaybe (err400 NotExists "no such remote schema") mSchema
  --url <- either return getUrlFromEnv eUrlVal

  hMgr <- askHttpManager
  sc <- askSchemaCache
  let resolvers = scRemoteResolvers sc
      newResolvers = Map.filterWithKey (\n _ -> n /= name) resolvers

  newGCtxMap <- GS.mkGCtxMap (scTables sc)
  (mergedGCtxMap, defGCtx) <- mergeSchemas newResolvers newGCtxMap hMgr
  removeRemoteSchemaFromCache newResolvers mergedGCtxMap defGCtx
  liftTx $ removeRemoteSchemaFromCatalog name
  return successMsg

removeRemoteSchemaFromCache
  :: CacheRWM m => RemoteSchemaMap -> GS.GCtxMap -> GS.GCtx -> m ()
removeRemoteSchemaFromCache newResolvers gCtxMap defGCtx = do
  sc <- askSchemaCache
  writeSchemaCache sc { scRemoteResolvers = newResolvers
                      , scGCtxMap = gCtxMap
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


removeRemoteSchemaFromCatalog :: Text -> Q.TxE QErr ()
removeRemoteSchemaFromCatalog name =
  Q.unitQE defaultTxErrorHandler [Q.sql|
    DELETE FROM hdb_catalog.remote_schemas
      WHERE name = $1
  |] (Identity name) True


fetchRemoteSchemaDef :: Text -> Q.TxE QErr (Maybe RemoteSchemaDef)
fetchRemoteSchemaDef name =
  fmap (fromRow . runIdentity) <$> Q.withQE defaultTxErrorHandler
    [Q.sql|
     SELECT definition from hdb_catalog.remote_schemas
       WHERE name = $1
     |] (Identity name) True
  where
    fromRow (Q.AltJ def) = def

fetchRemoteSchemas :: Q.TxE QErr [AddRemoteSchemaQuery]
fetchRemoteSchemas =
  map fromRow <$> Q.listQE defaultTxErrorHandler
    [Q.sql|
     SELECT name, definition, comment
       FROM hdb_catalog.remote_schemas
     |] () True
  where
    fromRow (n, Q.AltJ def, comm) = AddRemoteSchemaQuery n def comm
