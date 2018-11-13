{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.RQL.DDL.RemoteSchema where

import           Hasura.Prelude

import qualified Data.Aeson                  as J
import qualified Data.ByteString.Lazy        as BL
import qualified Data.HashMap.Strict         as Map
import qualified Database.PG.Query           as Q
import qualified Network.URI.Extended        as N

import           Hasura.GraphQL.RemoteServer
import           Hasura.RQL.Types

import qualified Hasura.GraphQL.Schema       as GS


instance HDBQuery AddRemoteSchemaQuery where
  type Phase1Res AddRemoteSchemaQuery = RemoteSchemaDef
  phaseOne   = addRemoteSchemaP1
  phaseTwo _ = addRemoteSchemaP2 True
  schemaCachePolicy = SCPReload

addRemoteSchemaP1
  :: (P1C m)
  => AddRemoteSchemaQuery -> m RemoteSchemaDef
addRemoteSchemaP1 (AddRemoteSchemaQuery url urlEnv hdrs name fwdHdrs) = do
  adminOnly
  eUrlEnv <- case (url, urlEnv) of
    (Just u, Nothing)  -> return $ Left u
    (Nothing, Just ue) -> return $ Right ue
    (Nothing, Nothing) ->
      throw400 InvalidParams "both `url` and `url_from_env` can't be empty"
    (Just _, Just _)   ->
      throw400 InvalidParams "both `url` and `url_from_env` can't be present"

  let hdrs' = fromMaybe [] hdrs
  return $ RemoteSchemaDef name eUrlEnv hdrs' fwdHdrs

addRemoteSchemaP2
  :: ( QErrM m
     , CacheRWM m
     , MonadTx m
     , MonadIO m
     , HasHttpManager m
     )
  => Bool
  -> RemoteSchemaDef
  -> m BL.ByteString
addRemoteSchemaP2 checkConflict def@(RemoteSchemaDef name eUrlVal headers _) = do
  url <- either return getUrlFromEnv eUrlVal
  manager <- askHttpManager
  sc <- askSchemaCache
  let gCtxMap = scGCtxMap sc
      defRemoteGCtx = scDefaultRemoteGCtx sc
  remoteGCtx <- fetchRemoteSchema manager url headers
  when checkConflict $
    forM_ (Map.toList gCtxMap) $ \(_, gCtx) ->
      GS.checkConflictingNodes gCtx remoteGCtx
  newGCtxMap <- mergeRemoteSchema gCtxMap remoteGCtx
  defGCtx <- mergeGCtx defRemoteGCtx remoteGCtx
  liftTx $ addRemoteSchemaToCatalog name def
  addRemoteSchemaToCache newGCtxMap defGCtx url def
  return successMsg

addRemoteSchemaToCache
  :: CacheRWM m
  => GS.GCtxMap -> GS.GCtx -> N.URI -> RemoteSchemaDef -> m ()
addRemoteSchemaToCache gCtxMap defGCtx url rmDef = do
  sc <- askSchemaCache
  let resolvers = scRemoteResolvers sc
  writeSchemaCache sc { scRemoteResolvers = Map.insert url rmDef resolvers
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


instance HDBQuery RemoveRemoteSchemaQuery where
  type Phase1Res RemoveRemoteSchemaQuery = RemoveRemoteSchemaQuery
  phaseOne   = removeRemoteSchemaP1
  phaseTwo _ = removeRemoteSchemaP2
  schemaCachePolicy = SCPReload

removeRemoteSchemaP1
  :: (P1C m)
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
  (RemoteSchemaDef _ eUrlVal _ _) <-
    liftMaybe (err400 NotExists "no such remote schema") mSchema
  url <- either return getUrlFromEnv eUrlVal

  hMgr <- askHttpManager
  sc <- askSchemaCache
  let resolvers = scRemoteResolvers sc
      newResolvers = Map.filterWithKey (\u _ -> u /= url) resolvers

  newGCtxMap <- GS.mkGCtxMap (scTables sc)
  (mergedGCtxMap, defGCtx) <- mergeSchemas newResolvers newGCtxMap hMgr
  removeRemoteSchemaFromCache url mergedGCtxMap defGCtx
  liftTx $ removeRemoteSchemaFromCatalog name
  return successMsg

removeRemoteSchemaFromCache
  :: CacheRWM m => N.URI -> GS.GCtxMap -> GS.GCtx -> m ()
removeRemoteSchemaFromCache url gCtxMap defGCtx = do
  sc <- askSchemaCache
  let resolvers = scRemoteResolvers sc
      newResolvers = Map.filterWithKey (\u _ -> u /= url) resolvers
  writeSchemaCache sc { scRemoteResolvers = newResolvers
                      , scGCtxMap = gCtxMap
                      , scDefaultRemoteGCtx = defGCtx
                      }

addRemoteSchemaToCatalog
  :: Text
  -> RemoteSchemaDef
  -> Q.TxE QErr ()
addRemoteSchemaToCatalog name def =
  Q.unitQE defaultTxErrorHandler [Q.sql|
    INSERT into hdb_catalog.remote_schemas
      (name, definition)
      VALUES ($1, $2)
  |] (name, Q.AltJ $ J.toJSON def) True


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

fetchRemoteSchemas :: Q.TxE QErr [RemoteSchemaDef]
fetchRemoteSchemas =
  map fromRow <$> Q.listQE defaultTxErrorHandler
    [Q.sql|
     SELECT name, definition
       FROM hdb_catalog.remote_schemas
     |] () True
  where
    fromRow :: (Text, Q.AltJ RemoteSchemaDef) -> RemoteSchemaDef
    fromRow (_, Q.AltJ def) = def
