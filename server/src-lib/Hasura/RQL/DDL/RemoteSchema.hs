{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Hasura.RQL.DDL.RemoteSchema where

import           Hasura.Prelude
import           Language.Haskell.TH.Syntax  (Lift)

import qualified Data.Aeson                  as J
import qualified Data.Aeson.Casing           as J
import qualified Data.Aeson.TH               as J
import qualified Data.ByteString.Lazy        as BL
import qualified Data.HashMap.Strict         as Map
import qualified Database.PG.Query           as Q
import qualified Network.URI.Extended        as N

import           Hasura.GraphQL.RemoteServer
import           Hasura.RQL.DDL.Headers      (HeaderConf (..))
import           Hasura.RQL.Types

import qualified Hasura.GraphQL.Schema       as GS

type UrlFromEnv = Text

data RemoteSchemaDef
  = RemoteSchemaDef
  { _rsName    :: !Text
  , _rsUrl     :: !(Either N.URI UrlFromEnv)
  , _rsHeaders :: ![HeaderConf]
  } deriving (Show, Eq, Lift)

instance J.FromJSON RemoteSchemaDef where
  parseJSON = J.withObject "RemoteSchemaDef" $ \o -> do
    mUrl        <- o J..:? "url"
    mUrlFromEnv <- o J..:? "url_from_env"
    headers     <- o J..: "headers"
    name        <- o J..: "name"

    eUrlVal <- case (mUrl, mUrlFromEnv) of
      (Just url, Nothing)    -> return $ Left url
      (Nothing, Just urlEnv) -> return $ Right urlEnv
      (Nothing, Nothing)     ->
        fail "both `url` and `url_from_env` can't be empty"
      (Just _, Just _)       ->
        fail "both `url` and `url_from_env` can't be present"

    return $ RemoteSchemaDef name eUrlVal headers

instance J.ToJSON RemoteSchemaDef where
  toJSON (RemoteSchemaDef name eUrlVal headers) =
    case eUrlVal of
      Left url ->
        J.object [ "url" J..= url
                 , "headers" J..= headers
                 , "name" J..= name ]
      Right urlFromEnv ->
        J.object [ "url_from_env" J..= urlFromEnv
                 , "headers" J..= headers
                 , "name" J..= name
                 ]


data AddRemoteSchemaQuery
  = AddRemoteSchemaQuery
  { _arsqUrl        :: !(Maybe N.URI)
  , _arsqUrlFromEnv :: !(Maybe Text)
  , _arsqHeaders    :: !(Maybe [HeaderConf])
  , _arsqName       :: !Text
  } deriving (Show, Eq, Lift)


instance HDBQuery AddRemoteSchemaQuery where
  type Phase1Res AddRemoteSchemaQuery = RemoteSchemaDef
  phaseOne   = addRemoteSchemaP1
  phaseTwo _ = addRemoteSchemaP2 True
  schemaCachePolicy = SCPReload

addRemoteSchemaP1
  :: (P1C m)
  => AddRemoteSchemaQuery -> m RemoteSchemaDef
addRemoteSchemaP1 (AddRemoteSchemaQuery url urlEnv hdrs name) = do
  adminOnly
  eUrlEnv <- case (url, urlEnv) of
    (Just u, Nothing)  -> return $ Left u
    (Nothing, Just ue) -> return $ Right ue
    (Nothing, Nothing) ->
      throw400 InvalidParams "both `url` and `url_from_env` can't be empty"
    (Just _, Just _)   ->
      throw400 InvalidParams "both `url` and `url_from_env` can't be present"

  let hdrs' = fromMaybe [] hdrs
  return $ RemoteSchemaDef name eUrlEnv hdrs'

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
addRemoteSchemaP2 checkConflict def@(RemoteSchemaDef name eUrlVal headers) = do
  url <- either return getUrlFromEnv eUrlVal
  manager <- askHttpManager
  sc <- askSchemaCache
  let gCtxMap = scGCtxMap sc
      defRemoteGCtx = scDefaultRemoteGCtx sc
  remoteGCtx <- fetchRemoteSchema manager url headers
  when checkConflict $
    forM_ (Map.toList gCtxMap) $ \(_, gCtx) ->
      GS.checkConflictingNodes gCtx remoteGCtx
  newGCtxMap <- mergeRemoteSchema gCtxMap (url, remoteGCtx)
  defGCtx <- mergeGCtx defRemoteGCtx remoteGCtx
  liftTx $ addRemoteSchemaToCatalog name def
  addRemoteSchemaToCache newGCtxMap defGCtx url headers
  return successMsg

addRemoteSchemaToCache
  :: CacheRWM m
  => GS.GCtxMap -> GS.GCtx -> N.URI -> [HeaderConf] -> m ()
addRemoteSchemaToCache gCtxMap defGCtx url hdrs = do
  sc <- askSchemaCache
  let resolvers = scRemoteResolvers sc
  writeSchemaCache sc { scRemoteResolvers = resolvers ++ [(url, hdrs)]
                      , scGCtxMap = gCtxMap
                      , scDefaultRemoteGCtx = defGCtx
                      }

writeRemoteSchemasToCache
  :: CacheRWM m
  => GS.GCtxMap -> [(N.URI, [HeaderConf])] -> m ()
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

data RemoveRemoteSchemaQuery
  = RemoveRemoteSchemaQuery
  { _rrsqName    :: !Text
  } deriving (Show, Eq, Lift)


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
  (RemoteSchemaDef _ eUrlVal _) <-
    liftMaybe (err400 NotExists "no such remote schema") mSchema
  url <- either return getUrlFromEnv eUrlVal

  hMgr <- askHttpManager
  sc <- askSchemaCache
  let resolvers = scRemoteResolvers sc
      newResolvers = filter (\(u, _) -> u /= url) resolvers

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
      newResolvers = filter (\(u, _) -> u /= url) resolvers
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


$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''AddRemoteSchemaQuery)
$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''RemoveRemoteSchemaQuery)
