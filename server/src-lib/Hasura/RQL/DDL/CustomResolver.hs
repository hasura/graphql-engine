{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Hasura.RQL.DDL.CustomResolver where

import           Hasura.Prelude
import           Language.Haskell.TH.Syntax    (Lift)
--import           Control.Lens                  ((&), (.~), (?~), (^.))

import qualified Data.Aeson                    as J
import qualified Data.Aeson.Casing             as J
import qualified Data.Aeson.TH                 as J
import qualified Data.ByteString.Lazy          as BL
import qualified Data.HashMap.Strict           as Map
import qualified Data.Text                     as T
import qualified Database.PG.Query             as Q
import qualified Network.URI.Extended          as N

import           Hasura.GraphQL.RemoteResolver
import           Hasura.RQL.DDL.Headers        (HeaderConf (..))
import           Hasura.RQL.Types

import qualified Hasura.GraphQL.Schema         as GS

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
  phaseOne   = addCustomResolverP1
  phaseTwo _ = addCustomResolverP2 True
  schemaCachePolicy = SCPReload

addCustomResolverP1
  :: (P1C m)
  => AddRemoteSchemaQuery -> m RemoteSchemaDef
addCustomResolverP1 (AddRemoteSchemaQuery url urlEnv hdrs name) = do
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

addCustomResolverP2
  :: ( QErrM m
     , CacheRWM m
     , MonadTx m
     , MonadIO m
     , HasHttpManager m
     )
  => Bool
  -> RemoteSchemaDef
  -> m BL.ByteString
addCustomResolverP2 checkConflict (RemoteSchemaDef name eUrlVal headers) = do
  url <- either return getUrlFromEnv eUrlVal
  manager <- askHttpManager
  sc <- askSchemaCache
  let gCtxMap = scGCtxMap sc
  remoteGCtx <- fetchRemoteSchema manager url headers
  when checkConflict $
    forM_ (Map.toList gCtxMap) $ \(_, gCtx) ->
      GS.checkConflictingNodes gCtx remoteGCtx
  newGCtxMap <- mergeRemoteSchema gCtxMap (url, remoteGCtx)
  liftTx $ addCustomResolverToCatalog eUrlVal headers name
  addCustomResolverToCache newGCtxMap url headers
  return successMsg

addCustomResolverToCache
  :: CacheRWM m
  => GS.GCtxMap -> N.URI -> [HeaderConf] -> m ()
addCustomResolverToCache gCtxMap url hdrs = do
  sc <- askSchemaCache
  let resolvers = scRemoteResolvers sc
  writeSchemaCache sc { scRemoteResolvers = resolvers ++ [(url, hdrs)]
                      , scGCtxMap = gCtxMap
                      }

writeCustomResolversToCache
  :: CacheRWM m
  => GS.GCtxMap -> [(N.URI, [HeaderConf])] -> m ()
writeCustomResolversToCache gCtxMap resolvers = do
  sc <- askSchemaCache
  writeSchemaCache sc { scRemoteResolvers = resolvers
                      , scGCtxMap = gCtxMap
                      }

data RemoveRemoteSchemaQuery
  = RemoveRemoteSchemaQuery
  { _rrsqName    :: !Text
  } deriving (Show, Eq, Lift)


instance HDBQuery RemoveRemoteSchemaQuery where
  type Phase1Res RemoveRemoteSchemaQuery = RemoveRemoteSchemaQuery
  phaseOne   = deleteCustomResolverP1
  phaseTwo _ = deleteCustomResolverP2
  schemaCachePolicy = SCPReload

deleteCustomResolverP1
  :: (P1C m)
  => RemoveRemoteSchemaQuery -> m RemoveRemoteSchemaQuery
deleteCustomResolverP1 q = adminOnly >> return q

deleteCustomResolverP2
  :: ( QErrM m
     , CacheRWM m
     , MonadTx m
     , MonadIO m
     , HasHttpManager m
     )
  => RemoveRemoteSchemaQuery
  -> m BL.ByteString
deleteCustomResolverP2 (RemoveRemoteSchemaQuery name) = do
  mUrl <- liftTx $ fetchRemoteResolverUrl name
  eUrlVal <- liftMaybe (err400 NotExists "no such custom resolver") mUrl
  url <- either return getUrlFromEnv eUrlVal

  hMgr <- askHttpManager
  sc <- askSchemaCache
  let resolvers = scRemoteResolvers sc
      newResolvers = filter (\(u, _) -> u /= url) resolvers

  newGCtxMap <- GS.mkGCtxMap (scTables sc)
  mergedGCtxMap <- mergeSchemas newResolvers newGCtxMap hMgr
  deleteCustomResolverFromCache url mergedGCtxMap
  liftTx $ deleteCustomResolverFromCatalog name
  return successMsg

deleteCustomResolverFromCache
  :: CacheRWM m => N.URI -> GS.GCtxMap -> m ()
deleteCustomResolverFromCache url gCtxMap = do
  sc <- askSchemaCache
  let resolvers = scRemoteResolvers sc
      newResolvers = filter (\(u, _) -> u /= url) resolvers
  writeSchemaCache sc { scRemoteResolvers = newResolvers
                      , scGCtxMap = gCtxMap
                      }

addCustomResolverToCatalog
  :: Either N.URI UrlFromEnv
  -> [HeaderConf]
  -> Text
  -> Q.TxE QErr ()
addCustomResolverToCatalog eUrlV headers name = do
  let (url, urlFromEnv) = case eUrlV of
        Left u    -> (Just $ T.pack $ show u, Nothing)
        Right ufe -> (Nothing, Just ufe)

  Q.unitQE defaultTxErrorHandler [Q.sql|
    INSERT into hdb_catalog.custom_resolver
      (name, url, url_from_env, headers)
      VALUES ($1, $2, $3, $4)
  |] (name, url, urlFromEnv, Q.AltJ $ J.toJSON headers) True


deleteCustomResolverFromCatalog :: Text -> Q.TxE QErr ()
deleteCustomResolverFromCatalog name =
  Q.unitQE defaultTxErrorHandler [Q.sql|
    DELETE FROM hdb_catalog.custom_resolver
      WHERE name = $1
  |] (Identity name) True


fetchRemoteResolverUrl :: Text -> Q.TxE QErr (Maybe (Either N.URI Text))
fetchRemoteResolverUrl name = do
  res <- Q.listQE defaultTxErrorHandler
    [Q.sql|
     SELECT url, url_from_env from hdb_catalog.custom_resolver
       WHERE name = $1
     |] (Identity name) True
  fromRow res
  where
    fromRow []    = return Nothing
    fromRow (x:_) = do
      eRes <- mkEither x
      return $ Just eRes

    mkEither (u, ufe) = case (u, ufe) of
      (Just u', Nothing)   -> do
        let mUrl = N.parseURI $ T.unpack u'
        url <- maybe (throw500 "not a valid URI in DB") return mUrl
        return $ Left url
      (Nothing, Just ufe') -> return $ Right ufe'
      _                    -> throw500 "wrong URI info in db"

fetchRemoteResolvers :: Q.TxE QErr [RemoteSchemaDef]
fetchRemoteResolvers = do
  res <- Q.listQE defaultTxErrorHandler
    [Q.sql|
     SELECT name, url, url_from_env, headers
       FROM hdb_catalog.custom_resolver
     |] () True
  mapM fromRow res
  where
    -- FIXME: better way to do this
    -- FIXME: check for json null in headers
    fromRow (name, url, urlEnv, Q.AltJ hdrs) =
      case (url, urlEnv) of
        (Just u, Nothing)  -> do
          url' <- mkUrl u
          return $ RemoteSchemaDef name (Left url') hdrs
        (Nothing, Just u)  -> return $ RemoteSchemaDef name (Right u) hdrs
        (Just _, Just u2)  -> return $ RemoteSchemaDef name (Right u2) hdrs
        (Nothing, Nothing) ->
          throw500 "both url, url_from_env cannot be empty"

    mkUrl u = maybe (throw500 "not a valid URI in DB") return $
              N.parseURI $ T.unpack u



$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''AddRemoteSchemaQuery)
$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''RemoveRemoteSchemaQuery)
