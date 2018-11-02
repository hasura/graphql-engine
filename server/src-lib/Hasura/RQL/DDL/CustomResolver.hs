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
import qualified Data.Text                     as T
import qualified Database.PG.Query             as Q
import qualified Network.URI.Extended          as N

import           Hasura.GraphQL.RemoteResolver
import           Hasura.RQL.DDL.Headers        (HeaderConf (..))
import           Hasura.RQL.Types

import qualified Hasura.GraphQL.Schema         as GS

type UrlFromEnv = Text

data CustomResolverDef
  = CustomResolverDef
  { _crName    :: !Text
  , _crUrl     :: !(Either N.URI UrlFromEnv)
  , _crHeaders :: ![HeaderConf]
  } deriving (Show, Eq, Lift)

instance J.FromJSON CustomResolverDef where
  parseJSON = J.withObject "CustomResolverDef" $ \o -> do
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

    return $ CustomResolverDef name eUrlVal headers

instance J.ToJSON CustomResolverDef where
  toJSON (CustomResolverDef name eUrlVal headers) =
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


data AddCustomResolverQuery
  = AddCustomResolverQuery
  { _acrqUrl     :: !(Either N.URI UrlFromEnv)
  , _acrqHeaders :: !(Maybe [HeaderConf])
  , _acrqName    :: !Text
  } deriving (Show, Eq, Lift)


instance J.FromJSON AddCustomResolverQuery where
  parseJSON = J.withObject "AddCustomResolverQuery" $ \o -> do
    mUrl        <- o J..:? "url"
    mUrlFromEnv <- o J..:? "url_from_env"
    headers     <- o J..:? "headers"
    name        <- o J..: "name"
    when (T.null name) $ fail "name cannot be empty string"

    eUrlVal <- case (mUrl, mUrlFromEnv) of
      (Just url, Nothing)    -> return $ Left url
      (Nothing, Just urlEnv) -> return $ Right urlEnv
      (Nothing, Nothing)     ->
        fail "both `url` and `url_from_env` can't be empty"
      (Just _, Just _)       ->
        fail "both `url` and `url_from_env` can't be present"

    return $ AddCustomResolverQuery eUrlVal headers name

instance J.ToJSON AddCustomResolverQuery where
  toJSON (AddCustomResolverQuery eUrlVal headers name) =
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


instance HDBQuery AddCustomResolverQuery where
  type Phase1Res AddCustomResolverQuery = AddCustomResolverQuery
  phaseOne   = addCustomResolverP1
  phaseTwo _ = addCustomResolverP2 True
  schemaCachePolicy = SCPReload

addCustomResolverP1
  :: (P1C m)
  => AddCustomResolverQuery -> m AddCustomResolverQuery
addCustomResolverP1 q = adminOnly >> return q

addCustomResolverP2
  :: ( QErrM m
     , CacheRWM m
     , MonadTx m
     , MonadIO m
     , HasHttpManager m
     , HasTypeMap m
     )
  => Bool
  -> AddCustomResolverQuery
  -> m BL.ByteString
addCustomResolverP2 checkConflict (AddCustomResolverQuery eUrlVal headers name) = do
  url <- either return getUrlFromEnv eUrlVal
  manager <- askHttpManager
  typeMap <- askTypeMap
  remoteGCtx <- fetchRemoteSchema manager url $ fromMaybe [] headers
  when checkConflict $
    GS.checkConflictingNodes typeMap remoteGCtx
  liftTx $ addCustomResolverToCatalog eUrlVal headers name
  addCustomResolverToCache url $ fromMaybe [] headers
  return successMsg

addCustomResolverToCache :: CacheRWM m => N.URI -> [HeaderConf] -> m ()
addCustomResolverToCache url hdrs = do
  sc <- askSchemaCache
  let resolvers = scRemoteResolvers sc
  writeSchemaCache sc { scRemoteResolvers = resolvers ++ [(url, hdrs)] }


data DeleteCustomResolverQuery
  = DeleteCustomResolverQuery
  { _dcrqName    :: !Text
  } deriving (Show, Eq, Lift)


instance HDBQuery DeleteCustomResolverQuery where
  type Phase1Res DeleteCustomResolverQuery = DeleteCustomResolverQuery
  phaseOne   = deleteCustomResolverP1
  phaseTwo _ = deleteCustomResolverP2
  schemaCachePolicy = SCPReload

deleteCustomResolverP1
  :: (P1C m)
  => DeleteCustomResolverQuery -> m DeleteCustomResolverQuery
deleteCustomResolverP1 q = adminOnly >> return q

deleteCustomResolverP2
  :: ( QErrM m
     , CacheRWM m
     , MonadTx m
     , MonadIO m
     , HasHttpManager m
     , HasTypeMap m
     )
  => DeleteCustomResolverQuery
  -> m BL.ByteString
deleteCustomResolverP2 (DeleteCustomResolverQuery name) = do
  mUrl <- liftTx $ fetchRemoteResolverUrl name
  url  <- liftMaybe (err400 NotExists "no such custom resolver") mUrl
  deleteCustomResolverFromCache url
  liftTx $ deleteCustomResolverFromCatalog name
  return successMsg

deleteCustomResolverFromCache
  :: CacheRWM m => Text -> m ()
deleteCustomResolverFromCache url = do
  sc <- askSchemaCache
  let resolvers = scRemoteResolvers sc
      newResolvers = filter (\(u, _) -> T.pack (show u) /= url) resolvers
  writeSchemaCache sc { scRemoteResolvers = newResolvers }


addCustomResolverToCatalog
  :: Either N.URI UrlFromEnv
  -> Maybe [HeaderConf]
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
  |] (name, url, urlFromEnv, Q.AltJ $ J.toJSON headers') True

  where headers' = fromMaybe [] headers


deleteCustomResolverFromCatalog :: Text -> Q.TxE QErr ()
deleteCustomResolverFromCatalog name =
  Q.unitQE defaultTxErrorHandler [Q.sql|
    DELETE FROM hdb_catalog.custom_resolver
      WHERE name = $1
  |] (Identity name) True


fetchRemoteResolverUrl :: Text -> Q.TxE QErr (Maybe Text)
fetchRemoteResolverUrl name =
  fromRow . map runIdentity <$> Q.withQE defaultTxErrorHandler
    [Q.sql|
     SELECT url from hdb_catalog.custom_resolver
       WHERE name = $1
     |] (Identity name) True
  where
    fromRow []    = Nothing
    fromRow (x:_) = Just x

fetchRemoteResolvers :: Q.TxE QErr [CustomResolverDef]
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
          return $ CustomResolverDef name (Left url') hdrs
        (Nothing, Just u)  -> return $ CustomResolverDef name (Right u) hdrs
        (Just _, Just u2)  -> return $ CustomResolverDef name (Right u2) hdrs
        (Nothing, Nothing) ->
          throw500 "both url, url_from_env cannot be empty"

    mkUrl u = maybe (throw500 "not a valid URI in DB") return $
              N.parseURI $ T.unpack u


$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''DeleteCustomResolverQuery)
