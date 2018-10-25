{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}

module Hasura.GraphQL.RemoteResolver where

import           Control.Exception             (try)
import           Control.Lens                  ((&), (.~), (?~), (^.))
import           Hasura.Prelude
import           Language.Haskell.TH.Syntax    (Lift)
import           System.Environment            (lookupEnv)

import qualified Data.Aeson                    as J
import qualified Data.ByteString.Lazy          as BL
import qualified Data.CaseInsensitive          as CI
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Database.PG.Query             as Q
import qualified Language.GraphQL.Draft.JSON   ()
import qualified Language.GraphQL.Draft.Syntax as G
import qualified Network.HTTP.Client           as HTTP
import qualified Network.URI.Extended          as N
import qualified Network.Wreq                  as Wreq

import           Hasura.RQL.Types
--import           Hasura.RQL.Types.Error
import           Hasura.RQL.DDL.Subscribe      (getHeadersFromConf)
import           Hasura.RQL.Types.Subscribe    (HeaderConf (..))

import qualified Hasura.GraphQL.Schema         as GS
import qualified Hasura.GraphQL.Validate.Types as VT

type UrlFromEnv = Text

data AddCustomResolverQuery
  = AddCustomResolverQuery
  { _acrqUrl     :: !(Either N.URI UrlFromEnv)
  , _acrqHeaders :: !(Maybe [HeaderConf])
  } deriving (Show, Eq, Lift)


instance J.FromJSON AddCustomResolverQuery where
  parseJSON = J.withObject "AddCustomResolverQuery" $ \o -> do
    mUrl <- o J..:? "url"
    mUrlFromEnv <- o J..:? "url_from_env"
    headers <- o J..:? "headers"

    eUrlVal <- case (mUrl, mUrlFromEnv) of
      (Just url, Nothing)    -> return $ Left url
      (Nothing, Just urlEnv) -> return $ Right urlEnv
      (Nothing, Nothing)     ->
        fail "both `url` and `url_from_env` can't be empty"
      (Just _, Just _)       ->
        fail "both `url` and `url_from_env` can't be present"

    return $ AddCustomResolverQuery eUrlVal headers

instance J.ToJSON AddCustomResolverQuery where
  toJSON (AddCustomResolverQuery eUrlVal headers) =
    case eUrlVal of
      Left url -> J.object [ "url" J..= url, "headers" J..= headers ]
      Right urlFromEnv ->
        J.object [ "url_from_env" J..= urlFromEnv
                 , "headers" J..= headers ]

fetchRemoteSchema
  :: (MonadIO m, MonadError QErr m)
  => HTTP.Manager -> Text -> [HeaderConf] -> m GS.RemoteGCtx
fetchRemoteSchema manager url headerConf = do
  headers <- getHeadersFromConf headerConf
  let hdrs = map (\(hn, hv) -> (CI.mk . T.encodeUtf8 $ hn, T.encodeUtf8 hv)) headers
  let options = Wreq.defaults
              & Wreq.headers .~ ("content-type", "application/json") : hdrs
              & Wreq.checkResponse ?~ (\_ _ -> return ())
              & Wreq.manager .~ Right manager

  -- FIXME: read and cache introspection query at compile time
  iq   <- liftIO $ BL.readFile "ws/introspection.json"
  res  <- liftIO $ try $ Wreq.postWith options (T.unpack url) iq
  resp <- either throwHttpErr return res

  let respData = resp ^. Wreq.responseBody
      statusCode = resp ^. Wreq.responseStatus . Wreq.statusCode
  when (statusCode /= 200) $ schemaErr respData

  schemaDoc <- either schemaErr return $ J.eitherDecode respData
  --liftIO $ print $ map G.getNamedTyp $ G._sdTypes schemaDoc
  let etTypeInfos = mapM VT.fromTyDef $ G._sdTypes schemaDoc
  typeInfos <- either schemaErr return etTypeInfos
  --liftIO $ print $ map VT.getNamedTy typeInfos
  let typMap = VT.mkTyInfoMap typeInfos
      (qRoot, mRoot, sRoot) = getRoots schemaDoc
  return $ GS.RemoteGCtx typMap qRoot (Just mRoot) sRoot

  where
    getRoots sc = (G._sdQueryRoot sc, G._sdMutationRoot sc, G._sdSubscriptionRoot sc)
    schemaErr err = throw400 RemoteSchemaError (T.pack $ show err)

    throwHttpErr :: (MonadError QErr m) => HTTP.HttpException -> m a
    throwHttpErr = schemaErr

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
  => AddCustomResolverQuery
  -> m BL.ByteString
addCustomResolverP2 (AddCustomResolverQuery eUrlVal headers) = do
  url <- either (return . T.pack . show) getUrlFromEnv eUrlVal
  manager <- askHttpManager
  typeMap <- askTypeMap
  remoteGCtx <- fetchRemoteSchema manager url $ fromMaybe [] headers
  GS.checkConflictingNodes remoteGCtx typeMap
  liftTx $ addCustomResolverToCatalog eUrlVal headers
  addCustomResolverToCache url $ fromMaybe [] headers
  return successMsg

  where
    getUrlFromEnv urlFromEnv = do
      mEnv <- liftIO . lookupEnv $ T.unpack urlFromEnv
      env <- maybe (throw400 Unexpected $ envNotFoundMsg urlFromEnv) return mEnv
      return $ T.pack env

    envNotFoundMsg e =
      "cannot find environment variable " <> e <> " for custom resolver"


addCustomResolverToCache :: CacheRWM m => Text -> [HeaderConf] -> m ()
addCustomResolverToCache url hdrs = do
  sc <- askSchemaCache
  writeSchemaCache sc { scRemoteResolver = Just (url, hdrs) }

instance HDBQuery AddCustomResolverQuery where
  type Phase1Res AddCustomResolverQuery = AddCustomResolverQuery
  phaseOne   = addCustomResolverP1
  phaseTwo _ = addCustomResolverP2
  schemaCachePolicy = SCPReload

addCustomResolverToCatalog
  :: Either N.URI UrlFromEnv
  -> Maybe [HeaderConf]
  -> Q.TxE QErr ()
addCustomResolverToCatalog eUrlV headers = do
  let (url, urlFromEnv) = case eUrlV of
        Left u    -> (Just $ T.pack $ show u, Nothing)
        Right ufe -> (Nothing, Just ufe)

  Q.unitQE defaultTxErrorHandler [Q.sql|
    INSERT into hdb_catalog.custom_resolver
      (url, url_from_env, headers)
      VALUES ($1, $2, $3)
  |] (url, urlFromEnv, Q.AltJ $ J.toJSON headers) True

fetchRemoteResolvers :: Q.TxE QErr (Maybe (Text, [HeaderConf]))
fetchRemoteResolvers =
  conv . fromRow <$> Q.listQE defaultTxErrorHandler [Q.sql|
    SELECT url, url_from_env, headers
      FROM hdb_catalog.custom_resolver
  |] () True
  where
    -- FIXME: better way to do this
    conv :: Maybe (Text, Maybe [HeaderConf]) -> Maybe (Text, [HeaderConf])
    conv mVal = case mVal of
      Nothing -> Nothing
      Just (x, mv) -> case mv of
        Nothing -> Just (x, [])
        Just v  -> Just (x, v)

    fromRow ::
      [(Maybe Text, Maybe Text, Q.AltJ (Maybe [HeaderConf]))]
      -> Maybe (Text, Maybe [HeaderConf])
    fromRow []                      = Nothing
    fromRow ((url, urlEnv, Q.AltJ hdrs):_) =
      case (url, urlEnv) of
        (Just u, Nothing) -> Just (u, hdrs)
        (Nothing, Just u) -> Just (u, hdrs)
        _                 -> Nothing
