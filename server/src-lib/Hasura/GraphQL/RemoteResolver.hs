{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Hasura.GraphQL.RemoteResolver where

import           Control.Applicative           (liftA2)
import           Control.Exception             (try)
import           Control.Lens                  ((&), (.~), (?~), (^.))
import           Data.FileEmbed                (embedStringFile)
import           Data.Foldable                 (foldlM)
import           Hasura.Prelude
import           Language.Haskell.TH.Syntax    (Lift)
import           System.Environment            (lookupEnv)

import qualified Data.Aeson                    as J
import qualified Data.Aeson.Casing             as J
import qualified Data.Aeson.TH                 as J
import qualified Data.ByteString.Lazy          as BL
import qualified Data.CaseInsensitive          as CI
import qualified Data.HashMap.Strict           as Map
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Database.PG.Query             as Q
import qualified Language.GraphQL.Draft.JSON   ()
import qualified Language.GraphQL.Draft.Syntax as G
import qualified Network.HTTP.Client           as HTTP
import qualified Network.URI.Extended          as N
import qualified Network.Wreq                  as Wreq

import           Hasura.RQL.DDL.Headers        (HeaderConf (..),
                                                getHeadersFromConf)
import           Hasura.RQL.Types

import qualified Hasura.GraphQL.Schema         as GS
import qualified Hasura.GraphQL.Validate.Types as VT


introspectionQuery :: BL.ByteString
introspectionQuery = $(embedStringFile "src-rsr/introspection.json")

fetchRemoteSchema
  :: (MonadIO m, MonadError QErr m)
  => HTTP.Manager -> N.URI -> [HeaderConf] -> m GS.RemoteGCtx
fetchRemoteSchema manager url headerConf = do
  headers <- getHeadersFromConf headerConf
  let hdrs = map (\(hn, hv) -> (CI.mk . T.encodeUtf8 $ hn, T.encodeUtf8 hv)) headers
  let options = Wreq.defaults
              & Wreq.headers .~ ("content-type", "application/json") : hdrs
              & Wreq.checkResponse ?~ (\_ _ -> return ())
              & Wreq.manager .~ Right manager

  res  <- liftIO $ try $ Wreq.postWith options (show url) introspectionQuery
  resp <- either throwHttpErr return res

  let respData = resp ^. Wreq.responseBody
      statusCode = resp ^. Wreq.responseStatus . Wreq.statusCode
  when (statusCode /= 200) $ schemaErr respData

  schemaDoc <- either schemaErr return $ J.eitherDecode respData
  --liftIO $ print $ map G.getNamedTyp $ G._sdTypes schemaDoc
  let etTypeInfos = mapM fromRemoteTyDef $ G._sdTypes schemaDoc
  typeInfos <- either schemaErr return etTypeInfos
  --liftIO $ print $ map VT.getNamedTy typeInfos
  let typMap = VT.mkTyInfoMap typeInfos
      (qRootN, mRootN, _) = getRootNames schemaDoc
      mQrTyp = Map.lookup qRootN typMap
      mMrTyp = Map.lookup mRootN typMap
  qrTyp <- liftMaybe noQueryRoot mQrTyp
  let mRmQR = VT.getObjTyM qrTyp
      mRmMR = join $ VT.getObjTyM <$> mMrTyp
  rmQR <- liftMaybe (err400 Unexpected "query root has to be an object type") mRmQR
  return $ GS.RemoteGCtx typMap rmQR mRmMR Nothing

  where
    noQueryRoot = err400 Unexpected "query root not found in remote schema"
    fromRemoteTyDef ty = VT.fromTyDef ty $ VT.RemoteType url
    getRootNames sc = ( G._sdQueryRoot sc
                      , G._sdMutationRoot sc
                      , G._sdSubscriptionRoot sc )
    schemaErr err = throw400 RemoteSchemaError (T.pack $ show err)

    throwHttpErr :: (MonadError QErr m) => HTTP.HttpException -> m a
    throwHttpErr = schemaErr

mergeRemoteSchemas
  :: (MonadError QErr m)
  => GS.GCtxMap
  -> [(a, GS.RemoteGCtx)]
  -> m GS.GCtxMap
mergeRemoteSchemas = foldlM mergeRemoteSchema

mergeRemoteSchema
  :: (MonadError QErr m)
  => GS.GCtxMap
  -> (a, GS.RemoteGCtx)
  -> m GS.GCtxMap
mergeRemoteSchema ctxMap (_, rmSchema) = do
  let rmTypes = GS._rgTypes rmSchema
  res <- forM (Map.toList ctxMap) $ \(role, gCtx) -> do
    let hsraTyMap = GS._gTypes gCtx
    GS.checkConflictingNodes hsraTyMap rmSchema
    -- merge hasura and remote query root
    let hQR = VT._otiFields $ GS._gQueryRoot gCtx
        rmQR = VT._otiFields $ GS._rgQueryRoot rmSchema
        newFlds = Map.union hQR rmQR
        newQR = (GS._gQueryRoot gCtx) { VT._otiFields = newFlds }
        -- merge hasura and remote mutation root
        hMR = VT._otiFields <$> GS._gMutRoot gCtx
        rmMR = VT._otiFields <$> GS._rgMutationRoot rmSchema
        newMutFldsM = liftA2 Map.union hMR rmMR
        newMutFlds = fromMaybe Map.empty newMutFldsM
        hMrM = GS._gMutRoot gCtx
        newMR = maybe Nothing
                (\hMr -> Just hMr { VT._otiFields = newMutFlds })
                hMrM
        -- merge the hasura and remote typemap
        newTyMap' = Map.insert (G.NamedType "query_root") (VT.TIObj newQR) $
                    Map.union hsraTyMap rmTypes
        newTyMap = maybe newTyMap'
                    (\mr -> Map.insert (G.NamedType "mutation_root") (VT.TIObj mr) newTyMap')
                    newMR

    let updatedGCtx = gCtx { GS._gTypes = newTyMap
                           , GS._gQueryRoot = newQR
                           , GS._gMutRoot = newMR
                           }
    return (role, updatedGCtx)
  return $ Map.fromList res


getUrlFromEnv :: (MonadIO m, MonadError QErr m) => Text -> m N.URI
getUrlFromEnv urlFromEnv = do
  mEnv <- liftIO . lookupEnv $ T.unpack urlFromEnv
  env  <- maybe (throw400 Unexpected $ envNotFoundMsg urlFromEnv) return
          mEnv
  maybe (throw400 Unexpected $ invalidUri env) return $ N.parseURI env
  where
    invalidUri uri = "not a valid URI: " <> T.pack uri
    envNotFoundMsg e =
      "cannot find environment variable " <> e <> " for custom resolver"


type UrlFromEnv = Text

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
addCustomResolverP2 (AddCustomResolverQuery eUrlVal headers name) = do
  url <- either return getUrlFromEnv eUrlVal
  manager <- askHttpManager
  typeMap <- askTypeMap
  remoteGCtx <- fetchRemoteSchema manager url $ fromMaybe [] headers
  GS.checkConflictingNodes typeMap remoteGCtx
  liftTx $ addCustomResolverToCatalog eUrlVal headers name
  addCustomResolverToCache url $ fromMaybe [] headers
  return successMsg


addCustomResolverToCache :: CacheRWM m => N.URI -> [HeaderConf] -> m ()
addCustomResolverToCache url hdrs = do
  sc <- askSchemaCache
  let resolvers = scRemoteResolvers sc
  writeSchemaCache sc { scRemoteResolvers = resolvers ++ [(url, hdrs)] }

instance HDBQuery AddCustomResolverQuery where
  type Phase1Res AddCustomResolverQuery = AddCustomResolverQuery
  phaseOne   = addCustomResolverP1
  phaseTwo _ = addCustomResolverP2
  schemaCachePolicy = SCPReload

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


data DeleteCustomResolverQuery
  = DeleteCustomResolverQuery
  { _dcrqName    :: !Text
  } deriving (Show, Eq, Lift)

$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''DeleteCustomResolverQuery)

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
  url <- liftTx $ fetchRemoteResolverUrl name
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

deleteCustomResolverFromCatalog :: Text -> Q.TxE QErr ()
deleteCustomResolverFromCatalog name =
  Q.unitQE defaultTxErrorHandler [Q.sql|
    DELETE FROM hdb_catalog.custom_resolver
      WHERE name = $1
  |] (Identity name) True


instance HDBQuery DeleteCustomResolverQuery where
  type Phase1Res DeleteCustomResolverQuery = DeleteCustomResolverQuery
  phaseOne   = deleteCustomResolverP1
  phaseTwo _ = deleteCustomResolverP2
  schemaCachePolicy = SCPReload


fetchRemoteResolverUrl :: Text -> Q.TxE QErr Text
fetchRemoteResolverUrl name =
  runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler
    [Q.sql|
     SELECT url from hdb_catalog.custom_resolver
       WHERE name = $1
     |] (Identity name) True

fetchRemoteResolvers :: Q.TxE QErr [(Either N.URI Text, [HeaderConf])]
fetchRemoteResolvers = do
  res <- Q.listQE defaultTxErrorHandler
    [Q.sql|
     SELECT url, url_from_env, headers
       FROM hdb_catalog.custom_resolver
     |] () True
  mapM fromRow res
  where
    -- FIXME: better way to do this
    -- FIXME: check for json null in headers
    fromRow (url, urlEnv, Q.AltJ hdrs) =
      case (url, urlEnv) of
        (Just u, Nothing)  -> do
          url' <- mkUrl u
          return (Left url', hdrs)
        (Nothing, Just u)  -> return (Right u, hdrs)
        (Just _, Just u2)  -> return (Right u2, hdrs)
        (Nothing, Nothing) ->
          throw500 "both url, url_from_env cannot be empty"

    mkUrl u = maybe (throw500 "not a valid URI in DB") return $
              N.parseURI $ T.unpack u
