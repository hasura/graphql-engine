{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hasura.GraphQL.RemoteResolver where

import           Control.Exception             (try)
import           Control.Lens                  ((&), (.~), (?~), (^.))
import           Data.FileEmbed                (embedStringFile)
import           Data.Foldable                 (foldlM)
import           Hasura.Prelude
import           System.Environment            (lookupEnv)

import qualified Data.Aeson                    as J
import qualified Data.ByteString.Lazy          as BL
import qualified Data.CaseInsensitive          as CI
import qualified Data.HashMap.Strict           as Map
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
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
  let mMrTyp = maybe Nothing (\mr -> Map.lookup mr typMap) mRootN
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
  case Map.null ctxMap of
    True  -> return onlyRmSchema
    False -> do
      res <- forM (Map.toList ctxMap) $ \(role, gCtx) -> do
        let hsraTyMap = GS._gTypes gCtx
        GS.checkConflictingNodes hsraTyMap rmSchema
        let newQR = mergeQueryRoot gCtx
            newMR = mergeMutRoot gCtx
            newTyMap = mergeTyMaps hsraTyMap rmTypes newQR newMR
        let updatedGCtx = gCtx { GS._gTypes = newTyMap
                               , GS._gQueryRoot = newQR
                               , GS._gMutRoot = newMR
                               }
        return (role, updatedGCtx)
      return $ Map.fromList res

  where
    onlyRmSchema =
      let hTy = GS._gTypes GS.emptyGCtx
          rmTy = GS._rgTypes rmSchema
          newQR = mergeQueryRoot GS.emptyGCtx
          newMR = mergeMutRoot GS.emptyGCtx
          newTyMap = mergeTyMaps hTy rmTy newQR newMR
          updated = GS.emptyGCtx { GS._gTypes = newTyMap
                                 , GS._gQueryRoot = newQR
                                 , GS._gMutRoot = newMR
                                 }
      in Map.insert adminRole updated ctxMap

    mergeQueryRoot gCtx =
      let hQR = VT._otiFields $ GS._gQueryRoot gCtx
          rmQR = VT._otiFields $ GS._rgQueryRoot rmSchema
          newFlds = Map.union hQR rmQR
          newQR = (GS._gQueryRoot gCtx) { VT._otiFields = newFlds }
      in newQR

    mergeMutRoot gCtx =
      let hMR = VT._otiFields <$> GS._gMutRoot gCtx
          rmMR = VT._otiFields <$> GS._rgMutationRoot rmSchema
          newMutFldsM = GS.mergeMaybeMaps hMR rmMR
          newMutFlds = fromMaybe Map.empty newMutFldsM
          hMrM = GS._gMutRoot gCtx
          newMR = maybe (Just $ mkNewMutRoot newMutFlds)
                  (\hMr -> Just hMr { VT._otiFields = newMutFlds })
                  hMrM
      in newMR

    mkNewMutRoot flds = VT.ObjTyInfo (Just "mutation root")
                        (G.NamedType "mutation_root") flds

    mergeTyMaps hTyMap rmTyMap newQR newMR =
      let newTyMap' = Map.insert (G.NamedType "query_root") (VT.TIObj newQR) $
                        Map.union hTyMap rmTyMap
      in maybe newTyMap' (\mr -> Map.insert
                                 (G.NamedType "mutation_root")
                                 (VT.TIObj mr) newTyMap') newMR


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
