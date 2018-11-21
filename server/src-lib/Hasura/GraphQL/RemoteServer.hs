{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hasura.GraphQL.RemoteServer where

import           Control.Exception             (try)
import           Control.Lens                  ((^.))
import           Data.FileEmbed                (embedStringFile)
import           Data.Foldable                 (foldlM)
import           Hasura.Prelude

import qualified Data.Aeson                    as J
import qualified Data.ByteString.Lazy          as BL
import qualified Data.CaseInsensitive          as CI
import qualified Data.HashMap.Strict           as Map
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Language.GraphQL.Draft.JSON   ()
import qualified Language.GraphQL.Draft.Syntax as G
import qualified Network.HTTP.Client           as HTTP
import qualified Network.Wreq                  as Wreq

import           Hasura.HTTP.Utils             (wreqOptions)
import           Hasura.RQL.DDL.Headers        (getHeadersFromConf)
import           Hasura.RQL.Types

import qualified Hasura.GraphQL.Schema         as GS
import qualified Hasura.GraphQL.Validate.Types as VT


introspectionQuery :: BL.ByteString
introspectionQuery = $(embedStringFile "src-rsr/introspection.json")

fetchRemoteSchema
  :: (MonadIO m, MonadError QErr m)
  => HTTP.Manager
  -> RemoteSchemaName
  -> RemoteSchemaInfo
  -> m GS.RemoteGCtx
fetchRemoteSchema manager name def@(RemoteSchemaInfo url headerConf _) = do
  headers <- getHeadersFromConf headerConf
  let hdrs = map (\(hn, hv) -> (CI.mk . T.encodeUtf8 $ hn, T.encodeUtf8 hv)) headers
      options = wreqOptions manager hdrs
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
      mMrTyp = maybe Nothing (\mr -> Map.lookup mr typMap) mRootN
  qrTyp <- liftMaybe noQueryRoot mQrTyp
  let mRmQR = VT.getObjTyM qrTyp
      mRmMR = join $ VT.getObjTyM <$> mMrTyp
  rmQR <- liftMaybe (err400 Unexpected "query root has to be an object type") mRmQR
  return $ GS.RemoteGCtx typMap rmQR mRmMR Nothing

  where
    noQueryRoot = err400 Unexpected "query root not found in remote schema"
    fromRemoteTyDef ty = VT.fromTyDef ty $ VT.RemoteType name def
    getRootNames sc = ( G._sdQueryRoot sc
                      , G._sdMutationRoot sc
                      , G._sdSubscriptionRoot sc )
    schemaErr err = throw400 RemoteSchemaError (T.pack $ show err)

    throwHttpErr :: (MonadError QErr m) => HTTP.HttpException -> m a
    throwHttpErr = schemaErr

mergeSchemas
  :: (MonadIO m, MonadError QErr m)
  => RemoteSchemaMap
  -> GS.GCtxMap
  -> HTTP.Manager
  -> m (GS.GCtxMap, GS.GCtx) -- the merged GCtxMap and the default GCtx without roles
mergeSchemas rmSchemaMap gCtxMap httpManager = do
  remoteSchemas <- forM (Map.toList rmSchemaMap) $ \(name, def) ->
    fetchRemoteSchema httpManager name def
  def <- mkDefaultRemoteGCtx remoteSchemas
  merged <- mergeRemoteSchema gCtxMap def
  return (merged, def)

mkDefaultRemoteGCtx
  :: (MonadError QErr m)
  => [GS.RemoteGCtx] -> m GS.GCtx
mkDefaultRemoteGCtx =
  foldlM (\combG -> mergeGCtx combG . convRemoteGCtx) GS.emptyGCtx

mergeRemoteSchema
  :: (MonadError QErr m)
  => GS.GCtxMap
  -> GS.GCtx
  -> m GS.GCtxMap
mergeRemoteSchema ctxMap mergedRemoteGCtx = do
  res <- forM (Map.toList ctxMap) $ \(role, gCtx) -> do
    updatedGCtx <- mergeGCtx gCtx mergedRemoteGCtx
    return (role, updatedGCtx)
  return $ Map.fromList res

mergeGCtx
  :: (MonadError QErr m)
  => GS.GCtx
  -> GS.GCtx
  -> m GS.GCtx
mergeGCtx gCtx rmMergedGCtx = do
  let rmTypes = GS._gTypes rmMergedGCtx
      hsraTyMap = GS._gTypes gCtx
  GS.checkSchemaConflicts gCtx rmMergedGCtx
  let newQR = mergeQueryRoot gCtx rmMergedGCtx
      newMR = mergeMutRoot gCtx rmMergedGCtx
      newTyMap = mergeTyMaps hsraTyMap rmTypes newQR newMR
      updatedGCtx = gCtx { GS._gTypes = newTyMap
                         , GS._gQueryRoot = newQR
                         , GS._gMutRoot = newMR
                         }
  return updatedGCtx

convRemoteGCtx :: GS.RemoteGCtx -> GS.GCtx
convRemoteGCtx rmGCtx =
  GS.emptyGCtx { GS._gTypes     = GS._rgTypes rmGCtx
               , GS._gQueryRoot = GS._rgQueryRoot rmGCtx
               , GS._gMutRoot   = GS._rgMutationRoot rmGCtx
               }


mergeQueryRoot :: GS.GCtx -> GS.GCtx -> VT.ObjTyInfo
mergeQueryRoot a b = GS._gQueryRoot a <> GS._gQueryRoot b

mergeMutRoot :: GS.GCtx -> GS.GCtx -> Maybe VT.ObjTyInfo
mergeMutRoot a b =
  let objA' = fromMaybe mempty $ GS._gMutRoot a
      objB  = fromMaybe mempty $ GS._gMutRoot b
      objA  = newRootOrEmpty objA' objB
      merged = objA <> objB
  in bool (Just merged) Nothing $ merged == mempty
  where
    newRootOrEmpty x y =
      if x == mempty && y /= mempty
      then mkNewEmptyMutRoot
      else x

mkNewEmptyMutRoot :: VT.ObjTyInfo
mkNewEmptyMutRoot = VT.ObjTyInfo (Just "mutation root")
                    (G.NamedType "mutation_root") Map.empty

mkNewMutRoot :: VT.ObjFieldMap -> VT.ObjTyInfo
mkNewMutRoot flds = VT.ObjTyInfo (Just "mutation root")
                    (G.NamedType "mutation_root") flds

mergeTyMaps
  :: VT.TypeMap
  -> VT.TypeMap
  -> VT.ObjTyInfo
  -> Maybe VT.ObjTyInfo
  -> VT.TypeMap
mergeTyMaps hTyMap rmTyMap newQR newMR =
  let newTyMap  = hTyMap <> rmTyMap
      newTyMap' = Map.insert (G.NamedType "query_root") (VT.TIObj newQR) $
                  newTyMap
  in maybe newTyMap' (\mr -> Map.insert
                              (G.NamedType "mutation_root")
                              (VT.TIObj mr) newTyMap') newMR
