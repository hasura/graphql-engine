{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  , getTopLevelNodes
  , gatherTypeLocs
  , assertSameLocationNodes
  , runRemoteGQ
  ) where

import           Control.Exception                      (try)
import           Control.Lens
import           Hasura.Prelude
import           Language.GraphQL.Draft.JSON            ()

import qualified Data.ByteString.Lazy                   as BL
import qualified Data.CaseInsensitive                   as CI
import qualified Data.HashMap.Strict                    as Map
import qualified Data.String.Conversions                as CS
import qualified Data.Text                              as T
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N
import qualified Network.URI                            as URI
import qualified Network.Wreq                           as Wreq

import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.Types

import qualified Hasura.GraphQL.Resolve                 as R
import qualified Hasura.GraphQL.Validate                as VQ
import qualified Hasura.GraphQL.Validate.Types          as VT
import qualified Hasura.Server.Query                    as RQ


runGQ
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool -> Q.TxIsolation
  -> UserInfo
  -> SchemaCache
  -> HTTP.Manager
  -> [N.Header]
  -> GraphQLRequest
  -> BL.ByteString -- this can be removed when we have a pretty-printer
  -> m BL.ByteString
runGQ pool isoL userInfo sc manager reqHdrs req rawReq = do

  (gCtx, _) <- flip runStateT sc $ getGCtx (userRole userInfo) gCtxRoleMap
  (opDef, opRoot, fragDefsL, varValsM) <- flip runReaderT gCtx $
                                          VQ.getQueryParts req

  let topLevelNodes = getTopLevelNodes opDef
      -- gather TypeLoc of topLevelNodes
      typeLocs = gatherTypeLocs gCtx topLevelNodes

  -- see if they are all the same
  assertSameLocationNodes typeLocs

  if null typeLocs
    then runHasuraGQ pool isoL userInfo sc opDef opRoot fragDefsL
         varValsM
    else
    case typeLocs of
      (typeLoc:_) -> case typeLoc of
        VT.HasuraType ->
          runHasuraGQ pool isoL userInfo sc opDef opRoot fragDefsL varValsM
        VT.RemoteType url hdrs ->
          runRemoteGQ manager userInfo sc reqHdrs rawReq url hdrs

      [] -> throw500 "unexpected: cannot find node in schema"

  where
    gCtxRoleMap = scGCtxMap sc

assertSameLocationNodes :: (MonadError QErr m) => [VT.TypeLoc] -> m ()
assertSameLocationNodes typeLocs =
  -- see if they are all the same
  unless (allEq typeLocs) $
    throw400 NotSupported msg
  where
    msg = "cannot mix nodes from two different graphql servers"
    allEq xs = case xs of
      []     -> True
      (y:ys) -> all ((==) y) ys

getTopLevelNodes :: G.TypedOperationDefinition -> [G.Name]
getTopLevelNodes opDef =
  map (\(G.SelectionField f) -> G._fName f) $ G._todSelectionSet opDef

gatherTypeLocs :: GCtx -> [G.Name] -> [VT.TypeLoc]
gatherTypeLocs gCtx nodes =
  catMaybes $ flip map nodes $ \node ->
    VT._fiLoc <$> Map.lookup node schemaNodes
  where
    schemaNodes =
      let qr = VT._otiFields $ _gQueryRoot gCtx
          mr = VT._otiFields <$> _gMutRoot gCtx
      in maybe qr (Map.union qr) mr


runHasuraGQ
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool -> Q.TxIsolation
  -> UserInfo
  -> SchemaCache
  -> G.TypedOperationDefinition
  -> VT.ObjTyInfo
  -> [G.FragmentDefinition]
  -> Maybe VariableValues
  -> m BL.ByteString
runHasuraGQ pool isoL userInfo sc opDef opRoot fragDefsL varValsM = do
  (gCtx, _) <- flip runStateT sc $ getGCtx (userRole userInfo) gCtxMap
  (opTy, fields) <- runReaderT
                    (VQ.validateGQ opDef opRoot fragDefsL varValsM) gCtx
  when (opTy == G.OperationTypeSubscription) $ throw400 UnexpectedPayload
    "subscriptions are not supported over HTTP, use websockets instead"
  let tx = R.resolveSelSet userInfo gCtx opTy fields
  resp <- liftIO (runExceptT $ runTx tx) >>= liftEither
  return $ encodeGQResp $ GQSuccess resp
  where
    gCtxMap = scGCtxMap sc
    runTx tx =
      Q.runTx pool (isoL, Nothing) $
      RQ.setHeadersTx (userVars userInfo) >> tx

runRemoteGQ
  :: (MonadIO m, MonadError QErr m)
  => HTTP.Manager
  -> UserInfo
  -> SchemaCache
  -> [N.Header]
  -> BL.ByteString
  -- ^ the raw request string
  -> URI.URI
  -> [HeaderConf]
  -> m BL.ByteString
runRemoteGQ manager userInfo sc reqHdrs q url hdrConf = do
  hdrs <- getHeadersFromConf hdrConf
  let confHdrs = map (\(k, v) -> (CI.mk $ CS.cs k, CS.cs v)) hdrs
      mRmSchemaDef = Map.lookup url (scRemoteResolvers sc)
      shouldFwd = maybe False _rsFwdClientHeaders mRmSchemaDef
      clientHdrs = bool [] filteredHeaders shouldFwd
  let options = Wreq.defaults
              & Wreq.headers .~ ("content-type", "application/json") :
                (userInfoToHdrs ++ clientHdrs ++ confHdrs)
              & Wreq.checkResponse ?~ (\_ _ -> return ())
              & Wreq.manager .~ Right manager

  res  <- liftIO $ try $ Wreq.postWith options (show url) q
  resp <- either httpThrow return res
  return $ resp ^. Wreq.responseBody

  where
    httpThrow :: (MonadError QErr m) => HTTP.HttpException -> m a
    httpThrow err = throw500 $ T.pack . show $ err

    userInfoToHdrs = map (\(k, v) -> (CI.mk $ CS.cs k, CS.cs v)) $
                 userInfoToList userInfo
    filteredHeaders = flip filter reqHdrs $ \(n, _) ->
      n `notElem` [ "Content-Length", "Content-MD5", "User-Agent", "Host"
                  , "Origin", "Referer" , "Accept", "Accept-Encoding"
                  , "Accept-Language", "Accept-Datetime"
                  , "Cache-Control", "Connection", "DNT"
                  ]
