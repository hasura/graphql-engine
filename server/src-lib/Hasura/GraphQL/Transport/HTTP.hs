{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  ) where

import           Control.Exception                      (try)
import           Control.Lens
import           Hasura.Prelude
import           Language.GraphQL.Draft.JSON            ()

import qualified Data.ByteString.Lazy                   as BL
import qualified Data.HashMap.Strict                    as Map
import qualified Data.Text                              as T
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.URI                            as N
import qualified Network.Wreq                           as Wreq

import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.RQL.Types

import qualified Hasura.GraphQL.Resolve                 as R
import qualified Hasura.GraphQL.Validate                as VQ
import qualified Hasura.GraphQL.Validate.Types          as VT
import qualified Hasura.Server.Query                    as RQ


runGQ
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool -> Q.TxIsolation
  -> UserInfo
  -> GCtxMap
  -> HTTP.Manager
  -> GraphQLRequest
  -> BL.ByteString -- this can be removed when we have a pretty-printer
  -> m BL.ByteString
runGQ pool isoL userInfo gCtxRoleMap manager req rawReq = do

  (opDef, opRoot, fragDefsL, varValsM) <- flip runReaderT gCtx $
                                          VQ.getQueryParts req

  let topLevelNodes = getTopLevelNodes opDef
  -- gather TypeLoc of topLevelNodes
  let typeLocs = catMaybes $ flip map topLevelNodes $ \node ->
        let mNode = Map.lookup node schemaNodes
        in VT._fiLoc <$> mNode

  -- see if they are all the same
  unless (allEq typeLocs) $
    throw400 NotSupported "cannot mix nodes from two different graphql servers"

  if null typeLocs
         --throw400 UnexpectedPayload "cannot find given node in root"
    then runHasuraGQ pool isoL userInfo gCtxRoleMap
         opDef opRoot fragDefsL varValsM
    else
    -- TODO: do we worry about head?
    case head typeLocs of
      VT.HasuraType     -> runHasuraGQ pool isoL userInfo gCtxRoleMap
                           opDef opRoot fragDefsL varValsM
      VT.RemoteType url -> runRemoteGQ manager rawReq url

  where
    gCtx = getGCtx (userRole userInfo) gCtxRoleMap
    getTopLevelNodes opDef =
      map (\(G.SelectionField f) -> G._fName f) $ G._todSelectionSet opDef

    schemaNodes =
      let qr = VT._otiFields $ _gQueryRoot gCtx
          mr = VT._otiFields <$> _gMutRoot gCtx
      in maybe Map.empty (Map.union qr) mr

    allEq xs = case xs of
      []     -> True
      (y:ys) -> all ((==) y) ys


runHasuraGQ
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool -> Q.TxIsolation
  -> UserInfo
  -> GCtxMap
  -> G.TypedOperationDefinition
  -> VT.ObjTyInfo
  -> [G.FragmentDefinition]
  -> Maybe VariableValues
  -> m BL.ByteString
runHasuraGQ pool isoL userInfo gCtxMap opDef opRoot fragDefsL varValsM = do
  (opTy, fields) <- runReaderT (VQ.validateGQ opDef opRoot fragDefsL varValsM) gCtx
  when (opTy == G.OperationTypeSubscription) $ throw400 UnexpectedPayload
    "subscriptions are not supported over HTTP, use websockets instead"
  let tx = R.resolveSelSet userInfo gCtx opTy fields
  resp <- liftIO (runExceptT $ runTx tx) >>= liftEither
  return $ encodeGQResp $ GQSuccess resp
  where
    gCtx = getGCtx (userRole userInfo) gCtxMap
    runTx tx =
      Q.runTx pool (isoL, Nothing) $
      RQ.setHeadersTx (userVars userInfo) >> tx

runRemoteGQ
  :: (MonadIO m, MonadError QErr m)
  => HTTP.Manager
  -- -> UserInfo -- do we send x-hasura headers to remote?
  -> BL.ByteString
  -- ^ the raw request string
  -> N.URI
  -> m BL.ByteString
runRemoteGQ manager q url = do
  let options = Wreq.defaults
              & Wreq.headers .~ [("content-type", "application/json")]
              & Wreq.checkResponse ?~ (\_ _ -> return ())
              & Wreq.manager .~ Right manager

  res  <- liftIO $ try $ Wreq.postWith options (show url) q
  resp <- either httpThrow return res
  return $ resp ^. Wreq.responseBody
  where
    httpThrow :: (MonadError QErr m) => HTTP.HttpException -> m a
    httpThrow err = throw500 $ T.pack . show $ err
