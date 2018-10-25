{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
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
  -> RoledGCtx
  -> HTTP.Manager
  -> GraphQLRequest
  -> BL.ByteString -- this can be removed when we have a pretty-printer
  -> m BL.ByteString
runGQ pool isoL userInfo gCtxRoleMap manager req rawReq = do
  -- FIXME: parseSchemaDoc doesn't work if there's a `schema` keyword present
  -- all the top-level nodes in the schema
  let role = userRole userInfo
  typeNames <- getTopLevelNodes role gCtxRoleMap
  topLevelNodes <- getTopLevelQueryNodes req

  -- find out if any or all of the top-nodes in the query are part of the
  -- remote schema
  let isany = any (`elem` typeNames) topLevelNodes
      isall = all (`elem` typeNames) topLevelNodes

  case (isany, isall) of
    (True, True)  -> runRemoteGQ manager userInfo gCtxRoleMap rawReq
    (True, False) ->
      throw400 UnexpectedPayload "can't mix remote and hasura nodes"
    (False, _)    -> runHasuraGQ pool isoL userInfo gCtxRoleMap req


-- FIXME: better way to retrieve all top-level nodes of the current query
getTopLevelQueryNodes
  :: (MonadError QErr m)
  => GraphQLRequest -> m [G.Name]
getTopLevelQueryNodes req = do
  let (GraphQLRequest opNameM q _) = req
      (selSets, opDefs, _) = G.partitionExDefs $ unGraphQLQuery q
  opDef <- VQ.getTypedOp opNameM selSets opDefs
  return $ map (\(G.SelectionField f) -> G._fName f) $ G._todSelectionSet opDef


runMaybe :: Maybe a -> b -> (a -> b) -> b
runMaybe mVal defaultVal action = maybe defaultVal action mVal

getTopLevelNodes
  :: (MonadIO m, MonadError QErr m)
  => RoleName -> RoledGCtx
  -> m [G.Name]
getTopLevelNodes role gCtxRoleMap = do
  let mUgCtx = Map.lookup role gCtxRoleMap
  ugCtx <- maybe (throw500 errMsg) return mUgCtx
  let mRmGCtx   = _ugRemoteCtx ugCtx
  runMaybe mRmGCtx (return []) $ \remoteCtx -> do
    let rmGCtx  = snd remoteCtx
        qr      = _rgQueryRoot rmGCtx
        mr      = fromMaybe (G.NamedType "Mutation") $ _rgMutationRoot rmGCtx
        types   = _rgTypes rmGCtx
        -- get the query root and mutation root objects
        rootTypes = flip filter (Map.elems types) $ \case
          VT.TIObj o -> VT._otiName o == qr || VT._otiName o == mr
          _          -> False

    let rootTypeFlds = map (\(VT.TIObj o) -> VT._otiFields o) rootTypes
    return $ join $ map Map.keys rootTypeFlds
  where
    errMsg = "invalid role " <> T.pack (show role) <> " in RoledGCtx"


-- getTopLevelNodes :: G.SchemaDocument -> [G.Name]
-- getTopLevelNodes (G.SchemaDocument schema _ _ _) =
--   join $ map extractFieldNames queryMutationDefinition
--   where
--     extractFieldNames objectdef =
--       let fields = G._otdFieldsDefinition objectdef
--       in map G._fldName fields

--     queryMutationDefinition =
--       -- trim the sum type tag for object definition
--       map (\(G.TypeDefinitionObject d) -> d) objectdefs
--       where
--       -- get only the object definitions which are called Query or Mutation
--         objectdefs = flip filter schema $
--           \case
--             G.TypeDefinitionObject d ->
--               (G._otdName d == "Query") || (G._otdName d == "Mutation")
--             _ -> False


runHasuraGQ
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool -> Q.TxIsolation
  -> UserInfo
  -> RoledGCtx --GCtxMap
  -- -> HTTP.Manager
  -> GraphQLRequest
  -- -> BL.ByteString
  -> m BL.ByteString
runHasuraGQ pool isoL userInfo gCtxMap req = do
  (opTy, fields) <- runReaderT (VQ.validateGQ req) $ _ugHasuraCtx gCtx
  when (opTy == G.OperationTypeSubscription) $ throw400 UnexpectedPayload
    "subscriptions are not supported over HTTP, use websockets instead"
  let tx = R.resolveSelSet userInfo gCtx opTy fields
  resp <- liftIO (runExceptT $ runTx tx) >>= liftEither
  return $ encodeGQResp $ GQSuccess resp
  where
    gCtx = getUniGCtx (userRole userInfo) gCtxMap
    runTx tx =
      Q.runTx pool (isoL, Nothing) $
      RQ.setHeadersTx userInfo >> tx


runRemoteGQ
  :: (MonadIO m, MonadError QErr m)
  => HTTP.Manager
  -> UserInfo
  -> RoledGCtx
  -> BL.ByteString
  -- ^ the raw request string
  -> m BL.ByteString
runRemoteGQ manager userInfo gCtxMap q = do
  let role  = userRole userInfo
      ugCtx = getUniGCtx role gCtxMap
      mRmCtx = _ugRemoteCtx ugCtx
  rmCtx <- liftMaybe (err400 Unexpected noRemote) mRmCtx
  let url = fst rmCtx
  when (url == "") $ throw400 Unexpected $ roleErr role
  --liftIO $ putStrLn "running remote graphql query"
  --liftIO $ print q
  let options = Wreq.defaults
              & Wreq.headers .~ [("content-type", "application/json")]
              & Wreq.checkResponse ?~ (\_ _ -> return ())
              & Wreq.manager .~ Right manager

  res  <- liftIO $ try $ Wreq.postWith options (T.unpack url) q
  resp <- either httpThrow return res
  return $ resp ^. Wreq.responseBody
  where
    httpThrow :: (MonadError QErr m) => HTTP.HttpException -> m a
    httpThrow err = throw500 $ T.pack . show $ err
    noRemote = "no remote resolver configured"
    roleErr (RoleName rn) = "no remote schema found for role " <> rn
