module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  ) where

import qualified Data.ByteString.Lazy                   as BL
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N

import           Hasura.EncJSON
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Context
import           Hasura.Server.Utils                    (bsToTxt,
                                                         filterRequestHeaders,
                                                         filterResponseHeaders)

import qualified Hasura.GraphQL.Execute                 as E

runGQ
  :: (MonadIO m, MonadError QErr m)
  => PGExecCtx
  -> UserInfo
  -> SQLGenCtx
  -> Bool
  -> E.PlanCache
  -> SchemaCache
  -> SchemaCacheVer
  -> HTTP.Manager
  -> [N.Header]
  -> GQLReqUnparsed
  -> BL.ByteString -- this can be removed when we have a pretty-printer
  -> m EncJSON
runGQ pgExecCtx userInfo sqlGenCtx enableAL planCache sc scVer
  manager reqHdrs req rawReq = do
  execPlan <- E.getResolvedExecPlan pgExecCtx planCache
              userInfo sqlGenCtx enableAL sc scVer req
  case execPlan of
    E.GExPHasura resolvedOp ->
      runHasuraGQ pgExecCtx userInfo resolvedOp
    E.GExPRemote rsi opDef  ->
      E.execRemoteGQ manager userInfo reqHdrs rawReq rsi opDef

runHasuraGQ
  :: (MonadIO m, MonadError QErr m)
  => PGExecCtx
  -> UserInfo
  -> E.ExecOp
  -> m EncJSON
runHasuraGQ pgExecCtx userInfo resolvedOp = do
  respE <- liftIO $ runExceptT $ case resolvedOp of
    E.ExOpQuery tx    ->
      runLazyTx' pgExecCtx tx
    E.ExOpMutation tx ->
      runLazyTx pgExecCtx $ withUserInfo userInfo tx
    E.ExOpSubs _ ->
      throw400 UnexpectedPayload
      "subscriptions are not supported over HTTP, use websockets instead"
  resp <- liftEither respE
  return $ encodeGQResp $ GQSuccess $ encJToLBS resp
-- runRemoteGQ
--   :: (MonadIO m, MonadError QErr m)
--   => HTTP.Manager
--   -> UserInfo
--   -> [N.Header]
--   -> BL.ByteString
--   -- ^ the raw request string
--   -> RemoteSchemaInfo
--   -> G.TypedOperationDefinition
--   -> m HResponse
-- runRemoteGQ manager userInfo reqHdrs q rsi opDef = do
--   let opTy = G._todType opDef
--   when (opTy == G.OperationTypeSubscription) $
--     throw400 NotSupported "subscription to remote server is not supported"
--   hdrs <- getHeadersFromConf hdrConf
--   let confHdrs   = map (\(k, v) -> (CI.mk $ CS.cs k, CS.cs v)) hdrs
--       clientHdrs = bool [] filteredHeaders fwdClientHdrs
--       options    = wreqOptions manager (userInfoToHdrs ++ clientHdrs ++ confHdrs)

--   res  <- liftIO $ try $ Wreq.postWith options (show url) q
--   resp <- either httpThrow return res
--   let respHdrs = map (\(k, v) -> Header (bsToTxt $ CI.original k, bsToTxt v)) $
--                  filterResponseHeaders $ resp ^. Wreq.responseHeaders
--   return $ HResponse (resp ^. Wreq.responseBody) (Just respHdrs)

--   where
--     RemoteSchemaInfo url hdrConf fwdClientHdrs = rsi
--     httpThrow :: (MonadError QErr m) => HTTP.HttpException -> m a
--     httpThrow err = throw500 $ T.pack . show $ err

--     userInfoToHdrs = map (\(k, v) -> (CI.mk $ CS.cs k, CS.cs v)) $
--                  userInfoToList userInfo
--     filteredHeaders = filterRequestHeaders reqHdrs
