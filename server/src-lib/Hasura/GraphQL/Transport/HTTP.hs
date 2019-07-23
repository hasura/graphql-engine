module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  ) where

import           Data.Aeson
import           Hasura.GraphQL.Validate
import qualified Network.HTTP.Types                     as N


import           Hasura.EncJSON
import           Hasura.GraphQL.Logging
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Context
import           Hasura.Server.Utils                    (RequestId)

import qualified Hasura.GraphQL.Execute                 as E

runGQ
  :: ( MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     )
  => RequestId
  -> UserInfo
  -> [N.Header]
  -> GQLReqUnparsed
  -> m (HttpResponse EncJSON)
runGQ reqId userInfo reqHdrs req = do
  E.ExecutionCtx _ sqlGenCtx pgExecCtx planCache sc scVer _ enableAL <- ask
  execPlans <-
    E.getResolvedExecPlan
      pgExecCtx
      planCache
      userInfo
      sqlGenCtx
      enableAL
      sc
      scVer
      req
  results <-
    forM execPlans $ \execPlan ->
      case execPlan of
        E.ExPHasura resolvedOp -> do
          hasuraJson <- runHasuraGQ reqId req userInfo resolvedOp
          pure (HttpResponse hasuraJson Nothing)
        E.ExPRemote rt -> do
          let (rsi, fields) = remoteTopQueryEither rt
          resp@(HttpResponse res _) <- E.execRemoteGQ
                reqId
                userInfo
                reqHdrs
                (rtqOperationType rt)
                rsi
                fields
          -- liftIO (putStrLn ("remote result = " ++ show res))
          return resp
        E.ExPMixed resolvedOp remoteRels -> do
          hasuraJson <- runHasuraGQ reqId req userInfo resolvedOp
          -- liftIO $ putStrLn ("hasura_JSON = " ++ show hasuraJson)
          let result =
                E.extractRemoteRelArguments
                  (scRemoteSchemas sc)
                  hasuraJson
                  remoteRels
          case result of
            Left errors -> return $ HttpResponse (encJFromJValue errors) Nothing
            Right (hasuraValue, remotes) -> do
              let batches =
                    E.produceBatches (E.getOpTypeFromExecOp resolvedOp) remotes
              results <-
                traverse
                  (\batch -> do
                     HttpResponse res _ <-
                       let batchQuery = E.batchRemoteTopQuery batch
                           (rsi, fields) = remoteTopQueryEither batchQuery
                        in E.execRemoteGQ
                             reqId
                             userInfo
                             reqHdrs
                             (rtqOperationType batchQuery)
                             rsi
                             fields
                     -- liftIO (putStrLn ("remote result = " ++ show res))
                     pure (batch, res))
                  batches
              let joinResult = (E.joinResults results hasuraValue)
              -- liftIO (putStrLn ("joined = " <> (L8.unpack . encode) joinResult))
              pure (HttpResponse (encJFromJValue $ toJSON joinResult) Nothing)
  let mergedResp = mergeResponseData (toList (fmap _hrBody results))
  pure (HttpResponse mergedResp (foldMap _hrHeaders results))

runHasuraGQ
  :: ( MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     )
  => RequestId
  -> GQLReqUnparsed
  -> UserInfo
  -> E.ExecOp
  -> m EncJSON
runHasuraGQ reqId query userInfo resolvedOp = do
  E.ExecutionCtx logger _ pgExecCtx _ _ _ _ _ <- ask
  respE <- liftIO $ runExceptT $ case resolvedOp of
    E.ExOpQuery tx genSql  -> do
      -- log the generated SQL and the graphql query
      liftIO $ logGraphqlQuery logger $ QueryLog query genSql reqId
      runLazyTx' pgExecCtx tx
    E.ExOpMutation tx -> do
      -- log the graphql query
      liftIO $ logGraphqlQuery logger $ QueryLog query Nothing reqId
      runLazyTx pgExecCtx $ withUserInfo userInfo tx
    E.ExOpSubs _ ->
      throw400 UnexpectedPayload
      "subscriptions are not supported over HTTP, use websockets instead"
  resp <- liftEither respE
  return $ encodeGQResp $ GQSuccess $ encJToLBS resp

-- | Merge the list of objects by the @data@ key.
-- TODO: Duplicate keys are ignored silently; handle this.
-- TODO: Original order of keys is not preserved, either.
mergeResponseData :: [EncJSON] -> EncJSON
mergeResponseData responses =
  let mergedGQResp =
        foldl
          (\accResp respM ->
             case respM of
               Nothing -> accResp
               Just resp ->
                 case (E.gqRespData resp, E.gqRespErrors resp) of
                   (Nothing, Nothing) -> accResp
                   (Nothing, Just errors) ->
                     accResp
                       { E.gqRespErrors =
                           maybe
                             (Just errors)
                             (\accErr -> Just $ accErr <> errors)
                             (E.gqRespErrors accResp)
                       }
                   (Just data', Nothing) ->
                     accResp
                       { E.gqRespData =
                           maybe
                             (Just data')
                             (\accData -> Just $ accData <> data')
                             (E.gqRespData accResp)
                       }
                   (Just data', Just errors) ->
                     accResp
                       { E.gqRespData =
                           maybe
                             (Just data')
                             (\accData -> Just $ accData <> data')
                             (E.gqRespData accResp)
                       , E.gqRespErrors =
                           maybe
                             (Just errors)
                             (\accErr -> Just $ accErr <> errors)
                             (E.gqRespErrors accResp)
                       })
          E.emptyResp
          (fmap (decode . encJToLBS) responses)
   in encJFromJValue mergedGQResp
