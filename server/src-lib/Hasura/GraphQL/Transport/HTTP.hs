module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  ) where

import qualified Data.Aeson.Ordered                     as OJ
import qualified Data.Text                              as T
import           Hasura.GraphQL.Validate
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N


import           Hasura.EncJSON
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Context

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
  -> m (HttpResponse EncJSON)
runGQ pgExecCtx userInfo sqlGenCtx enableAL planCache sc scVer manager reqHdrs req = do
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
          hasuraJson <- runHasuraGQ pgExecCtx userInfo resolvedOp
          pure (HttpResponse hasuraJson Nothing)
        E.ExPRemote rt -> do
          let (rsi, fields) = remoteTopQueryEither rt
          resp@(HttpResponse _ _) <- E.execRemoteGQ
                manager
                userInfo
                reqHdrs
                (rtqOperationType rt)
                rsi
                fields
          -- liftIO (putStrLn ("remote result = " ++ show res))
          return resp
        E.ExPMixed resolvedOp remoteRels -> do
          hasuraJson <- runHasuraGQ pgExecCtx userInfo resolvedOp
          -- liftIO $ putStrLn ("hasura_JSON = " ++ show hasuraJson)
          let result =
                E.extractRemoteRelArguments
                  (scRemoteSchemas sc)
                  hasuraJson
                  remoteRels
          case result of
            Left errors -> return $ HttpResponse (OJ.toEncJSON (E.gqrespValueToValue errors)) Nothing
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
                             manager
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
              pure (HttpResponse (OJ.toEncJSON $ E.gqrespValueToValue joinResult) Nothing)
  let mergedRespResult = mergeResponseData (toList (fmap _hrBody results))
  case mergedRespResult of
    Left e -> throw400 UnexpectedPayload
              ("could not merge data from results: " <> T.pack e)
    Right mergedResp -> pure (HttpResponse mergedResp (foldMap _hrHeaders results))


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

-- | Merge the list of objects by the @data@ key.
mergeResponseData :: [EncJSON] -> Either String EncJSON
mergeResponseData responses = do
  resps <- traverse ((OJ.eitherDecode . encJToLBS) >=> E.parseGQRespValue) responses
  let mergedGQResp =
        foldM
          (\accResp resp ->
             case (E.gqRespData resp, E.gqRespErrors resp) of
               (Nothing, Nothing) -> pure accResp
               (Nothing, Just errors) ->
                 pure
                   accResp
                     { E.gqRespErrors =
                         maybe
                           (Just errors)
                           (\accErr -> Just $ accErr <> errors)
                           (E.gqRespErrors accResp)
                     }
               (Just data', Nothing) -> do
                 combined <-
                   maybe
                     (pure (Just data'))
                     (\accData -> fmap Just $ OJ.union accData data')
                     (E.gqRespData accResp)
                 pure accResp {E.gqRespData = combined}
               (Just data', Just errors) -> do
                 combined <-
                   maybe
                     (pure (Just data'))
                     (\accData -> fmap Just $ OJ.union accData data')
                     (E.gqRespData accResp)
                 pure
                   accResp
                     { E.gqRespData = combined
                     , E.gqRespErrors =
                         maybe
                           (Just errors)
                           (\accErr -> Just $ accErr <> errors)
                           (E.gqRespErrors accResp)
                     })
          E.emptyResp
          resps
   in fmap (OJ.toEncJSON . E.gqrespValueToValue) mergedGQResp
