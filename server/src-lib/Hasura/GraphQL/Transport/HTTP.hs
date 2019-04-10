module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  , mergeResponse
  ) where

--import           Control.Exception                      (catch, try)
import           Debug.Trace

--import qualified Control.Concurrent.Async               as A
import qualified Data.Aeson                             as J
import qualified Data.HashMap.Lazy                      as Map
import qualified Database.PG.Query                      as Q
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N

import           Hasura.EncJSON
import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude
import           Hasura.RQL.Types

import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.GraphQL.Resolve                 as R
import qualified Hasura.GraphQL.Validate                as V
import qualified Language.GraphQL.Draft.Syntax          as G


runGQ
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool -> Q.TxIsolation
  -> UserInfo
  -> SQLGenCtx
  -> SchemaCache
  -> HTTP.Manager
  -> [N.Header]
  -> GraphQLRequest
  -> m EncJSON
runGQ pool isoL userInfo sqlGenCtx sc manager reqHdrs req = do
  execPlan <- E.getExecPlan userInfo sc req
  let E.GQExecPlan hasuraPlan remotePlans _ = execPlan
  case (hasuraPlan, remotePlans) of
     (Nothing, []) -> throw500 "no exec plan found"
     (Just (E.GExPHasura gCtx rootSelSet), []) ->
       runHasuraGQ pool isoL userInfo sqlGenCtx gCtx rootSelSet
     (_, _) ->
       runMixedGQ pool isoL userInfo sqlGenCtx manager reqHdrs execPlan

runMixedGQ
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool
  -> Q.TxIsolation
  -> UserInfo
  -> SQLGenCtx
  -> HTTP.Manager
  -> [N.Header]
  -> E.GQExecPlan
  -> m EncJSON
runMixedGQ pool isoL userInfo sqlGenCtx manager reqHdrs plan = do
  let E.GQExecPlan hasuraPlan remotePlans opTy = plan
  when (opTy == G.OperationTypeSubscription) $
    throw400 UnexpectedPayload
    "subscriptions are not supported over HTTP, use websockets instead"

  res <- run (hasuraPlan, remotePlans)
  (datas, errs) <- mergeResponse res
  return $ encJFromJValue $ J.object [ "data" J..= datas
                                     , "errors" J..= errs
                                     ]
  where
    -- TODO: use async
    run (hasuraPlan, remotePlans) = do
      hasuraRes <- case hasuraPlan of
        Nothing -> return []
        Just (E.GExPHasura gCtx rootSelSet) -> do
          res <- runHasuraGQ pool isoL userInfo sqlGenCtx gCtx rootSelSet
          return [res]
      remoteRes <- forM remotePlans $ \(E.GExPRemote rsi newq rs) ->
        E.execRemoteGQ manager userInfo reqHdrs newq rsi rs
      return $ hasuraRes ++ remoteRes


runHasuraGQ
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool
  -> Q.TxIsolation
  -> UserInfo
  -> SQLGenCtx
  -> GCtx
  -> V.RootSelSet
  -> m EncJSON
runHasuraGQ pool isoL userInfo sqlGenCtx gCtx rootSelSet = do
  tx <- case rootSelSet of
    V.RQuery selSet ->
      return $ R.resolveQuerySelSet userInfo gCtx sqlGenCtx selSet
    V.RMutation selSet ->
      return $ R.resolveMutSelSet userInfo gCtx sqlGenCtx selSet
    V.RSubscription _  ->
      throw400 UnexpectedPayload
      "subscriptions are not supported over HTTP, use websockets instead"
  resp <- liftIO (runExceptT $ runTx tx) >>= liftEither
  return $ encodeGQResp $ GQSuccess $ encJToLBS resp
  where
    runTx tx = runLazyTx pool isoL $ withUserInfo userInfo tx


-- merge response from different graphql servers
mergeResponse :: (MonadError QErr m) => [EncJSON] -> m (J.Object, [J.Value])
mergeResponse allRes = do
  let interimResBS = map encJToLBS allRes
  traceM "OUR INTERIM RESULT"
  traceM $ show interimResBS
  interimRes <- forM interimResBS $ \res -> do
    let obj = J.decode res :: (Maybe J.Object)
    onNothing obj $ do
      traceM $ show res
      throw500 "could not parse response as JSON"

  -- TODO: the order is not guaranteed! should we have a orderedmap?
  let datas = onlyObjs $ mapMaybe (Map.lookup "data") interimRes
      errs  = mapMaybe (Map.lookup "errors") interimRes
  return (Map.unions datas, errs)
  where
    -- TODO: should validate response and throw error?
    onlyObjs jVals =
      let fn jVal = case jVal of
            J.Object o -> Just o
            _          -> Nothing
      in mapMaybe fn jVals
