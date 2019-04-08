module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  ) where

import qualified Database.PG.Query                      as Q
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N

import           Hasura.EncJSON
import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude
import           Hasura.RQL.Types

import qualified Data.Aeson                             as J
import qualified Data.HashMap.Lazy                      as Map
import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.GraphQL.Resolve                 as R
import qualified Hasura.GraphQL.Validate                as V


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
  let (E.GQExecPlan hasuraPlan remotePlans) = execPlan
  case (hasuraPlan, remotePlans) of
     (Nothing, []) -> throw500 "no exec plan found"
     (Just (E.GExPHasura gCtx rootSelSet), []) -> runHasuraGQ pool isoL userInfo sqlGenCtx gCtx rootSelSet
     (_, _) -> runMixedGQ pool isoL userInfo sqlGenCtx manager reqHdrs execPlan

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
  let (E.GQExecPlan hasuraPlan remotePlans) = plan
  hasuraRes <- case hasuraPlan of
    Nothing -> return []
    Just (E.GExPHasura gCtx rootSelSet) -> do
      res <- runHasuraGQ pool isoL userInfo sqlGenCtx gCtx rootSelSet
      return [res]

  remoteRes <- forM remotePlans $ \case
    E.GExPRemote rsi newq rs ->
      E.execRemoteGQ manager userInfo reqHdrs newq rsi rs

  let resSet = hasuraRes ++ remoteRes
      interimResBS = map encJToLBS resSet
  interimRes <- forM interimResBS $ \res -> do
    let x = J.decode res :: (Maybe J.Object)
    onNothing x $ throw500 "could not parse response as JSON"
  let datas = onlyObjs $ mapMaybe (Map.lookup "data") interimRes
      errs  = mapMaybe (Map.lookup "errors") interimRes

  return $ encJFromJValue $ J.object [ "data" J..= Map.unions datas
                                     , "errors" J..= errs
                                     ]

  where
    -- TODO: should validate response and throw error?
    onlyObjs jVals =
      let fn jVal = case jVal of
            J.Object o -> Just o
            _          -> Nothing
      in mapMaybe fn jVals

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
