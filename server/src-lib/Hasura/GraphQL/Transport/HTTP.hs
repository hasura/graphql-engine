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
  case execPlan of
    E.GExPHasura gCtx rootSelSet ->
      runHasuraGQ pool isoL userInfo sqlGenCtx gCtx rootSelSet
    E.GExPRemote rsi opDef ->
      E.execRemoteGQ manager userInfo reqHdrs req rsi opDef
    E.GExPMixed plans ->
      runMixedGQ pool isoL userInfo sqlGenCtx manager reqHdrs req plans


runMixedGQ
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool
  -> Q.TxIsolation
  -> UserInfo
  -> SQLGenCtx
  -> HTTP.Manager
  -> [N.Header]
  -> GraphQLRequest
  -> [E.GQExecPlan]
  -> m EncJSON
runMixedGQ pool isoL userInfo sqlGenCtx manager reqHdrs req plans = do
  resSet <- forM plans $ \case
    E.GExPHasura gCtx rootSelSet ->
      runHasuraGQ pool isoL userInfo sqlGenCtx gCtx rootSelSet
    E.GExPRemote rsi opDef ->
      let newQ = E.transformGQRequest req opDef
      in E.execRemoteGQ manager userInfo reqHdrs newQ rsi opDef
    E.GExPMixed _ ->
      throw500 "internal-unexpected: mixed plan is nested in mixed plan"

  let interimResBS = map encJToLBS resSet
  liftIO $ print interimResBS
  interimRes <- forM interimResBS $ \res -> do
    let x = J.decode res :: (Maybe J.Object)
    onNothing x $ throw500 "could not parse response as JSON"

  let datas = mapMaybe (Map.lookup "data") interimRes
      errs  = mapMaybe (Map.lookup "errors") interimRes

  return $ encJFromJValue $ J.object [ "data" J..= datas
                                     , "errors" J..= errs
                                     ]

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
