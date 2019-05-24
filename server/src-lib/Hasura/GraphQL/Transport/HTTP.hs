module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  ) where

import qualified Data.Aeson                             as J
import qualified Data.Aeson.Casing                      as J
import qualified Data.Aeson.TH                          as J
import qualified Data.ByteString.Lazy                   as BL
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N

import           Hasura.EncJSON
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude
import           Hasura.RQL.Types

import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.GraphQL.Execute.Query           as EQ
import qualified Hasura.Logging                         as L

runGQ
  :: (MonadIO m, MonadError QErr m)
  => PGExecCtx
  -> L.Logger
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
runGQ pgExecCtx logger userInfo sqlGenCtx enableAL planCache sc scVer
  manager reqHdrs req rawReq = do
  execPlan <- E.getResolvedExecPlan pgExecCtx planCache
              userInfo sqlGenCtx enableAL sc scVer req
  case execPlan of
    E.GExPHasura resolvedOp ->
      runHasuraGQ pgExecCtx logger userInfo resolvedOp
    E.GExPRemote rsi opDef  ->
      E.execRemoteGQ manager userInfo reqHdrs rawReq rsi opDef

runHasuraGQ
  :: (MonadIO m, MonadError QErr m)
  => PGExecCtx
  -> L.Logger
  -> UserInfo
  -> E.ExecOp
  -> m EncJSON
runHasuraGQ pgExecCtx logger userInfo resolvedOp = do
  respE <- liftIO $ runExceptT $ case resolvedOp of
    E.ExOpQuery tx genSql  -> do
      -- log the generated SQL
      onJust genSql $ \genSql' ->
        liftIO $ logQueryDetails logger $ mkQueryLog genSql'
      runLazyTx' pgExecCtx tx
    E.ExOpMutation tx ->
      runLazyTx pgExecCtx $ withUserInfo userInfo tx
    E.ExOpSubs _ ->
      throw400 UnexpectedPayload
      "subscriptions are not supported over HTTP, use websockets instead"
  resp <- liftEither respE
  return $ encodeGQResp $ GQSuccess $ encJToLBS resp

logQueryDetails :: (MonadIO m) => L.Logger -> QueryLog -> m ()
logQueryDetails logger = liftIO . L.unLogger logger

mkQueryLog :: EQ.GeneratedSql -> QueryLog
mkQueryLog sql = QueryLog Nothing (Just $ EQ.encodeSql sql)

data QueryLog
  = QueryLog
  { _qlQuery        :: !(Maybe GQLReqUnparsed)
  , _qlGeneratedSql :: !(Maybe J.Value)
  }
$(J.deriveToJSON (J.aesonDrop 3 J.snakeCase) ''QueryLog)

instance L.ToEngineLog QueryLog where
  toEngineLog ql = (L.LevelInfo, "query-log", J.toJSON ql)
