-- | The RQL metadata query ('/v2/query')
module Hasura.Server.API.QueryV2 where

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.Schema
import           Hasura.RQL.DML.Count
import           Hasura.RQL.DML.Delete
import           Hasura.RQL.DML.Insert
import           Hasura.RQL.DML.Select
import           Hasura.RQL.DML.Update
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Run
import           Hasura.Server.Version (HasVersion)
import           Hasura.Session

import qualified Hasura.Tracing        as Tracing

import           Control.Lens          (makePrisms, (^?))
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH

import qualified Data.Environment      as Env
import qualified Database.PG.Query     as Q
import qualified Network.HTTP.Client   as HTTP

data RQLQuery
  = RQInsert !InsertQuery
  | RQSelect !SelectQuery
  | RQUpdate !UpdateQuery
  | RQDelete !DeleteQuery
  | RQCount  !CountQuery

  | RQRunSql !RunSQL

$(deriveJSON
  defaultOptions { constructorTagModifier = snakeCase . drop 2
                 , sumEncoding = TaggedObject "type" "args"
                 }
  ''RQLQuery)
$(makePrisms ''RQLQuery)

runQuery
  :: ( HasVersion
     , MonadIO m
     , MonadError QErr m
     , Tracing.MonadTrace m
     )
  => Env.Environment
  -> UserInfo
  -> HTTP.Manager
  -> SQLGenCtx
  -> PGExecCtx
  -> RebuildableSchemaCache Run
  -> Metadata
  -> RQLQuery
  -> m EncJSON
runQuery env userInfo httpManager sqlGenCtx pgExecCtx schemaCache metadata request = do
  traceCtx <- Tracing.currentContext
  let accessMode = fromMaybe Q.ReadWrite $ request ^? _RQRunSql.rTxAccessMode
  runQueryM env request & Tracing.interpTraceT \x -> do
    a <- x & runCacheRWT schemaCache
           & peelRun (RunCtx userInfo httpManager sqlGenCtx) pgExecCtx accessMode (Just traceCtx) metadata
           & runExceptT
    liftEither a <&> \(((r, tracemeta), _, _), _) -> (r, tracemeta)

runQueryM
  :: ( HasVersion
     , MonadIO m
     , UserInfoM m
     , CacheRWM m
     , MonadTx m
     , HasSQLGenCtx m
     , Tracing.MonadTrace m
     )
  => Env.Environment -> RQLQuery -> m EncJSON
runQueryM env = \case
  RQInsert q -> runInsert env defaultSource q
  RQSelect q -> runSelect defaultSource q
  RQUpdate q -> runUpdate env defaultSource q
  RQDelete q -> runDelete env defaultSource q
  RQCount  q -> runCount defaultSource q
  RQRunSql q -> runRunSQL q
