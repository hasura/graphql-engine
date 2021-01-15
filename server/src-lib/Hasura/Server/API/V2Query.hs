-- | The RQL query ('/v2/query')
module Hasura.Server.API.V2Query where

import           Control.Lens
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH

import qualified Data.Environment            as Env
import qualified Network.HTTP.Client         as HTTP

import           Hasura.EncJSON
import           Hasura.Metadata.Class
import           Hasura.Prelude
import           Hasura.RQL.DDL.Schema
import           Hasura.RQL.DML.Count
import           Hasura.RQL.DML.Delete
import           Hasura.RQL.DML.Insert
import           Hasura.RQL.DML.Select
import           Hasura.RQL.DML.Types
import           Hasura.RQL.DML.Update
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Run
import           Hasura.Server.Types         (InstanceId (..))
import           Hasura.Server.Version       (HasVersion)
import           Hasura.Session

import qualified Hasura.Tracing              as Tracing

data RQLQuery
  = RQInsert !InsertQuery
  | RQSelect !SelectQuery
  | RQUpdate !UpdateQuery
  | RQDelete !DeleteQuery
  | RQCount  !CountQuery

  | RQRunSql !RunSQL
  | RQBulk ![RQLQuery]
  deriving (Show)

$(deriveJSON
  defaultOptions { constructorTagModifier = snakeCase . drop 2
                 , sumEncoding = TaggedObject "type" "args"
                 }
  ''RQLQuery)

queryNeedsAdmin :: RQLQuery -> Bool
queryNeedsAdmin = \case
  RQRunSql _ -> True
  RQBulk   l -> any queryNeedsAdmin l
  _          -> False

runQuery
  :: ( HasVersion
     , MonadIO m
     , MonadBaseControl IO m
     , Tracing.MonadTrace m
     , MonadMetadataStorage m
     , MonadResolveSource m
     )
  => Env.Environment
  -> InstanceId
  -> UserInfo
  -> RebuildableSchemaCache
  -> HTTP.Manager
  -> SQLGenCtx
  -> RemoteSchemaPermsCtx
  -> RQLQuery
  -> m (EncJSON, RebuildableSchemaCache)
runQuery env instanceId userInfo schemaCache httpManager sqlGenCtx remoteSchemaPermCtx rqlQuery = do
  metadata <- fetchMetadata
  result <- runQueryM env rqlQuery & Tracing.interpTraceT \x -> do
    (((js, tracemeta), meta), rsc, ci) <-
         x & runMetadataT metadata
           & runCacheRWT schemaCache
           & peelRun runCtx
           & runExceptT
           & liftEitherM
    pure ((js, rsc, ci, meta), tracemeta)
  withReload result
  where
    runCtx = RunCtx userInfo httpManager sqlGenCtx remoteSchemaPermCtx

    withReload (result, updatedCache, invalidations, updatedMetadata) = do
      when (queryModifiesSchema rqlQuery) $ do
        -- set modified metadata in storage
        setMetadata updatedMetadata
        -- notify schema cache sync
        notifySchemaCacheSync instanceId invalidations
      pure (result, updatedCache)

queryModifiesSchema :: RQLQuery -> Bool
queryModifiesSchema = \case
  RQRunSql q  -> isSchemaCacheBuildRequiredRunSQL q
  RQBulk l    -> any queryModifiesSchema l
  _           -> False

runQueryM
  :: ( HasVersion
     , MonadError QErr m
     , MonadIO m
     , MonadBaseControl IO m
     , UserInfoM m
     , CacheRWM m
     , HasSQLGenCtx m
     , Tracing.MonadTrace m
     , MetadataM m
     )
  => Env.Environment -> RQLQuery -> m EncJSON
runQueryM env = \case
  RQInsert q -> runInsert env q
  RQSelect q -> runSelect q
  RQUpdate q -> runUpdate env q
  RQDelete q -> runDelete env q
  RQCount  q -> runCount q
  RQRunSql q -> runRunSQL q
  RQBulk   l -> encJFromList <$> indexedMapM (runQueryM env) l
