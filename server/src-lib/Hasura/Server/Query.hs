module Hasura.Server.Query where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Time                          (UTCTime)
import           Language.Haskell.TH.Syntax         (Lift)
import qualified Network.HTTP.Client                as HTTP

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.EventTrigger
import           Hasura.RQL.DDL.Metadata
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.DDL.QueryCollection
import           Hasura.RQL.DDL.QueryTemplate
import           Hasura.RQL.DDL.Relationship
import           Hasura.RQL.DDL.Relationship.Rename
import           Hasura.RQL.DDL.RemoteSchema
import           Hasura.RQL.DDL.Schema.Function
import           Hasura.RQL.DDL.Schema.Table
import           Hasura.RQL.DML.Count
import           Hasura.RQL.DML.Delete
import           Hasura.RQL.DML.Insert
import           Hasura.RQL.DML.QueryTemplate
import           Hasura.RQL.DML.Select
import           Hasura.RQL.DML.Update
import           Hasura.RQL.Types
import           Hasura.Server.Init                 (InstanceId (..))
import           Hasura.Server.Utils

import qualified Database.PG.Query                  as Q

data RQLQuery
  = RQAddExistingTableOrView !TrackTable
  | RQTrackTable !TrackTable
  | RQUntrackTable !UntrackTable

  | RQTrackFunction !TrackFunction
  | RQUntrackFunction !UnTrackFunction

  | RQCreateObjectRelationship !CreateObjRel
  | RQCreateArrayRelationship !CreateArrRel
  | RQDropRelationship !DropRel
  | RQSetRelationshipComment !SetRelComment
  | RQRenameRelationship !RenameRel

  | RQCreateInsertPermission !CreateInsPerm
  | RQCreateSelectPermission !CreateSelPerm
  | RQCreateUpdatePermission !CreateUpdPerm
  | RQCreateDeletePermission !CreateDelPerm

  | RQDropInsertPermission !DropInsPerm
  | RQDropSelectPermission !DropSelPerm
  | RQDropUpdatePermission !DropUpdPerm
  | RQDropDeletePermission !DropDelPerm
  | RQSetPermissionComment !SetPermComment

  | RQGetInconsistentMetadata !GetInconsistentMetadata
  | RQDropInconsistentMetadata !DropInconsistentMetadata

  | RQInsert !InsertQuery
  | RQSelect !SelectQuery
  | RQUpdate !UpdateQuery
  | RQDelete !DeleteQuery
  | RQCount !CountQuery
  | RQBulk ![RQLQuery]

  -- schema-stitching, custom resolver related
  | RQAddRemoteSchema !AddRemoteSchemaQuery
  | RQRemoveRemoteSchema !RemoteSchemaNameQuery
  | RQReloadRemoteSchema !RemoteSchemaNameQuery

  | RQCreateEventTrigger !CreateEventTriggerQuery
  | RQDeleteEventTrigger !DeleteEventTriggerQuery
  | RQRedeliverEvent     !RedeliverEventQuery
  | RQInvokeEventTrigger !InvokeEventTriggerQuery

  | RQCreateQueryTemplate !CreateQueryTemplate
  | RQDropQueryTemplate !DropQueryTemplate
  | RQExecuteQueryTemplate !ExecQueryTemplate
  | RQSetQueryTemplateComment !SetQueryTemplateComment

  -- query collections, allow list related
  | RQCreateQueryCollection !CreateCollection
  | RQDropQueryCollection !DropCollection
  | RQAddQueryToCollection !AddQueryToCollection
  | RQDropQueryFromCollection !DropQueryFromCollection
  | RQAddCollectionToAllowlist !CollectionReq
  | RQDropCollectionFromAllowlist !CollectionReq

  | RQRunSql !RunSQL

  | RQReplaceMetadata !ReplaceMetadata
  | RQExportMetadata !ExportMetadata
  | RQClearMetadata !ClearMetadata
  | RQReloadMetadata !ReloadMetadata

  | RQDumpInternalState !DumpInternalState
  deriving (Show, Eq, Lift)

$(deriveJSON
  defaultOptions { constructorTagModifier = snakeCase . drop 2
                 , sumEncoding = TaggedObject "type" "args"
                 }
  ''RQLQuery)

newtype Run a
  = Run {unRun :: StateT SchemaCache (ReaderT (UserInfo, HTTP.Manager, SQLGenCtx) (LazyTx QErr)) a}
  deriving ( Functor, Applicative, Monad
           , MonadError QErr
           , MonadState SchemaCache
           , MonadReader (UserInfo, HTTP.Manager, SQLGenCtx)
           , CacheRM
           , CacheRWM
           , MonadTx
           , MonadIO
           )

instance UserInfoM Run where
  askUserInfo = asks _1

instance HasHttpManager Run where
  askHttpManager = asks _2

instance HasSQLGenCtx Run where
  askSQLGenCtx = asks _3

fetchLastUpdate :: Q.TxE QErr (Maybe (InstanceId, UTCTime))
fetchLastUpdate = do
  l <- Q.listQE defaultTxErrorHandler
    [Q.sql|
       SELECT instance_id::text, occurred_at
       FROM hdb_catalog.hdb_schema_update_event
       ORDER BY occurred_at DESC LIMIT 1
          |] () True
  case l of
    []           -> return Nothing
    [(instId, occurredAt)] ->
      return $ Just (InstanceId instId, occurredAt)
    -- never happens
    _            -> throw500 "more than one row returned by query"

recordSchemaUpdate :: InstanceId -> Q.TxE QErr ()
recordSchemaUpdate instanceId =
  liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
             INSERT INTO
                  hdb_catalog.hdb_schema_update_event
                  (instance_id, occurred_at)
             VALUES ($1::uuid, DEFAULT)
            |] (Identity $ getInstanceId instanceId) True

peelRun
  :: SchemaCache
  -> UserInfo
  -> HTTP.Manager
  -> SQLGenCtx
  -> PGExecCtx
  -> Run a -> ExceptT QErr IO (a, SchemaCache)
peelRun sc userInfo httMgr sqlGenCtx pgExecCtx (Run m) =
  runLazyTx pgExecCtx $ withUserInfo userInfo lazyTx
  where
    lazyTx = runReaderT (runStateT m sc) (userInfo, httMgr, sqlGenCtx)

runQuery
  :: (MonadIO m, MonadError QErr m)
  => PGExecCtx -> InstanceId
  -> UserInfo -> SchemaCache -> HTTP.Manager
  -> SQLGenCtx -> RQLQuery -> m (EncJSON, SchemaCache)
runQuery pgExecCtx instanceId userInfo sc hMgr sqlGenCtx query = do
  resE <- liftIO $ runExceptT $
    peelRun sc userInfo hMgr sqlGenCtx pgExecCtx $ runQueryM query
  either throwError withReload resE
  where
    withReload r = do
      when (queryNeedsReload query) $ do
        e <- liftIO $ runExceptT $ runLazyTx pgExecCtx
             $ liftTx $ recordSchemaUpdate instanceId
        liftEither e
      return r

queryNeedsReload :: RQLQuery -> Bool
queryNeedsReload qi = case qi of
  RQAddExistingTableOrView _      -> True
  RQTrackTable _                  -> True
  RQUntrackTable _                -> True
  RQTrackFunction _               -> True
  RQUntrackFunction _             -> True

  RQCreateObjectRelationship _    -> True
  RQCreateArrayRelationship  _    -> True
  RQDropRelationship  _           -> True
  RQSetRelationshipComment  _     -> False
  RQRenameRelationship _          -> True

  RQCreateInsertPermission _      -> True
  RQCreateSelectPermission _      -> True
  RQCreateUpdatePermission _      -> True
  RQCreateDeletePermission _      -> True

  RQDropInsertPermission _        -> True
  RQDropSelectPermission _        -> True
  RQDropUpdatePermission _        -> True
  RQDropDeletePermission _        -> True
  RQSetPermissionComment _        -> False

  RQGetInconsistentMetadata _     -> False
  RQDropInconsistentMetadata _    -> True

  RQInsert _                      -> False
  RQSelect _                      -> False
  RQUpdate _                      -> False
  RQDelete _                      -> False
  RQCount _                       -> False

  RQAddRemoteSchema _             -> True
  RQRemoveRemoteSchema _          -> True
  RQReloadRemoteSchema _          -> True

  RQCreateEventTrigger _          -> True
  RQDeleteEventTrigger _          -> True
  RQRedeliverEvent _              -> False
  RQInvokeEventTrigger _          -> False

  RQCreateQueryTemplate _         -> True
  RQDropQueryTemplate _           -> True
  RQExecuteQueryTemplate _        -> False
  RQSetQueryTemplateComment _     -> False

  RQCreateQueryCollection _       -> True
  RQDropQueryCollection _         -> True
  RQAddQueryToCollection _        -> True
  RQDropQueryFromCollection _     -> True
  RQAddCollectionToAllowlist _    -> True
  RQDropCollectionFromAllowlist _ -> True

  RQRunSql _                      -> True

  RQReplaceMetadata _             -> True
  RQExportMetadata _              -> False
  RQClearMetadata _               -> True
  RQReloadMetadata _              -> True

  RQDumpInternalState _           -> False

  RQBulk qs                       -> any queryNeedsReload qs

runQueryM
  :: ( QErrM m, CacheRWM m, UserInfoM m, MonadTx m
     , MonadIO m, HasHttpManager m, HasSQLGenCtx m
     )
  => RQLQuery
  -> m EncJSON
runQueryM rq =
  withPathK "args" $ runQueryM' <* rebuildGCtx
  where
    rebuildGCtx = when (queryNeedsReload rq) buildGCtxMap

    runQueryM' = case rq of
      RQAddExistingTableOrView q   -> runTrackTableQ q
      RQTrackTable q               -> runTrackTableQ q
      RQUntrackTable q             -> runUntrackTableQ q

      RQTrackFunction q            -> runTrackFunc q
      RQUntrackFunction q          -> runUntrackFunc q

      RQCreateObjectRelationship q -> runCreateObjRel q
      RQCreateArrayRelationship  q -> runCreateArrRel q
      RQDropRelationship  q        -> runDropRel q
      RQSetRelationshipComment  q  -> runSetRelComment q
      RQRenameRelationship q       -> runRenameRel q

      RQCreateInsertPermission q   -> runCreatePerm q
      RQCreateSelectPermission q   -> runCreatePerm q
      RQCreateUpdatePermission q   -> runCreatePerm q
      RQCreateDeletePermission q   -> runCreatePerm q

      RQDropInsertPermission q     -> runDropPerm q
      RQDropSelectPermission q     -> runDropPerm q
      RQDropUpdatePermission q     -> runDropPerm q
      RQDropDeletePermission q     -> runDropPerm q
      RQSetPermissionComment q     -> runSetPermComment q

      RQGetInconsistentMetadata q  -> runGetInconsistentMetadata q
      RQDropInconsistentMetadata q -> runDropInconsistentMetadata q

      RQInsert q                   -> runInsert q
      RQSelect q                   -> runSelect q
      RQUpdate q                   -> runUpdate q
      RQDelete q                   -> runDelete q
      RQCount  q                   -> runCount q

      RQAddRemoteSchema    q       -> runAddRemoteSchema q
      RQRemoveRemoteSchema q       -> runRemoveRemoteSchema q
      RQReloadRemoteSchema q       -> runReloadRemoteSchema q

      RQCreateEventTrigger q       -> runCreateEventTriggerQuery q
      RQDeleteEventTrigger q       -> runDeleteEventTriggerQuery q
      RQRedeliverEvent q           -> runRedeliverEvent q
      RQInvokeEventTrigger q       -> runInvokeEventTrigger q

      RQCreateQueryTemplate q      -> runCreateQueryTemplate q
      RQDropQueryTemplate q        -> runDropQueryTemplate q
      RQExecuteQueryTemplate q     -> runExecQueryTemplate q
      RQSetQueryTemplateComment q  -> runSetQueryTemplateComment q

      RQCreateQueryCollection q        -> runCreateCollection q
      RQDropQueryCollection q          -> runDropCollection q
      RQAddQueryToCollection q         -> runAddQueryToCollection q
      RQDropQueryFromCollection q      -> runDropQueryFromCollection q
      RQAddCollectionToAllowlist q     -> runAddCollectionToAllowlist q
      RQDropCollectionFromAllowlist q  -> runDropCollectionFromAllowlist q

      RQReplaceMetadata q          -> runReplaceMetadata q
      RQClearMetadata q            -> runClearMetadata q
      RQExportMetadata q           -> runExportMetadata q
      RQReloadMetadata q           -> runReloadMetadata q

      RQDumpInternalState q        -> runDumpInternalState q

      RQRunSql q                   -> runRunSQL q

      RQBulk qs                    -> encJFromList <$> indexedMapM runQueryM qs
