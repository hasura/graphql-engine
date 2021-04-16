-- | The RQL query ('/v1/query')

module Hasura.Server.API.Query where

import           Hasura.Prelude

import qualified Data.Environment                   as Env
import qualified Data.HashMap.Strict                as HM
import qualified Database.PG.Query                  as Q
import qualified Network.HTTP.Client                as HTTP

import           Control.Monad.Trans.Control        (MonadBaseControl)
import           Control.Monad.Unique
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Network.HTTP.Client.Extended

import qualified Hasura.Tracing                     as Tracing

import           Hasura.EncJSON
import           Hasura.Metadata.Class
import           Hasura.RQL.DDL.Action
import           Hasura.RQL.DDL.ComputedField
import           Hasura.RQL.DDL.CustomTypes
import           Hasura.RQL.DDL.Endpoint
import           Hasura.RQL.DDL.EventTrigger
import           Hasura.RQL.DDL.Metadata
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.DDL.QueryCollection
import           Hasura.RQL.DDL.Relationship
import           Hasura.RQL.DDL.Relationship.Rename
import           Hasura.RQL.DDL.RemoteRelationship
import           Hasura.RQL.DDL.RemoteSchema
import           Hasura.RQL.DDL.ScheduledTrigger
import           Hasura.RQL.DDL.Schema
import           Hasura.RQL.DML.Count
import           Hasura.RQL.DML.Delete
import           Hasura.RQL.DML.Insert
import           Hasura.RQL.DML.Select
import           Hasura.RQL.DML.Types
import           Hasura.RQL.DML.Update
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Run
import           Hasura.Server.Types
import           Hasura.Server.Utils
import           Hasura.Server.Version              (HasVersion)
import           Hasura.Session

data RQLQueryV1
  = RQAddExistingTableOrView !(TrackTable 'Postgres)
  | RQTrackTable !(TrackTable 'Postgres)
  | RQUntrackTable !(UntrackTable 'Postgres)
  | RQSetTableIsEnum !SetTableIsEnum
  | RQSetTableCustomization !SetTableCustomization

  | RQTrackFunction !(TrackFunction 'Postgres)
  | RQUntrackFunction !(UnTrackFunction 'Postgres)

  | RQCreateObjectRelationship !(CreateObjRel 'Postgres)
  | RQCreateArrayRelationship !(CreateArrRel 'Postgres)
  | RQDropRelationship !(DropRel 'Postgres)
  | RQSetRelationshipComment !(SetRelComment 'Postgres)
  | RQRenameRelationship !(RenameRel 'Postgres)

  -- computed fields related
  | RQAddComputedField !(AddComputedField 'Postgres)
  | RQDropComputedField !(DropComputedField 'Postgres)

  | RQCreateRemoteRelationship !(RemoteRelationship 'Postgres)
  | RQUpdateRemoteRelationship !(RemoteRelationship 'Postgres)
  | RQDeleteRemoteRelationship !DeleteRemoteRelationship

  | RQCreateInsertPermission !(CreateInsPerm 'Postgres)
  | RQCreateSelectPermission !(CreateSelPerm 'Postgres)
  | RQCreateUpdatePermission !(CreateUpdPerm 'Postgres)
  | RQCreateDeletePermission !(CreateDelPerm 'Postgres)

  | RQDropInsertPermission !(DropPerm 'Postgres (InsPerm 'Postgres))
  | RQDropSelectPermission !(DropPerm 'Postgres (SelPerm 'Postgres))
  | RQDropUpdatePermission !(DropPerm 'Postgres (UpdPerm 'Postgres))
  | RQDropDeletePermission !(DropPerm 'Postgres (DelPerm 'Postgres))
  | RQSetPermissionComment !(SetPermComment 'Postgres)

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
  | RQIntrospectRemoteSchema !RemoteSchemaNameQuery

  | RQCreateEventTrigger !CreateEventTriggerQuery
  | RQDeleteEventTrigger !DeleteEventTriggerQuery
  | RQRedeliverEvent     !RedeliverEventQuery
  | RQInvokeEventTrigger !InvokeEventTriggerQuery

  -- scheduled triggers
  | RQCreateCronTrigger !CreateCronTrigger
  | RQDeleteCronTrigger !ScheduledTriggerName

  | RQCreateScheduledEvent !CreateScheduledEvent

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

  | RQCreateAction !CreateAction
  | RQDropAction !DropAction
  | RQUpdateAction !UpdateAction
  | RQCreateActionPermission !CreateActionPermission
  | RQDropActionPermission !DropActionPermission

  | RQCreateRestEndpoint !CreateEndpoint
  | RQDropRestEndpoint !DropEndpoint

  | RQDumpInternalState !DumpInternalState

  | RQSetCustomTypes !CustomTypes
  deriving (Eq)

data RQLQueryV2
  = RQV2TrackTable !(TrackTableV2 'Postgres)
  | RQV2SetTableCustomFields !SetTableCustomFields -- deprecated
  | RQV2TrackFunction !TrackFunctionV2
  | RQV2ReplaceMetadata !ReplaceMetadataV2
  deriving (Eq)

data RQLQuery
  = RQV1 !RQLQueryV1
  | RQV2 !RQLQueryV2
  deriving (Eq)

instance FromJSON RQLQuery where
  parseJSON = withObject "Object" $ \o -> do
    mVersion <- o .:? "version"
    let version = fromMaybe VIVersion1 mVersion
        val = Object o
    case version of
      VIVersion1 -> RQV1 <$> parseJSON val
      VIVersion2 -> RQV2 <$> parseJSON val

instance ToJSON RQLQuery where
  toJSON = \case
    RQV1 q -> embedVersion VIVersion1 $ toJSON q
    RQV2 q -> embedVersion VIVersion2 $ toJSON q
    where
      embedVersion version (Object o) =
        Object $ HM.insert "version" (toJSON version) o
      -- never happens since JSON value of RQL queries are always objects
      embedVersion _ _ = error "Unexpected: toJSON of RQL queries are not objects"

$(deriveJSON
  defaultOptions { constructorTagModifier = snakeCase . drop 2
                 , sumEncoding = TaggedObject "type" "args"
                 }
  ''RQLQueryV1)

$(deriveJSON
  defaultOptions { constructorTagModifier = snakeCase . drop 4
                 , sumEncoding = TaggedObject "type" "args"
                 , tagSingleConstructors = True
                 }
  ''RQLQueryV2
 )

runQuery
  :: ( HasVersion, MonadIO m, Tracing.MonadTrace m
     , MonadBaseControl IO m, MonadMetadataStorage m
     , MonadResolveSource m
     )
  => Env.Environment
  -> InstanceId
  -> UserInfo -> RebuildableSchemaCache -> HTTP.Manager
  -> ServerConfigCtx -> RQLQuery -> m (EncJSON, RebuildableSchemaCache)
runQuery env instanceId userInfo sc hMgr serverConfigCtx query = do
  (metadata, currentResourceVersion) <- fetchMetadata
  result <- runQueryM env query & Tracing.interpTraceT \x -> do
    (((js, tracemeta), meta), rsc, ci) <-
         x & runMetadataT metadata
           & runCacheRWT sc
           & peelRun runCtx
           & runExceptT
           & liftEitherM
    pure ((js, rsc, ci, meta), tracemeta)
  withReload currentResourceVersion result
  where
    runCtx = RunCtx userInfo hMgr serverConfigCtx

    withReload currentResourceVersion (result, updatedCache, invalidations, updatedMetadata) = do
      when (queryModifiesSchemaCache query) $ do
        case (_sccMaintenanceMode serverConfigCtx) of
          MaintenanceModeDisabled -> do
            -- set modified metadata in storage
            newResourceVersion <- setMetadata currentResourceVersion updatedMetadata
            -- notify schema cache sync
            notifySchemaCacheSync newResourceVersion instanceId invalidations
          MaintenanceModeEnabled ->
            throw500 "metadata cannot be modified in maintenance mode"
      pure (result, updatedCache)

-- | A predicate that determines whether the given query might modify/rebuild the schema cache. If
-- so, it needs to acquire the global lock on the schema cache so that other queries do not modify
-- it concurrently.
--
-- Ideally, we would enforce this using the type system — queries for which this function returns
-- 'False' should not be allowed to modify the schema cache. But for now we just ensure consistency
-- by hand.
queryModifiesSchemaCache :: RQLQuery -> Bool
queryModifiesSchemaCache (RQV1 qi) = case qi of
  RQAddExistingTableOrView _      -> True
  RQTrackTable _                  -> True
  RQUntrackTable _                -> True
  RQTrackFunction _               -> True
  RQUntrackFunction _             -> True
  RQSetTableIsEnum _              -> True

  RQCreateObjectRelationship _    -> True
  RQCreateArrayRelationship  _    -> True
  RQDropRelationship  _           -> True
  RQSetRelationshipComment  _     -> False
  RQRenameRelationship _          -> True

  RQAddComputedField _            -> True
  RQDropComputedField _           -> True

  RQCreateRemoteRelationship _    -> True
  RQUpdateRemoteRelationship _    -> True
  RQDeleteRemoteRelationship _    -> True

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
  RQIntrospectRemoteSchema _      -> False

  RQCreateEventTrigger _          -> True
  RQDeleteEventTrigger _          -> True
  RQRedeliverEvent _              -> False
  RQInvokeEventTrigger _          -> False

  RQCreateCronTrigger _           -> True
  RQDeleteCronTrigger _           -> True

  RQCreateScheduledEvent _        -> False

  RQCreateQueryCollection _       -> True
  RQDropQueryCollection _         -> True
  RQAddQueryToCollection _        -> True
  RQDropQueryFromCollection _     -> True
  RQAddCollectionToAllowlist _    -> True
  RQDropCollectionFromAllowlist _ -> True

  RQRunSql q                      -> isSchemaCacheBuildRequiredRunSQL q

  RQReplaceMetadata _             -> True
  RQExportMetadata _              -> False
  RQClearMetadata _               -> True
  RQReloadMetadata _              -> True

  RQCreateRestEndpoint _          -> True
  RQDropRestEndpoint _            -> True

  RQCreateAction _                -> True
  RQDropAction _                  -> True
  RQUpdateAction _                -> True
  RQCreateActionPermission _      -> True
  RQDropActionPermission _        -> True

  RQDumpInternalState _           -> False
  RQSetCustomTypes _              -> True
  RQSetTableCustomization _       -> True

  RQBulk qs                       -> any queryModifiesSchemaCache qs

queryModifiesSchemaCache (RQV2 qi) = case qi of
  RQV2TrackTable _           -> True
  RQV2SetTableCustomFields _ -> True
  RQV2TrackFunction _        -> True
  RQV2ReplaceMetadata _      -> True

getQueryAccessMode :: (MonadError QErr m) => RQLQuery -> m Q.TxAccess
getQueryAccessMode q = fromMaybe Q.ReadOnly <$> getQueryAccessMode' q
  where
    getQueryAccessMode' ::
         (MonadError QErr m) => RQLQuery -> m (Maybe Q.TxAccess)
    getQueryAccessMode' (RQV1 q') =
      case q' of
        RQSelect _ -> pure Nothing
        RQCount _ -> pure Nothing
        RQRunSql RunSQL {rTxAccessMode} -> pure $ Just rTxAccessMode
        RQBulk qs -> foldM reconcileAccessModeWith Nothing (zip [0 :: Integer ..] qs)
        _ -> pure $ Just Q.ReadWrite
      where
        reconcileAccessModeWith expectedMode (i, query) = do
          queryMode <- getQueryAccessMode' query
          onLeft (reconcileAccessModes expectedMode queryMode) $ \errMode ->
            throw400 BadRequest $
            "incompatible access mode requirements in bulk query, " <>
            "expected access mode: " <>
            maybe "ANY" tshow expectedMode <>
            " but " <>
            "$.args[" <>
            tshow i <>
            "] forces " <>
            tshow errMode
    getQueryAccessMode' (RQV2 _) = pure $ Just Q.ReadWrite

-- | onRight, return reconciled access mode. onLeft, return conflicting access mode
reconcileAccessModes :: Maybe Q.TxAccess -> Maybe Q.TxAccess -> Either Q.TxAccess (Maybe Q.TxAccess)
reconcileAccessModes Nothing mode = pure mode
reconcileAccessModes mode Nothing = pure mode
reconcileAccessModes (Just mode1) (Just mode2)
  | mode1 == mode2 = pure $ Just mode1
  | otherwise = Left mode2

runQueryM
  :: ( HasVersion, CacheRWM m, UserInfoM m
     , MonadBaseControl IO m, MonadIO m, MonadUnique m
     , HasHttpManagerM m
     , HasServerConfigCtx m
     , Tracing.MonadTrace m
     , MetadataM m
     , MonadMetadataStorageQueryAPI m
     )
  => Env.Environment
  -> RQLQuery
  -> m EncJSON
runQueryM env rq = withPathK "args" $ case rq of
  RQV1 q -> runQueryV1M q
  RQV2 q -> runQueryV2M q
  where
    runQueryV1M = \case
      RQAddExistingTableOrView q      -> runTrackTableQ q
      RQTrackTable q                  -> runTrackTableQ q
      RQUntrackTable q                -> runUntrackTableQ q
      RQSetTableIsEnum q              -> runSetExistingTableIsEnumQ q
      RQSetTableCustomization q       -> runSetTableCustomization q

      RQTrackFunction q               -> runTrackFunc q
      RQUntrackFunction q             -> runUntrackFunc q

      RQCreateObjectRelationship q    -> runCreateRelationship ObjRel q
      RQCreateArrayRelationship  q    -> runCreateRelationship ArrRel q
      RQDropRelationship  q           -> runDropRel q
      RQSetRelationshipComment  q     -> runSetRelComment q
      RQRenameRelationship q          -> runRenameRel q

      RQAddComputedField q            -> runAddComputedField q
      RQDropComputedField q           -> runDropComputedField q

      RQCreateInsertPermission q      -> runCreatePerm q
      RQCreateSelectPermission q      -> runCreatePerm q
      RQCreateUpdatePermission q      -> runCreatePerm q
      RQCreateDeletePermission q      -> runCreatePerm q

      RQDropInsertPermission q        -> runDropPerm q
      RQDropSelectPermission q        -> runDropPerm q
      RQDropUpdatePermission q        -> runDropPerm q
      RQDropDeletePermission q        -> runDropPerm q
      RQSetPermissionComment q        -> runSetPermComment q

      RQGetInconsistentMetadata q     -> runGetInconsistentMetadata q
      RQDropInconsistentMetadata q    -> runDropInconsistentMetadata q

      RQInsert q                      -> runInsert env q
      RQSelect q                      -> runSelect q
      RQUpdate q                      -> runUpdate env q
      RQDelete q                      -> runDelete env q
      RQCount  q                      -> runCount q

      RQAddRemoteSchema    q          -> runAddRemoteSchema env q
      RQRemoveRemoteSchema q          -> runRemoveRemoteSchema q
      RQReloadRemoteSchema q          -> runReloadRemoteSchema q
      RQIntrospectRemoteSchema q      -> runIntrospectRemoteSchema q

      RQCreateRemoteRelationship q    -> runCreateRemoteRelationship q
      RQUpdateRemoteRelationship q    -> runUpdateRemoteRelationship q
      RQDeleteRemoteRelationship q    -> runDeleteRemoteRelationship q

      RQCreateEventTrigger q          -> runCreateEventTriggerQuery q
      RQDeleteEventTrigger q          -> runDeleteEventTriggerQuery q
      RQRedeliverEvent q              -> runRedeliverEvent q
      RQInvokeEventTrigger q          -> runInvokeEventTrigger q

      RQCreateCronTrigger q           -> runCreateCronTrigger q
      RQDeleteCronTrigger q           -> runDeleteCronTrigger q

      RQCreateScheduledEvent q        -> runCreateScheduledEvent q

      RQCreateQueryCollection q       -> runCreateCollection q
      RQDropQueryCollection q         -> runDropCollection q
      RQAddQueryToCollection q        -> runAddQueryToCollection q
      RQDropQueryFromCollection q     -> runDropQueryFromCollection q
      RQAddCollectionToAllowlist q    -> runAddCollectionToAllowlist q
      RQDropCollectionFromAllowlist q -> runDropCollectionFromAllowlist q

      RQReplaceMetadata q             -> runReplaceMetadata q
      RQClearMetadata q               -> runClearMetadata q
      RQExportMetadata q              -> runExportMetadata q
      RQReloadMetadata q              -> runReloadMetadata q

      RQCreateAction q                -> runCreateAction q
      RQDropAction q                  -> runDropAction q
      RQUpdateAction q                -> runUpdateAction q
      RQCreateActionPermission q      -> runCreateActionPermission q
      RQDropActionPermission q        -> runDropActionPermission q

      RQCreateRestEndpoint q          -> runCreateEndpoint q
      RQDropRestEndpoint q            -> runDropEndpoint q

      RQDumpInternalState q           -> runDumpInternalState q

      RQRunSql q                      -> runRunSQL q

      RQSetCustomTypes q              -> runSetCustomTypes q

      RQBulk qs                       -> encJFromList <$> indexedMapM (runQueryM env) qs

    runQueryV2M = \case
      RQV2TrackTable q           -> runTrackTableV2Q q
      RQV2SetTableCustomFields q -> runSetTableCustomFieldsQV2 q
      RQV2TrackFunction q        -> runTrackFunctionV2 q
      RQV2ReplaceMetadata q      -> runReplaceMetadataV2 q

requiresAdmin :: RQLQuery -> Bool
requiresAdmin = \case
  RQV1 q -> case q of
    RQAddExistingTableOrView _      -> True
    RQTrackTable _                  -> True
    RQUntrackTable _                -> True
    RQSetTableIsEnum _              -> True
    RQSetTableCustomization _       -> True

    RQTrackFunction _               -> True
    RQUntrackFunction _             -> True

    RQCreateObjectRelationship _    -> True
    RQCreateArrayRelationship  _    -> True
    RQDropRelationship  _           -> True
    RQSetRelationshipComment  _     -> True
    RQRenameRelationship _          -> True

    RQAddComputedField _            -> True
    RQDropComputedField _           -> True

    RQCreateRemoteRelationship _    -> True
    RQUpdateRemoteRelationship _    -> True
    RQDeleteRemoteRelationship _    -> True

    RQCreateInsertPermission _      -> True
    RQCreateSelectPermission _      -> True
    RQCreateUpdatePermission _      -> True
    RQCreateDeletePermission _      -> True

    RQDropInsertPermission _        -> True
    RQDropSelectPermission _        -> True
    RQDropUpdatePermission _        -> True
    RQDropDeletePermission _        -> True
    RQSetPermissionComment _        -> True

    RQGetInconsistentMetadata _     -> True
    RQDropInconsistentMetadata _    -> True

    RQInsert _                      -> False
    RQSelect _                      -> False
    RQUpdate _                      -> False
    RQDelete _                      -> False
    RQCount  _                      -> False

    RQAddRemoteSchema    _          -> True
    RQRemoveRemoteSchema _          -> True
    RQReloadRemoteSchema _          -> True
    RQIntrospectRemoteSchema _      -> True

    RQCreateEventTrigger _          -> True
    RQDeleteEventTrigger _          -> True
    RQRedeliverEvent _              -> True
    RQInvokeEventTrigger _          -> True

    RQCreateCronTrigger _           -> True
    RQDeleteCronTrigger _           -> True

    RQCreateScheduledEvent _        -> True

    RQCreateQueryCollection _       -> True
    RQDropQueryCollection _         -> True
    RQAddQueryToCollection _        -> True
    RQDropQueryFromCollection _     -> True
    RQAddCollectionToAllowlist _    -> True
    RQDropCollectionFromAllowlist _ -> True

    RQReplaceMetadata _             -> True
    RQClearMetadata _               -> True
    RQExportMetadata _              -> True
    RQReloadMetadata _              -> True

    RQCreateRestEndpoint _          -> True
    RQDropRestEndpoint _            -> True

    RQCreateAction _                -> True
    RQDropAction _                  -> True
    RQUpdateAction _                -> True
    RQCreateActionPermission _      -> True
    RQDropActionPermission _        -> True

    RQDumpInternalState _           -> True
    RQSetCustomTypes _              -> True

    RQRunSql _                      -> True

    RQBulk qs                       -> any requiresAdmin qs

  RQV2 q -> case q of
    RQV2TrackTable _           -> True
    RQV2SetTableCustomFields _ -> True
    RQV2TrackFunction _        -> True
    RQV2ReplaceMetadata _      -> True
