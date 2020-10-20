-- | The legacy RQL query ('/v1/query') for backward compatibility.
-- It is executed only when single postgres source is defined.
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE UndecidableInstances #-}
module Hasura.Server.API.V1Query where

import           Control.Lens                       hiding ((.=))
import           Control.Monad.Trans.Control        (MonadBaseControl)
import           Control.Monad.Unique
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Time                          (UTCTime)

import qualified Data.Environment                   as Env
import qualified Data.HashMap.Strict                as HM
import qualified Data.Text                          as T
import qualified Database.PG.Query                  as Q

import           Hasura.Class
import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.Action
import           Hasura.RQL.DDL.ComputedField
import           Hasura.RQL.DDL.CustomTypes
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
import           Hasura.RQL.DML.Update
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Run
import           Hasura.Server.Types
import           Hasura.Server.Version              (HasVersion)

import qualified Hasura.Tracing                     as Tracing

-- | The version integer
data APIVersion
  = VIVersion1
  | VIVersion2
  deriving (Show, Eq)

instance ToJSON APIVersion where
  toJSON VIVersion1 = toJSON @Int 1
  toJSON VIVersion2 = toJSON @Int 2

instance FromJSON APIVersion where
  parseJSON v = do
    verInt :: Int <- parseJSON v
    case verInt of
      1 -> return VIVersion1
      2 -> return VIVersion2
      i -> fail $ "expected 1 or 2, encountered " ++ show i

-- | Monad to execute the `/v1/query` API
newtype Run m a = Run {unRun :: BaseRunT (LazyTxT QErr m) a}
  deriving ( Functor, Applicative, Monad
           , MonadError QErr
           , MonadReader RunCtx
           , MonadIO
           , MonadTx
           , MonadBase IO
           , MonadUnique
           , MonadMetadata
           , UserInfoM
           , HasHttpManager
           , HasSQLGenCtx
           )

deriving instance (MonadIO m, MonadBaseControl IO m) => MonadBaseControl IO (Run m)

instance MonadTrans Run where
  lift = Run . BaseRunT . lift . lift . lift

executeMetadataRun :: (Monad m) => MetadataStorageT m a -> Run m a
executeMetadataRun m =
  liftEitherM $ lift $ runMetadataStorageT m

instance (MonadMetadataStorage m) => MonadScheduledEvents (Run m) where
  createEvent                = executeMetadataRun . insertScheduledEvent
  dropFutureCronEvents       = executeMetadataRun . clearFutureCronEvents
  fetchInvocations a b       = executeMetadataRun $ getInvocations a b
  fetchScheduledEvents ev    = executeMetadataRun $ do
    let totalCountToJSON WithTotalCount{..} =
          object ["count" .= _wtcCount, "events" .= _wtcData]
    case _gseScheduledEvent ev of
      SEOneOff    -> totalCountToJSON <$> getOneOffScheduledEvents (_gsePagination ev) (_gseStatus ev)
      SECron name -> totalCountToJSON <$> getCronEvents name (_gsePagination ev) (_gseStatus ev)
  dropEvent a b              = executeMetadataRun $ deleteScheduledEvent a b

-- peelRun
--   :: (MonadIO m)
--   => SchemaCacheRef
--   -> InstanceId
--   -> L.Logger L.Hasura
--   -> PGSourceConfig
--   -> Q.TxAccess
--   -> Maybe Tracing.TraceContext
--   -> Bool
--   -> RunCtx
--   -> Metadata
--   -> Run m (a, RebuildableSchemaCache, CacheInvalidations)
--   -> ExceptT QErr m a
-- peelRun cacheRef instanceId pgSourceConfig accessMode maybeTraceCtx
--   queryChangeMetadata runCtx metadata (Run m) =
--   runLazyTx (_pscExecCtx pgSourceConfig) accessMode
--   . withUserInfo userInfo
--   . maybe id withTraceContext maybeTraceCtx $
--     withSCUpdate cacheRef instanceId logger
--     ((r, rsc, ci), meta) <- runBaseRunT runCtx metadata m
--     if queryChangeMetadata then
--       withSCUpdate cacheRef instanceId
--   runL
--   runBaseRunT runCtx metadata m
--   where
--     userInfo = _rcUserInfo runCtx

data RQLQueryV1
  = RQAddExistingTableOrView !TrackTable
  | RQTrackTable !TrackTable
  | RQUntrackTable !UntrackTable
  | RQSetTableIsEnum !SetTableIsEnum

  | RQTrackFunction !TrackFunction
  | RQUntrackFunction !UnTrackFunction

  | RQCreateObjectRelationship !CreateObjRel
  | RQCreateArrayRelationship !CreateArrRel
  | RQDropRelationship !DropRel
  | RQSetRelationshipComment !SetRelComment
  | RQRenameRelationship !RenameRel

  -- computed fields related
  | RQAddComputedField !AddComputedField
  | RQDropComputedField !DropComputedField

  | RQCreateRemoteRelationship !RemoteRelationship
  | RQUpdateRemoteRelationship !RemoteRelationship
  | RQDeleteRemoteRelationship !DeleteRemoteRelationship

  | RQCreateInsertPermission !CreateInsPerm
  | RQCreateSelectPermission !CreateSelPerm
  | RQCreateUpdatePermission !CreateUpdPerm
  | RQCreateDeletePermission !CreateDelPerm

  | RQDropInsertPermission !(DropPerm InsPerm)
  | RQDropSelectPermission !(DropPerm SelPerm)
  | RQDropUpdatePermission !(DropPerm UpdPerm)
  | RQDropDeletePermission !(DropPerm DelPerm)
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

  | RQReplaceMetadata !MetadataNoSources
  | RQExportMetadata !ExportMetadata
  | RQClearMetadata !ClearMetadata
  | RQReloadMetadata !ReloadMetadata

  | RQCreateAction !CreateAction
  | RQDropAction !DropAction
  | RQUpdateAction !UpdateAction
  | RQCreateActionPermission !CreateActionPermission
  | RQDropActionPermission !DropActionPermission

  | RQDumpInternalState !DumpInternalState

  | RQSetCustomTypes !CustomTypes
  deriving (Show, Eq)

data RQLQueryV2
  = RQV2TrackTable !TrackTableV2
  | RQV2SetTableCustomFields !SetTableCustomFields
  | RQV2TrackFunction !TrackFunctionV2
  deriving (Show, Eq)

data RQLQuery
  = RQV1 !RQLQueryV1
  | RQV2 !RQLQueryV2
  deriving (Show, Eq)

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

fetchLastUpdate :: Q.TxE QErr (Maybe (InstanceId, UTCTime, CacheInvalidations))
fetchLastUpdate = over (_Just._3) Q.getAltJ <$> Q.withQE defaultTxErrorHandler [Q.sql|
  SELECT instance_id::text, occurred_at, invalidations
  FROM hdb_catalog.hdb_schema_update_event
  ORDER BY occurred_at DESC LIMIT 1
  |] () True

recordSchemaUpdate :: InstanceId -> CacheInvalidations -> Q.TxE QErr ()
recordSchemaUpdate instanceId invalidations =
  liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
             INSERT INTO hdb_catalog.hdb_schema_update_event
               (instance_id, occurred_at, invalidations) VALUES ($1::uuid, DEFAULT, $2::json)
             ON CONFLICT ((occurred_at IS NOT NULL))
             DO UPDATE SET instance_id = $1::uuid, occurred_at = DEFAULT, invalidations = $2::json
            |] (instanceId, Q.AltJ invalidations) True

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

  RQCreateAction _                -> True
  RQDropAction _                  -> True
  RQUpdateAction _                -> True
  RQCreateActionPermission _      -> True
  RQDropActionPermission _        -> True

  RQDumpInternalState _           -> False
  RQSetCustomTypes _              -> True

  RQBulk qs                       -> any queryModifiesSchemaCache qs

queryModifiesSchemaCache (RQV2 qi) = case qi of
  RQV2TrackTable _           -> True
  RQV2SetTableCustomFields _ -> True
  RQV2TrackFunction _        -> True

getQueryAccessMode :: (MonadError QErr m) => RQLQuery -> m Q.TxAccess
getQueryAccessMode q = (fromMaybe Q.ReadOnly) <$> getQueryAccessMode' q
  where
    getQueryAccessMode' ::
         (MonadError QErr m) => RQLQuery -> m (Maybe Q.TxAccess)
    getQueryAccessMode' (RQV1 q') =
      case q' of
        RQSelect _ -> pure Nothing
        RQCount _ -> pure Nothing
        RQRunSql RunSQL {_rTxAccessMode} -> pure $ Just _rTxAccessMode
        RQBulk qs -> foldM reconcileAccessModeWith Nothing (zip [0 :: Integer ..] qs)
        _ -> pure $ Just Q.ReadWrite
      where
        reconcileAccessModeWith expectedMode (i, query) = do
          queryMode <- getQueryAccessMode' query
          onLeft (reconcileAccessModes expectedMode queryMode) $ \errMode ->
            throw400 BadRequest $
            "incompatible access mode requirements in bulk query, " <>
            "expected access mode: " <>
            (T.pack $ maybe "ANY" show expectedMode) <>
            " but " <>
            "$.args[" <>
            (T.pack $ show i) <>
            "] forces " <>
            (T.pack $ show errMode)
    getQueryAccessMode' (RQV2 _) = pure $ Just Q.ReadWrite

-- | onRight, return reconciled access mode. onLeft, return conflicting access mode
reconcileAccessModes :: Maybe Q.TxAccess -> Maybe Q.TxAccess -> Either Q.TxAccess (Maybe Q.TxAccess)
reconcileAccessModes Nothing mode = pure mode
reconcileAccessModes mode Nothing = pure mode
reconcileAccessModes (Just mode1) (Just mode2)
  | mode1 == mode2 = pure $ Just mode1
  | otherwise = Left mode2

runQueryM
  :: ( HasVersion, QErrM m, CacheRWM m, UserInfoM m, MonadTx m
     , MonadIO m, MonadUnique m, HasHttpManager m, HasSQLGenCtx m
     , MonadScheduledEvents m
     , MonadMetadata m
     , Tracing.MonadTrace m
     )
  => Env.Environment
  -> SourceName
  -> RQLQuery
  -> m EncJSON
runQueryM env source rq = withPathK "args" $ case rq of
  RQV1 q -> runQueryV1M q
  RQV2 q -> runQueryV2M q
  where
    runQueryV1M = \case
      RQAddExistingTableOrView q   -> runTrackTableQ q
      RQTrackTable q               -> runTrackTableQ q
      RQUntrackTable q             -> runUntrackTableQ q
      RQSetTableIsEnum q           -> runSetExistingTableIsEnumQ q

      RQTrackFunction q            -> runTrackFunc q
      RQUntrackFunction q          -> runUntrackFunc q

      RQCreateObjectRelationship q -> runCreateRelationship ObjRel q
      RQCreateArrayRelationship  q -> runCreateRelationship ArrRel q
      RQDropRelationship  q        -> runDropRel q
      RQSetRelationshipComment  q  -> runSetRelComment q
      RQRenameRelationship q       -> runRenameRel q

      RQAddComputedField q        -> runAddComputedField q
      RQDropComputedField q       -> runDropComputedField q

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

      RQInsert q                   -> runInsert env source q
      RQSelect q                   -> runSelect source q
      RQUpdate q                   -> runUpdate env source q
      RQDelete q                   -> runDelete env source q
      RQCount  q                   -> runCount source q

      RQAddRemoteSchema    q       -> runAddRemoteSchema env q
      RQRemoveRemoteSchema q       -> runRemoveRemoteSchema q
      RQReloadRemoteSchema q       -> runReloadRemoteSchema q
      RQIntrospectRemoteSchema q   -> runIntrospectRemoteSchema q

      RQCreateRemoteRelationship q -> runCreateRemoteRelationship q
      RQUpdateRemoteRelationship q -> runUpdateRemoteRelationship q
      RQDeleteRemoteRelationship q -> runDeleteRemoteRelationship q

      RQCreateEventTrigger q       -> runCreateEventTriggerQuery q
      RQDeleteEventTrigger q       -> runDeleteEventTriggerQuery q
      RQRedeliverEvent q           -> runRedeliverEvent q
      RQInvokeEventTrigger q       -> runInvokeEventTrigger q

      RQCreateCronTrigger q      -> runCreateCronTrigger q
      RQDeleteCronTrigger q      -> runDeleteCronTrigger q

      RQCreateScheduledEvent q   -> runCreateScheduledEvent q

      RQCreateQueryCollection q        -> runCreateCollection q
      RQDropQueryCollection q          -> runDropCollection q
      RQAddQueryToCollection q         -> runAddQueryToCollection q
      RQDropQueryFromCollection q      -> runDropQueryFromCollection q
      RQAddCollectionToAllowlist q     -> runAddCollectionToAllowlist q
      RQDropCollectionFromAllowlist q  -> runDropCollectionFromAllowlist q

      RQReplaceMetadata q          -> runReplaceMetadata $ RMWithoutSources q
      RQClearMetadata q            -> runClearMetadata q
      RQExportMetadata q           -> runExportMetadata q
      RQReloadMetadata q           -> runReloadMetadata q

      RQCreateAction q           -> runCreateAction q
      RQDropAction q             -> runDropAction q
      RQUpdateAction q           -> runUpdateAction q
      RQCreateActionPermission q -> runCreateActionPermission q
      RQDropActionPermission q   -> runDropActionPermission q

      RQDumpInternalState q        -> runDumpInternalState q

      RQRunSql q                   -> runRunSQL source q

      RQSetCustomTypes q           -> runSetCustomTypes q

      RQBulk qs                    -> encJFromList <$> indexedMapM (runQueryM env source) qs

    runQueryV2M = \case
      RQV2TrackTable q           -> runTrackTableV2Q q
      RQV2SetTableCustomFields q -> runSetTableCustomFieldsQV2 q
      RQV2TrackFunction q        -> runTrackFunctionV2 q


requiresAdmin :: RQLQuery -> Bool
requiresAdmin = \case
  RQV1 q -> case q of
    RQAddExistingTableOrView _      -> True
    RQTrackTable _                  -> True
    RQUntrackTable _                -> True
    RQSetTableIsEnum _              -> True

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
