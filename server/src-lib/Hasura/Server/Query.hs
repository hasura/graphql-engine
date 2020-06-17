{-# LANGUAGE NamedFieldPuns #-}

module Hasura.Server.Query where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Time                          (UTCTime)
import           Language.Haskell.TH.Syntax         (Lift)

import qualified Data.HashMap.Strict                as HM
import qualified Data.Text                          as T
import qualified Database.PG.Query                  as Q
import qualified Network.HTTP.Client                as HTTP

import           Hasura.EncJSON
import           Hasura.Prelude
-- import           Hasura.RQL.DDL.Action
import           Hasura.RQL.DDL.ComputedField
-- import           Hasura.RQL.DDL.CustomTypes
import           Hasura.RQL.DDL.EventTrigger
import           Hasura.RQL.DDL.Metadata
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.DDL.QueryCollection
import           Hasura.RQL.DDL.Relationship
import           Hasura.RQL.DDL.Relationship.Rename
import           Hasura.RQL.DDL.RemoteSchema
import           Hasura.RQL.DDL.Schema
import           Hasura.RQL.DML.Count
import           Hasura.RQL.DML.Delete
import           Hasura.RQL.DML.Insert
import           Hasura.RQL.DML.Select
import           Hasura.RQL.DML.Update
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Run
import           Hasura.Server.Init                 (InstanceId (..))
import           Hasura.Server.Utils
import           Hasura.Server.Version              (HasVersion)


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

  | RQCreateEventTrigger !CreateEventTriggerQuery
  | RQDeleteEventTrigger !DeleteEventTriggerQuery
  | RQRedeliverEvent     !RedeliverEventQuery
  | RQInvokeEventTrigger !InvokeEventTriggerQuery

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

  -- | RQCreateAction !CreateAction
  -- | RQDropAction !DropAction
  -- | RQUpdateAction !UpdateAction
  -- | RQCreateActionPermission !CreateActionPermission
  -- | RQDropActionPermission !DropActionPermission

  | RQDumpInternalState !DumpInternalState
  -- | RQSetCustomTypes !CustomTypes
  deriving (Show, Eq, Lift)

data RQLQueryV2
  = RQV2TrackTable !TrackTableV2
  | RQV2SetTableCustomFields !SetTableCustomFields
  | RQV2TrackFunction !TrackFunctionV2
  deriving (Show, Eq, Lift)

data RQLQuery
  = RQV1 !RQLQueryV1
  | RQV2 !RQLQueryV2
  deriving (Show, Eq, Lift)

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

runQuery
  :: (HasVersion, MonadIO m, MonadError QErr m)
  => PGExecCtx -> InstanceId
  -> UserInfo -> RebuildableSchemaCache Run -> HTTP.Manager
  -> SQLGenCtx -> SystemDefined -> RQLQuery -> m (EncJSON, RebuildableSchemaCache Run)
runQuery pgExecCtx instanceId userInfo sc hMgr sqlGenCtx systemDefined query = do
  accessMode <- getQueryAccessMode query
  resE <- runQueryM query
    & runHasSystemDefinedT systemDefined
    & runCacheRWT sc
    & peelRun runCtx pgExecCtx accessMode
    & runExceptT
    & liftIO
  either throwError withReload resE
  where
    runCtx = RunCtx userInfo hMgr sqlGenCtx
    withReload (result, updatedCache, invalidations) = do
      when (queryModifiesSchemaCache query) $ do
        e <- liftIO $ runExceptT $ runLazyTx pgExecCtx Q.ReadWrite $ liftTx $
          recordSchemaUpdate instanceId invalidations
        liftEither e
      return (result, updatedCache)

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

  RQCreateQueryCollection _       -> True
  RQDropQueryCollection _         -> True
  RQAddQueryToCollection _        -> True
  RQDropQueryFromCollection _     -> True
  RQAddCollectionToAllowlist _    -> True
  RQDropCollectionFromAllowlist _ -> True

  RQRunSql RunSQL{rTxAccessMode}  ->
    case rTxAccessMode of
      Q.ReadOnly  -> False
      Q.ReadWrite -> True

  RQReplaceMetadata _             -> True
  RQExportMetadata _              -> False
  RQClearMetadata _               -> True
  RQReloadMetadata _              -> True

  -- RQCreateAction _                -> True
  -- RQDropAction _                  -> True
  -- RQUpdateAction _                -> True
  -- RQCreateActionPermission _      -> True
  -- RQDropActionPermission _        -> True

  RQDumpInternalState _           -> False
  -- RQSetCustomTypes _              -> True

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
     , MonadIO m, HasHttpManager m, HasSQLGenCtx m
     , HasSystemDefined m
     )
  => RQLQuery
  -> m EncJSON
runQueryM rq = withPathK "args" $ case rq of
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

      -- RQCreateAction q           -> runCreateAction q
      -- RQDropAction q             -> runDropAction q
      -- RQUpdateAction q           -> runUpdateAction q
      -- RQCreateActionPermission q -> runCreateActionPermission q
      -- RQDropActionPermission q   -> runDropActionPermission q

      RQDumpInternalState q        -> runDumpInternalState q

      RQRunSql q                   -> runRunSQL q

      -- RQSetCustomTypes q           -> runSetCustomTypes q

      RQBulk qs                    -> encJFromList <$> indexedMapM runQueryM qs

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

    RQCreateEventTrigger _          -> True
    RQDeleteEventTrigger _          -> True
    RQRedeliverEvent _              -> True
    RQInvokeEventTrigger _          -> True

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

    -- RQCreateAction _                -> True
    -- RQDropAction _                  -> True
    -- RQUpdateAction _                -> True
    -- RQCreateActionPermission _      -> True
    -- RQDropActionPermission _        -> True

    RQDumpInternalState _           -> True
    -- RQSetCustomTypes _              -> True

    RQRunSql _                      -> True

    RQBulk qs                       -> any requiresAdmin qs

  RQV2 q -> case q of
    RQV2TrackTable _           -> True
    RQV2SetTableCustomFields _ -> True
    RQV2TrackFunction _        -> True
