{-# LANGUAGE ScopedTypeVariables #-}

-- |
--  Send anonymized metrics to the telemetry server regarding usage of various
--  features of Hasura.
--
-- The general workflow for telemetry is as follows:
--
-- 1. We generate metrics for each backend in the graphql-engine code and send it to 'telemetryUrl'.
--    The relevant types can be found in "Hasura.Server.Telemetry.Types".
-- 2. The 'telemetryUrl' endpoint is handled by code in:
--    <https://github.com/hasura/hasura-analytics/tree/hge-upgrade>, specifically
--    <manager/main.go> and <manager/analytics.go>.
--    This server endpoint receives the telemetry payload and sends it to another graphql-engine
--    which runs locally and is backed by a postgres database.
--    The database schema for the telemetry endpoint can also be found in the same repo under <hge/migrations/>.
-- 3. The information from the postgres db can be viewed in Metabase:
--    <https://metabase.telemetry.hasura.io/browse/2/schema/public>.
--
-- For more information about telemetry in general, visit the user-facing docs on the topic:
-- <https://hasura.io/docs/latest/graphql/core/guides/telemetry>.
module Hasura.Server.Telemetry
  ( runTelemetry,
    mkTelemetryLog,
  )
where

import CI qualified
import Control.Concurrent.Extended qualified as C
import Control.Exception (try)
import Control.Lens
import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HashMap
import Data.Int (Int64)
import Data.List qualified as L
import Data.List.Extended qualified as L
import Data.Text qualified as T
import Data.Text.Conversions (UTF8 (..), decodeText)
import Data.Text.Extended (toTxt)
import Hasura.App.State qualified as State
import Hasura.HTTP
import Hasura.Logging
import Hasura.LogicalModel.Cache (LogicalModelInfo)
import Hasura.NativeQuery.Cache (NativeQueryInfo (_nqiArguments))
import Hasura.Prelude
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.BackendTag
import Hasura.RQL.Types.BackendType (BackendType, backendTypeFromBackendSourceKind)
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.CustomTypes
import Hasura.RQL.Types.Metadata.Instances ()
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.Roles (RoleName)
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.Source
import Hasura.SQL.AnyBackend qualified as Any
import Hasura.Server.AppStateRef qualified as HGE
import Hasura.Server.Init.Config
import Hasura.Server.ResourceChecker
import Hasura.Server.Telemetry.Counters (dumpServiceTimingMetrics)
import Hasura.Server.Telemetry.Types
import Hasura.Server.Types
import Hasura.Server.Version
import Hasura.StoredProcedure.Cache (StoredProcedureInfo (_spiArguments))
import Hasura.Table.Cache
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types qualified as HTTP
import Network.Wreq qualified as Wreq

-- * Logging and error handling

-- | Logging related
data TelemetryLog = TelemetryLog
  { _tlLogLevel :: !LogLevel,
    _tlType :: !Text,
    _tlMessage :: !Text,
    _tlHttpError :: !(Maybe TelemetryHttpError)
  }
  deriving (Show)

data TelemetryHttpError = TelemetryHttpError
  { tlheStatus :: !(Maybe HTTP.Status),
    tlheUrl :: !Text,
    tlheHttpException :: !(Maybe HttpException),
    tlheResponse :: !(Maybe Text)
  }
  deriving (Show)

instance J.ToJSON TelemetryLog where
  toJSON tl =
    J.object
      [ "type" J..= _tlType tl,
        "message" J..= _tlMessage tl,
        "http_error" J..= (J.toJSON <$> _tlHttpError tl)
      ]

instance J.ToJSON TelemetryHttpError where
  toJSON tlhe =
    J.object
      [ "status_code" J..= (HTTP.statusCode <$> tlheStatus tlhe),
        "url" J..= tlheUrl tlhe,
        "response" J..= tlheResponse tlhe,
        "http_exception" J..= (J.toJSON <$> tlheHttpException tlhe)
      ]

instance ToEngineLog TelemetryLog Hasura where
  toEngineLog tl = (_tlLogLevel tl, ELTInternal ILTTelemetry, J.toJSON tl)

newtype ServerTelemetryRow = ServerTelemetryRow
  { _strServerMetrics :: ServerTelemetry
  }
  deriving (Generic)

data ServerTelemetry = ServerTelemetry
  { _stResourceCpu :: Maybe Int,
    _stResourceMemory :: Maybe Int64,
    _stResourceCheckerErrorCode :: Maybe ResourceCheckerError
  }
  deriving (Generic)

instance J.ToJSON ServerTelemetry where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

instance J.ToJSON ServerTelemetryRow where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

mkHttpError ::
  Text ->
  Maybe (Wreq.Response BL.ByteString) ->
  Maybe HttpException ->
  TelemetryHttpError
mkHttpError url mResp httpEx =
  case mResp of
    Nothing -> TelemetryHttpError Nothing url httpEx Nothing
    Just resp ->
      let status = resp ^. Wreq.responseStatus
          body = decodeText $ UTF8 (resp ^. Wreq.responseBody)
       in TelemetryHttpError (Just status) url httpEx body

mkTelemetryLog :: Text -> Text -> Maybe TelemetryHttpError -> TelemetryLog
mkTelemetryLog = TelemetryLog LevelInfo

-- * Endpoint

telemetryUrl :: Text
telemetryUrl = "https://telemetry.hasura.io/v1/http"

-- * Execution

-- | An infinite loop that sends updated telemetry data ('Metrics') every 24
-- hours. The send time depends on when the server was started and will
-- naturally drift.
runTelemetry ::
  forall m impl.
  ( MonadIO m,
    State.HasAppEnv m
  ) =>
  Logger Hasura ->
  -- | an action that always returns the latest schema cache ref
  HGE.AppStateRef impl ->
  MetadataDbId ->
  PGVersion ->
  ComputeResourcesResponse ->
  m Void
runTelemetry (Logger logger) appStateRef metadataDbUid pgVersion computeResources = do
  State.AppEnv {..} <- State.askAppEnv
  let options = wreqOptions appEnvManager []
  forever $ liftIO $ do
    telemetryStatus <- State.acEnableTelemetry <$> HGE.getAppContext appStateRef
    case telemetryStatus of
      TelemetryEnabled -> do
        schemaCache <- HGE.getSchemaCache appStateRef
        serviceTimings <- dumpServiceTimingMetrics
        experimentalFeatures <- State.acExperimentalFeatures <$> HGE.getAppContext appStateRef
        ci <- CI.getCI
        -- Creates a telemetry payload for a specific backend.
        let telemetryForSource :: forall (b :: BackendType). SourceInfo b -> TelemetryPayload
            telemetryForSource =
              mkTelemetryPayload
                metadataDbUid
                appEnvInstanceId
                currentVersion
                pgVersion
                ci
                serviceTimings
                (scRemoteSchemas schemaCache)
                (scActions schemaCache)
                experimentalFeatures
            telemetries =
              map
                (\sourceinfo -> (Any.dispatchAnyBackend @HasTag) sourceinfo telemetryForSource)
                (HashMap.elems (scSources schemaCache))
            payloads = J.encode <$> telemetries

            serverTelemetry =
              J.encode
                $ ServerTelemetryRow
                $ ServerTelemetry
                  (_rcrCpu computeResources)
                  (_rcrMemory computeResources)
                  (_rcrErrorCode computeResources)

        for_ (serverTelemetry : payloads) $ \payload -> do
          logger $ debugLBS $ "metrics_info: " <> payload
          resp <- try $ Wreq.postWith options (T.unpack telemetryUrl) payload
          either logHttpEx handleHttpResp resp
        C.sleep $ days 1
      TelemetryDisabled -> C.sleep $ seconds 1
  where
    logHttpEx :: HTTP.HttpException -> IO ()
    logHttpEx ex = do
      let httpErr = Just $ mkHttpError telemetryUrl Nothing (Just $ HttpException ex)
      logger $ mkTelemetryLog "http_exception" "http exception occurred" httpErr

    handleHttpResp resp = do
      let statusCode = resp ^. Wreq.responseStatus . Wreq.statusCode
      logger $ debugLBS $ "http_success: " <> resp ^. Wreq.responseBody
      when (statusCode /= 200) $ do
        let httpErr = Just $ mkHttpError telemetryUrl (Just resp) Nothing
        logger $ mkTelemetryLog "http_error" "failed to post telemetry" httpErr

-- * Generate metrics

-- | Generate a telemetry payload for a specific source by computing their
--   relevant metrics.
--   Additional information that may not be relevant to a particular source
--   such as service timing, remote schemas and actions, will be reported
--   only with the default source.
mkTelemetryPayload ::
  forall (b :: BackendType).
  MetadataDbId ->
  InstanceId ->
  Version ->
  PGVersion ->
  Maybe CI.CI ->
  ServiceTimingMetrics ->
  RemoteSchemaMap ->
  ActionCache ->
  HashSet ExperimentalFeature ->
  SourceInfo b ->
  TelemetryPayload
mkTelemetryPayload metadataDbId instanceId version pgVersion ci serviceTimings remoteSchemaMap actionCache experimentalFeatures sourceInfo =
  let topic = versionToTopic version
      sourceMetadata =
        SourceMetadata
          { _smDbUid = forDefaultSource (mdDbIdToDbUid metadataDbId),
            _smBackendType = backendTypeFromBackendSourceKind $ _siSourceKind sourceInfo,
            _smDbKind = toTxt (_siSourceKind sourceInfo),
            _smDbVersion = forDefaultSource (pgToDbVersion pgVersion)
          }
      -- We use this function to attach additional information that is not associated
      -- with a particular source, such as service timing, remote schemas and actions.
      -- These will be reported only with the default source.
      forDefaultSource :: forall a. a -> Maybe a
      forDefaultSource =
        if _siName sourceInfo == SNDefault
          then Just
          else const Nothing
      metrics =
        computeMetrics
          sourceInfo
          (forDefaultSource serviceTimings)
          (forDefaultSource remoteSchemaMap)
          (forDefaultSource actionCache)
      telemetry =
        HasuraTelemetry metadataDbId instanceId version ci sourceMetadata metrics experimentalFeatures
   in TelemetryPayload topic telemetry

-- | Compute the relevant metrics for a specific source.
computeMetrics ::
  forall (b :: BackendType).
  SourceInfo b ->
  Maybe ServiceTimingMetrics ->
  Maybe RemoteSchemaMap ->
  Maybe ActionCache ->
  Metrics
computeMetrics sourceInfo _mtServiceTimings remoteSchemaMap actionCache =
  let _mtTables = countSourceTables (isNothing . _tciViewInfo . _tiCoreInfo)
      _mtViews = countSourceTables (isJust . _tciViewInfo . _tiCoreInfo)
      _mtEnumTables = countSourceTables (isJust . _tciEnumValues . _tiCoreInfo)
      allRels = join $ HashMap.elems $ HashMap.map (getRels . _tciFieldInfoMap . _tiCoreInfo) sourceTableCache
      (manualRels, autoRels) = L.partition riIsManual allRels
      _mtRelationships = RelationshipMetric (length manualRels) (length autoRels)
      rolePerms = join $ HashMap.elems $ HashMap.map permsOfTbl sourceTableCache
      _pmRoles = length $ L.uniques $ fst <$> rolePerms
      allPerms = snd <$> rolePerms
      _pmInsert = calcPerms _permIns allPerms
      _pmSelect = calcPerms _permSel allPerms
      _pmUpdate = calcPerms _permUpd allPerms
      _pmDelete = calcPerms _permDel allPerms
      _mtPermissions =
        PermissionMetric {..}
      _mtEventTriggers =
        HashMap.size
          $ HashMap.filter (not . HashMap.null)
          $ HashMap.map _tiEventTriggerInfoMap sourceTableCache
      _mtRemoteSchemas = HashMap.size <$> remoteSchemaMap
      _mtFunctions = HashMap.size $ HashMap.filter (not . isSystemDefined . _fiSystemDefined) sourceFunctionCache
      _mtActions = computeActionsMetrics <$> actionCache
      _mtNativeQueries = countNativeQueries (HashMap.elems $ _siNativeQueries sourceInfo)
      _mtStoredProcedures = countStoredProcedures (HashMap.elems $ _siStoredProcedures sourceInfo)
      _mtLogicalModels = countLogicalModels (HashMap.elems $ _siLogicalModels sourceInfo)
   in Metrics {..}
  where
    sourceTableCache = _siTables sourceInfo
    sourceFunctionCache = _siFunctions sourceInfo
    countSourceTables predicate = length . filter predicate $ HashMap.elems sourceTableCache

    calcPerms :: (RolePermInfo b -> Maybe a) -> [RolePermInfo b] -> Int
    calcPerms fn perms = length $ mapMaybe fn perms

    permsOfTbl :: TableInfo b -> [(RoleName, RolePermInfo b)]
    permsOfTbl = HashMap.toList . _tiRolePermInfoMap

    countLogicalModels :: [LogicalModelInfo b] -> LogicalModelsMetrics
    countLogicalModels =
      foldMap
        (\_ -> mempty {_lmmCount = 1})

    countNativeQueries :: [NativeQueryInfo b] -> NativeQueriesMetrics
    countNativeQueries =
      foldMap
        ( \nativeQuery ->
            if null (_nqiArguments nativeQuery)
              then mempty {_nqmWithoutParameters = 1}
              else mempty {_nqmWithParameters = 1}
        )

    countStoredProcedures :: [StoredProcedureInfo b] -> StoredProceduresMetrics
    countStoredProcedures =
      foldMap
        ( \storedProcedure ->
            if null (_spiArguments storedProcedure)
              then mempty {_spmWithoutParameters = 1}
              else mempty {_spmWithParameters = 1}
        )

-- | Compute the relevant metrics for actions from the action cache.
computeActionsMetrics :: ActionCache -> ActionMetric
computeActionsMetrics actionCache =
  ActionMetric syncActionsLen asyncActionsLen queryActionsLen typeRelationships customTypesLen
  where
    actions = HashMap.elems actionCache
    syncActionsLen = length . filter ((== ActionMutation ActionSynchronous) . _adType . _aiDefinition) $ actions
    asyncActionsLen = length . filter ((== ActionMutation ActionAsynchronous) . _adType . _aiDefinition) $ actions
    queryActionsLen = length . filter ((== ActionQuery) . _adType . _aiDefinition) $ actions

    outputTypesLen = length . L.nub . map (_adOutputType . _aiDefinition) $ actions
    inputTypesLen = length . L.nub . concatMap (map _argType . _adArguments . _aiDefinition) $ actions
    customTypesLen = inputTypesLen + outputTypesLen

    typeRelationships =
      length
        . L.nub
        . concatMap
          ( \aInfo -> case (snd . _aiOutputType $ aInfo) of
              AOTObject aot -> map _atrName $ _aotRelationships aot
              AOTScalar _ -> []
          )
        $ actions

-- | Decide which topic (telemetry table) we should use based on the version.
versionToTopic :: Version -> Topic
versionToTopic = \case
  VersionDev _ -> Topic "server_metrics_v2_test"
  VersionRelease _ -> Topic "server_metrics_v2"
  VersionCE _ -> Topic "server_metrics_v2"
