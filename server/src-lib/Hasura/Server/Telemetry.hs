{-# LANGUAGE RecordWildCards #-}
{-|
  Send anonymized metrics to the telemetry server regarding usage of various
  features of Hasura.
-}

module Hasura.Server.Telemetry
  ( runTelemetry
  , mkTelemetryLog
  )
  where

import           Control.Exception                (try)
import           Control.Lens
import           Data.Text.Conversions            (UTF8 (..), decodeText)

import           Hasura.HTTP
import           Hasura.Logging
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Init
import           Hasura.Server.Telemetry.Counters
import           Hasura.Server.Version
import           Hasura.Session

import qualified CI
import qualified Control.Concurrent.Extended      as C
import qualified Data.Aeson                       as A
import qualified Data.Aeson.Casing                as A
import qualified Data.Aeson.TH                    as A
import qualified Data.ByteString.Lazy             as BL
import qualified Data.HashMap.Strict              as Map
import qualified Data.List                        as L
import qualified Data.Text                        as T
import qualified Network.HTTP.Client              as HTTP
import qualified Network.HTTP.Types               as HTTP
import qualified Network.Wreq                     as Wreq

data RelationshipMetric
  = RelationshipMetric
  { _rmManual :: !Int
  , _rmAuto   :: !Int
  } deriving (Show, Eq)
$(A.deriveToJSON (A.aesonDrop 3 A.snakeCase) ''RelationshipMetric)

data PermissionMetric
  = PermissionMetric
  { _pmSelect :: !Int
  , _pmInsert :: !Int
  , _pmUpdate :: !Int
  , _pmDelete :: !Int
  , _pmRoles  :: !Int
  } deriving (Show, Eq)
$(A.deriveToJSON (A.aesonDrop 3 A.snakeCase) ''PermissionMetric)

data ActionMetric
    = ActionMetric
    { _amSynchronous       :: !Int
    , _amAsynchronous      :: !Int
    , _amQueryActions      :: !Int
    , _amTypeRelationships :: !Int
    , _amCustomTypes       :: !Int
    } deriving (Show, Eq)
$(A.deriveToJSON (A.aesonDrop 3 A.snakeCase) ''ActionMetric)

data Metrics
  = Metrics
  { _mtTables         :: !Int
  , _mtViews          :: !Int
  , _mtEnumTables     :: !Int
  , _mtRelationships  :: !RelationshipMetric
  , _mtPermissions    :: !PermissionMetric
  , _mtEventTriggers  :: !Int
  , _mtRemoteSchemas  :: !Int
  , _mtFunctions      :: !Int
  , _mtServiceTimings :: !ServiceTimingMetrics
  , _mtPgVersion      :: !PGVersion
  , _mtActions        :: !ActionMetric
  } deriving (Show, Eq)
$(A.deriveToJSON (A.aesonDrop 3 A.snakeCase) ''Metrics)

data HasuraTelemetry
  = HasuraTelemetry
  { _htDbUid       :: !Text
  , _htInstanceUid :: !InstanceId
  , _htVersion     :: !Version
  , _htCi          :: !(Maybe CI.CI)
  , _htMetrics     :: !Metrics
  } deriving (Show)
$(A.deriveToJSON (A.aesonDrop 3 A.snakeCase) ''HasuraTelemetry)

data TelemetryPayload
  = TelemetryPayload
  { _tpTopic :: !Text
  , _tpData  :: !HasuraTelemetry
  } deriving (Show)
$(A.deriveToJSON (A.aesonDrop 3 A.snakeCase) ''TelemetryPayload)

telemetryUrl :: Text
telemetryUrl = "https://telemetry.hasura.io/v1/http"

mkPayload :: Text -> InstanceId -> Version -> Metrics -> IO TelemetryPayload
mkPayload dbId instanceId version metrics = do
  ci <- CI.getCI
  let topic = case version of
        VersionDev _     -> "server_test"
        VersionRelease _ -> "server"
  pure $ TelemetryPayload topic $ HasuraTelemetry dbId instanceId version ci metrics

-- | An infinite loop that sends updated telemetry data ('Metrics') every 24
-- hours. The send time depends on when the server was started and will
-- naturally drift.
runTelemetry
  :: HasVersion
  => Logger Hasura
  -> HTTP.Manager
  -> IO SchemaCache
  -- ^ an action that always returns the latest schema cache
  -> Text
  -> InstanceId
  -> PGVersion
  -> IO void
runTelemetry (Logger logger) manager getSchemaCache dbId instanceId pgVersion = do
  let options = wreqOptions manager []
  forever $ do
    schemaCache <- getSchemaCache
    serviceTimings <- dumpServiceTimingMetrics
    let metrics = computeMetrics schemaCache serviceTimings pgVersion
    payload <- A.encode <$> mkPayload dbId instanceId currentVersion metrics
    logger $ debugLBS $ "metrics_info: " <> payload
    resp <- try $ Wreq.postWith options (T.unpack telemetryUrl) payload
    either logHttpEx handleHttpResp resp
    C.sleep $ days 1

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

computeMetrics :: SchemaCache -> ServiceTimingMetrics -> PGVersion -> Metrics
computeMetrics sc _mtServiceTimings _mtPgVersion =
  let _mtTables = countUserTables (isNothing . _tciViewInfo . _tiCoreInfo)
      _mtViews = countUserTables (isJust . _tciViewInfo . _tiCoreInfo)
      _mtEnumTables = countUserTables (isJust . _tciEnumValues . _tiCoreInfo)
      allRels = join $ Map.elems $ Map.map (getRels . _tciFieldInfoMap . _tiCoreInfo) userTables
      (manualRels, autoRels) = L.partition riIsManual allRels
      _mtRelationships = RelationshipMetric (length manualRels) (length autoRels)
      rolePerms = join $ Map.elems $ Map.map permsOfTbl userTables
      _pmRoles = length $ L.nub $ fst <$> rolePerms
      allPerms = snd <$> rolePerms
      _pmInsert = calcPerms _permIns allPerms
      _pmSelect = calcPerms _permSel allPerms
      _pmUpdate = calcPerms _permUpd allPerms
      _pmDelete = calcPerms _permDel allPerms
      _mtPermissions =
        PermissionMetric{..}
      _mtEventTriggers = Map.size $ Map.filter (not . Map.null)
                    $ Map.map _tiEventTriggerInfoMap userTables
      _mtRemoteSchemas   = Map.size $ scRemoteSchemas sc
      _mtFunctions = Map.size $ Map.filter (not . isSystemDefined . fiSystemDefined) $ scFunctions sc

      -- FIXME:
      _mtActions = ActionMetric 0 0 0 0 0 -- computeActionsMetrics (scActions sc) (snd . scCustomTypes $ sc)

  in Metrics{..}

  where
    userTables = Map.filter (not . isSystemDefined . _tciSystemDefined . _tiCoreInfo) $ scTables sc
    countUserTables predicate = length . filter predicate $ Map.elems userTables

    calcPerms :: (RolePermInfo -> Maybe a) -> [RolePermInfo] -> Int
    calcPerms fn perms = length $ mapMaybe fn perms

    permsOfTbl :: TableInfo -> [(RoleName, RolePermInfo)]
    permsOfTbl = Map.toList . _tiRolePermInfoMap

-- computeActionsMetrics :: ActionCache -> AnnotatedObjects -> ActionMetric
-- computeActionsMetrics ac ao =
--   ActionMetric syncActionsLen asyncActionsLen queryActionsLen typeRelationships customTypesLen
--   where actions = Map.elems ac
--         syncActionsLen  = length . filter ((==(ActionMutation ActionSynchronous)) . _adType . _aiDefinition) $ actions
--         asyncActionsLen  = length . filter ((==(ActionMutation ActionAsynchronous)) . _adType . _aiDefinition) $ actions
--         queryActionsLen = length . filter ((==ActionQuery) . _adType . _aiDefinition) $ actions

--         outputTypesLen = length . nub . (map (_adOutputType . _aiDefinition)) $ actions
--         inputTypesLen = length . nub . concat . (map ((map _argType) . _adArguments . _aiDefinition)) $ actions
--         customTypesLen = inputTypesLen + outputTypesLen

--         typeRelationships = length . nub . concat . map ((getActionTypeRelationshipNames ao) . _aiDefinition) $ actions

--         -- gives the count of relationships associated with an action
--         getActionTypeRelationshipNames :: AnnotatedObjects -> ResolvedActionDefinition -> [RelationshipName]
--         getActionTypeRelationshipNames annotatedObjs actionDefn =
--           let typeName = G.getBaseType $ unGraphQLType $ _adOutputType actionDefn
--               annotatedObj = Map.lookup (ObjectTypeName typeName) annotatedObjs
--           in maybe [] (Map.keys . _aotRelationships) annotatedObj

-- | Logging related

data TelemetryLog
  = TelemetryLog
  { _tlLogLevel  :: !LogLevel
  , _tlType      :: !Text
  , _tlMessage   :: !Text
  , _tlHttpError :: !(Maybe TelemetryHttpError)
  } deriving (Show)

data TelemetryHttpError
  = TelemetryHttpError
  { tlheStatus        :: !(Maybe HTTP.Status)
  , tlheUrl           :: !T.Text
  , tlheHttpException :: !(Maybe HttpException)
  , tlheResponse      :: !(Maybe T.Text)
  } deriving (Show)

instance A.ToJSON TelemetryLog where
  toJSON tl =
    A.object [ "type" A..= _tlType tl
             , "message" A..= _tlMessage tl
             , "http_error" A..= (A.toJSON <$> _tlHttpError tl)
             ]

instance A.ToJSON TelemetryHttpError where
  toJSON tlhe =
    A.object [ "status_code" A..= (HTTP.statusCode <$> tlheStatus tlhe)
             , "url" A..= tlheUrl tlhe
             , "response" A..= tlheResponse tlhe
             , "http_exception" A..= (A.toJSON <$> tlheHttpException tlhe)
             ]


instance ToEngineLog TelemetryLog Hasura where
  toEngineLog tl = (_tlLogLevel tl, ELTInternal ILTTelemetry, A.toJSON tl)

mkHttpError
  :: Text
  -> Maybe (Wreq.Response BL.ByteString)
  -> Maybe HttpException
  -> TelemetryHttpError
mkHttpError url mResp httpEx =
  case mResp of
    Nothing   -> TelemetryHttpError Nothing url httpEx Nothing
    Just resp ->
      let status = resp ^. Wreq.responseStatus
          body = decodeText $ UTF8 (resp ^. Wreq.responseBody)
      in TelemetryHttpError (Just status) url httpEx body

mkTelemetryLog :: Text -> Text -> Maybe TelemetryHttpError -> TelemetryLog
mkTelemetryLog = TelemetryLog LevelInfo
