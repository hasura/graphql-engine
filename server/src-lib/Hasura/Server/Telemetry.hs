{-|
  Send anonymized metrics to the telemetry server regarding usage of various
  features of Hasura.
-}

module Hasura.Server.Telemetry
  ( runTelemetry
  , getDbId
  , mkTelemetryLog
  )
  where

import           Control.Exception       (try)
import           Control.Lens
import           Data.IORef
import           Data.List

import           Hasura.HTTP
import           Hasura.Logging
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Init
import           Hasura.Server.Version

import qualified CI
import qualified Control.Concurrent      as C
import qualified Data.Aeson              as A
import qualified Data.Aeson.Casing       as A
import qualified Data.Aeson.TH           as A
import qualified Data.ByteString.Lazy    as BL
import qualified Data.HashMap.Strict     as Map
import qualified Data.String.Conversions as CS
import qualified Data.Text               as T
import qualified Database.PG.Query       as Q
import qualified Network.HTTP.Client     as HTTP
import qualified Network.HTTP.Types      as HTTP
import qualified Network.Wreq            as Wreq


data RelationshipMetric
  = RelationshipMetric
  { _rmManual :: !Int
  , _rmAuto   :: !Int
  } deriving (Show, Eq)
$(A.deriveJSON (A.aesonDrop 3 A.snakeCase) ''RelationshipMetric)

data PermissionMetric
  = PermissionMetric
  { _pmSelect :: !Int
  , _pmInsert :: !Int
  , _pmUpdate :: !Int
  , _pmDelete :: !Int
  , _pmRoles  :: !Int
  } deriving (Show, Eq)
$(A.deriveJSON (A.aesonDrop 3 A.snakeCase) ''PermissionMetric)

data Metrics
  = Metrics
  { _mtTables        :: !Int
  , _mtViews         :: !Int
  , _mtRelationships :: !RelationshipMetric
  , _mtPermissions   :: !PermissionMetric
  , _mtEventTriggers :: !Int
  , _mtRemoteSchemas :: !Int
  , _mtFunctions     :: !Int
  } deriving (Show, Eq)
$(A.deriveJSON (A.aesonDrop 3 A.snakeCase) ''Metrics)

data HasuraTelemetry
  = HasuraTelemetry
  { _htDbUid       :: !Text
  , _htInstanceUid :: !InstanceId
  , _htVersion     :: !Text
  , _htCi          :: !(Maybe CI.CI)
  , _htMetrics     :: !Metrics
  } deriving (Show, Eq)
$(A.deriveJSON (A.aesonDrop 3 A.snakeCase) ''HasuraTelemetry)

data TelemetryPayload
  = TelemetryPayload
  { _tpTopic :: !Text
  , _tpData  :: !HasuraTelemetry
  } deriving (Show, Eq)
$(A.deriveJSON (A.aesonDrop 3 A.snakeCase) ''TelemetryPayload)

telemetryUrl :: Text
telemetryUrl = "https://telemetry.hasura.io/v1/http"

mkPayload :: Text -> InstanceId -> Text -> Metrics -> IO TelemetryPayload
mkPayload dbId instanceId version metrics = do
  ci <- CI.getCI
  return $ TelemetryPayload topic $
    HasuraTelemetry dbId instanceId version ci metrics
  where topic = bool "server" "server_test" isDevVersion

runTelemetry
  :: Logger
  -> HTTP.Manager
  -> IORef (SchemaCache, SchemaCacheVer)
  -> Text
  -> InstanceId
  -> IO ()
runTelemetry (Logger logger) manager cacheRef dbId instanceId = do
  let options = wreqOptions manager []
  forever $ do
    schemaCache <- fmap fst $ readIORef cacheRef
    let metrics = computeMetrics schemaCache
    payload <- A.encode <$> mkPayload dbId instanceId currentVersion metrics
    logger $ debugLBS $ "metrics_info: " <> payload
    resp <- try $ Wreq.postWith options (T.unpack telemetryUrl) payload
    either logHttpEx handleHttpResp resp
    C.threadDelay aDay

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

    aDay = 86400 * 1000 * 1000

computeMetrics :: SchemaCache -> Metrics
computeMetrics sc =
  let nTables = Map.size $ Map.filter (isNothing . tiViewInfo) usrTbls
      nViews  = Map.size $ Map.filter (isJust . tiViewInfo) usrTbls
      allRels = join $ Map.elems $ Map.map relsOfTbl usrTbls
      (manualRels, autoRels) = partition riIsManual allRels
      relMetrics = RelationshipMetric (length manualRels) (length autoRels)
      rolePerms = join $ Map.elems $ Map.map permsOfTbl usrTbls
      nRoles = length $ nub $ fst <$> rolePerms
      allPerms = snd <$> rolePerms
      insPerms = calcPerms _permIns allPerms
      selPerms = calcPerms _permSel allPerms
      updPerms = calcPerms _permUpd allPerms
      delPerms = calcPerms _permDel allPerms
      permMetrics =
        PermissionMetric selPerms insPerms updPerms delPerms nRoles
      evtTriggers = Map.size $ Map.filter (not . Map.null)
                    $ Map.map tiEventTriggerInfoMap usrTbls
      rmSchemas   = Map.size $ scRemoteSchemas sc
      funcs = Map.size $ Map.filter (not . fiSystemDefined) $ scFunctions sc

  in Metrics nTables nViews relMetrics permMetrics evtTriggers rmSchemas funcs

  where
    usrTbls = Map.filter (not . tiSystemDefined) $ scTables sc

    calcPerms :: (RolePermInfo -> Maybe a) -> [RolePermInfo] -> Int
    calcPerms fn perms = length $ catMaybes $ map fn perms

    relsOfTbl :: TableInfo -> [RelInfo]
    relsOfTbl = mapMaybe (preview _FIRelationship) . toList . tiFieldInfoMap

    permsOfTbl :: TableInfo -> [(RoleName, RolePermInfo)]
    permsOfTbl = Map.toList . tiRolePermInfoMap


getDbId :: Q.TxE QErr Text
getDbId =
  (runIdentity . Q.getRow) <$>
  Q.withQE defaultTxErrorHandler
  [Q.sql|
    SELECT (hasura_uuid :: text) FROM hdb_catalog.hdb_version
  |] () False


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


instance ToEngineLog TelemetryLog where
  toEngineLog tl = (_tlLogLevel tl, ELTTelemetryLog, A.toJSON tl)

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
          body = CS.cs $ resp ^. Wreq.responseBody
      in TelemetryHttpError (Just status) url httpEx (Just body)

mkTelemetryLog :: Text -> Text -> Maybe TelemetryHttpError -> TelemetryLog
mkTelemetryLog = TelemetryLog LevelInfo
