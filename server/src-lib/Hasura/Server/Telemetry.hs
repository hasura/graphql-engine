{-|
  Send anonymized metrics to the telemetry server regarding usage of various
  features of Hasura.

  Design:
    1. On startup and migration, check and insert a `hasura_pg_uuid` into `hdb_version` table
    2. On startup generate a uuid for the current server instance
    3. Run a background thread to send telemetry
      1. this should have its own logger and log type
      2. uses http manager to make http requests to the telemetry server
    4. Telemetry includes:
      1. no of tables
      2. no of views
      3. no of relationships, grouped by automatic, manual
      4. no of permissions, grouped by table, grouped by select, insert etc.
      5. no of event triggers
      6. no of remote schemas
-}

module Hasura.Server.Telemetry
  ( runTelemetry
  , getPgUuid
  , generateFingerprint
  , mkTelemetryLog
  )
  where

import           Control.Exception          (try)
import           Control.Lens

import           Hasura.HTTP
import           Hasura.Logging
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Logging
import           Hasura.Server.Version      (currentVersion)

import qualified Control.Concurrent         as C
import qualified Data.Aeson                 as A
import qualified Data.Aeson.Casing          as A
import qualified Data.Aeson.TH              as A
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.String.Conversions    as CS
import qualified Data.Text                  as T
import qualified Database.PG.Query          as Q
import qualified Network.HTTP.Client        as HTTP
import qualified Network.HTTP.Types         as HTTP
import qualified Network.Wreq               as Wreq


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
  } deriving (Show, Eq)
$(A.deriveJSON (A.aesonDrop 3 A.snakeCase) ''Metrics)

data HasuraTelemetry
  = HasuraTelemetry
  { _htPgUuid       :: !Text
  , _htInstanceUuid :: !Text
  , _htVersion      :: !Text
  , _htMetrics      :: !Metrics
  } deriving (Show, Eq)
$(A.deriveJSON (A.aesonDrop 3 A.snakeCase) ''HasuraTelemetry)


telemetryUrl :: Text
--telemetryUrl = "https://telemetry.hasura.io/graphql-engine/server/v1/metrics"
telemetryUrl = "https://httpbin.org/post"

generateFingerprint :: Q.TxE QErr Text
generateFingerprint =
  Q.catchE defaultTxErrorHandler $
    (runIdentity . Q.getRow) <$>
    Q.withQ [Q.sql| SELECT gen_random_uuid() :: text |] () False

getPgUuid :: Q.TxE QErr Text
getPgUuid =
  (runIdentity . Q.getRow) <$> Q.withQE defaultTxErrorHandler [Q.sql|
          SELECT hasura_uuid :: text
          FROM hdb_catalog.hdb_version
          |] () False

runTelemetry :: Logger -> HTTP.Manager -> Text -> Text -> IO ()
runTelemetry (Logger logger) manager hasuraUUID pgUUID = do
  let options = wreqOptions manager []
  forever $ do
    let metrics = gatherMetrics
        payload = HasuraTelemetry pgUUID hasuraUUID currentVersion metrics
    resp <- try $ Wreq.postWith options (T.unpack telemetryUrl) (A.encode payload)
    case resp of
      Left e -> logHttpErr e
      Right res -> do
        let statusCode = res ^. Wreq.responseStatus . Wreq.statusCode
        when (statusCode /= 200) $ do
          let httpErr = Just $ mkHttpError telemetryUrl (Just res) Nothing
          logger $ mkTelemetryLog "http_error" "failed to post telemetry" httpErr
    C.threadDelay aDay
  where
    logHttpErr :: HTTP.HttpException -> IO ()
    logHttpErr ex = do
      let httpErr = Just $ mkHttpError telemetryUrl Nothing (Just $ HttpException ex)
      logger $ mkTelemetryLog "http_exception" "http exception occurred" httpErr

    aDay = 86400 * 1000 * 1000

gatherMetrics :: Metrics
gatherMetrics = undefined


data TelemetryLog
  = TelemetryLog
  { _tlLogLevel  :: !LogLevel
  , _tlType      :: !Text
  , _tlError     :: !Text
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
             , "error" A..= _tlError tl
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
  toEngineLog tl = (_tlLogLevel tl, "telemetry-log", A.toJSON tl)

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
