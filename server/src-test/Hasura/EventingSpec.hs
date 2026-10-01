-- | Tests for stuff under Hasura.Eventing hierarchy
module Hasura.EventingSpec (spec) where

import Control.Concurrent.STM.TVar
import Control.Lens ((.~))
import Data.Aeson qualified as J
import Data.Aeson.KeyMap qualified as KM
import Data.HashMap.Strict qualified as HashMap
import Data.SerializableBlob qualified as SB
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time.Clock
import Hasura.Authentication.Session (SessionVariables)
import Hasura.Eventing.EventTrigger
import Hasura.Eventing.HTTP (HTTPErr (..), HTTPResp (..), RequestDetails (..), sanitiseReqJSON, sanitiseRespJSON)
import Hasura.Eventing.ScheduledTrigger
import Hasura.GraphQL.Execute.Action.Types (ActionHandlerLog (..))
import Hasura.Prelude
import Hasura.RQL.DDL.Webhook.Transform (mkRequestContext)
import Hasura.RQL.Types.Action (ActionName (..), ActionType (..))
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Eventing
import Hasura.RQL.Types.Headers (HeaderConf (..), HeaderValue (..))
import Hasura.RQL.Types.Webhook.Transform.Class (TemplatingEngine (..))
import Hasura.RQL.Types.Webhook.Transform.Request (RequestTransformCtx (..))
import Hasura.Server.Types (RedactActionHandlerLogsStatus (..))
import Language.GraphQL.Draft.Syntax (unsafeMkName)
import Network.HTTP.Client.Transformable qualified as HTTP
import System.Cron.Parser
import Test.Hspec

spec :: Spec
spec = do
  scheduleTriggersSpec
  eventTriggersLockingUnlockingSpec
  sanitiseReqJSONSpec
  sanitiseRespJSONSpec
  actionHandlerLogSpec

scheduleTriggersSpec :: Spec
scheduleTriggersSpec = do
  -- https://hasura.io/docs/latest/graphql/core/scheduled-triggers/create-cron-trigger.html
  --
  -- FYI this is quite helpful for experimenting with cron expressions:
  -- https://crontab.guru/
  describe "cron" $ do
    it "calculates future events sanely" $ do
      cronTest
        "* * * * *"
        [ "2021-04-20 16:20:00 UTC",
          "2021-04-20 16:21:00 UTC",
          "2021-04-20 16:22:00 UTC"
        ]

      cronTest
        "5 0 * 8 *" -- “At 00:05 in August.”
        [ "2021-08-01 00:05:00 UTC",
          "2021-08-02 00:05:00 UTC",
          "2021-08-03 00:05:00 UTC"
        ]

      cronTest
        "15 14 1 * *" -- “At 14:15 on day-of-month 1.”
        [ "2021-05-01 14:15:00 UTC",
          "2021-06-01 14:15:00 UTC",
          "2021-07-01 14:15:00 UTC"
        ]

      cronTest
        "0 22 * * 1-5" -- “At 22:00 on every day-of-week from Monday through Friday.”
        [ "2021-04-20 22:00:00 UTC",
          "2021-04-21 22:00:00 UTC",
          "2021-04-22 22:00:00 UTC"
        ]
  where
    -- A few unit tests for schedule projection into the future, from an
    -- arbitrary time:
    now = read "2021-04-20 16:19:19.450 UTC" :: UTCTime -- Tuesday
    cronTest cronExpr expected = case parseCronSchedule cronExpr of
      Left e -> error $ "Fix test: " <> show e
      Right sched ->
        generateScheduleTimes now 3 sched
          `shouldBe` map read expected

eventTriggersLockingUnlockingSpec :: Spec
eventTriggersLockingUnlockingSpec = do
  describe "check locking and unlocking of events" $ do
    lockedEventsContainer <- runIO $ newTVarIO mempty
    let eventId = EventId "a7aece90-4a6a-4a8c-ad9d-da5f25dacad9"

    it "locks events correctly" $ do
      saveLockedEventTriggerEvents SNDefault [eventId] lockedEventsContainer
      currentLockedEvents <- readTVarIO lockedEventsContainer
      currentLockedEvents `shouldBe` (HashMap.singleton SNDefault (Set.singleton eventId))

    it "unlocks (removes) an event correctly from the locked events" $ do
      removeEventTriggerEventFromLockedEvents SNDefault eventId lockedEventsContainer
      currentLockedEvents <- readTVarIO lockedEventsContainer
      currentLockedEvents `shouldBe` HashMap.empty

-- | Tests for the redaction applied to trigger delivery logs by
-- 'sanitiseReqJSON'. The webhook URL and headers are always redacted; the
-- request body, session variables and request transform context are redacted
-- only when payload redaction is enabled.
sanitiseReqJSONSpec :: Spec
sanitiseReqJSONSpec = describe "sanitiseReqJSON" $ do
  let webhookVarName = "MY_WEBHOOK_URL" :: Text
      -- a from-env header, which is always redacted (existing behaviour)
      logHeaders = [HeaderConf "X-Api-Key" (HVEnv "MY_SECRET_ENV")]
      redactedHeadersJSON = J.object ["X-Api-Key" J..= J.String "<from_env: MY_SECRET_ENV>"]
      sessionVarsJSON = J.object ["x-hasura-role" J..= J.String "admin", "x-hasura-user-id" J..= J.String "42"]
      sessionVars = case J.fromJSON sessionVarsJSON of
        J.Success sv -> sv :: SessionVariables
        J.Error e -> error ("fix test: could not build SessionVariables: " <> e)
      secretMessage = "secret user message"
      payloadJSON =
        J.object
          [ "event" J..= J.object ["session_variables" J..= sessionVarsJSON],
            "message" J..= J.String secretMessage
          ]
      payloadBS = J.encode payloadJSON
      mkReq = case HTTP.mkRequestEither "http://webhook.internal/deliver" of
        Left e -> error ("fix test: could not build request: " <> show e)
        Right r -> r & HTTP.body .~ HTTP.RequestBodyLBS payloadBS
      -- request transform context is populated only when a transform is set
      reqCtx = mkRequestContext (RequestTransformCtx Nothing payloadJSON (Just sessionVars) Nothing Kriti)
      reqDetailsPlain =
        RequestDetails
          { _rdOriginalRequest = mkReq,
            _rdOriginalSize = 0,
            _rdTransformedRequest = Nothing,
            _rdTransformedSize = Nothing,
            _rdReqTransformCtx = Nothing,
            _rdSessionVars = Just sessionVars
          }
      reqDetailsTransformed =
        reqDetailsPlain
          { _rdTransformedRequest = Just mkReq,
            _rdTransformedSize = Just 0,
            _rdReqTransformCtx = Just reqCtx
          }
      run status = sanitiseReqJSON status webhookVarName logHeaders
      atKey :: [J.Key] -> J.Value -> Maybe J.Value
      atKey [] v = Just v
      atKey (k : ks) (J.Object o) = KM.lookup k o >>= atKey ks
      atKey _ _ = Nothing
      containsSecret (Just (J.String s)) = secretMessage `T.isInfixOf` s
      containsSecret _ = False
      isObject (Just (J.Object _)) = True
      isObject _ = False

  describe "with redaction disabled (default)" $ do
    it "keeps the request body but still redacts the URL and headers (non-transformed)" $ do
      let result = run False reqDetailsPlain
      atKey ["original_request", "url"] result `shouldBe` Just (J.String webhookVarName)
      atKey ["original_request", "headers"] result `shouldBe` Just redactedHeadersJSON
      containsSecret (atKey ["original_request", "body"] result) `shouldBe` True

    it "keeps the session variables (non-transformed)" $ do
      let result = run False reqDetailsPlain
      atKey ["session_vars"] result `shouldBe` Just sessionVarsJSON

    it "keeps the body, session variables and transform context (transformed)" $ do
      let result = run False reqDetailsTransformed
      atKey ["transformed_request", "url"] result `shouldBe` Just (J.String webhookVarName)
      containsSecret (atKey ["transformed_request", "body"] result) `shouldBe` True
      atKey ["session_vars"] result `shouldBe` Just sessionVarsJSON
      isObject (atKey ["req_transform_ctx"] result) `shouldBe` True

  describe "with redaction enabled" $ do
    it "redacts the request body and session variables, keeping the URL/header redaction (non-transformed)" $ do
      let result = run True reqDetailsPlain
      atKey ["original_request", "url"] result `shouldBe` Just (J.String webhookVarName)
      atKey ["original_request", "headers"] result `shouldBe` Just redactedHeadersJSON
      atKey ["original_request", "body"] result `shouldBe` Just J.Null
      atKey ["session_vars"] result `shouldBe` Just J.Null

    it "redacts the body of both requests, the session variables and the transform context (transformed)" $ do
      let result = run True reqDetailsTransformed
      atKey ["original_request", "body"] result `shouldBe` Just J.Null
      atKey ["transformed_request", "body"] result `shouldBe` Just J.Null
      atKey ["session_vars"] result `shouldBe` Just J.Null
      atKey ["req_transform_ctx"] result `shouldBe` Just J.Null

-- | Tests for the redaction applied to the webhook response (or delivery error)
-- in trigger delivery logs by 'sanitiseRespJSON'. The response body is redacted
-- only when payload redaction is enabled; the status code and size are always
-- retained.
sanitiseRespJSONSpec :: Spec
sanitiseRespJSONSpec = describe "sanitiseRespJSON" $ do
  let respSecret = "secret response data"
      okResp =
        HTTPResp
          { hrsStatus = 500,
            hrsHeaders = [],
            hrsBody = SB.fromText respSecret,
            hrsSize = 123
          } ::
          HTTPResp 'EventType
      -- a non-2xx response nests the 'HTTPResp' under the error envelope
      errResp = Left (HStatus okResp) :: Either (HTTPErr 'EventType) (HTTPResp 'EventType)
      successResp = Right okResp :: Either (HTTPErr 'EventType) (HTTPResp 'EventType)
      atKey :: [J.Key] -> J.Value -> Maybe J.Value
      atKey [] v = Just v
      atKey (k : ks) (J.Object o) = KM.lookup k o >>= atKey ks
      atKey _ _ = Nothing

  describe "with redaction disabled (default)" $ do
    it "keeps the response body of a successful response" $ do
      let result = sanitiseRespJSON False successResp
      atKey ["body"] result `shouldBe` Just (J.String respSecret)
      atKey ["status"] result `shouldBe` Just (J.Number 500)

    it "keeps the response body nested in a non-2xx error envelope" $ do
      let result = sanitiseRespJSON False errResp
      atKey ["type"] result `shouldBe` Just (J.String "status")
      atKey ["detail", "body"] result `shouldBe` Just (J.String respSecret)

  describe "with redaction enabled" $ do
    it "redacts the response body of a successful response, keeping the status and size" $ do
      let result = sanitiseRespJSON True successResp
      atKey ["body"] result `shouldBe` Just J.Null
      atKey ["status"] result `shouldBe` Just (J.Number 500)
      atKey ["size"] result `shouldBe` Just (J.Number 123)

    it "redacts the response body nested in a non-2xx error envelope" $ do
      let result = sanitiseRespJSON True errResp
      atKey ["type"] result `shouldBe` Just (J.String "status")
      atKey ["detail", "body"] result `shouldBe` Just J.Null
      atKey ["detail", "status"] result `shouldBe` Just (J.Number 500)

-- | Tests for the redaction applied to the @action-handler-log@ by the
-- 'ActionHandlerLog' 'J.ToJSON' instance. The request body is redacted to JSON
-- @null@ only when payload redaction is enabled; the sizes and action metadata
-- are always retained.
actionHandlerLogSpec :: Spec
actionHandlerLogSpec = describe "ActionHandlerLog ToJSON" $ do
  let actionSecret = "secret action input"
      reqBodyJSON = J.object ["input" J..= J.object ["msg" J..= J.String actionSecret]]
      reqBodyBS = J.encode reqBodyJSON
      mkReq = case HTTP.mkRequestEither "http://action.internal/handler" of
        Left e -> error ("fix test: could not build request: " <> show e)
        Right r -> r & HTTP.body .~ HTTP.RequestBodyLBS reqBodyBS
      mkLog redact =
        ActionHandlerLog
          { _ahlRequest = mkReq,
            _ahlRequestTrans = Nothing,
            _ahlRequestSize = 0,
            _ahlTransformedRequestSize = Nothing,
            _ahlResponseSize = 42,
            _ahlActionName = ActionName (unsafeMkName "myAction"),
            _ahlActionType = ActionQuery,
            _ahlRedactLogs = redact
          }
      atKey :: [J.Key] -> J.Value -> Maybe J.Value
      atKey [] v = Just v
      atKey (k : ks) (J.Object o) = KM.lookup k o >>= atKey ks
      atKey _ _ = Nothing
      containsSecret (Just (J.String s)) = actionSecret `T.isInfixOf` s
      containsSecret _ = False

  it "keeps the request body when redaction is disabled (default)" $ do
    let result = J.toJSON (mkLog RedactActionHandlerLogsDisabled)
    containsSecret (atKey ["request", "body"] result) `shouldBe` True
    atKey ["response_size"] result `shouldBe` Just (J.Number 42)

  it "redacts the request body to null when redaction is enabled, keeping sizes" $ do
    let result = J.toJSON (mkLog RedactActionHandlerLogsEnabled)
    atKey ["request", "body"] result `shouldBe` Just J.Null
    atKey ["response_size"] result `shouldBe` Just (J.Number 42)
