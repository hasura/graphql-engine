{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Functions to setup and run a dedicated webhook server
module Harness.Webhook
  ( runEventsWebhook,
    EventsQueue (..),
    runInputValidationWebhook,
  )
where

import Control.Concurrent.Async as Async
import Control.Concurrent.Chan qualified as Chan
import Control.Exception.Safe (bracket)
import Data.Aeson qualified as J
import Data.Aeson.TH qualified as J
import Data.Parser.JSONPath (parseJSONPath)
import Data.Text qualified as T
import Data.Text.Extended ((<>>))
import Harness.Http qualified as Http
import Harness.Test.TestResource (AcquiredResource (..), Managed, mkTestResource)
import Harness.TestEnvironment (Server (..), serverUrl)
import Hasura.Base.Error (iResultToMaybe)
import Hasura.Prelude
import Hasura.RQL.Types.Roles (RoleName)
import Hasura.Server.Utils (executeJSONPath, quoteRegex)
import Network.HTTP.Client.Transformable qualified as HTTP
import Network.Socket qualified as Socket
import Network.Wai.Extended qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Text.Regex.TDFA qualified as TDFA
import Web.Spock.Core qualified as Spock

newtype EventsQueue = EventsQueue (Chan.Chan J.Value)

-- | This function starts a new thread with a minimal server on the
-- first available port. It returns the corresponding 'Server'.
--
-- This new server serves the following routes:
--   - GET on @/@, which returns a simple 200 OK;
--   - POST on @/echo@, which extracts the event data from the body
--     of the request and inserts it into the `EventsQueue`.
--   - POST on @/nextRetry@, which extracts the event data from the body
--     of the request and inserts it into the `EventsQueue` and returns 503
--     error code.
--
-- This function performs a health check, using a GET on /, to ensure that the
-- server was started correctly, and will throw an exception if the health check
-- fails. This function does NOT attempt to kill the thread in such a case,
-- which might result in a leak if the thread is still running but the server
-- fails its health check.
runEventsWebhook :: Managed (Server, EventsQueue)
runEventsWebhook = mkTestResource do
  let urlPrefix = "http://127.0.0.1"
  port <- bracket (Warp.openFreePort) (Socket.close . snd) (pure . fst)
  eventsQueueChan <- Chan.newChan
  let eventsQueue = EventsQueue eventsQueueChan
      extractEventDataInsertIntoEventQueue = do
        req <- Spock.request
        body <- liftIO $ Wai.strictRequestBody req
        let jsonBody = J.decode body
        let eventDataPayload =
              -- Only extract the data payload from the request body
              let mkJSONPathE = either (error . T.unpack) id . parseJSONPath
                  eventJSONPath = mkJSONPathE "$.event.data"
               in iResultToMaybe =<< executeJSONPath eventJSONPath <$> jsonBody
        liftIO
          $ Chan.writeChan eventsQueueChan
          $ fromMaybe (error "error in parsing the event data from the body") eventDataPayload
  thread <- Async.async
    $ Spock.runSpockNoBanner port
    $ Spock.spockT id
    $ do
      Spock.get "/"
        $ Spock.json
        $ J.String "OK"
      Spock.post "/hello"
        $ Spock.json
        $ J.String "world"
      Spock.post "/echo" $ do
        extractEventDataInsertIntoEventQueue
        Spock.json $ J.object ["success" J..= True]
      Spock.post "/nextRetry" $ do
        extractEventDataInsertIntoEventQueue
        Spock.setStatus HTTP.status503

  let server = Server {port = fromIntegral port, urlPrefix, thread}
  pure
    AcquiredResource
      { resourceValue = (server, eventsQueue),
        waitForResource = Http.healthCheck $ serverUrl server,
        teardownResource = Async.cancel thread
      }

data RequestBody rows = RequestBody
  { _rbRole :: RoleName,
    _rbData :: rows
  }
  deriving (Eq, Show)

instance (J.FromJSON rows) => J.FromJSON (RequestBody rows) where
  parseJSON = J.withObject "request body" \o -> do
    role <- o J..: "role"
    input <- (o J..: "data") >>= (J..: "input")
    pure $ RequestBody role input

$(J.deriveToJSON hasuraJSON ''RequestBody)

data TweetRow = TweetRow
  { _trId :: Maybe Int,
    _trUserId :: Maybe Int,
    _trContent :: Text
  }
  deriving (Eq, Show)

$(J.deriveJSON hasuraJSON ''TweetRow)

data UserRow = UserRow
  { _urId :: Maybe Int,
    _urName :: Text,
    _urEmail :: Maybe Text,
    _urPhoneNumber :: Maybe Text,
    _urTweetsByIdToUserId :: Maybe [TweetRow]
  }
  deriving (Eq, Show)

instance J.FromJSON UserRow where
  parseJSON = J.withObject "user row" \o -> do
    _urId <- o J..:? "id"
    _urName <- o J..: "name"
    _urEmail <- o J..:? "email"
    _urPhoneNumber <- o J..:? "phone_number"
    _urTweetsByIdToUserId <- do
      maybeTweetsByIdToUserId <- o J..:? "tweets_by_id_to_user_id"
      maybe (pure Nothing) (J..: "data") maybeTweetsByIdToUserId
    pure UserRow {..}

$(J.deriveToJSON hasuraJSON ''UserRow)

isValidEmail :: Text -> Bool
isValidEmail =
  TDFA.match
    $$( quoteRegex
          TDFA.defaultCompOpt
            { TDFA.caseSensitive = False,
              TDFA.multiline = True,
              TDFA.lastStarGreedy = True
            }
          TDFA.defaultExecOpt
          "^[a-zA-Z0-9+_.-]+@[a-zA-Z0-9.-]+$"
      )

isValidPhoneNumber :: Text -> Bool
isValidPhoneNumber =
  TDFA.match
    $$( quoteRegex
          TDFA.defaultCompOpt
            { TDFA.caseSensitive = False
            }
          TDFA.defaultExecOpt
            { TDFA.captureGroups = False
            }
          "^([9]{1})([234789]{1})([0-9]{8})$"
      )

-- | This function starts a new thread with a minimal server on the
-- first available port. It returns the corresponding 'Server'.
--
-- This new server serves the following routes:
--   - GET on @/@, which returns a simple 200 OK;
--   - POST on @/validateUser@, which extracts the users' data from the body
--     of the request and validates email, phone number and # of tweets.
--   - POST on @/validateTweet@, which extracts the tweets' data from the body
--     of the request and validates content length of the tweets.
--
-- This function performs a health check, using a GET on /, to ensure that the
-- server was started correctly, and will throw an exception if the health check
-- fails. This function does NOT attempt to kill the thread in such a case,
-- which might result in a leak if the thread is still running but the server
-- fails its health check.
runInputValidationWebhook :: Managed Server
runInputValidationWebhook = mkTestResource do
  let urlPrefix = "http://127.0.0.1"
  port <- bracket (Warp.openFreePort) (Socket.close . snd) (pure . fst)
  thread <- Async.async
    $ Spock.runSpockNoBanner port
    $ Spock.spockT id
    $ do
      Spock.get "/" $ do
        Spock.json $ J.String "OK"
      Spock.post "/validateUser" $ do
        jsonBody <- Spock.jsonBody'
        let rows :: [UserRow] = _rbData jsonBody
        r <- runExceptT do
          for_ rows $ \(UserRow _id _name email phoneNumber tweets) -> do
            for_ email $ \email' ->
              unless (isValidEmail email')
                $ throwError
                $ "Invalid email id "
                <>> email'
            for_ phoneNumber $ \phoneNumber' ->
              unless (isValidPhoneNumber phoneNumber')
                $ throwError
                $ "Invalid phone number "
                <>> phoneNumber'
            when (length (fromMaybe [] tweets) > 1)
              $ throwError "Only one tweet is allowed to be added with a user"
        either returnInvalid (const $ returnValid) r
      Spock.post "/validateTweet" $ do
        jsonBody <- Spock.jsonBody'
        let rows :: [TweetRow] = _rbData jsonBody
        r <- runExceptT do
          for rows $ \(TweetRow _id _userId content) ->
            when (T.length content > 30)
              $ throwError ("Tweet should not contain more than 30 characters" :: Text)
        either returnInvalid (const $ returnValid) r

  let server = Server {port = fromIntegral port, urlPrefix, thread}
  pure
    AcquiredResource
      { resourceValue = server,
        waitForResource = Http.healthCheck $ serverUrl server,
        teardownResource = Async.cancel thread
      }
  where
    returnInvalid message = do
      Spock.setStatus HTTP.status400
      Spock.json $ J.object ["message" J..= message]

    returnValid = Spock.setStatus HTTP.status200
