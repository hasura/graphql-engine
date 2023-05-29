-- | Functions to setup and run a dedicated webhook server
module Harness.Webhook
  ( run,
    EventsQueue (..),
  )
where

import Control.Concurrent.Async as Async
import Control.Concurrent.Chan qualified as Chan
import Control.Exception.Safe (bracket)
import Data.Aeson qualified as J
import Data.Parser.JSONPath (parseJSONPath)
import Data.Text qualified as T
import Harness.Http qualified as Http
import Harness.Test.TestResource (AcquiredResource (..), Managed, mkTestResource)
import Harness.TestEnvironment (Server (..), serverUrl)
import Hasura.Base.Error (iResultToMaybe)
import Hasura.Prelude
import Hasura.Server.Utils (executeJSONPath)
import Network.HTTP.Client.Transformable qualified as HTTP
import Network.Socket qualified as Socket
import Network.Wai.Extended qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
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
run :: Managed (Server, EventsQueue)
run = mkTestResource do
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
