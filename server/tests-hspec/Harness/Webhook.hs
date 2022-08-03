-- | Functions to setup and run a dedicated webhook server
module Harness.Webhook
  ( run,
    EventsQueue (..),
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan qualified as Chan
import Control.Exception.Safe (bracket)
import Data.Aeson qualified as Aeson
import Data.Parser.JSONPath (parseJSONPath)
import Data.Text qualified as T
import Harness.Http qualified as Http
import Harness.TestEnvironment (Server (..), serverUrl)
import Hasura.Base.Error (iResultToMaybe)
import Hasura.Prelude
import Hasura.Server.Utils (executeJSONPath)
import Network.Socket qualified as Socket
import Network.Wai.Extended qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Web.Spock.Core qualified as Spock

newtype EventsQueue = EventsQueue (Chan.Chan Aeson.Value)

-- | This function starts a new thread with a minimal server on the
-- first available port. It returns the corresponding 'Server'.
--
-- This new server serves the following routes:
--   - GET on @/@, which returns a simple 200 OK;
--   - POST on @/echo@, which extracts the event data from the body
--     of the request and inserts it into the `EventsQueue`.
--
-- This function performs a health check, using a GET on /, to ensure that the
-- server was started correctly, and will throw an exception if the health check
-- fails. This function does NOT attempt to kill the thread in such a case,
-- which might result in a leak if the thread is still running but the server
-- fails its health check.
run :: IO (Server, EventsQueue)
run = do
  let urlPrefix = "http://127.0.0.1"
  port <- bracket (Warp.openFreePort) (Socket.close . snd) (pure . fst)
  eventsQueueChan <- Chan.newChan
  let eventsQueue = EventsQueue eventsQueueChan
  threadId <- forkIO $
    Spock.runSpockNoBanner port $
      Spock.spockT id $ do
        Spock.get "/" $
          Spock.json $ Aeson.String "OK"
        Spock.post "/hello" $
          Spock.json $ Aeson.String "world"
        Spock.post "/echo" $ do
          req <- Spock.request
          body <- liftIO $ Wai.strictRequestBody req
          let jsonBody = Aeson.decode body
          let eventDataPayload =
                -- Only extract the data payload from the request body
                let mkJSONPathE = either (error . T.unpack) id . parseJSONPath
                    eventJSONPath = mkJSONPathE "$.event.data"
                 in iResultToMaybe =<< executeJSONPath eventJSONPath <$> jsonBody
          liftIO $
            Chan.writeChan eventsQueueChan $
              fromMaybe (error "error in parsing the event data from the body") eventDataPayload
          Spock.setHeader "Content-Type" "application/json; charset=utf-8"
          Spock.json $ Aeson.object ["success" Aeson..= True]
  let server = Server {port = fromIntegral port, urlPrefix, threadId}
  Http.healthCheck $ serverUrl server
  pure (server, eventsQueue)
