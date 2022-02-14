-- | Functions to setup and run a dedicated graphql server.
module Harness.RemoteServer
  ( run,
  )
where

-------------------------------------------------------------------------------

import Control.Concurrent (forkIO)
import Control.Exception.Safe (bracket)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 (ByteString)
import Harness.Http qualified as Http
import Harness.State (Server (..), serverUrl)
import Network.Socket qualified as Socket
import Network.Wai.Extended qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Web.Spock.Core qualified as Spock
import Prelude

-------------------------------------------------------------------------------

-- | Given a function that deals with an incoming GraphQL request, run a GraphQL
-- server. This function returns the resulting 'Server'.
run ::
  -- | Given an incoming 'ByteString', return the resulting 'ByteString.
  -- This signature is made to be compatible with Morpheus' @interpeter@.
  (ByteString -> IO ByteString) ->
  IO Server
run interpreter = do
  let urlPrefix = "http://127.0.0.1"
  port <- bracket (Warp.openFreePort) (Socket.close . snd) (pure . fst)
  threadId <- forkIO $
    Spock.runSpockNoBanner port $
      Spock.spockT id $ do
        Spock.get "/" $ do
          Spock.json $ Aeson.String "OK"
        Spock.post "/graphql" $ do
          req <- Spock.request
          body <- liftIO $ Wai.strictRequestBody req
          result <- liftIO $ interpreter body
          Spock.setHeader "Content-Type" "application/json; charset=utf-8"
          Spock.lazyBytes result
  let server = Server {port = fromIntegral port, urlPrefix, threadId}
  Http.healthCheck $ serverUrl server
  pure server
