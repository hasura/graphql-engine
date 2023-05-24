module Hasura.UpgradeTests.Server
  ( Server,
    serverGraphqlUrl,
    serverMetadataUrl,
    serverQueryUrl,
    withBaseHge,
    withCurrentHge,
  )
where

import Control.Concurrent.Async qualified as Async
import Control.Exception (bracket)
import Data.Text qualified as Text
import Harness.Constants qualified as Constants
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Http qualified as Http
import Hasura.Prelude
import Hasura.Server.Init (ServeOptions (..), unsafePort)
import Hasura.UpgradeTests.Database
import Network.Socket qualified as Socket
import TestContainers qualified as TC
import TestContainers.Config qualified as TC
import TestContainers.Monad qualified as TC
import Unsafe.Coerce (unsafeCoerce)

type Url = String

-- | Represents a running HGE server.
newtype Server = Server Url
  deriving newtype (Show)

-- | Constructs a GraphQL endpoint for the given server.
serverGraphqlUrl :: Server -> Url
serverGraphqlUrl (Server url) = url <> "/v1/graphql"

-- | Constructs a metadata endpoint for the given server.
serverMetadataUrl :: Server -> Url
serverMetadataUrl (Server url) = url <> "/v1/metadata"

-- | Constructs a query endpoint for the given server.
serverQueryUrl :: Server -> Url
serverQueryUrl (Server url) = url <> "/v2/query"

-- | Starts HGE with the given version number, and runs an action.
--
-- It uses the images from Docker Hub, so the version must be released. "latest"
-- corresponds to the latest released version.
--
-- The database will be used as both the metadata and source database.
--
-- The server is run using host networking (and therefore expects a database URL
-- that is host-facing), because 'withCurrentHge' is run as a process directly
-- on the host. These two processes are expected to share a metadata database,
-- and therefore must agree on the source database connection URL.
withBaseHge :: TC.ImageTag -> DatabaseSchema -> (Server -> IO a) -> IO a
withBaseHge version (DatabaseSchema schemaUrl) f =
  TC.runTestContainer TC.defaultConfig do
    port <- liftIO getFreePort
    _container <-
      TC.run
        $ TC.containerRequest (TC.fromTag ("hasura/graphql-engine:" <> version))
        & TC.setSuffixedName "hge-test-upgrade-base-server"
        & TC.setCmd
          [ "graphql-engine",
            "--database-url",
            Text.pack schemaUrl,
            "serve",
            "--server-port",
            tshow port
          ]
        & TC.withNetwork hostNetwork
    let url = "http://localhost:" <> show port
    liftIO do
      Http.healthCheck $ url <> "/healthz"
      f $ Server url

-- | Starts HGE from code, and runs an action.
--
-- The database will be used as the metadata database. Because this is designed
-- to be run after 'withBaseHge', it is expected that the metadata is already
-- configured with a source and some tracked relations.
withCurrentHge :: DatabaseSchema -> (Server -> IO a) -> IO a
withCurrentHge (DatabaseSchema schemaUrl) f = do
  port <- getFreePort
  let serverApp =
        GraphqlEngine.runApp
          schemaUrl
          Constants.serveOptions {soPort = unsafePort port}
  Async.withAsync serverApp \_ -> do
    let url = "http://localhost:" <> show port
    Http.healthCheck $ url <> "/healthz"
    f $ Server url

-- | This represents the "host" Docker network.
hostNetwork :: TC.Network
-- Unfortunately, the 'TC.Network' constructor is not exposed, and so we need
-- to cheat to get one. It's a newtype, so it's not too hard.
--
-- A better solution would be to patch the upstream library to expose a
-- 'hostNetwork' function.
hostNetwork = unsafeCoerce ("host" :: Text)

-- | Looks for a free port and returns it.
--
-- The port is not locked in anyway, so theoretically, it could be acquired by
-- something else before we get a chance to use it. In practice, this is
-- unlikely, as these tests run sequentially.
getFreePort :: IO Int
getFreePort = bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close \sock -> do
  Socket.bind sock (Socket.SockAddrInet Socket.defaultPort 0)
  port <- Socket.socketPort sock
  pure $ fromIntegral port
