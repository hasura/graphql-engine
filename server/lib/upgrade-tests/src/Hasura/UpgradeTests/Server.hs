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
import Data.Aeson qualified as J
import Data.Aeson.KeyMap ((!?))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as J
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Harness.Constants qualified as Constants
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Http qualified as Http
import Hasura.Base.Error (runAesonParser, showQErr)
import Hasura.Prelude
import Hasura.Server.Init (ServeOptions (..), unsafePort)
import Hasura.UpgradeTests.Database
import Network.Socket qualified as Socket
import TestContainers qualified as TC
import TestContainers.Config qualified as TC
import TestContainers.Monad qualified as TC

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

-- | Uses Docker to start HGE with the given version number, and runs an action.
--
-- It uses the images from Docker Hub, so the version must be released. "latest"
-- corresponds to the latest released version.
--
-- The database will be used as both the metadata and source database.
withBaseHge :: TC.Network -> TC.ImageTag -> DatabaseSchema -> (Server -> IO a) -> IO a
withBaseHge network version databaseSchema f = do
  let databaseSchemaUrl = databaseSchemaUrlForContainer databaseSchema
  TC.runTestContainer TC.defaultConfig do
    container <-
      TC.run
        $ TC.containerRequest (TC.fromTag ("hasura/graphql-engine:" <> version))
        & TC.setSuffixedName "hge-test-upgrade-base-server"
        & TC.setCmd
          [ "graphql-engine",
            "--database-url",
            Text.pack databaseSchemaUrl,
            "serve"
          ]
        & TC.withNetwork network
        & TC.setExpose [8080]
    let url = "http://localhost:" <> show (TC.containerPort container 8080)
    liftIO do
      Http.healthCheck $ url <> "/healthz"
      configureServer url databaseSchemaUrl
      f $ Server url

-- | Starts HGE in a new thread, and runs an action.
--
-- This uses HGE as a library, so the version of HGE is the one that this is
-- compiled against.
--
-- The database will be used as the metadata database. Because this is designed
-- to be run after 'withBaseHge', it is expected that the metadata is already
-- configured with a source and some tracked relations.
withCurrentHge :: DatabaseSchema -> (Server -> IO a) -> IO a
withCurrentHge databaseSchema f = do
  port <- getFreePort
  let databaseSchemaUrl = databaseSchemaUrlForHost databaseSchema
  let serverApp =
        GraphqlEngine.runApp
          databaseSchemaUrl
          Constants.serveOptions {soPort = unsafePort port}
  Async.withAsync serverApp \_ -> do
    let url = "http://localhost:" <> show port
    Http.healthCheck $ url <> "/healthz"
    configureServer url databaseSchemaUrl
    f $ Server url

-- | Sets the default source's database URL to the given value.
--
-- This leaves the rest of the metadata intact.
--
-- This code is incredibly unpleasant. Perhaps it could be improved with Aeson lenses or Autodocodec.
configureServer :: String -> String -> IO ()
configureServer serverUrl databaseSchemaUrl = do
  metadata <- Http.postValue (serverUrl <> "/v1/metadata") mempty $ J.object [("type", "export_metadata"), ("args", J.object mempty)]
  newMetadata <-
    either (fail . Text.unpack . showQErr) pure
      . runExcept
      $ runAesonParser modifyMetadata metadata
  void $ Http.postValue (serverUrl <> "/v1/metadata") mempty $ J.object [("type", "replace_metadata"), ("args", J.toJSON newMetadata)]
  where
    modifyMetadata :: J.Value -> J.Parser J.Value
    modifyMetadata metadata = flip (J.withObject "metadata") metadata \metadataObject -> do
      let sources = fromMaybe (J.Array mempty) (metadataObject !? "sources")
      -- updating this is unpleasant because it's an array, keyed by a property, not an object
      newSources <- flip (J.withArray "sources") sources \sourcesArray ->
        case Vector.findIndex (\case J.Object source -> source !? "name" == Just "default"; _ -> False) sourcesArray of
          -- if there is a source, update it
          Just index -> (\newSource -> J.Array $ sourcesArray Vector.// [(index, newSource)]) <$> modifySource (sourcesArray Vector.! index)
          -- if not, add a new one
          Nothing -> pure . J.Array $ Vector.snoc sourcesArray defaultSourceValue
      pure . J.Object $ KeyMap.insert "sources" newSources metadataObject

    modifySource :: J.Value -> J.Parser J.Value
    modifySource source = flip (J.withObject "source") source \sourceObject ->
      pure . J.Object $ KeyMap.insert "configuration" configurationValue sourceObject

    configurationValue = J.object [("connection_info", J.object [("database_url", J.String (Text.pack databaseSchemaUrl))])]
    defaultSourceValue =
      J.object
        [ ("name", "default"),
          ("kind", "postgres"),
          ("configuration", configurationValue),
          ("tables", J.Array mempty)
        ]

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
