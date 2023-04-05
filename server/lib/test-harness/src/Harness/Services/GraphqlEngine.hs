-- | This module houses low-level functions and types to help access and work
-- with Graphql Engine.
module Harness.Services.GraphqlEngine
  ( HgeBinPath (..),
    HgeServerInstance (..),
    getHgeServerInstanceUrl,
    HgeConfig (..),
    withHge,
    spawnServer,
    emptyHgeConfig,
    hgePost,
    hgePostGraphql,
    PostGraphql (..),
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async qualified as Async
import Control.Monad.Managed
import Data.Aeson qualified as A
import Data.Attoparsec.ByteString as Atto
import Data.ByteString qualified as BS
import Data.Has
import Data.IORef
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import Data.Vector (fromList)
import Harness.Exceptions
import Harness.Http qualified as Http
import Harness.Logging
import Harness.PassthroughEnvVars
import Harness.Services.Database.Postgres
import Harness.Test.CustomOptions
import Harness.Yaml
import Hasura.Prelude
import Network.HTTP.Simple qualified as Http
import Network.Socket qualified as Socket
import Network.Wai.Handler.Warp qualified as Warp
import System.Exit (ExitCode)
import System.IO
import System.Process
import Test.Hspec

-- | The path to the 'graphql-engine' executable.
newtype HgeBinPath = HgeBinPath FilePath

data HgeConfig = HgeConfig
  { hgeConfigEnvironmentVars :: [(String, String)]
  }

data HgeServerInstance = HgeServerInstance
  { hgeServerHost :: Text,
    hgeServerPort :: Int
  }

getHgeServerInstanceUrl :: HgeServerInstance -> Text
getHgeServerInstanceUrl (HgeServerInstance {hgeServerHost, hgeServerPort}) =
  "http://" <> hgeServerHost <> ":" <> tshow hgeServerPort <> "/"

emptyHgeConfig :: HgeConfig
emptyHgeConfig = HgeConfig []

-- | Spawn a graphql-engine instance with specific environment variables set.
--
-- The logs emitted by the engine process are embedded in the test logs.
--
-- Note: The engine process will not be terminated if the test suite process
-- crashes. Ensuring that would require making Hge listen for heartbeats, or
-- use a helper process that does.
withHge ::
  ( Has HgeBinPath testEnvironment,
    Has PostgresServerUrl testEnvironment,
    Has Logger testEnvironment,
    Has PassthroughEnvVars testEnvironment
  ) =>
  HgeConfig ->
  SpecWith (PostGraphql, (ShouldReturnYamlF, (HgeServerInstance, testEnvironment))) ->
  SpecWith testEnvironment
withHge hgeConfig specs = do
  flip aroundWith specs \action testEnvironment -> runManaged do
    server <- spawnServer testEnvironment hgeConfig
    liftIO $
      action
        ( PostGraphql (hgePostGraphql (server, testEnvironment)),
          (ShouldReturnYamlF (shouldReturnYamlFInternal defaultOptions), (server, testEnvironment))
        )

-- | spin up a Manager HGE instance and check it is healthy
spawnServer ::
  ( Has PostgresServerUrl testEnvironment,
    Has Logger testEnvironment,
    Has HgeBinPath testEnvironment,
    Has PassthroughEnvVars testEnvironment
  ) =>
  testEnvironment ->
  HgeConfig ->
  Managed HgeServerInstance
spawnServer testEnv (HgeConfig {hgeConfigEnvironmentVars}) = do
  let (HgeBinPath hgeBinPath) = getter testEnv
      pgUrl = getter testEnv
      (PassthroughEnvVars envVars) = getter testEnv
  freshDb <- mkFreshPostgresDb testEnv
  let allEnv = hgeConfigEnvironmentVars <> envVars
      metadataDbUrl = mkFreshDbConnectionString pgUrl freshDb
  ((_, Just hgeStdOut, Just hgeStdErr, _), port) <-
    managed
      ( bracket
          ( do
              port <- bracket (Warp.openFreePort) (Socket.close . snd) (pure . fst)
              testLogMessage testEnv $ HgeInstanceStartMessage port

              process <-
                createProcess
                  ( proc
                      hgeBinPath
                      [ "serve",
                        "--enable-console",
                        "--server-port",
                        show port,
                        "--metadata-database-url",
                        T.unpack (getPostgresServerUrl metadataDbUrl)
                      ]
                  )
                    { env =
                        Just $
                          ("HASURA_GRAPHQL_GRACEFUL_SHUTDOWN_TIMEOUT", "0")
                            : allEnv,
                      std_out = CreatePipe,
                      std_err = CreatePipe,
                      create_group = True
                    }
                  `catchAny` ( \exn ->
                                 error $
                                   unlines
                                     [ "Failed to spawn Graphql-Engine process:",
                                       show exn
                                     ]
                             )
              return $ (process, port)
          )
          ( \(process@(_, _, _, ph), port) -> forkIO $ do
              startTime <- getCurrentTime
              interruptProcessGroupOf ph
              exitCode <- waitForProcess ph
              cleanupProcess process
              endTime <- getCurrentTime
              testLogMessage testEnv $ HgeInstanceShutdownMessage port exitCode (diffUTCTime endTime startTime)
          )
      )
  let logger = getter @Logger testEnv
  hgeLogRelayThread logger hgeStdOut
  hgeStdErrRelayThread logger hgeStdErr
  liftIO do
    let server = HgeServerInstance "127.0.0.1" port
    result <- Http.healthCheck' (T.unpack $ getHgeServerInstanceUrl server <> "/healthz")
    case result of
      Http.Healthy -> pure server
      Http.Unhealthy failures -> do
        runLogger logger $ HgeInstanceFailedHealthcheckMessage failures
        error "Graphql-Engine failed http healthcheck."

-- | Log message type used to indicate a HGE server instance has started.
data HgeInstanceStartMessage = HgeInstanceStartMessage {hiStartPort :: Int}

instance LoggableMessage HgeInstanceStartMessage where
  fromLoggableMessage HgeInstanceStartMessage {..} =
    A.object
      [ ("type", A.String "HgeInstanceStartMessage"),
        ("port", A.Number (fromIntegral hiStartPort))
      ]

-- | Log message type used to indicate a HGE server instance failed the
-- healthcheck.
data HgeInstanceFailedHealthcheckMessage = HgeInstanceFailedHealthcheckMessage
  {hisfFailures :: [Http.HttpException]}

instance LoggableMessage HgeInstanceFailedHealthcheckMessage where
  fromLoggableMessage HgeInstanceFailedHealthcheckMessage {..} =
    A.object
      [ ("type", A.String "HgeInstanceFailedHealthcheckMessage"),
        ("failures", A.Array (fromList (map (A.String . tshow) hisfFailures)))
      ]

-- | Log message type used to indicate a HGE server instance has shutdown.
data HgeInstanceShutdownMessage = HgeInstanceShutdownMessage
  { hiShutdownPort :: Int,
    hiShutdownExitCode :: ExitCode,
    hiShutdownDuration :: NominalDiffTime
  }

instance LoggableMessage HgeInstanceShutdownMessage where
  fromLoggableMessage HgeInstanceShutdownMessage {..} =
    A.object
      [ ("type", A.String "HgeInstanceShutdownMessage"),
        ("port", A.Number (fromIntegral hiShutdownPort)),
        ("duration", A.Number (realToFrac hiShutdownDuration)),
        ("exit-code", A.String (tshow hiShutdownExitCode))
      ]

-- | Log message type used to indicate a single log-A.object output by a HGE
-- server instance (on StdOut).
data HgeLogMessage = HgeLogMessage {hgeLogMessage :: A.Value}

instance LoggableMessage HgeLogMessage where
  fromLoggableMessage HgeLogMessage {..} =
    A.object
      [ ("type", A.String "HgeLogMessage"),
        ("message", hgeLogMessage)
      ]

-- | Log message type used to indicate a chunk of log output text by a HGE
-- server instance which could not be parsed as a json A.object.
data HgeUnparsableLogMessage = HgeUnparsableLogMessage {hgeUnparsableLogMessage :: Text}

instance LoggableMessage HgeUnparsableLogMessage where
  fromLoggableMessage HgeUnparsableLogMessage {..} =
    A.object
      [ ("type", A.String "HgeUnparsableLogMessage"),
        ("message", A.String hgeUnparsableLogMessage)
      ]

-- | Log message type used to indicate a single line output by a HGE server
-- instance on StdErr.
data HgeStdErrLogMessage = HgeStdErrLogMessage {hgeStdErrLogMessage :: Text}

instance LoggableMessage HgeStdErrLogMessage where
  fromLoggableMessage HgeStdErrLogMessage {..} =
    A.object
      [ ("type", A.String "HgeStdErrLogMessage"),
        ("message", A.String hgeStdErrLogMessage)
      ]

-- | A thread that reads from the engine's StdErr handle and makes one test-log
-- message per line.
hgeStdErrRelayThread :: Logger -> Handle -> Managed ()
hgeStdErrRelayThread logger hgeOutput = do
  _ <-
    managed
      ( bracket
          ( Async.async $ forever $ do
              nextChunk <- BS.hGetLine hgeOutput
              runLogger logger $ HgeStdErrLogMessage (decodeUtf8 nextChunk)
          )
          Async.cancel
      )
  return ()

-- | A thread that reads from the engine's StdOut handle and makes one test-log
-- message per json-A.object, on a best-effort basis.
hgeLogRelayThread :: Logger -> Handle -> Managed ()
hgeLogRelayThread logger hgeOutput = do
  resultRef <- liftIO $ newIORef (Atto.parse logParser "")
  _ <-
    managed
      ( bracket
          ( Async.async $ forever $ do
              nextChunk <- (<> "\n") <$> BS.hGetLine hgeOutput
              processChunk resultRef nextChunk
          )
          ( \threadHandle -> do
              Async.cancel threadHandle
              processChunk resultRef ""
              processChunk resultRef ""
          )
      )
  return ()
  where
    processChunk :: IORef (Atto.Result A.Value) -> BS.ByteString -> IO ()
    processChunk ref nextChunk = do
      result <- readIORef ref
      result' <- processDone result
      case result' of
        Atto.Fail {} -> do
          runLogger logger $ HgeUnparsableLogMessage (tshow result)
          writeIORef ref (Atto.parse logParser "")
          processChunk ref nextChunk
        Atto.Partial k -> do
          result'' <- processDone (k nextChunk)
          writeIORef ref result''
        Atto.Done {} ->
          runLogger logger $ HgeUnparsableLogMessage "Impossible: Done{}-case in 'processChunk'"

    processDone :: Atto.Result A.Value -> IO (Atto.Result A.Value)
    processDone result =
      case result of
        Atto.Done rest parsed -> do
          runLogger logger $ HgeLogMessage parsed
          if BS.empty == rest
            then return $ Atto.parse logParser ""
            else processDone $ Atto.parse logParser rest
        _ -> return result

    logParser :: Atto.Parser A.Value
    logParser = A.json' <* (option () (void (string "\n")) <|> endOfInput)

hgePost ::
  ( Has HgeServerInstance env,
    Has Logger env
  ) =>
  env ->
  Int ->
  Text ->
  Http.RequestHeaders ->
  A.Value ->
  IO A.Value
hgePost env statusCode path headers requestBody = do
  let hgeUrl = getHgeServerInstanceUrl $ getter env
  let fullUrl = T.unpack $ hgeUrl <> path
  testLogMessage env $ LogHGERequest path requestBody
  responseBody <- withFrozenCallStack $ Http.postValueWithStatus statusCode fullUrl headers requestBody
  testLogMessage env $ LogHGEResponse path responseBody
  return responseBody

hgePostGraphql ::
  ( Has HgeServerInstance env,
    Has Logger env
  ) =>
  env ->
  A.Value ->
  IO A.Value
hgePostGraphql env query = do
  hgePost env 200 "/v1/graphql" [] (A.object ["query" A..= query])

-- | Newtype-wrapper which enables late binding of 'postGraphql' on the test environment.
-- This makes 'TestEnvironment'-based specs more readily compatible with componontised fixtures.
newtype PostGraphql = PostGraphql {getPostGraphql :: A.Value -> IO A.Value}
