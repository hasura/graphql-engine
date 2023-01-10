-- | This module houses low-level functions and types to help access and work
-- with Graphql Engine.
module Harness.Services.GraphqlEngine
  ( HgeBinPath (..),
    HgeServerInstance (getHgeServerInstanceUrl),
    withHge,
    emptyHgeConfig,
  )
where

import Control.Concurrent.Async qualified as Async
import Control.Monad.Managed
import Data.Aeson
import Data.Attoparsec.ByteString as Atto
import Data.ByteString qualified as BS
import Data.Has
import Data.IORef
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Vector (fromList)
import Harness.Exceptions
import Harness.Http qualified as Http
import Harness.Logging
import Harness.Services.Postgres
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

newtype HgeServerInstance = HgeServerInstance {getHgeServerInstanceUrl :: Text}

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
  ( Has HgeBinPath a,
    Has PostgresServerUrl a,
    Has Logger a
  ) =>
  HgeConfig ->
  SpecWith (HgeServerInstance, a) ->
  SpecWith a
withHge HgeConfig {..} specs = do
  flip aroundWith specs \action a -> runManaged do
    let hgeBin = getter a
        pgUrl = getter a
    let logger = getter @Logger a
    port <- spawnServer logger pgUrl hgeBin
    liftIO do
      let urlPrefix = "http://127.0.0.1"
      let server = HgeServerInstance (urlPrefix <> ":" <> tshow port <> "/")
      result <- Http.healthCheck' (T.unpack $ getHgeServerInstanceUrl server)
      case result of
        Http.Healthy -> action (server, a)
        Http.Unhealthy failures -> do
          runLogger logger $ HgeInstanceFailedHealthcheckMessage failures
          error "Graphql-Engine failed http healthcheck."
  where
    spawnServer :: Logger -> PostgresServerUrl -> HgeBinPath -> Managed Warp.Port
    spawnServer logger pgUrl (HgeBinPath hgeBinPath) = do
      freshDb <- mkFreshPostgresDb logger pgUrl
      let metadataDbUrl = mkFreshDbConnectionString pgUrl freshDb
      ((_, Just hgeStdOut, Just hgeStdErr, _), port) <-
        managed
          ( bracket
              ( do
                  port <- bracket (Warp.openFreePort) (Socket.close . snd) (pure . fst)
                  runLogger logger $ HgeInstanceStartMessage port

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
                                : hgeConfigEnvironmentVars,
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
              ( \(process@(_, _, _, ph), port) -> do
                  interruptProcessGroupOf ph
                  exitCode <- waitForProcess ph
                  cleanupProcess process
                  runLogger logger $ HgeInstanceShutdownMessage port exitCode
              )
          )
      hgeLogRelayThread logger hgeStdOut
      hgeStdErrRelayThread logger hgeStdErr
      return port

-- | Log message type used to indicate a HGE server instance has started.
data HgeInstanceStartMessage = HgeInstanceStartMessage {hiStartPort :: Int}

instance LoggableMessage HgeInstanceStartMessage where
  fromLoggableMessage HgeInstanceStartMessage {..} =
    object
      [ ("type", String "HgeInstanceStartMessage"),
        ("port", Number (fromIntegral hiStartPort))
      ]

-- | Log message type used to indicate a HGE server instance failed the
-- healthcheck.
data HgeInstanceFailedHealthcheckMessage = HgeInstanceFailedHealthcheckMessage
  {hisfFailures :: [Http.HttpException]}

instance LoggableMessage HgeInstanceFailedHealthcheckMessage where
  fromLoggableMessage HgeInstanceFailedHealthcheckMessage {..} =
    object
      [ ("type", String "HgeInstanceFailedHealthcheckMessage"),
        ("failures", Array (fromList (map (String . tshow) hisfFailures)))
      ]

-- | Log message type used to indicate a HGE server instance has shutdown.
data HgeInstanceShutdownMessage = HgeInstanceShutdownMessage
  { hiShutdownPort :: Int,
    hiShutdownExitCode :: ExitCode
  }

instance LoggableMessage HgeInstanceShutdownMessage where
  fromLoggableMessage HgeInstanceShutdownMessage {..} =
    object
      [ ("type", String "HgeInstanceShutdownMessage"),
        ("port", Number (fromIntegral hiShutdownPort)),
        ("exit-code", String (tshow hiShutdownExitCode))
      ]

-- | Log message type used to indicate a single log-object output by a HGE
-- server instance (on StdOut).
data HgeLogMessage = HgeLogMessage {hgeLogMessage :: Value}

instance LoggableMessage HgeLogMessage where
  fromLoggableMessage HgeLogMessage {..} =
    object
      [ ("type", String "HgeLogMessage"),
        ("message", hgeLogMessage)
      ]

-- | Log message type used to indicate a chunk of log output text by a HGE
-- server instance which could not be parsed as a json object.
data HgeUnparsableLogMessage = HgeUnparsableLogMessage {hgeUnparsableLogMessage :: Text}

instance LoggableMessage HgeUnparsableLogMessage where
  fromLoggableMessage HgeUnparsableLogMessage {..} =
    object
      [ ("type", String "HgeUnparsableLogMessage"),
        ("message", String hgeUnparsableLogMessage)
      ]

-- | Log message type used to indicate a single line output by a HGE server
-- instance on StdErr.
data HgeStdErrLogMessage = HgeStdErrLogMessage {hgeStdErrLogMessage :: Text}

instance LoggableMessage HgeStdErrLogMessage where
  fromLoggableMessage HgeStdErrLogMessage {..} =
    object
      [ ("type", String "HgeStdErrLogMessage"),
        ("message", String hgeStdErrLogMessage)
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
-- message per json-object, on a best-effort basis.
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
    processChunk :: IORef (Atto.Result Value) -> BS.ByteString -> IO ()
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

    processDone :: Atto.Result Value -> IO (Atto.Result Value)
    processDone result =
      case result of
        Atto.Done rest parsed -> do
          runLogger logger $ HgeLogMessage parsed
          if BS.empty == rest
            then return $ Atto.parse logParser ""
            else processDone $ Atto.parse logParser rest
        _ -> return result

    logParser :: Atto.Parser Value
    logParser = json' <* (option () (void (string "\n")) <|> endOfInput)
