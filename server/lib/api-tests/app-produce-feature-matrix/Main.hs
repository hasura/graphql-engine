-- | Generate a feature matrix compatability report from a postgres connection string.
module Main (main) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (flushTQueue, newTQueueIO, writeTQueue)
import Data.ByteString qualified as BS (ByteString, intercalate, writeFile)
import Data.Char (toLower)
import Database.PostgreSQL.Simple.Options qualified as Options
import Harness.Exceptions
import Harness.Logging
import Harness.TestEnvironment qualified as TestEnvironment
import Hasura.FeatureMatrix qualified as FeatureMatrix
import Hasura.Prelude
import Options.Generic (ParseRecord, Unwrapped, Wrapped, unwrapRecord, (:::), type (<!>), type (<?>))
import Spec qualified
import SpecHook qualified
import System.Directory qualified as Directory
import System.Environment qualified as Environment
import System.Exit (exitFailure)
import System.FilePath qualified as FilePath
import System.IO (hPutStrLn, stderr)
import System.Log.FastLogger as FL
import Test.Hspec qualified as Hspec

main :: IO ()
main = do
  (options, hspecArgs) <- getOptionsAndHspecArgs
  checkFileAndDirectory options
  (logs, isSuccess) <- runSuite options.connectionString hspecArgs
  BS.writeFile options.output $ FeatureMatrix.render logs
  putStrLn $ "Feature matrix output has been written to: file://" <> options.output
  unless isSuccess exitFailure

-- * Handle arguments

-- | Process cli args and get the options for the feature matrix
--   and the hspec options. Hspec options can be defined with `--hspec <options>`.
getOptionsAndHspecArgs :: IO (Options Unwrapped, [String])
getOptionsAndHspecArgs = do
  (ourArgs, hspecArgs) <- readArgs
  options <-
    Environment.withArgs ourArgs $ unwrapRecord "Feature matrix compatibility tester tool"
  absoluteOutputPath <- Directory.makeAbsolute (output options)
  pure
    ( options {output = absoluteOutputPath},
      hspecArgs
    )

-- | Command-line options for optparse-generic.
data Options w = Options
  { connectionString ::
      w
        ::: String
          <?> "Postgres connection string"
          <!> "postgresql://hasura:hasura@127.0.0.1:65002/hasura",
    output ::
      w
        ::: FilePath
          <?> "Feature matrix output file path"
          <!> "/tmp/feature_matrix_tool_output.html",
    overrideOutputFile ::
      w
        ::: Bool
          <?> "Override output file if exists",
    createDirectory ::
      w
        ::: Bool
          <?> "Create directory if not exists",
    -- this is just a flag, we take care of splitting the arguments ourselves.
    noAsk ::
      w
        ::: Bool
          <?> "Do not ask to override output file or create a directory if missing",
    -- this is just a flag, we take care of splitting the arguments ourselves.
    hspec ::
      w
        ::: Bool
          <?> "arguments for hspec"
  }
  deriving (Generic)

instance ParseRecord (Options Wrapped)

-- | getArgs and split on @--hspec@.
readArgs :: IO ([String], [String])
readArgs = splitOnHspec <$> Environment.getArgs

splitOnHspec :: [String] -> ([String], [String])
splitOnHspec = \case
  [] -> ([], [])
  "--hspec" : rest -> (["--hspec"], rest)
  arg : rest ->
    case splitOnHspec rest of
      (leftList, rightList) -> (arg : leftList, rightList)

-- * Runner

-- | Run test suite with the connection string and hspec arguments
--   return the logs from the suite, to be used for rendering the
--   feature matrix, and whether the run was successful.
runSuite :: String -> [String] -> IO (BS.ByteString, Bool)
runSuite uri hspecArgs = do
  putStrLn $ "Running suite against: " <> show uri <> "."
  Environment.withArgs hspecArgs $ do
    -- write the logs to this queue
    queue <- newTQueueIO
    -- setup mode and logging
    postgresOptions <- Options.parseConnectionString uri `onLeft` error
    (logger', cleanupLogger) <- FL.newFastLogger $ FL.LogCallback (atomically . writeTQueue queue) (pure ())
    SpecHook.setupGlobalConfig
      (TestEnvironment.TestNewPostgresVariant postgresOptions)
      (flLogger logger', cleanupLogger)
    -- run the tests
    isSuccess <- catch
      (Hspec.hspec Spec.spec $> True)
      \(SomeException e) -> hPutStrLn stderr (displayException e) $> False
    -- fetch the logs
    logs <- BS.intercalate "\n" . map FL.fromLogStr <$> atomically (flushTQueue queue)
    pure (logs, isSuccess)

-- * Utils

-- | Check file and directory configuration.
checkFileAndDirectory :: Options Unwrapped -> IO ()
checkFileAndDirectory options = do
  checkFile options
  checkDirectory options

-- | Check that file does not exists, ask to override, or terminate.
checkFile :: Options Unwrapped -> IO ()
checkFile options = do
  let filepath = options.output
  fileExists <- Directory.doesFileExist filepath
  when (fileExists && not options.overrideOutputFile) do
    when options.noAsk $ errorWithoutStackTrace $ "Output file '" <> filepath <> "' already exists."
    putStrLn $ "Output file '" <> filepath <> "' already exists. Override? (y/N)"
    answer <- getLine
    if map toLower answer `elem` ["y", "yes"]
      then putStrLn "The file will be overriden."
      else errorWithoutStackTrace "Terminating."

-- | Check that directory exists, ask to create, or terminate.
checkDirectory :: Options Unwrapped -> IO ()
checkDirectory options = do
  let directory = FilePath.takeDirectory options.output
  dirExists <- Directory.doesDirectoryExist directory
  unless dirExists do
    if options.createDirectory
      then Directory.createDirectoryIfMissing True directory
      else do
        when options.noAsk $ errorWithoutStackTrace $ "Directory '" <> directory <> "' does not exists."
        putStrLn $ "Directory '" <> directory <> "' does not exists. Create? (y/N)"
        answer <- getLine
        if map toLower answer `elem` ["y", "yes"]
          then do
            Directory.createDirectoryIfMissing True directory
            putStrLn "Directory created."
          else errorWithoutStackTrace "Terminating."
