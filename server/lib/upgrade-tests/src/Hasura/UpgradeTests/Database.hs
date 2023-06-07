-- | Starts a database in a Docker container.
module Hasura.UpgradeTests.Database
  ( Database,
    DatabaseSchema (..),
    dbContainer,
    newSchema,
    runSql,
  )
where

import Control.Concurrent.Extended (sleep)
import Control.Exception (bracket)
import Data.ByteString.Char8 qualified as ByteString
import Data.Text qualified as Text
import Database.PG.Query qualified as PG
import Hasura.Prelude
import System.Random (randomRIO)
import TestContainers qualified as TC

type Url = String

type Sql = Text

-- | Represents a running database.
newtype Database = Database Url
  deriving newtype (Show)

-- | Represents an initialized schema on a running database.
newtype DatabaseSchema = DatabaseSchema
  { -- | The connection URL for the schema.
    databaseSchemaUrl :: Url
  }
  deriving newtype (Show)

-- | This starts a database in a test Docker container.
--
-- The database will be cleaned up when leaving the 'TC.TestContainer' monad.
dbContainer :: TC.TestContainer Database
dbContainer = do
  container <-
    TC.run
      $ TC.containerRequest (TC.fromTag ("postgis/postgis:15-3.3-alpine"))
      & TC.setSuffixedName "hge-test-upgrade-db"
      & TC.setCmd ["-F"]
      & TC.setEnv [("POSTGRES_PASSWORD", "password")]
      & TC.setExpose [5432]
      & TC.setWaitingFor (TC.waitUntilTimeout 30 (TC.waitUntilMappedPortReachable 5432))
  -- The container has a complicated startup script that starts the server,
  -- shuts it down, then starts it again, so waiting for the port is not enough.
  liftIO $ sleep 5
  -- We provide a URL that can be used from the host.
  pure . Database $ "postgresql://postgres:password@localhost:" <> show (TC.containerPort container 5432)

-- | This creates a new, randomly-named schema on the given database.
--
-- It is assumed that the schema will be cleaned up when the database is.
newSchema :: Database -> IO DatabaseSchema
newSchema (Database url) = do
  schemaName <- replicateM 16 $ randomRIO ('a', 'z')
  runSql url $ "CREATE DATABASE \"" <> Text.pack schemaName <> "\""
  pure . DatabaseSchema $ url <> "/" <> schemaName

-- | Run arbitrary SQL on a given connection URL.
--
-- The SQL can contain multiple statements, and is run unprepared.
runSql :: Url -> Sql -> IO ()
runSql url sql = runTx url $ PG.multiQE PG.PGExecErrTx (PG.fromText sql)

-- | Runs an arbitrary transaction on a given connection URL.
runTx :: (PG.FromPGConnErr e, Show e) => Url -> PG.TxET e IO a -> IO a
runTx url tx = do
  let connInfo =
        PG.ConnInfo
          { ciRetries = 0,
            ciDetails = PG.CDDatabaseURI (ByteString.pack url)
          }
  bracket
    (PG.initPGPool connInfo PG.defaultConnParams nullPGLogger)
    PG.destroyPGPool
    \pool -> do
      result <- runExceptT (PG.runTx' pool tx)
      result `onLeft` (fail . show)

nullPGLogger :: PG.PGLogger
nullPGLogger = const (pure ())
