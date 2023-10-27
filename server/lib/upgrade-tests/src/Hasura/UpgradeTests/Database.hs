-- | Starts a database in a Docker container.
module Hasura.UpgradeTests.Database
  ( Database,
    DatabaseSchema (databaseSchemaUrlForContainer, databaseSchemaUrlForHost),
    dbContainer,
    newSchema,
    runSql,
  )
where

import Control.Concurrent.Extended (sleep)
import Control.Exception (bracket)
import Data.Aeson qualified as J
import Data.ByteString.Char8 qualified as ByteString
import Data.Text qualified as Text
import Database.PG.Query qualified as PG
import Hasura.Prelude
import System.Random (randomRIO)
import TestContainers qualified as TC

type Url = String

type Sql = Text

-- | Represents a running database.
data Database = Database
  { -- | The connection URL for the database, inside the Docker network.
    databaseUrlForContainer :: Url,
    -- | The connection URL for the database, from the host network.
    databaseUrlForHost :: Url
  }
  deriving stock (Show)

-- | Represents an initialized schema on a running database.
data DatabaseSchema = DatabaseSchema
  { -- | The connection URL for the schema, inside the Docker network.
    databaseSchemaUrlForContainer :: Url,
    -- | The connection URL for the schema, from the host network.
    databaseSchemaUrlForHost :: Url
  }
  deriving stock (Show)

-- | This starts a database in a test Docker container.
--
-- The database will be cleaned up when leaving the 'TC.TestContainer' monad.
dbContainer :: TC.Network -> TC.TestContainer Database
dbContainer network = do
  container <-
    TC.run
      $ TC.containerRequest (TC.fromTag ("postgis/postgis:16-3.4-alpine"))
      & TC.setSuffixedName "hge-test-upgrade-db"
      & TC.withNetwork network
      & TC.withNetworkAlias "db"
      & TC.setCmd ["-F"]
      & TC.setEnv [("POSTGRES_PASSWORD", "password")]
      & TC.setExpose [5432]
      & TC.setWaitingFor (TC.waitUntilTimeout 30 (TC.waitUntilMappedPortReachable 5432))
  -- The container has a complicated startup script that starts the server,
  -- shuts it down, then starts it again, so waiting for the port is not enough.
  liftIO $ sleep 5
  -- We provide a URL that can be used from the host.
  pure
    $ Database
      { databaseUrlForContainer = "postgresql://postgres:password@db",
        databaseUrlForHost = "postgresql://postgres:password@localhost:" <> show (TC.containerPort container 5432)
      }

-- | This creates a new, randomly-named schema on the given database.
--
-- It is assumed that the schema will be cleaned up when the database is.
newSchema :: Database -> IO DatabaseSchema
newSchema database = do
  schemaName <- replicateM 16 $ randomRIO ('a', 'z')
  runSql (databaseUrlForHost database) $ "CREATE DATABASE \"" <> Text.pack schemaName <> "\""
  pure
    $ DatabaseSchema
      { databaseSchemaUrlForContainer = databaseUrlForContainer database <> "/" <> schemaName,
        databaseSchemaUrlForHost = databaseUrlForHost database <> "/" <> schemaName
      }

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
    (PG.initPGPool connInfo J.Null PG.defaultConnParams nullPGLogger)
    PG.destroyPGPool
    \pool -> do
      result <- runExceptT (PG.runTx' pool tx)
      result `onLeft` (fail . show)

nullPGLogger :: PG.PGLogger
nullPGLogger = const (pure ())
