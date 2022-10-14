{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | CitusQL helpers. Pretty much the same as postgres. Could refactor
-- if we add more things here.
module Harness.Backend.Citus
  ( livenessCheck,
    run_,
    defaultSourceMetadata,
    createTable,
    insertTable,
    trackTable,
    dropTable,
    untrackTable,
    setup,
    teardown,
    setupPermissions,
    teardownPermissions,
    setupTablesAction,
    setupPermissionsAction,
  )
where

import Control.Concurrent.Extended (sleep)
import Control.Monad.Reader
import Data.Aeson (Value)
import Data.ByteString.Char8 qualified as S8
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.Extended (commaSeparated)
import Data.Time (defaultTimeLocale, formatTime)
import Database.PostgreSQL.Simple qualified as Postgres
import Harness.Constants as Constants
import Harness.Exceptions
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType (BackendType (Citus), defaultSource)
import Harness.Test.Fixture (SetupAction (..))
import Harness.Test.Permissions qualified as Permissions
import Harness.Test.Schema (BackendScalarType (..), BackendScalarValue (..), ScalarValue (..))
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Prelude
import System.Process.Typed

-- | Check the citus server is live and ready to accept connections.
livenessCheck :: HasCallStack => IO ()
livenessCheck = loop Constants.postgresLivenessCheckAttempts
  where
    loop 0 = error ("Liveness check failed for Citus.")
    loop attempts =
      catch
        ( bracket
            ( Postgres.connectPostgreSQL
                (fromString Constants.citusConnectionString)
            )
            Postgres.close
            (const (pure ()))
        )
        ( \(_failure :: ExitCodeException) -> do
            sleep Constants.httpHealthCheckIntervalSeconds
            loop (attempts - 1)
        )

-- | Run a plain SQL query. On error, print something useful for
-- debugging.
run_ :: HasCallStack => String -> IO ()
run_ q =
  catch
    ( bracket
        ( Postgres.connectPostgreSQL
            (fromString Constants.citusConnectionString)
        )
        Postgres.close
        (\conn -> void (Postgres.execute_ conn (fromString q)))
    )
    ( \(e :: Postgres.SqlError) ->
        error
          ( unlines
              [ "Citus query error:",
                S8.unpack (Postgres.sqlErrorMsg e),
                "SQL was:",
                q
              ]
          )
    )

-- | Metadata source information for the default Citus instance.
defaultSourceMetadata :: Value
defaultSourceMetadata =
  [yaml|
name: citus
kind: citus
tables: []
configuration:
  connection_info:
    database_url: *citusConnectionString
    pool_settings: {}
  |]

-- | Serialize Table into a Citus-SQL statement, as needed, and execute it on the Citus backend
createTable :: HasCallStack => Schema.Table -> IO ()
createTable Schema.Table {tableName, tableColumns, tablePrimaryKey = pk, tableReferences, tableUniqueConstraints} = do
  run_ $
    T.unpack $
      T.unwords
        [ "CREATE TABLE",
          T.pack Constants.citusDb <> "." <> wrapIdentifier tableName,
          "(",
          commaSeparated $
            (mkColumn <$> tableColumns)
              <> (bool [mkPrimaryKey pk] [] (null pk))
              <> (mkReference <$> tableReferences),
          ");"
        ]

  for_ tableUniqueConstraints (createUniqueConstraint tableName)

createUniqueConstraint :: Text -> Schema.UniqueConstraint -> IO ()
createUniqueConstraint tableName (Schema.UniqueConstraintColumns cols) =
  run_ $ T.unpack $ T.unwords $ ["CREATE UNIQUE INDEX ON ", tableName, "("] ++ [commaSeparated cols] ++ [")"]
createUniqueConstraint tableName (Schema.UniqueConstraintExpression ex) =
  run_ $ T.unpack $ T.unwords $ ["CREATE UNIQUE INDEX ON ", tableName, "((", ex, "))"]

scalarType :: HasCallStack => Schema.ScalarType -> Text
scalarType = \case
  Schema.TInt -> "SERIAL"
  Schema.TStr -> "VARCHAR"
  Schema.TUTCTime -> "TIMESTAMP"
  Schema.TBool -> "BOOLEAN"
  Schema.TGeography -> "GEOGRAPHY"
  Schema.TCustomType txt -> Schema.getBackendScalarType txt bstCitus

mkColumn :: Schema.Column -> Text
mkColumn Schema.Column {columnName, columnType, columnNullable, columnDefault} =
  T.unwords
    [ wrapIdentifier columnName,
      scalarType columnType,
      bool "NOT NULL" "DEFAULT NULL" columnNullable,
      maybe "" ("DEFAULT " <>) columnDefault
    ]

mkPrimaryKey :: [Text] -> Text
mkPrimaryKey key =
  T.unwords
    [ "PRIMARY KEY",
      "(",
      commaSeparated $ map wrapIdentifier key,
      ")"
    ]

mkReference :: Schema.Reference -> Text
mkReference Schema.Reference {referenceLocalColumn, referenceTargetTable, referenceTargetColumn} =
  T.unwords
    [ "FOREIGN KEY",
      "(",
      wrapIdentifier referenceLocalColumn,
      ")",
      "REFERENCES",
      referenceTargetTable,
      "(",
      wrapIdentifier referenceTargetColumn,
      ")",
      "ON DELETE CASCADE",
      "ON UPDATE CASCADE"
    ]

-- | Serialize tableData into a Citus-SQL insert statement and execute it.
insertTable :: HasCallStack => Schema.Table -> IO ()
insertTable Schema.Table {tableName, tableColumns, tableData}
  | null tableData = pure ()
  | otherwise = do
    run_ $
      T.unpack $
        T.unwords
          [ "INSERT INTO",
            T.pack Constants.citusDb <> "." <> wrapIdentifier tableName,
            "(",
            commaSeparated (wrapIdentifier . Schema.columnName <$> tableColumns),
            ")",
            "VALUES",
            commaSeparated $ mkRow <$> tableData,
            ";"
          ]

-- | Citus identifiers which may be case-sensitive needs to be wrapped in @""@.
wrapIdentifier :: Text -> Text
wrapIdentifier identifier = "\"" <> identifier <> "\""

-- | 'ScalarValue' serializer for Citus
serialize :: ScalarValue -> Text
serialize = \case
  VInt i -> tshow i
  VStr s -> "'" <> T.replace "'" "\'" s <> "'"
  VUTCTime t -> T.pack $ formatTime defaultTimeLocale "'%F %T'" t
  VBool b -> if b then "TRUE" else "FALSE"
  VGeography (Schema.WKT wkt) -> T.concat ["st_geogfromtext(\'", wkt, "\')"]
  VNull -> "NULL"
  VCustomValue bsv -> Schema.formatBackendScalarValueType $ Schema.backendScalarValue bsv bsvCitus

mkRow :: [Schema.ScalarValue] -> Text
mkRow row =
  T.unwords
    [ "(",
      commaSeparated $ serialize <$> row,
      ")"
    ]

-- | Serialize Table into a Citus-SQL DROP statement and execute it
dropTable :: HasCallStack => Schema.Table -> IO ()
dropTable Schema.Table {tableName} = do
  run_ $
    T.unpack $
      T.unwords
        [ "DROP TABLE", -- we don't want @IF EXISTS@ here, because we don't want this to fail silently
          T.pack Constants.citusDb <> "." <> tableName,
          ";"
        ]

-- | Post an http request to start tracking the table
trackTable :: HasCallStack => TestEnvironment -> Schema.Table -> IO ()
trackTable testEnvironment table =
  Schema.trackTable Citus (defaultSource Citus) table testEnvironment

-- | Post an http request to stop tracking the table
untrackTable :: HasCallStack => TestEnvironment -> Schema.Table -> IO ()
untrackTable testEnvironment table =
  Schema.untrackTable Citus (defaultSource Citus) table testEnvironment

-- | Setup the schema in the most expected way.
-- NOTE: Certain test modules may warrant having their own local version.
setup :: HasCallStack => [Schema.Table] -> (TestEnvironment, ()) -> IO ()
setup tables (testEnvironment, _) = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource testEnvironment defaultSourceMetadata Nothing
  -- Setup and track tables
  for_ tables $ \table -> do
    createTable table
    insertTable table
    trackTable testEnvironment table
  -- Setup relationships
  for_ tables $ \table -> do
    Schema.trackObjectRelationships Citus table testEnvironment
    Schema.trackArrayRelationships Citus table testEnvironment

setupTablesAction :: [Schema.Table] -> TestEnvironment -> SetupAction
setupTablesAction ts env =
  SetupAction
    (setup ts (env, ()))
    (const $ teardown ts (env, ()))

setupPermissionsAction :: [Permissions.Permission] -> TestEnvironment -> SetupAction
setupPermissionsAction permissions env =
  SetupAction
    (setupPermissions permissions env)
    (const $ teardownPermissions permissions env)

-- | Teardown the schema and tracking in the most expected way.
-- NOTE: Certain test modules may warrant having their own version.
teardown :: HasCallStack => [Schema.Table] -> (TestEnvironment, ()) -> IO ()
teardown (reverse -> tables) (testEnvironment, _) = do
  finally
    -- Teardown relationships first
    ( forFinally_ tables $ \table ->
        Schema.untrackRelationships Citus table testEnvironment
    )
    -- Then teardown tables
    ( forFinally_ tables $ \table ->
        finally
          (untrackTable testEnvironment table)
          (dropTable table)
    )

-- | Setup the given permissions to the graphql engine in a TestEnvironment.
setupPermissions :: [Permissions.Permission] -> TestEnvironment -> IO ()
setupPermissions permissions env = Permissions.setup Citus permissions env

-- | Remove the given permissions from the graphql engine in a TestEnvironment.
teardownPermissions :: [Permissions.Permission] -> TestEnvironment -> IO ()
teardownPermissions permissions env = Permissions.teardown Citus permissions env
