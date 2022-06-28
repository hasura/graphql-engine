{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS -Wno-redundant-constraints #-}

-- | BigQuery helpers. This module contains BigQuery specific schema
-- setup/teardown functions because BigQuery API has a different API
-- (dataset field, manual_configuration field etc)
module Harness.Backend.BigQuery
  ( run_,
    runSql_,
    getServiceAccount,
    getProjectId,
    createTable,
    defaultSourceMetadata,
    trackTable,
    dropTable,
    untrackTable,
    setup,
    teardown,
    setupTablesAction,
    setupPermissionsAction,
  )
where

import Control.Concurrent.Extended
import Data.Aeson (Value (..))
import Data.List qualified as List
import Data.String
import Data.Text qualified as T
import Data.Text.Extended (commaSeparated)
import Data.Time (defaultTimeLocale, formatTime)
import GHC.Stack
import Harness.Constants as Constants
import Harness.Env
import Harness.Exceptions
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Context (BackendType (BigQuery))
import Harness.Test.Fixture
import Harness.Test.Permissions qualified as Permissions
import Harness.Test.Schema
  ( BackendScalarType (..),
    BackendScalarValue (..),
    ScalarValue (..),
    Table (..),
  )
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Backends.BigQuery.Connection (initConnection)
import Hasura.Backends.BigQuery.Execute qualified as Execute
import Hasura.Backends.BigQuery.Source (ServiceAccount)
import Hasura.Prelude

getServiceAccount :: HasCallStack => IO ServiceAccount
getServiceAccount = getEnvJson Constants.bigqueryServiceKeyVar

getProjectId :: (HasCallStack) => IO Text
getProjectId = getEnvString Constants.bigqueryProjectIdVar

-- | Run a plain Standard SQL string against the server, ignore the
-- result. Just checks for errors.
run_ :: (HasCallStack) => ServiceAccount -> Text -> String -> IO ()
run_ serviceAccount projectId query = do
  conn <- initConnection serviceAccount projectId Nothing
  res <- Execute.executeBigQuery conn Execute.BigQuery {Execute.query = fromString query, Execute.parameters = mempty}
  res `onLeft` (`bigQueryError` query)

runSql_ :: HasCallStack => String -> IO ()
runSql_ query = do
  serviceAccount <- getServiceAccount
  projectId <- getProjectId
  catch
    ( bracket
        (initConnection serviceAccount projectId Nothing)
        (const (pure ()))
        (\conn -> void $ handleResult <$> (Execute.executeBigQuery conn Execute.BigQuery {Execute.query = fromString query, Execute.parameters = mempty}))
    )
    ( \(e :: SomeException) ->
        error
          ( unlines
              [ "BigQuery error:",
                show e,
                "SQL was:",
                query
              ]
          )
    )
  where
    handleResult :: Either Execute.ExecuteProblem () -> IO ()
    handleResult (Left _) = throwString "Error handling bigquery"
    handleResult (Right ()) = pure ()

bigQueryError :: HasCallStack => Execute.ExecuteProblem -> String -> IO ()
bigQueryError e query =
  error
    ( unlines
        [ "BigQuery query error:",
          T.unpack (Execute.executeProblemMessage Execute.InsecurelyShowDetails e),
          "SQL was:",
          query
        ]
    )

-- | Serialize Table into a SQL statement, as needed, and execute it on the BigQuery backend
createTable :: Schema.Table -> IO ()
createTable table@Schema.Table {tableName, tableColumns} = do
  serviceAccount <- getServiceAccount
  projectId <- getProjectId
  run_
    serviceAccount
    projectId
    $ T.unpack $
      T.unwords
        ( [ "CREATE TABLE",
            T.pack Constants.bigqueryDataset <> "." <> tableName,
            "(",
            commaSeparated (mkColumn <$> tableColumns),
            -- Primary keys are not supported by BigQuery
            -- Foreign keys are not support by BigQuery
            ")"
          ]
            <> tableInsertions table
            <> [";"]
        )

-- | Generates a temporary table from structs, which is used to populate the table above.
-- Along the lines of:
-- `AS SELECT * FROM UNNEST([STRUCT(1 AS id, 'Alice' AS name), STRUCT(2 AS id, 'Bob' AS name), ...])`
-- Unlike `INSERT INTO` queries, this is allowed on the BigQuery sandbox.
tableInsertions :: Schema.Table -> [Text]
tableInsertions (Schema.Table {tableData = []}) = []
tableInsertions (Schema.Table {tableColumns, tableData}) =
  ["AS SELECT * FROM", "UNNEST", "(", "["] <> List.intercalate [","] (map tableInsertion tableData) <> ["]", ")"]
  where
    tableInsertion :: [ScalarValue] -> [Text]
    tableInsertion row = ["STRUCT", "("] <> List.intercalate [","] (zipWith cellInsertion tableColumns row) <> [")"]
    cellInsertion :: Schema.Column -> ScalarValue -> [Text]
    -- We need to explicitly `CAST` a `NULL` to ensure that the transient table has the correct type for the column.
    -- If all the values in a column are `NULL`, BigQuery will infer the type as `INT64` and then fail to create the table.
    cellInsertion column VNull = ["CAST", "(", serialize VNull, "AS", scalarType (Schema.columnType column), ")", "AS", Schema.columnName column]
    cellInsertion column value = [serialize value, "AS", Schema.columnName column]

scalarType :: HasCallStack => Schema.ScalarType -> Text
scalarType = \case
  Schema.TInt -> "INT64"
  Schema.TStr -> "STRING"
  Schema.TUTCTime -> "DATETIME"
  Schema.TBool -> "BIT"
  Schema.TCustomType txt -> Schema.getBackendScalarType txt bstBigQuery

-- | Create column. BigQuery doesn't support default values. Also,
-- currently we don't support specifying NOT NULL constraint.
mkColumn :: Schema.Column -> Text
mkColumn Schema.Column {columnName, columnType} =
  T.unwords
    [ columnName,
      scalarType columnType
    ]

-- | 'ScalarValue' serializer for BigQuery
serialize :: ScalarValue -> Text
serialize = \case
  VInt i -> tshow i
  VStr s -> "'" <> T.replace "'" "\'" s <> "'"
  VUTCTime t -> T.pack $ formatTime defaultTimeLocale "DATETIME '%F %T'" t
  VBool b -> tshow @Int $ if b then 1 else 0
  VNull -> "NULL"
  VCustomValue bsv -> Schema.formatBackendScalarValueType $ Schema.backendScalarValue bsv bsvBigQuery

-- | Serialize Table into an SQL DROP statement and execute it
dropTable :: Schema.Table -> IO ()
dropTable Schema.Table {tableName} = do
  serviceAccount <- getServiceAccount
  projectId <- getProjectId
  run_
    serviceAccount
    projectId
    $ T.unpack $
      T.unwords
        [ "DROP TABLE", -- we don't want @IF EXISTS@ here, because we don't want this to fail silently
          T.pack Constants.bigqueryDataset <> "." <> tableName,
          ";"
        ]

-- | Post an http request to start tracking
-- Overriding here because bigquery's API is uncommon
trackTable :: TestEnvironment -> Schema.Table -> IO ()
trackTable testEnvironment Schema.Table {tableName} = do
  let datasetName = T.pack Constants.bigqueryDataset
      source = defaultSource BigQuery
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: bigquery_track_table
args:
  source: *source
  table:
    dataset: *datasetName
    name: *tableName
|]

-- | Post an http request to stop tracking the table
-- Overriding `Schema.trackTable` here because bigquery's API expects a `dataset` key
untrackTable :: TestEnvironment -> Schema.Table -> IO ()
untrackTable testEnvironment Schema.Table {tableName} = do
  let datasetName = T.pack Constants.bigqueryDataset
      source = defaultSource BigQuery
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: bigquery_untrack_table
args:
  source: *source
  table:
    dataset: *datasetName
    name: *tableName
|]

-- | Metadata source information for the default BigQuery instance
defaultSourceMetadata :: IO Value
defaultSourceMetadata = do
  let dataset = Constants.bigqueryDataset
      source = defaultSource BigQuery
      backendType = defaultBackendTypeString BigQuery
  serviceAccount <- getServiceAccount
  projectId <- getProjectId
  pure $
    [yaml|
type: replace_metadata
args:
  version: 3
  sources:
  - name: *source
    kind: *backendType
    tables: []
    configuration:
      service_account: *serviceAccount
      project_id: *projectId
      datasets: [*dataset]
|]

-- | Setup the schema in the most expected way.
-- NOTE: Certain test modules may warrant having their own local version.
setup :: [Schema.Table] -> (TestEnvironment, ()) -> IO ()
setup tables' (testEnvironment, _) = do
  let dataset = Constants.bigqueryDataset
      source = defaultSource BigQuery
      backendType = defaultBackendTypeString BigQuery
      tables =
        map
          ( \t ->
              t
                { tableReferences = [],
                  tableManualRelationships = tableReferences t <> tableManualRelationships t
                }
          )
          tables'
  -- Clear and reconfigure the metadata
  serviceAccount <- getServiceAccount
  projectId <- getProjectId
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: replace_metadata
args:
  version: 3
  sources:
  - name: *source
    kind: *backendType
    tables: []
    configuration:
      service_account: *serviceAccount
      project_id: *projectId
      datasets: [*dataset]
|]
  -- Setup and track tables
  for_ tables $ \table -> do
    retryIfJobRateLimitExceeded $ createTable table
    trackTable testEnvironment table
  -- Setup relationships
  for_ tables $ \table -> do
    Schema.trackObjectRelationships BigQuery table testEnvironment
    Schema.trackArrayRelationships BigQuery table testEnvironment

-- | Teardown the schema and tracking in the most expected way.
-- NOTE: Certain test modules may warrant having their own version.
teardown :: [Schema.Table] -> (TestEnvironment, ()) -> IO ()
teardown (reverse -> tables) (testEnvironment, _) = do
  finally
    -- Teardown relationships first
    ( forFinally_ tables $ \table ->
        Schema.untrackRelationships BigQuery table testEnvironment
    )
    -- Then teardown tables
    ( forFinally_ tables $ \table -> do
        finally
          (untrackTable testEnvironment table)
          (dropTable table)
    )

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

-- | Setup the given permissions to the graphql engine in a TestEnvironment.
setupPermissions :: [Permissions.Permission] -> TestEnvironment -> IO ()
setupPermissions permissions env = Permissions.setup "bq" permissions env

-- | Remove the given permissions from the graphql engine in a TestEnvironment.
teardownPermissions :: [Permissions.Permission] -> TestEnvironment -> IO ()
teardownPermissions permissions env = Permissions.teardown "bq" permissions env

-- | We get @jobRateLimitExceeded@ errors from BigQuery if we run too many DML operations in short intervals.
--   This functions tries to fix that by retrying after a few seconds if there's an error.
--   Will always try at least once.
--
--   See <https://cloud.google.com/bigquery/docs/troubleshoot-quotas>.
retryIfJobRateLimitExceeded :: IO () -> IO ()
retryIfJobRateLimitExceeded action = retry 0
  where
    retry retryNumber = do
      action `catch` \(SomeException err) ->
        if "jobRateLimitExceeded" `T.isInfixOf` (tshow err)
          && retryNumber < maxRetriesRateLimitExceeded
          then do
            -- exponential backoff
            sleep (seconds $ 2 ^ retryNumber)
            retry (retryNumber + 1)
          else throwIO err
