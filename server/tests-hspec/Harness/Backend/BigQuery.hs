{-# OPTIONS -Wno-redundant-constraints #-}
{-# LANGUAGE ViewPatterns #-}

-- | BigQuery helpers.
module Harness.Backend.BigQuery
  ( run_,
    getServiceAccount,
    getProjectId,
    createTable,
    insertTable,
    trackTable,
    dropTable,
    untrackTable,
    setup,
    teardown,
  )
where

import Data.Bool (bool)
import Data.Foldable (for_)
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Extended (commaSeparated)
import GHC.Stack
import Harness.Constants as Constants
import Harness.Env
import Harness.Exceptions (SomeException, handle)
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.State (State)
import Harness.Test.Schema qualified as Schema
import Hasura.Backends.BigQuery.Connection (initConnection)
import Hasura.Backends.BigQuery.Execute (BigQuery (..), executeBigQuery)
import Hasura.Backends.BigQuery.Source (ServiceAccount)
import Prelude

getServiceAccount :: HasCallStack => IO ServiceAccount
getServiceAccount = getEnvJson Constants.bigqueryServiceKeyVar

getProjectId :: (HasCallStack) => IO Text
getProjectId = getEnvString Constants.bigqueryProjectIdVar

-- | Run a plain Standard SQL string against the server, ignore the
-- result. Just checks for errors.
run_ :: (HasCallStack) => ServiceAccount -> Text -> String -> IO ()
run_ serviceAccount projectId query =
  handle (\(e :: SomeException) -> bigQueryError e query) $ do
    conn <- initConnection serviceAccount projectId Nothing
    res <- executeBigQuery conn BigQuery {query = fromString query, parameters = mempty}
    case res of
      Left err -> bigQueryError err query
      Right () -> pure ()

bigQueryError :: (Show e, HasCallStack) => e -> String -> IO ()
bigQueryError e query =
  error
    ( unlines
        [ "BigQuery query error:",
          show e,
          "SQL was:",
          query
        ]
    )

-- | Serialize Table into a SQL statement, as needed, and execute it on the BigQuery backend
createTable :: Schema.Table -> IO ()
createTable Schema.Table {tableName, tableColumns} = do
  serviceAccount <- getServiceAccount
  projectId <- getProjectId
  run_
    serviceAccount
    projectId
    $ T.unpack $
      T.unwords
        [ "CREATE TABLE",
          T.pack Constants.bigqueryDataset <> "." <> tableName,
          "(",
          commaSeparated $
            (mkColumn <$> tableColumns),
          -- Primary keys are not supported by BigQuery
          -- Foreign keys are not support by BigQuery
          ");"
        ]

scalarType :: HasCallStack => Schema.ScalarType -> Text
scalarType = \case
  Schema.TInt -> "INT64"
  Schema.TStr -> "STRING"
  Schema.TUTCTime -> "DATETIME"
  Schema.TBool -> "BIT"
  t -> error $ "Unexpected scalar type used for BigQuery: " <> show t

mkColumn :: Schema.Column -> Text
mkColumn Schema.Column {columnName, columnType, columnNullable, columnDefault} =
  T.unwords
    [ columnName,
      scalarType columnType,
      bool "NOT NULL" "DEFAULT NULL" columnNullable,
      maybe "" ("DEFAULT " <>) columnDefault
    ]

-- | Serialize tableData into an SQL insert statement and execute it.
insertTable :: Schema.Table -> IO ()
insertTable Schema.Table {tableName, tableColumns, tableData} = do
  serviceAccount <- getServiceAccount
  projectId <- getProjectId
  run_
    serviceAccount
    projectId
    $ T.unpack $
      T.unwords
        [ "INSERT INTO",
          T.pack Constants.bigqueryDataset <> "." <> tableName,
          "(",
          commaSeparated (Schema.columnName <$> tableColumns),
          ")",
          "VALUES",
          commaSeparated $ mkRow <$> tableData,
          ";"
        ]

mkRow :: [Schema.ScalarValue] -> Text
mkRow row =
  T.unwords
    [ "(",
      commaSeparated $ Schema.serialize <$> row,
      ")"
    ]

-- | Post an http request to start tracking
-- Overriding here because bigquery's API is uncommon
trackTable :: State -> Schema.Table -> IO ()
trackTable state Schema.Table {tableName} = do
  let datasetName = T.pack Constants.bigqueryDataset
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: bigquery_track_table
args:
  source: bigquery
  table:
    dataset: *datasetName
    name: *tableName
|]

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

-- | Post an http request to stop tracking the table
-- Overriding `Schema.trackTable` here because bigquery's API expects a `dataset` key
untrackTable :: State -> Schema.Table -> IO ()
untrackTable state Schema.Table {tableName} = do
  let datasetName = T.pack Constants.bigqueryDataset
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: bigquery_untrack_table
args:
  source: bigquery
  table:
    dataset: *datasetName
    name: *tableName
|]

-- | Setup the schema in the most expected way.
-- NOTE: Certain test modules may warrant having their own local version.
setup :: [Schema.Table] -> (State, ()) -> IO ()
setup tables (state, _) = do
  let dataset = Constants.bigqueryDataset
  -- Clear and reconfigure the metadata
  serviceAccount <- getServiceAccount
  projectId <- getProjectId
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: replace_metadata
args:
  version: 3
  sources:
  - name: bigquery
    kind: bigquery
    tables: []
    configuration:
      service_account: *serviceAccount
      project_id: *projectId
      datasets: [*dataset]
|]
  -- Setup and track tables
  for_ tables $ \table -> do
    createTable table
    insertTable table
    trackTable state table
  -- Setup relationships
  for_ tables $ \table -> do
    Schema.trackObjectRelationships "bigquery" (T.pack Constants.bigqueryDataset) table state
    Schema.trackArrayRelationships "bigquery" (T.pack Constants.bigqueryDataset) table state

-- | Teardown the schema and tracking in the most expected way.
-- NOTE: Certain test modules may warrant having their own version.
teardown :: [Schema.Table] -> (State, ()) -> IO ()
teardown (reverse -> tables) (state, _) = do
  -- Teardown relationships first
  for_ tables $ \table ->
    Schema.untrackRelationships "bigquery" (T.pack Constants.bigqueryDataset) table state
  -- Then teardown tables
  for_ tables $ \table -> do
    untrackTable state table
    dropTable table
