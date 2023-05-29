{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.BigQuery.Schema.Introspection
  ( listAllTables,
  )
where

import Data.Aeson (toJSON)
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.String.Interpolate (i)
import Data.Text.Lazy qualified as LT
import Hasura.Backends.BigQuery.Execute (BigQuery (..), OutputValue (..), RecordSet (..), ShowDetails (..))
import Hasura.Backends.BigQuery.Execute qualified as Execute
import Hasura.Backends.BigQuery.Source (BigQueryDataset (..), BigQuerySourceConfig (..))
import Hasura.Backends.BigQuery.Types (TableName (..))
import Hasura.Base.Error (QErr, throw500, throw500WithDetail)
import Hasura.Prelude
import Hasura.RQL.Types.BackendType qualified as Backend
import Hasura.RQL.Types.Common (SourceName)
import Hasura.RQL.Types.Metadata (MetadataM)
import Hasura.RQL.Types.SchemaCache (CacheRM, askSourceConfig)

-- | List all tables, tracked or untracked, on a given BigQuery source. All
-- given datasets' tables will be included.
listAllTables :: (CacheRM m, MetadataM m, MonadError QErr m, MonadIO m) => SourceName -> m [TableName]
listAllTables sourceName = do
  sourceConfig <- askSourceConfig @'Backend.BigQuery sourceName

  let queryPerDataset :: BigQueryDataset -> LT.Text
      queryPerDataset (BigQueryDataset dataset) =
        [i|
          select table_name, table_schema
          FROM #{dataset}.INFORMATION_SCHEMA.TABLES
        |]

      query :: LT.Text
      query =
        LT.intercalate "union all"
          $ map queryPerDataset
          $ _scDatasets sourceConfig

  (_, recordSet) <-
    Execute.streamBigQuery (_scConnection sourceConfig) (BigQuery query mempty)
      `onLeftM` \err -> throw500WithDetail (Execute.executeProblemMessage HideDetails err) (toJSON err)

  results <- for (toList (rows recordSet)) \row -> do
    tableName <- case InsOrdHashMap.lookup "table_name" row of
      Just (TextOutputValue tableName) -> pure tableName
      _ -> throw500 "Unexpected BigQuery introspection result (table_name)"

    tableNameSchema <- case InsOrdHashMap.lookup "table_schema" row of
      Just (TextOutputValue tableNameSchema) -> pure tableNameSchema
      _ -> throw500 "Unexpected BigQuery introspection result (table_schema)"

    pure (tableName, tableNameSchema)

  pure [TableName {..} | (tableName, tableNameSchema) <- results]
