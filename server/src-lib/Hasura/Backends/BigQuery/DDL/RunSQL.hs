-- |
-- Working example:
--
-- \$ curl -XPOST http://localhost:8080/v2/query -d @- <<EOF
-- {
--   "type":"bigquery_run_sql",
--   "args": {
--     "sql":"select 3 * 4 as foo, \"Hello, World!\" as bar",
--     "source":"chinook"
--   }
-- }
-- EOF
-- {"result_type":"TuplesOk","result":[["foo","bar"],["12","Hello, World!"]]}
module Hasura.Backends.BigQuery.DDL.RunSQL
  ( runSQL,
    runDatabaseInspection,
    BigQueryRunSQL,
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Text (encodeToLazyText)
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Vector qualified as V
import Hasura.Backends.BigQuery.Execute qualified as Execute
import Hasura.Backends.BigQuery.Source (BigQueryDataset (..), BigQuerySourceConfig (..))
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.RQL.DDL.Schema (RunSQLRes (..))
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build

data BigQueryRunSQL = BigQueryRunSQL
  { _mrsSql :: Text,
    _mrsSource :: SourceName
  }
  deriving (Eq, Generic, Show)

instance J.FromJSON BigQueryRunSQL where
  parseJSON = J.genericParseJSON hasuraJSON

instance J.ToJSON BigQueryRunSQL where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

runSQL ::
  (MonadIO m, CacheRWM m, MonadError QErr m, MetadataM m) =>
  BigQueryRunSQL ->
  m EncJSON
runSQL = runSQL_ recordSetAsHeaderAndRows

-- | The SQL query in the request is ignored
runDatabaseInspection ::
  (MonadIO m, CacheRWM m, MonadError QErr m, MetadataM m) =>
  BigQueryRunSQL ->
  m EncJSON
runDatabaseInspection (BigQueryRunSQL _query source) = do
  BigQuerySourceConfig {_scDatasets = dataSets} <- askSourceConfig @'BigQuery source
  let queries =
        [ "SELECT *, ARRAY(SELECT as STRUCT * from "
            <> getBigQueryDataset dataSet
            <> ".INFORMATION_SCHEMA.COLUMNS WHERE table_name = t.table_name) as columns from "
            <> getBigQueryDataset dataSet
            <> ".INFORMATION_SCHEMA.TABLES as t"
          | dataSet <- dataSets
        ]
      query' = T.intercalate " UNION ALL " queries
  runSQL_ recordSetAsSchema (BigQueryRunSQL query' source)

runSQL_ ::
  (MonadIO m, CacheRWM m, MonadError QErr m, MetadataM m) =>
  (Execute.RecordSet -> J.Value) ->
  BigQueryRunSQL ->
  m EncJSON
runSQL_ f (BigQueryRunSQL query source) = do
  sourceConfig <- askSourceConfig @'BigQuery source
  result <-
    Execute.streamBigQuery
      (_scConnection sourceConfig)
      Execute.BigQuery {query = LT.fromStrict query, parameters = mempty}
  case result of
    Left executeProblem -> do
      let errorMessage = Execute.executeProblemMessage Execute.HideDetails executeProblem
      throwError (err400 BigQueryError errorMessage) {qeInternal = Just $ ExtraInternal $ J.toJSON executeProblem}
    Right recordSet ->
      pure
        ( encJFromJValue
            (RunSQLRes "TuplesOk" (f (snd recordSet)))
        )

recordSetAsHeaderAndRows :: Execute.RecordSet -> J.Value
recordSetAsHeaderAndRows Execute.RecordSet {rows} = J.toJSON (thead : tbody)
  where
    thead =
      case rows V.!? 0 of
        Nothing -> []
        Just row ->
          map (J.toJSON . (coerce :: Execute.FieldNameText -> Text)) (InsOrdHashMap.keys row)
    tbody :: [[J.Value]]
    tbody = map (map J.toJSON . InsOrdHashMap.elems) (toList rows)

recordSetAsSchema :: Execute.RecordSet -> J.Value
recordSetAsSchema rs@(Execute.RecordSet {rows}) =
  recordSetAsHeaderAndRows
    $ rs
      { Execute.rows =
          InsOrdHashMap.adjust
            (Execute.TextOutputValue . LT.toStrict . encodeToLazyText . J.toJSON)
            (Execute.FieldNameText "columns")
            <$> rows
      }
