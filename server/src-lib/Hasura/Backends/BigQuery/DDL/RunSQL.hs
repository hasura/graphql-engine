{- |
Working example:

$ curl -XPOST http://localhost:8080/v2/query -d @- <<EOF
{
  "type":"bigquery_run_sql",
  "args": {
    "sql":"select 3 * 4 as foo, \"Hello, World!\" as bar",
    "source":"chinook"
  }
}
EOF
{"result_type":"TuplesOk","result":[["foo","bar"],["12","Hello, World!"]]}
-}

module Hasura.Backends.BigQuery.DDL.RunSQL
  ( runSQL
  , runDatabaseInspection
  , BigQueryRunSQL
  )
where

import           Hasura.Prelude

import qualified Data.Aeson                       as J
import qualified Data.HashMap.Strict.InsOrd       as OMap
import qualified Data.Text                        as T
import qualified Data.Text.Lazy                   as LT
import qualified Data.Vector                      as V

import           Data.Aeson.TH                    (deriveJSON)
import           Data.Aeson.Text                  (encodeToLazyText)

import qualified Hasura.Backends.BigQuery.Execute as Execute
import qualified Hasura.Backends.BigQuery.Types   as BigQuery

import           Hasura.Backends.BigQuery.Source  (BigQuerySourceConfig (..))
import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.RQL.DDL.Schema            (RunSQLRes (..))
import           Hasura.RQL.Types                 (CacheRWM, MetadataM, SourceName, askSourceConfig)
import           Hasura.SQL.Backend


data BigQueryRunSQL
  = BigQueryRunSQL
  { _mrsSql    :: Text
  , _mrsSource :: !SourceName
  } deriving (Show, Eq)
$(deriveJSON hasuraJSON ''BigQueryRunSQL)

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
  BigQuerySourceConfig{_scDatasets = dataSets} <- askSourceConfig @'BigQuery source
  let queries = ["SELECT *, ARRAY(SELECT as STRUCT * from " <>
                dataSet <> ".INFORMATION_SCHEMA.COLUMNS WHERE table_name = t.table_name) as columns from " <>
                dataSet <> ".INFORMATION_SCHEMA.TABLES as t" | dataSet <- dataSets]
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
      sourceConfig
      Execute.BigQuery {query = LT.fromStrict query, parameters = mempty, cardinality = BigQuery.Many}
  case result of
    Left queryError -> throw400 BigQueryError (tshow queryError) -- TODO: Pretty print the error type.
    Right recordSet ->
      pure
        (encJFromJValue
           (RunSQLRes "TuplesOk" (f recordSet)))

recordSetAsHeaderAndRows :: Execute.RecordSet -> J.Value
recordSetAsHeaderAndRows Execute.RecordSet {rows} = J.toJSON (thead : tbody)
  where
    thead =
      case rows V.!? 0 of
        Nothing -> []
        Just row ->
          map (J.toJSON . (coerce :: Execute.FieldNameText -> Text)) (OMap.keys row)
    tbody :: [[J.Value]]
    tbody = map (\row -> map J.toJSON (OMap.elems row)) (toList rows)

recordSetAsSchema :: Execute.RecordSet -> J.Value
recordSetAsSchema rs@(Execute.RecordSet {rows}) =
  recordSetAsHeaderAndRows $
    rs { Execute.rows = OMap.adjust
                  (Execute.TextOutputValue . LT.toStrict . encodeToLazyText . J.toJSON)
                  (Execute.FieldNameText "columns") <$> rows }
