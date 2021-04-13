-- Working example:
--
-- $ echo '{"type":"bigquery_run_sql","args":{"sql":"select 3 * 4 as foo, \"Hello, World!\" as bar", "source":"chinook"}}' | curl -XPOST -d @- http://localhost:8080/v2/query
-- {"result_type":"TuplesOk","result":[["foo","bar"],["12","Hello, World!"]]}

module Hasura.Backends.BigQuery.DDL.RunSQL
  ( runSQL
  , runDatabaseInspection
  , BigQueryRunSQL
  )
where

import           Control.Monad.IO.Class
import qualified Data.Aeson                                  as J
import           Data.Aeson.TH                               (deriveJSON)
import           Data.Aeson.Text                             (encodeToLazyText)
import qualified Data.HashMap.Strict.InsOrd                  as OMap
import qualified Data.Text                                   as T
import qualified Data.Text.Lazy                              as LT
import qualified Data.Vector                                 as V

import           Hasura.Backends.BigQuery.DataLoader.Execute as Execute (BigQuery (..),
                                                                         OutputValue (..),
                                                                         RecordSet (..),
                                                                         streamBigQuery)
import qualified Hasura.Backends.BigQuery.DataLoader.Plan    as Plan
import           Hasura.Backends.BigQuery.Source             (BigQuerySourceConfig (..))
import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.Schema                       (RunSQLRes (..))
import           Hasura.RQL.Types                            (CacheRWM, Code (..), MetadataM, QErr,
                                                              SourceName, askSourceConfig, throw400)

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
  BigQuerySourceConfig{_scDatasets = dataSets} <- askSourceConfig source
  let queries = ["SELECT *, ARRAY(SELECT as STRUCT * from " <>
                dataSet <> ".INFORMATION_SCHEMA.COLUMNS WHERE table_name = t.table_name) as columns from " <>
                dataSet <> ".INFORMATION_SCHEMA.TABLES as t" | dataSet <- dataSets]
      query' = T.intercalate " UNION ALL " queries
  runSQL_ recordSetAsSchema (BigQueryRunSQL query' source)

runSQL_ ::
  (MonadIO m, CacheRWM m, MonadError QErr m, MetadataM m) =>
  (RecordSet -> J.Value) ->
  BigQueryRunSQL ->
  m EncJSON
runSQL_ f (BigQueryRunSQL query source) = do
  sourceConfig <- askSourceConfig source
  result <-
    streamBigQuery
      sourceConfig
      Execute.BigQuery {query = LT.fromStrict query, parameters = mempty}
  case result of
    Left queryError -> throw400 BigQueryError (T.pack (show queryError)) -- TODO: Pretty print the error type.
    Right recordSet ->
      pure
        (encJFromJValue
           (RunSQLRes "TuplesOk" (f recordSet)))

recordSetAsHeaderAndRows :: RecordSet -> J.Value
recordSetAsHeaderAndRows RecordSet {rows} = J.toJSON (thead : tbody)
  where
    thead =
      case rows V.!? 0 of
        Nothing -> []
        Just row ->
          map (J.toJSON . (coerce :: Plan.FieldName -> Text)) (OMap.keys row)
    tbody :: [[J.Value]]
    tbody = map (\row -> map J.toJSON (OMap.elems row)) (toList rows)

recordSetAsSchema :: RecordSet -> J.Value
recordSetAsSchema rs@(RecordSet {rows}) =
  recordSetAsHeaderAndRows $
    rs { rows = OMap.adjust
                  (TextOutputValue . LT.toStrict . encodeToLazyText . J.toJSON)
                  (Plan.FieldName "columns") <$> rows }
