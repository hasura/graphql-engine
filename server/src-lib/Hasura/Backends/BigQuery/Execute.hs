{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Execute a Select query against the BigQuery REST API.
module Hasura.Backends.BigQuery.Execute
  ( executeSelect,
    runExecute,
    streamBigQuery,
    executeBigQuery,
    executeProblemMessage,
    insertDataset,
    deleteDataset,
    BigQuery (..),
    Execute,
    ExecuteProblem (..),
    FieldNameText (..),
    Job (..),
    OutputValue (..),
    RecordSet (..),
    ShowDetails (..),
    Value (..),
  )
where

import Control.Applicative
import Control.Concurrent.Extended (sleep)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson ((.!=), (.:), (.:?), (.=))
import Data.Aeson qualified as J
import Data.Aeson.Types qualified as J
import Data.ByteString.Lazy qualified as BL
import Data.Foldable
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Data.Text.Read qualified as TR
import Data.Time
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics
import Hasura.Backends.BigQuery.Connection
import Hasura.Backends.BigQuery.Source
import Hasura.Backends.BigQuery.ToQuery qualified as ToQuery
import Hasura.Backends.BigQuery.Types as BigQuery
import Hasura.Prelude hiding (head, state, tail)
import Network.HTTP.Simple
import Network.HTTP.Types

--------------------------------------------------------------------------------
-- Types

-- | A set of records produced by the database. These are joined
-- together. There are all sorts of optimizations possible here, from
-- using a matrix/flat vector, unboxed sums for Value, etc. Presently
-- we choose a naive implementation in the interest of getting other
-- work done.
data RecordSet = RecordSet
  { rows :: Vector (InsOrdHashMap FieldNameText OutputValue),
    wantedFields :: Maybe [Text]
  }
  deriving (Show)

-- | As opposed to BigQuery.FieldName which is a qualified name, this
-- is just the unqualified text name itself.
newtype FieldNameText
  = FieldNameText Text
  deriving (Show, Ord, Eq, Hashable, J.FromJSON, J.ToJSONKey, IsString)

data OutputValue
  = DecimalOutputValue Decimal
  | BigDecimalOutputValue BigDecimal
  | IntegerOutputValue Int64
  | FloatOutputValue Float64
  | GeographyOutputValue Geography
  | TextOutputValue Text
  | TimestampOutputValue Timestamp
  | DateOutputValue Date
  | TimeOutputValue Time
  | DatetimeOutputValue Datetime
  | BytesOutputValue Base64
  | BoolOutputValue Bool
  | ArrayOutputValue (Vector OutputValue)
  | RecordOutputValue (InsOrdHashMap FieldNameText OutputValue)
  | JsonOutputValue J.Value
  | NullOutputValue -- TODO: Consider implications.
  deriving (Show, Eq, Generic)

instance Hashable OutputValue

instance J.ToJSON OutputValue where
  toJSON = \case
    NullOutputValue -> J.toJSON J.Null
    DecimalOutputValue i -> J.toJSON i
    BigDecimalOutputValue i -> J.toJSON i
    FloatOutputValue i -> J.toJSON i
    TextOutputValue i -> J.toJSON i
    BytesOutputValue i -> J.toJSON i
    DateOutputValue i -> J.toJSON i
    TimestampOutputValue i -> J.toJSON i
    TimeOutputValue i -> J.toJSON i
    DatetimeOutputValue i -> J.toJSON i
    GeographyOutputValue i -> J.toJSON i
    BoolOutputValue i -> J.toJSON i
    IntegerOutputValue i -> J.toJSON i
    ArrayOutputValue vector -> J.toJSON vector
    JsonOutputValue value -> value
    RecordOutputValue record -> J.toJSON record

data ExecuteReader = ExecuteReader
  { sourceConfig :: BigQuerySourceConfig
  }

data ExecuteProblem
  = GetJobDecodeProblem String
  | CreateQueryJobDecodeProblem String
  | InsertDatasetDecodeProblem String
  | ExecuteRunBigQueryProblem BigQueryProblem
  | RESTRequestNonOK Status J.Value
  deriving (Generic)

-- | We use this to hide certain details from the front-end, while allowing
-- them in tests. We have not actually decided whether showing the details is
-- insecure, but until we decide otherwise, it's probably best to err on the side
-- of caution.
data ShowDetails = HideDetails | InsecurelyShowDetails

instance J.ToJSON ExecuteProblem where
  toJSON =
    J.object . \case
      GetJobDecodeProblem err -> ["get_job_decode_problem" J..= err]
      CreateQueryJobDecodeProblem err -> ["create_query_job_decode_problem" J..= err]
      ExecuteRunBigQueryProblem problem -> ["execute_run_bigquery_problem" J..= problem]
      InsertDatasetDecodeProblem problem -> ["insert_dataset__bigquery_problem" J..= problem]
      RESTRequestNonOK _ resp -> ["rest_request_non_ok" J..= resp]

executeProblemMessage :: ShowDetails -> ExecuteProblem -> Text
executeProblemMessage showDetails = \case
  GetJobDecodeProblem err -> "Fetching BigQuery job status, cannot decode HTTP response; " <> tshow err
  CreateQueryJobDecodeProblem err -> "Creating BigQuery job, cannot decode HTTP response: " <> tshow err
  ExecuteRunBigQueryProblem err ->
    "Cannot execute BigQuery request" <> showErr err
  InsertDatasetDecodeProblem err ->
    "Cannot create BigQuery dataset" <> showErr err
  RESTRequestNonOK status body ->
    let summary = "BigQuery HTTP request failed with status " <> tshow (statusCode status) <> " " <> tshow (statusMessage status)
     in case showDetails of
          HideDetails -> summary
          InsecurelyShowDetails -> summary <> " and body:\n" <> LT.toStrict (LT.decodeUtf8 (J.encode body))
  where
    showErr :: forall a. (Show a) => a -> Text
    showErr err =
      case showDetails of
        HideDetails -> ""
        InsecurelyShowDetails -> ":\n" <> tshow err

-- | Execute monad; as queries are performed, the record sets are
-- stored in the map.
newtype Execute a = Execute
  { unExecute :: ReaderT ExecuteReader (ExceptT ExecuteProblem IO) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader ExecuteReader,
      MonadIO,
      MonadError ExecuteProblem
    )

data BigQuery = BigQuery
  { query :: LT.Text,
    parameters :: InsOrdHashMap ParameterName Parameter
  }
  deriving (Show)

data Parameter = Parameter
  { typ :: ScalarType,
    value :: Value
  }
  deriving (Show)

newtype ParameterName
  = ParameterName LT.Text
  deriving (Show, J.ToJSON, Ord, Eq, Hashable)

data BigQueryField = BigQueryField
  { name :: FieldNameText,
    typ :: BigQueryFieldType,
    mode :: Mode
  }
  deriving (Show)

data BigQueryFieldType
  = FieldSTRING
  | FieldBYTES
  | FieldINTEGER
  | FieldFLOAT
  | FieldBOOL
  | FieldTIMESTAMP
  | FieldDATE
  | FieldTIME
  | FieldDATETIME
  | FieldGEOGRAPHY
  | FieldDECIMAL
  | FieldBIGDECIMAL
  | FieldJSON
  | FieldSTRUCT (Vector BigQueryField)
  deriving (Show)

data Mode
  = Nullable
  | NotNullable
  | Repeated
  deriving (Show)

data IsNullable
  = IsNullable
  | IsRequired

--------------------------------------------------------------------------------
-- Constants

-- | Delay between attempts to get job results if the job is incomplete.
streamDelaySeconds :: DiffTime
streamDelaySeconds = 1

bigQueryProjectUrl :: Text -> String
bigQueryProjectUrl projectId =
  "https://bigquery.googleapis.com/bigquery/v2/projects/" <> T.unpack projectId

--------------------------------------------------------------------------------
-- Executing the planned actions forest

runExecute ::
  (MonadIO m) =>
  BigQuerySourceConfig ->
  Execute (BigQuery.Job, RecordSet) ->
  m (Either ExecuteProblem (BigQuery.Job, RecordSet))
runExecute sourceConfig m =
  liftIO
    ( runExceptT
        ( runReaderT
            (unExecute (m >>= traverse getFinalRecordSet))
            (ExecuteReader {sourceConfig})
        )
    )

executeSelect :: Select -> Execute (BigQuery.Job, RecordSet)
executeSelect select = do
  conn <- asks (_scConnection . sourceConfig)
  (job, recordSet) <-
    streamBigQuery conn (selectToBigQuery select) >>= liftEither
  pure (job, recordSet {wantedFields = selectFinalWantedFields select})

-- | This is needed to strip out unneeded fields (join keys) in the
-- final query.  This is a relic of the data loader approach. A later
-- improvement would be to update the FromIr code to explicitly
-- reselect the query. But the purpose of this commit is to drop the
-- dataloader code and not modify the from IR code which is more
-- delicate.
getFinalRecordSet :: RecordSet -> Execute RecordSet
getFinalRecordSet recordSet =
  pure
    recordSet
      { rows =
          fmap
            ( InsOrdHashMap.filterWithKey
                ( \(FieldNameText k) _ ->
                    all (elem k) (wantedFields recordSet)
                )
            )
            (rows recordSet)
      }

--------------------------------------------------------------------------------
-- Make a big query from a select

selectToBigQuery :: Select -> BigQuery
selectToBigQuery select =
  BigQuery
    { query = LT.toLazyText query,
      parameters =
        InsOrdHashMap.fromList
          ( map
              ( \(int, (TypedValue typ value)) ->
                  ( ParameterName (LT.toLazyText (ToQuery.paramName int)),
                    Parameter {typ, value}
                  )
              )
              (InsOrdHashMap.toList params)
          )
    }
  where
    (query, params) =
      ToQuery.renderBuilderPretty (ToQuery.fromSelect select)

--------------------------------------------------------------------------------
-- JSON serialization

typeToBigQueryJson :: ScalarType -> J.Value
typeToBigQueryJson =
  \case
    DecimalScalarType -> atomic "NUMERIC"
    BigDecimalScalarType -> atomic "BIGNUMERIC"
    IntegerScalarType -> atomic "INTEGER"
    DateScalarType -> atomic "DATE"
    TimeScalarType -> atomic "TIME"
    DatetimeScalarType -> atomic "DATETIME"
    JsonScalarType -> atomic "JSON"
    TimestampScalarType -> atomic "TIMESTAMP"
    FloatScalarType -> atomic "FLOAT"
    GeographyScalarType -> atomic "GEOGRAPHY"
    StringScalarType -> atomic "STRING"
    BytesScalarType -> atomic "BYTES"
    BoolScalarType -> atomic "BOOL"
    StructScalarType -> atomic "STRUCT"
  where
    atomic ty = J.object ["type" J..= (ty :: Text)]

-- | Make a JSON representation of the type of the given value.
valueToBigQueryJson :: Value -> J.Value
valueToBigQueryJson = go
  where
    go =
      \case
        NullValue -> J.object [("value", J.Null)]
        DecimalValue i -> J.object ["value" .= i]
        BigDecimalValue i -> J.object ["value" .= i]
        IntegerValue i -> J.object ["value" .= i]
        FloatValue i -> J.object ["value" .= i]
        TimestampValue i -> J.object ["value" .= i]
        DateValue (Date i) -> J.object ["value" .= i]
        TimeValue (Time i) -> J.object ["value" .= i]
        DatetimeValue (Datetime i) -> J.object ["value" .= i]
        GeographyValue (Geography i) -> J.object ["value" .= i]
        StringValue i -> J.object ["value" .= J.String i]
        BytesValue i -> J.object ["value" .= i]
        JsonValue i -> J.object ["value" .= i]
        BoolValue i ->
          J.object
            [ "value"
                .= J.String
                  ( if i
                      then "true"
                      else "false"
                  )
            ]
        ArrayValue vs ->
          J.object ["array_values" .= J.Array (fmap go vs)]

--------------------------------------------------------------------------------
-- Execute a query as a job and stream the results into a record set

-- | TODO: WARNING: This function hasn't been tested on Big Data(tm),
-- and therefore I was unable to get BigQuery to produce paginated
-- results that would contain the 'pageToken' field in the JSON
-- response. Until that test has been done, we should consider this a
-- preliminary implementation.
streamBigQuery ::
  (MonadIO m) => BigQueryConnection -> BigQuery -> m (Either ExecuteProblem (BigQuery.Job, RecordSet))
streamBigQuery conn bigquery = do
  jobResult <- runExceptT $ createQueryJob conn bigquery
  case jobResult of
    Right job -> loop Nothing Nothing
      where
        loop pageToken mrecordSet = do
          results <- getJobResults conn job Fetch {pageToken}
          case results of
            Left problem -> pure (Left problem)
            Right
              ( JobComplete
                  JobResults
                    { pageToken = mpageToken',
                      recordSet = recordSet'@RecordSet {rows = rows'}
                    }
                ) -> do
                let extendedRecordSet =
                      case mrecordSet of
                        Nothing -> recordSet'
                        Just recordSet@RecordSet {rows} ->
                          (recordSet {rows = rows <> rows'})
                case mpageToken' of
                  Nothing -> pure (Right (job, extendedRecordSet))
                  Just pageToken' ->
                    loop (pure pageToken') (pure extendedRecordSet)
            Right JobIncomplete {} -> do
              liftIO (sleep streamDelaySeconds)
              loop pageToken mrecordSet
    Left e -> pure (Left e)

-- | Execute a query without expecting any output (e.g. CREATE TABLE or INSERT)
executeBigQuery :: (MonadIO m) => BigQueryConnection -> BigQuery -> m (Either ExecuteProblem ())
executeBigQuery conn bigquery = do
  jobResult <- runExceptT $ createQueryJob conn bigquery
  case jobResult of
    Right job -> loop Nothing
      where
        loop mrecordSet = do
          results <- getJobResults conn job Fetch {pageToken = Nothing}
          case results of
            Left problem -> pure (Left problem)
            Right (JobComplete _) -> pure (Right ())
            Right JobIncomplete {} -> do
              liftIO (sleep streamDelaySeconds)
              loop mrecordSet
    Left e -> pure (Left e)

--------------------------------------------------------------------------------
-- Querying results from a job

data JobResults = JobResults
  { pageToken :: Maybe Text,
    recordSet :: RecordSet
  }
  deriving (Show)

instance J.FromJSON JobResults where
  parseJSON =
    J.withObject
      "JobResults"
      ( \o -> do
          recordSet <- parseRecordSetPayload o
          pageToken <-
            fmap
              ( \mtoken -> do
                  token <- mtoken
                  guard (not (T.null token))
                  pure token
              )
              (o .:? "pageToken")
          pure JobResults {..}
      )

data JobResultsResponse
  = JobIncomplete
  | JobComplete JobResults
  deriving (Show)

instance J.FromJSON JobResultsResponse where
  parseJSON j =
    J.withObject
      "JobResultsResponse"
      ( \o -> do
          kind <- o .: "kind"
          if kind == ("bigquery#getQueryResultsResponse" :: Text)
            then do
              complete <- o .: "jobComplete"
              if complete
                then fmap JobComplete (J.parseJSON j)
                else pure JobIncomplete
            else fail ("Invalid kind: " <> show kind)
      )
      j

data Fetch = Fetch
  { pageToken :: Maybe Text
  }
  deriving (Show)

-- | Get results of a job.
getJobResults ::
  (MonadIO m) =>
  BigQueryConnection ->
  BigQuery.Job ->
  Fetch ->
  m (Either ExecuteProblem JobResultsResponse)
getJobResults conn Job {jobId, location} Fetch {pageToken} = runExceptT $ do
  -- https://cloud.google.com/bigquery/docs/reference/rest/v2/jobs/get#query-parameters
  let url =
        "GET "
          <> bigQueryProjectUrl (getBigQueryProjectId $ _bqProjectId conn)
          <> "/queries/"
          <> T.unpack jobId
          <> "?alt=json&prettyPrint=false"
          <> "&location="
          <> T.unpack location
          <> "&"
          <> T.unpack (encodeParams extraParameters)

      req =
        jsonRequestHeader (parseRequest_ url)

      extraParameters = pageTokenParam
        where
          pageTokenParam =
            case pageToken of
              Nothing -> []
              Just token -> [("pageToken", token)]

      encodeParams = T.intercalate "&" . map (\(k, v) -> k <> "=" <> v)

  resp <- runBigQueryExcept conn req
  case getResponseStatusCode resp of
    200 ->
      J.eitherDecode (getResponseBody resp)
        `onLeft` (throwError . GetJobDecodeProblem)
    _ ->
      throwError
        $ RESTRequestNonOK
          (getResponseStatus resp)
        $ parseAsJsonOrText
        $ getResponseBody resp

--------------------------------------------------------------------------------
-- Creating jobs

-- | Make a Request return `JSON`
jsonRequestHeader :: Request -> Request
jsonRequestHeader =
  setRequestHeader "Content-Type" ["application/json"]

-- | Create a job asynchronously.
createQueryJob :: (MonadError ExecuteProblem m, MonadIO m) => BigQueryConnection -> BigQuery -> m Job
createQueryJob conn BigQuery {..} = do
  let url =
        "POST "
          <> bigQueryProjectUrl (getBigQueryProjectId $ _bqProjectId conn)
          <> "/jobs?alt=json&prettyPrint=false"

      req =
        jsonRequestHeader
          $ setRequestBodyLBS body
          $ parseRequest_ url

      body =
        J.encode
          ( J.object
              [ "configuration"
                  .= J.object
                    [ "jobType" .= "QUERY",
                      "query"
                        .= J.object
                          [ "query" .= query,
                            "useLegacySql" .= False, -- Important, it makes `quotes` work properly.
                            "parameterMode" .= "NAMED",
                            "queryParameters"
                              .= map
                                ( \(name, Parameter {..}) ->
                                    J.object
                                      [ "name" .= J.toJSON name,
                                        "parameterType" .= typeToBigQueryJson typ,
                                        "parameterValue" .= valueToBigQueryJson value
                                      ]
                                )
                                (InsOrdHashMap.toList parameters)
                          ]
                    ]
              ]
          )

  resp <- runBigQueryExcept conn req
  case getResponseStatusCode resp of
    200 ->
      J.eitherDecode (getResponseBody resp)
        `onLeft` (throwError . CreateQueryJobDecodeProblem)
    _ ->
      throwError
        $ RESTRequestNonOK
          (getResponseStatus resp)
        $ parseAsJsonOrText
        $ getResponseBody resp

data Dataset = Dataset
  { datasetId :: Text
  }
  deriving (Show)

instance J.FromJSON Dataset where
  parseJSON =
    J.withObject
      "Dataset"
      ( \o -> do
          datasetId <- o .: "id"
          pure (Dataset datasetId)
      )

-- | Delete a dataset
deleteDataset :: (MonadError ExecuteProblem m, MonadIO m) => BigQueryConnection -> Text -> m ()
deleteDataset conn datasetId = do
  let url =
        "DELETE "
          <> bigQueryProjectUrl (getBigQueryProjectId $ _bqProjectId conn)
          <> "/datasets/"
          <> T.unpack datasetId
          <> "/?force=true&deleteContents=true"

  let req = jsonRequestHeader (parseRequest_ url)

  resp <- runBigQueryExcept conn req
  case getResponseStatusCode resp of
    204 -> pure ()
    _ ->
      throwError
        $ RESTRequestNonOK
          (getResponseStatus resp)
        $ parseAsJsonOrText
        $ getResponseBody resp

-- | Run request and map errors into ExecuteProblem
runBigQueryExcept ::
  (MonadError ExecuteProblem m, MonadIO m) =>
  BigQueryConnection ->
  Request ->
  m (Response BL.ByteString)
runBigQueryExcept conn req = do
  runBigQuery conn req >>= \case
    Right a -> pure a
    Left e -> throwError (ExecuteRunBigQueryProblem e)

-- | Insert a new dataset
insertDataset :: (MonadError ExecuteProblem m, MonadIO m) => BigQueryConnection -> Text -> m Dataset
insertDataset conn datasetId =
  do
    let url =
          "POST "
            <> bigQueryProjectUrl (getBigQueryProjectId $ _bqProjectId conn)
            <> "/datasets?alt=json&prettyPrint=false"

        req =
          jsonRequestHeader
            $ setRequestBodyLBS body
            $ parseRequest_ url

        body =
          J.encode
            ( J.object
                [ "id" .= datasetId,
                  "datasetReference"
                    .= J.object
                      [ "datasetId" .= datasetId,
                        "projectId" .= _bqProjectId conn
                      ]
                ]
            )

    resp <- runBigQueryExcept conn req
    case getResponseStatusCode resp of
      200 ->
        J.eitherDecode (getResponseBody resp)
          `onLeft` (throwError . InsertDatasetDecodeProblem)
      _ ->
        throwError
          $ RESTRequestNonOK
            (getResponseStatus resp)
          $ parseAsJsonOrText
          $ getResponseBody resp

-- | Parse given @'ByteString' as JSON value. If not a valid JSON, encode to plain text.
parseAsJsonOrText :: BL.ByteString -> J.Value
parseAsJsonOrText bytestring =
  fromMaybe (J.String $ lbsToTxt bytestring) $ J.decode bytestring

--------------------------------------------------------------------------------
-- Consuming recordset from big query

parseRecordSetPayload :: J.Object -> J.Parser RecordSet
parseRecordSetPayload resp = do
  mSchema <- resp .:? "schema"
  columns <- maybe (pure V.empty) (.: "fields") mSchema :: J.Parser (Vector BigQueryField)
  rowsJSON <- fmap (fromMaybe V.empty) (resp .:? "rows" :: J.Parser (Maybe (Vector J.Value)))
  rows <-
    V.imapM
      (\i row -> parseRow columns row J.<?> J.Index i)
      rowsJSON
      J.<?> J.Key "rows"
  pure RecordSet {wantedFields = Nothing, rows}

--------------------------------------------------------------------------------
-- Schema-driven JSON deserialization

parseRow :: Vector BigQueryField -> J.Value -> J.Parser (InsOrdHashMap FieldNameText OutputValue)
parseRow columnTypes value = do
  result <- parseBigQueryRow columnTypes value
  case result of
    RecordOutputValue row -> pure row
    _ -> fail ("Expected a record when parsing a top-level row: " ++ show value)

-- | Parse a row, which at the top-level of the "rows" output has no
-- {"v":..} wrapper. But when appearing nestedly, does have the
-- wrapper. See 'parseBigQueryValue'.
parseBigQueryRow :: Vector BigQueryField -> J.Value -> J.Parser OutputValue
parseBigQueryRow columnTypes =
  J.withObject
    "RECORD"
    ( \o -> do
        fields <- o .: "f" J.<?> J.Key "RECORD"
        values <-
          sequence
            ( V.izipWith
                ( \i typ field ->
                    parseBigQueryField typ field J.<?> J.Index i
                )
                columnTypes
                fields
            )
            J.<?> J.Key "f"
        pure (RecordOutputValue (InsOrdHashMap.fromList (V.toList values)))
    )

parseBigQueryValue :: IsNullable -> BigQueryFieldType -> J.Value -> J.Parser OutputValue
parseBigQueryValue isNullable fieldType object =
  case fieldType of
    FieldSTRUCT types ->
      has_v isNullable (parseBigQueryRow types) object J.<?> J.Key "RECORD"
    FieldDECIMAL ->
      has_v isNullable (fmap DecimalOutputValue . J.parseJSON) object
        J.<?> J.Key "DECIMAL"
    FieldBIGDECIMAL ->
      has_v isNullable (fmap BigDecimalOutputValue . J.parseJSON) object
        J.<?> J.Key "BIGDECIMAL"
    FieldINTEGER ->
      has_v isNullable (fmap IntegerOutputValue . J.parseJSON) object
        J.<?> J.Key "INTEGER"
    FieldDATE ->
      has_v isNullable (fmap DateOutputValue . J.parseJSON) object
        J.<?> J.Key "DATE"
    FieldTIME ->
      has_v isNullable (fmap TimeOutputValue . J.parseJSON) object
        J.<?> J.Key "TIME"
    FieldDATETIME ->
      has_v isNullable (fmap DatetimeOutputValue . J.parseJSON) object
        J.<?> J.Key "DATETIME"
    FieldTIMESTAMP ->
      has_v isNullable (fmap TimestampOutputValue . parseTimestamp) object
        J.<?> J.Key "TIMESTAMP"
    FieldGEOGRAPHY ->
      has_v isNullable (fmap GeographyOutputValue . J.parseJSON) object
        J.<?> J.Key "GEOGRAPHY"
    FieldFLOAT ->
      has_v isNullable (fmap FloatOutputValue . J.parseJSON) object
        J.<?> J.Key "FLOAT"
    FieldBOOL ->
      has_v isNullable (fmap (BoolOutputValue . (== "true")) . J.parseJSON) object
        J.<?> J.Key "BOOL"
    FieldSTRING ->
      has_v isNullable (fmap TextOutputValue . J.parseJSON) object
        J.<?> J.Key "STRING"
    FieldBYTES ->
      has_v isNullable (fmap BytesOutputValue . J.parseJSON) object
        J.<?> J.Key "BYTES"
    FieldJSON ->
      has_v isNullable (fmap JsonOutputValue . parseJson) object
        J.<?> J.Key "JSON"

-- | This is a little unfortunate: in its JSON responses, BigQuery gives JSON
-- fields as strings. So, to parse a JSON response, we need to parse it out of
-- a JSON string type, hence the unintuitive type signature here.
parseJson :: J.Value -> J.Parser J.Value
parseJson = J.withText "JSON" \str ->
  J.eitherDecode (txtToLbs str) `onLeft` fail

-- | Parse upstream timestamp value in epoch milliseconds and convert it to calendar date time format
-- https://cloud.google.com/bigquery/docs/reference/standard-sql/data-types#timestamp_type
parseTimestamp :: J.Value -> J.Parser Timestamp
parseTimestamp =
  fmap (Timestamp . utctimeToISO8601Text) . J.withText "FieldTIMESTAMP" textToUTCTime
  where
    textToUTCTime :: Text -> J.Parser UTCTime
    textToUTCTime =
      either fail (pure . flip addUTCTime (UTCTime (fromGregorian 1970 0 0) 0) . fst)
        . (TR.rational :: TR.Reader NominalDiffTime)

    utctimeToISO8601Text :: UTCTime -> Text
    utctimeToISO8601Text = T.pack . iso8601Show

parseBigQueryField :: BigQueryField -> J.Value -> J.Parser (FieldNameText, OutputValue)
parseBigQueryField BigQueryField {name, typ, mode} value1 =
  case mode of
    Repeated ->
      ( do
          values <- has_v_generic J.parseJSON value1
          outputs <-
            V.imapM
              ( \i value2 ->
                  parseBigQueryValue IsRequired typ value2
                    J.<?> J.Index i
              )
              values
          pure (name, ArrayOutputValue outputs)
      )
        J.<?> J.Key "REPEATED"
    Nullable -> do
      output <-
        parseBigQueryValue IsNullable typ value1 J.<?> J.Key "NULLABLE"
      pure (name, output)
    NotNullable -> do
      output <-
        parseBigQueryValue IsRequired typ value1 J.<?> J.Key "REQUIRED"
      pure (name, output)

-- Every value, after the top-level row, is wrapped in this.
has_v ::
  IsNullable ->
  (J.Value -> J.Parser OutputValue) ->
  J.Value ->
  J.Parser OutputValue
has_v isNullable f =
  J.withObject
    "HAS_V"
    ( \o ->
        o .: "v" >>= \v ->
          case v of
            J.Null
              | IsNullable <- isNullable -> pure NullOutputValue
            _ -> f v J.<?> J.Key "v"
    )

-- Every value, after the top-level row, is wrapped in this.
has_v_generic ::
  (J.Value -> J.Parser a) ->
  J.Value ->
  J.Parser a
has_v_generic f =
  J.withObject
    "HAS_V"
    (\o -> o .: "v" >>= \v -> (f v J.<?> J.Key "v"))

--------------------------------------------------------------------------------
-- Generic JSON deserialization

instance J.FromJSON BigQueryField where
  parseJSON =
    J.withObject
      "BigQueryField"
      ( \o -> do
          name <- o .: "name"
          typ <-
            do
              flag :: Text <- o .: "type"
              if
                | flag == "NUMERIC" || flag == "DECIMAL" -> pure FieldDECIMAL
                | flag == "BIGNUMERIC" || flag == "BIGDECIMAL" ->
                    pure FieldBIGDECIMAL
                | flag == "INT64" || flag == "INTEGER" -> pure FieldINTEGER
                | flag == "FLOAT64" || flag == "FLOAT" -> pure FieldFLOAT
                | flag == "BOOLEAN" || flag == "BOOL" -> pure FieldBOOL
                | flag == "STRING" -> pure FieldSTRING
                | flag == "JSON" -> pure FieldJSON
                | flag == "DATE" -> pure FieldDATE
                | flag == "TIME" -> pure FieldTIME
                | flag == "DATETIME" -> pure FieldDATETIME
                | flag == "TIMESTAMP" -> pure FieldTIMESTAMP
                | flag == "GEOGRAPHY" -> pure FieldGEOGRAPHY
                | flag == "BYTES" -> pure FieldBYTES
                | flag == "RECORD" || flag == "STRUCT" ->
                    do
                      fields <- o .: "fields"
                      pure (FieldSTRUCT fields)
                | otherwise -> fail ("Unsupported field type: " ++ show flag)
          mode <- o .:? "mode" .!= Nullable
          pure BigQueryField {..}
      )

instance J.FromJSON Mode where
  parseJSON j = do
    s <- J.parseJSON j
    case s :: Text of
      "NULLABLE" -> pure Nullable
      "REPEATED" -> pure Repeated
      _ -> pure NotNullable
