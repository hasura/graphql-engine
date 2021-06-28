{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules  #-}

-- | Execute a Select query against the BigQuery REST API.

module Hasura.Backends.BigQuery.Execute
  ( executeSelect
  , runExecute
  , streamBigQuery
  , BigQuery(..)
  , OutputValue(..)
  , RecordSet(..)
  , Execute
  , Value(..)
  , FieldNameText(..)
  ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception.Safe
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson                          ((.:), (.:?), (.=))
import qualified Data.Aeson                          as Aeson
import qualified Data.Aeson.Types                    as Aeson
import qualified Data.ByteString.Lazy                as L
import           Data.Foldable
import qualified Data.HashMap.Strict.InsOrd          as OMap
import           Data.Maybe
import qualified Data.Text                           as T
import qualified Data.Text.Lazy                      as LT
import qualified Data.Text.Lazy.Builder              as LT
import           Data.Vector                         (Vector)
import qualified Data.Vector                         as V
import           GHC.Generics
import           Hasura.Backends.BigQuery.Connection
import           Hasura.Backends.BigQuery.Source
import qualified Hasura.Backends.BigQuery.ToQuery    as ToQuery
import           Hasura.Backends.BigQuery.Types      as BigQuery
import           Hasura.Prelude                      hiding (head, state, tail)
import           Network.HTTP.Simple
import           Network.HTTP.Types

--------------------------------------------------------------------------------
-- Types

-- | A set of records produced by the database. These are joined
-- together. There are all sorts of optimizations possible here, from
-- using a matrix/flat vector, unboxed sums for Value, etc. Presently
-- we choose a naive implementation in the interest of getting other
-- work done.
data RecordSet = RecordSet
  { rows         :: !(Vector (InsOrdHashMap FieldNameText OutputValue))
  , wantedFields :: !(Maybe [Text])
  } deriving (Show)

-- | As opposed to BigQuery.FieldName which is a qualified name, this
-- is just the unqualified text name itself.
newtype FieldNameText =
  FieldNameText Text
  deriving (Show, Ord, Eq, Hashable, Aeson.FromJSON, Aeson.ToJSONKey, IsString)

data OutputValue
  = DecimalOutputValue !Decimal
  | BigDecimalOutputValue !BigDecimal
  | IntegerOutputValue !Int64
  | FloatOutputValue !Float64
  | GeographyOutputValue !Geography
  | TextOutputValue !Text
  | TimestampOutputValue !Timestamp
  | DateOutputValue !Date
  | TimeOutputValue !Time
  | DatetimeOutputValue !Datetime
  | BytesOutputValue !Base64
  | BoolOutputValue !Bool
  | ArrayOutputValue !(Vector OutputValue)
  | RecordOutputValue !(InsOrdHashMap FieldNameText OutputValue)
  | NullOutputValue -- TODO: Consider implications.
  deriving (Show, Eq, Generic)
instance Hashable OutputValue
instance Aeson.ToJSON OutputValue where
  toJSON = \case
    NullOutputValue           -> Aeson.toJSON Aeson.Null
    DecimalOutputValue !i     -> Aeson.toJSON i
    BigDecimalOutputValue !i  -> Aeson.toJSON i
    FloatOutputValue !i       -> Aeson.toJSON i
    TextOutputValue !i        -> Aeson.toJSON i
    BytesOutputValue !i       -> Aeson.toJSON i
    DateOutputValue !i        -> Aeson.toJSON i
    TimestampOutputValue !i   -> Aeson.toJSON i
    TimeOutputValue !i        -> Aeson.toJSON i
    DatetimeOutputValue !i    -> Aeson.toJSON i
    GeographyOutputValue !i   -> Aeson.toJSON i
    BoolOutputValue !i        -> Aeson.toJSON i
    IntegerOutputValue !i     -> Aeson.toJSON i
    ArrayOutputValue !vector  -> Aeson.toJSON vector
    RecordOutputValue !record -> Aeson.toJSON record

data ExecuteReader = ExecuteReader
  { credentials :: !BigQuerySourceConfig
  }

data ExecuteProblem
  = GetJobDecodeProblem String
  | CreateQueryJobDecodeProblem String
  | ErrorResponseFromServer Status L.ByteString
  | GetJobResultsProblem SomeException
  | RESTRequestNonOK Status Text
  | CreateQueryJobProblem SomeException
  | ExecuteRunBigQueryProblem BigQueryProblem
  deriving (Show)

-- | Execute monad; as queries are performed, the record sets are
-- stored in the map.
newtype Execute a = Execute
  { unExecute :: ReaderT ExecuteReader (ExceptT ExecuteProblem IO) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader ExecuteReader
             , MonadIO
             , MonadError ExecuteProblem
             )

-- | Big query parameters must be accompanied by an explicit type
-- signature.
data BigQueryType
  = DECIMAL
  | INTEGER
  | FLOAT
  | BYTES
  | STRING
  | BOOL
  | ARRAY BigQueryType
  | GEOGRAPHY
  | DATE
  | TIMESTAMP
  | DATETIME
  | TIME
  | BIGDECIMAL
  deriving (Show, Eq)

data BigQuery = BigQuery
  { query       :: !LT.Text
  , parameters  :: !(InsOrdHashMap ParameterName Parameter)
  , cardinality :: BigQuery.Cardinality
  } deriving (Show)

data Parameter = Parameter
  { typ   :: !BigQueryType
  , value :: !Value
  } deriving (Show)

newtype ParameterName =
  ParameterName LT.Text deriving (Show, Aeson.ToJSON, Ord, Eq, Hashable)

data BigQueryField = BigQueryField
  { name :: !FieldNameText
  , typ  :: !BigQueryFieldType
  , mode :: !Mode
  } deriving (Show)

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
streamDelaySeconds :: Int
streamDelaySeconds = 1

--------------------------------------------------------------------------------
-- Executing the planned actions forest

runExecute ::
     MonadIO m
  => BigQuerySourceConfig
  -> Execute RecordSet
  -> m (Either ExecuteProblem RecordSet)
runExecute credentials m =
  liftIO
    (runExceptT
       (runReaderT
          (unExecute (m >>= getFinalRecordSet))
          (ExecuteReader {credentials})))

executeSelect :: Select -> Execute RecordSet
executeSelect select = do
  credentials <- asks credentials
  recordSet <-
    streamBigQuery credentials (selectToBigQuery select) >>= liftEither
  pure recordSet {wantedFields = selectFinalWantedFields select}

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
            (\row ->
               OMap.filterWithKey
                 (\(FieldNameText k) _ ->
                    maybe True (elem k) (wantedFields recordSet))
                 row)
            (rows recordSet)
      }

--------------------------------------------------------------------------------
-- Make a big query from a select

selectToBigQuery :: Select -> BigQuery
selectToBigQuery select =
  BigQuery
    { query = LT.toLazyText query
    , parameters =
        OMap.fromList
          (map
             (\(int, value) ->
                ( ParameterName (LT.toLazyText (ToQuery.paramName int))
                , Parameter {typ = valueType value, value}))
             (OMap.toList params))
    , cardinality = selectCardinality select
    }
  where
    (query, params) =
      ToQuery.renderBuilderPretty (ToQuery.fromSelect select)

--------------------------------------------------------------------------------
-- Type system

-- | Make a BigQuery type for the given value.
valueType :: Value -> BigQueryType
valueType =
  \case
    DecimalValue {} -> DECIMAL
    BigDecimalValue {} -> BIGDECIMAL
    IntegerValue {} -> INTEGER
    FloatValue {} -> FLOAT
    GeographyValue {} -> GEOGRAPHY
    StringValue {} -> STRING
    BytesValue {} -> BYTES
    BoolValue {} -> BOOL
    DatetimeValue {} -> DATETIME
    TimeValue {} -> TIME
    DateValue {} -> DATE
    TimestampValue {} -> TIMESTAMP
    ArrayValue values ->
      ARRAY
        (case values V.!? 0 of
           Just v  -> valueType v
           -- Above: We base the type from the first element. Later,
           -- we could add some kind of sanity check that they are all
           -- the same type.
           Nothing -> STRING
           -- Above: If the array is null, it doesn't matter what type
           -- the element is. So we put STRING.
         )
    NullValue -> STRING
    -- Above: If the value is null, it doesn't matter what type
    -- the element is. So we put STRING.

--------------------------------------------------------------------------------
-- JSON serialization

-- | Make a JSON representation of the type of the given value.
valueToBigQueryJson :: Value -> Aeson.Value
valueToBigQueryJson = go
  where
    go =
      \case
        NullValue -> Aeson.Null -- TODO: I haven't tested whether BigQuery is happy with this null value.
        DecimalValue i -> Aeson.object ["value" .= i]
        BigDecimalValue i -> Aeson.object ["value" .= i]
        IntegerValue i -> Aeson.object ["value" .= i]
        FloatValue i -> Aeson.object ["value" .= i]
        TimestampValue i -> Aeson.object ["value" .= i]
        DateValue (Date i) -> Aeson.object ["value" .= i]
        TimeValue (Time i) -> Aeson.object ["value" .= i]
        DatetimeValue (Datetime i) -> Aeson.object ["value" .= i]
        GeographyValue (Geography i) -> Aeson.object ["value" .= i]
        StringValue i -> Aeson.object ["value" .= Aeson.String i]
        BytesValue i -> Aeson.object ["value" .= i]
        BoolValue i ->
          Aeson.object
            [ "value" .=
              Aeson.String
                (if i
                   then "true"
                   else "false")
            ]
        ArrayValue vs ->
          Aeson.object ["array_values" .= Aeson.Array (fmap go vs)]

--------------------------------------------------------------------------------
-- Execute a query as a job and stream the results into a record set

-- | TODO: WARNING: This function hasn't been tested on Big Data(tm),
-- and therefore I was unable to get BigQuery to produce paginated
-- results that would contain the 'pageToken' field in the JSON
-- response. Until that test has been done, we should consider this a
-- preliminary implementation.
streamBigQuery ::
     MonadIO m => BigQuerySourceConfig -> BigQuery -> m (Either ExecuteProblem RecordSet)
streamBigQuery credentials bigquery = do
  jobResult <- createQueryJob credentials bigquery
  case jobResult of
    Right job -> do records <- loop Nothing Nothing
                    -- liftIO (print records)
                    pure records
      where loop pageToken mrecordSet = do
              results <- getJobResults credentials job Fetch {pageToken}
              case results of
                Left problem -> pure (Left problem)
                Right (JobComplete JobResults { pageToken = mpageToken'
                                              , recordSet = recordSet'@RecordSet {rows = rows'}
                                              }) -> do
                  let extendedRecordSet =
                        case mrecordSet of
                          Nothing -> recordSet'
                          Just recordSet@RecordSet {rows} ->
                            (recordSet {rows = rows <> rows'})
                  case mpageToken' of
                    Nothing -> pure (Right extendedRecordSet)
                    Just pageToken' ->
                      loop (pure pageToken') (pure extendedRecordSet)
                Right JobIncomplete {} -> do
                  liftIO (threadDelay (1000 * 1000 * streamDelaySeconds))
                  loop pageToken mrecordSet
    Left e -> pure (Left e)

--------------------------------------------------------------------------------
-- Querying results from a job

data JobResults = JobResults
  { pageToken :: Maybe Text
  , recordSet :: RecordSet
  } deriving (Show)

instance Aeson.FromJSON JobResults where
  parseJSON =
    Aeson.withObject
      "JobResults"
      (\o -> do
         recordSet <- parseRecordSetPayload o
         pageToken <-
           fmap
             (\mtoken -> do
                token <- mtoken
                guard (not (T.null token))
                pure token)
             (o .:? "pageToken")
         pure JobResults {..})

data JobResultsResponse
  = JobIncomplete
  | JobComplete JobResults
  deriving (Show)

instance Aeson.FromJSON JobResultsResponse where
  parseJSON j =
    Aeson.withObject
      "JobResultsResponse"
      (\o -> do
         kind <- o .: "kind"
         if kind == ("bigquery#getQueryResultsResponse" :: Text)
           then do
             complete <- o .: "jobComplete"
             if complete
               then fmap JobComplete (Aeson.parseJSON j)
               else pure JobIncomplete
           else fail ("Invalid kind: " <> show kind))
      j

data Fetch = Fetch
  { pageToken :: Maybe Text
  } deriving (Show)

-- | Get results of a job.
getJobResults ::
     MonadIO m
  => BigQuerySourceConfig
  -> Job
  -> Fetch
  -> m (Either ExecuteProblem JobResultsResponse)
getJobResults sc@BigQuerySourceConfig {..} Job {jobId} Fetch {pageToken} =
  liftIO (catchAny run (pure . Left . GetJobResultsProblem))
  where
    url =
      "GET https://bigquery.googleapis.com/bigquery/v2/projects/" <>
       T.unpack _scProjectId <>
       "/queries/" <>
       T.unpack jobId <>
       "?alt=json&key=" <>
       -- T.unpack apiToken <>
       "&" <>
       T.unpack (encodeParams extraParameters)
    run = do
      let req = setRequestHeader "Content-Type" ["application/json"]
                  $ parseRequest_ url
      eResp <- runBigQuery sc req
      case eResp of
        Left e -> pure (Left (ExecuteRunBigQueryProblem e))
        Right resp ->
          case getResponseStatusCode resp of
            200 -> case Aeson.eitherDecode (getResponseBody resp) of
                     Left e        -> pure (Left (GetJobDecodeProblem e))
                     Right results -> pure (Right results)
            _   -> do
                    pure $ Left $ RESTRequestNonOK (getResponseStatus resp) $ lbsToTxt $ getResponseBody resp
    extraParameters = pageTokenParam
      where
        pageTokenParam =
          case pageToken of
            Nothing    -> []
            Just token -> [("pageToken", token)]
    encodeParams = T.intercalate "&" . map (\(k, v) -> k <> "=" <> v)

--------------------------------------------------------------------------------
-- Creating jobs

data Job = Job
  { state :: Text
  , jobId :: Text
  } deriving (Show)

instance Aeson.FromJSON Job where
  parseJSON =
    Aeson.withObject
      "Job"
      (\o -> do
         kind <- o .: "kind"
         if kind == ("bigquery#job" :: Text)
           then do
             state <-
               do status <- o .: "status"
                  status .: "state"
             jobId <-
               do ref <- o .: "jobReference"
                  ref .: "jobId"
             pure Job {state, jobId}
           else fail ("Invalid kind: " <> show kind))

-- | Create a job asynchronously.
createQueryJob :: MonadIO m => BigQuerySourceConfig -> BigQuery -> m (Either ExecuteProblem Job)
createQueryJob sc@BigQuerySourceConfig {..} BigQuery {..} =
  liftIO (do -- putStrLn (LT.unpack query)
             catchAny run (pure . Left . CreateQueryJobProblem))
  where
    run = do
      let url = "POST https://content-bigquery.googleapis.com/bigquery/v2/projects/" <>
                T.unpack _scProjectId <>
                "/jobs?alt=json&key="
                -- <> T.unpack apiToken
      let req = setRequestHeader "Content-Type" ["application/json"]
                  $ setRequestBodyLBS body
                  $ parseRequest_ url
      eResp <- runBigQuery sc req
      case eResp of
        Left e -> pure (Left (ExecuteRunBigQueryProblem e))
        Right resp ->
          case getResponseStatusCode resp of
            200 ->
              case Aeson.eitherDecode (getResponseBody resp) of
                Left e    -> pure (Left (CreateQueryJobDecodeProblem e))
                Right job -> pure (Right job)
            _ -> do

                    pure $ Left $ RESTRequestNonOK (getResponseStatus resp) $ lbsToTxt $ getResponseBody resp
    body =
      Aeson.encode
        (Aeson.object
           [ "configuration" .=
             Aeson.object
               [ "jobType" .= "QUERY"
               , "query" .=
                 Aeson.object
                   [ "query" .= query
                   , "useLegacySql" .= False -- Important, it makes `quotes` work properly.
                   , "parameterMode" .= "NAMED"
                   , "queryParameters" .=
                     map
                       (\(name, Parameter {..}) ->
                          Aeson.object
                            [ "name" .= Aeson.toJSON name
                            , "parameterType" .= Aeson.toJSON typ
                            , "parameterValue" .= valueToBigQueryJson value
                            ])
                       (OMap.toList parameters)
                   ]
               ]
           ])

--------------------------------------------------------------------------------
-- Consuming recordset from big query

parseRecordSetPayload :: Aeson.Object -> Aeson.Parser RecordSet
parseRecordSetPayload resp = do
  schema <- resp .: "schema"
  columns <- schema .: "fields" :: Aeson.Parser (Vector BigQueryField)
  rowsJSON <- fmap (fromMaybe mempty) (resp .:? "rows" :: Aeson.Parser (Maybe (Vector Aeson.Value)))
  rows <-
    V.imapM
      (\i row -> parseRow columns row Aeson.<?> Aeson.Index i)
      rowsJSON Aeson.<?> Aeson.Key "rows"
  pure RecordSet {wantedFields = Nothing, rows}

--------------------------------------------------------------------------------
-- Schema-driven JSON deserialization

parseRow :: Vector BigQueryField -> Aeson.Value -> Aeson.Parser (InsOrdHashMap FieldNameText OutputValue)
parseRow columnTypes value = do
  result <- parseBigQueryRow columnTypes value
  case result of
    RecordOutputValue row -> pure row
    _                     -> fail ("Expected a record when parsing a top-level row: " ++ show value)

-- | Parse a row, which at the top-level of the "rows" output has no
-- {"v":..} wrapper. But when appearing nestedly, does have the
-- wrapper. See 'parseBigQueryValue'.
parseBigQueryRow :: Vector BigQueryField -> Aeson.Value -> Aeson.Parser OutputValue
parseBigQueryRow columnTypes =
  Aeson.withObject
    "RECORD"
    (\o -> do
       fields <- o .: "f" Aeson.<?> Aeson.Key "RECORD"
       values <-
         sequence
           (V.izipWith
              (\i typ field ->
                 parseBigQueryField typ field Aeson.<?> Aeson.Index i)
              columnTypes
              fields) Aeson.<?>
         Aeson.Key "f"
       pure (RecordOutputValue (OMap.fromList (V.toList values))))

parseBigQueryValue :: IsNullable -> BigQueryFieldType -> Aeson.Value -> Aeson.Parser OutputValue
parseBigQueryValue isNullable fieldType object =
  case fieldType of
    FieldSTRUCT types ->
      has_v isNullable (parseBigQueryRow types) object Aeson.<?> Aeson.Key "RECORD"
    FieldDECIMAL ->
      has_v isNullable (fmap DecimalOutputValue . Aeson.parseJSON) object Aeson.<?>
      Aeson.Key "DECIMAL"
    FieldBIGDECIMAL ->
      has_v isNullable (fmap BigDecimalOutputValue . Aeson.parseJSON) object Aeson.<?>
      Aeson.Key "BIGDECIMAL"
    FieldINTEGER ->
      has_v isNullable (fmap IntegerOutputValue . Aeson.parseJSON) object Aeson.<?>
      Aeson.Key "INTEGER"
    FieldDATE ->
      has_v isNullable (fmap DateOutputValue . Aeson.parseJSON) object Aeson.<?>
      Aeson.Key "DATE"
    FieldTIME ->
      has_v isNullable (fmap TimeOutputValue . Aeson.parseJSON) object Aeson.<?>
      Aeson.Key "TIME"
    FieldDATETIME ->
      has_v isNullable (fmap DatetimeOutputValue . Aeson.parseJSON) object Aeson.<?>
      Aeson.Key "DATETIME"
    FieldTIMESTAMP ->
      has_v isNullable (fmap (TimestampOutputValue . Timestamp . utctimeToISO8601Text) . Aeson.withText "FieldTIMESTAMP" textToUTCTime) object Aeson.<?>
      Aeson.Key "TIMESTAMP"
    FieldGEOGRAPHY ->
      has_v isNullable (fmap GeographyOutputValue . Aeson.parseJSON) object Aeson.<?>
      Aeson.Key "GEOGRAPHY"
    FieldFLOAT ->
      has_v isNullable (fmap FloatOutputValue . Aeson.parseJSON) object Aeson.<?>
      Aeson.Key "FLOAT"
    FieldBOOL ->
      has_v isNullable (fmap (BoolOutputValue . (== "true")) . Aeson.parseJSON) object Aeson.<?>
      Aeson.Key "BOOL"
    FieldSTRING ->
      has_v isNullable (fmap TextOutputValue . Aeson.parseJSON) object Aeson.<?>
      Aeson.Key "STRING"
    FieldBYTES ->
      has_v isNullable (fmap BytesOutputValue . Aeson.parseJSON) object Aeson.<?>
      Aeson.Key "BYTES"

parseBigQueryField :: BigQueryField -> Aeson.Value -> Aeson.Parser (FieldNameText, OutputValue)
parseBigQueryField BigQueryField {name, typ, mode} value1 =
  case mode of
    Repeated ->
      (do values <- has_v_generic Aeson.parseJSON value1
          outputs <-
            V.imapM
              (\i value2 ->
                 parseBigQueryValue IsRequired typ value2 Aeson.<?>
                 Aeson.Index i)
              values
          pure (name, ArrayOutputValue outputs)) Aeson.<?>
      Aeson.Key "REPEATED"
    Nullable -> do
      output <-
        parseBigQueryValue IsNullable typ value1 Aeson.<?> Aeson.Key "NULLABLE"
      pure (name, output)
    NotNullable -> do
      output <-
        parseBigQueryValue IsRequired typ value1 Aeson.<?> Aeson.Key "REQUIRED"
      pure (name, output)

-- Every value, after the top-level row, is wrapped in this.
has_v ::
     IsNullable
  -> (Aeson.Value -> Aeson.Parser OutputValue)
  -> Aeson.Value
  -> Aeson.Parser OutputValue
has_v isNullable f =
  Aeson.withObject
    "HAS_V"
    (\o ->
       o .: "v" >>= \v ->
         case v of
           Aeson.Null
             | IsNullable <- isNullable -> pure NullOutputValue
           _ -> f v Aeson.<?> Aeson.Key "v")

-- Every value, after the top-level row, is wrapped in this.
has_v_generic ::
     (Aeson.Value -> Aeson.Parser a)
  -> Aeson.Value
  -> Aeson.Parser a
has_v_generic f =
  Aeson.withObject
    "HAS_V"
    (\o -> o .: "v" >>= \v -> (f v Aeson.<?> Aeson.Key "v"))

--------------------------------------------------------------------------------
-- Generic JSON deserialization

instance Aeson.ToJSON BigQueryType where
  toJSON =
    \case
      ARRAY t    -> Aeson.object ["type" .= ("ARRAY" :: Text), "arrayType" .= t]
      DECIMAL    -> atomic "NUMERIC"
      BIGDECIMAL -> atomic "BIGNUMERIC"
      INTEGER    -> atomic "INTEGER"
      DATE       -> atomic "DATE"
      TIME       -> atomic "TIME"
      DATETIME   -> atomic "DATETIME"
      TIMESTAMP  -> atomic "TIMESTAMP"
      FLOAT      -> atomic "FLOAT"
      GEOGRAPHY  -> atomic "GEOGRAPHY"
      STRING     -> atomic "STRING"
      BYTES      -> atomic "BYTES"
      BOOL       -> atomic "BOOL"
    where
      atomic ty = Aeson.object ["type" .= (ty :: Text)]

instance Aeson.FromJSON BigQueryField where
  parseJSON =
    Aeson.withObject
      "BigQueryField"
      (\o -> do
         name <- o .: "name"
         typ <-
           do flag :: Text <- o .: "type"
              if | flag == "NUMERIC" || flag == "DECIMAL" -> pure FieldDECIMAL
                 | flag == "BIGNUMERIC" || flag == "BIGDECIMAL" ->
                   pure FieldBIGDECIMAL
                 | flag == "INT64" || flag == "INTEGER" -> pure FieldINTEGER
                 | flag == "FLOAT64" || flag == "FLOAT" -> pure FieldFLOAT
                 | flag == "BOOLEAN" || flag == "BOOL" -> pure FieldBOOL
                 | flag == "STRING" -> pure FieldSTRING
                 | flag == "DATE" -> pure FieldDATE
                 | flag == "TIME" -> pure FieldTIME
                 | flag == "DATETIME" -> pure FieldDATETIME
                 | flag == "TIMESTAMP" -> pure FieldTIMESTAMP
                 | flag == "GEOGRAPHY" -> pure FieldGEOGRAPHY
                 | flag == "BYTES" -> pure FieldBYTES
                 | flag == "RECORD" || flag == "STRUCT" ->
                   do fields <- o .: "fields"
                      pure (FieldSTRUCT fields)
                 | otherwise -> fail ("Unsupported field type: " ++ show flag)
         mode <- o .: "mode"
         pure BigQueryField {..})

instance Aeson.FromJSON Mode where
  parseJSON j = do
    s <- Aeson.parseJSON j
    case s :: Text of
      "NULLABLE" -> pure Nullable
      "REPEATED" -> pure Repeated
      _          -> pure NotNullable
