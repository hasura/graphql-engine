{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- | Execute the plan given from .Plan.

module Hasura.SQL.BigQuery.Execute
  ( execute
  , runExecute
  , getCredentialsEnv
  , Credentials(..)
  , RecordSet(..)
  , Execute
  , Value(..)
  ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception.Safe
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson ((.=),(.:),(.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as L
import           Data.Foldable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as OMap
import           Data.Hashable (Hashable)
import           Data.IORef
import           Data.Int
import           Data.Maybe
import           Data.Sequence (Seq)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.Text.Read as T
import           Data.Tree
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           GHC.Generics
import           Hasura.SQL.BigQuery.Credentials
import qualified Hasura.SQL.BigQuery.Plan as Plan
import qualified Hasura.SQL.BigQuery.Plan as Select (Select (..))
import qualified Hasura.SQL.BigQuery.ToQuery as ToQuery
import           Hasura.SQL.BigQuery.Types as BigQuery
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           Prelude hiding (head,tail)

--------------------------------------------------------------------------------
-- Types

-- | A set of records produced by the database. These are joined
-- together. There are all sorts of optimizations possible here, from
-- using a matrix/flat vector, unboxed sums for Value, etc. Presently
-- we choose a naive implementation in the interest of getting other
-- work done.
data RecordSet = RecordSet
  { origin :: !(Maybe Plan.PlannedAction)
  , rows :: !(Vector (InsOrdHashMap Plan.FieldName OutputValue))
  , wantedFields :: !(Maybe [Text])
  } deriving (Show)

data OutputValue
  = IntegerOutputValue !Int
  | TextOutputValue !Text
  | BoolOutputValue !Bool
  | ArrayOutputValue !(Vector OutputValue)
  | RecordOutputValue !(InsOrdHashMap Plan.FieldName OutputValue)
  | NullOutputValue -- TODO: Consider implications.
  deriving (Show, Eq, Generic)
instance Hashable OutputValue

data ExecuteReader = ExecuteReader
  { recordSets :: IORef (InsOrdHashMap Plan.Ref RecordSet)
  , credentials :: !Credentials
  }

data ExecuteProblem
  = GetJobDecodeProblem String
  | CreateQueryJobDecodeProblem String
  | ErrorResponseFromServer Status L.ByteString
  | GetJobResultsProblem SomeException
  | RESTRequestNonOK Status
  | CreateQueryJobProblem SomeException
  | JoinProblem ExecuteProblem
  | UnacceptableJoinProvenanceBUG JoinProvenance
  | MissingRecordSetBUG Plan.Ref
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
  = INT64
  | STRING
  | BOOL
  | ARRAY BigQueryType
  deriving (Show, Eq)

data BigQuery = BigQuery
  { query :: !LT.Text
  , parameters :: !(InsOrdHashMap ParameterName Parameter)
  } deriving (Show)

data Parameter = Parameter
  { typ :: !BigQueryType
  , value :: !Value
  } deriving (Show)

newtype ParameterName =
  ParameterName LT.Text deriving (Show, Aeson.ToJSON, Ord, Eq, Hashable)

data BigQueryField = BigQueryField
  { name :: !Plan.FieldName
  , typ :: !BigQueryFieldType
  , mode :: !Mode
  } deriving (Show)

data BigQueryFieldType
  = FieldINTEGER
  | FieldSTRING
  | FieldBOOL
  | FieldRECORD (Vector BigQueryField)
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
  => Credentials
  -> Plan.HeadAndTail
  -> Execute a
  -> m (Either ExecuteProblem RecordSet)
runExecute credentials headAndTail m = do
  recordSets <- liftIO (newIORef mempty)
  liftIO
    (runExceptT (runReaderT
        (unExecute (m >> getFinalRecordSet headAndTail))
        (ExecuteReader {credentials, recordSets})))

execute :: Forest Plan.PlannedAction -> Execute ()
execute = traverse_ (traverse_ executePlannedAction)

executePlannedAction :: Plan.PlannedAction -> Execute ()
executePlannedAction =
  \case
    Plan.PlannedAction {ref, action} -> do
      recordSet <-
        case action of
          Plan.SelectAction select -> do
            credentials <- asks credentials
            relationshipIn <-
              maybe
                (pure [])
                makeRelationshipIn
                (Plan.selectRelationship select)
            recordSet <-
              streamBigQuery
                credentials
                (selectToBigQuery
                   select
                     { Plan.selectWhere =
                         Plan.selectWhere select <> Where relationshipIn
                     }) >>= liftEither
            maybe
              pure
              unwrapAggs
              (Plan.selectAggUnwrap select)
              recordSet {wantedFields = Select.wantedFields select}
          Plan.JoinAction Plan.Join {..} -> do
            left <- getRecordSet leftRecordSet
            right <- getRecordSet rightRecordSet
            case joinProvenance of
              ArrayJoinProvenance ->
                case leftArrayJoin wantedFields joinFieldName joinOn left right of
                  Left problem -> throwError (JoinProblem problem)
                  Right recordSet -> pure recordSet
              ObjectJoinProvenance ->
                case leftObjectJoin wantedFields joinFieldName joinOn left right of
                  Left problem -> throwError (JoinProblem problem)
                  Right recordSet -> pure recordSet
              ArrayAggregateJoinProvenance ->
                case leftObjectJoin wantedFields joinFieldName joinOn left right of
                  Left problem -> throwError (JoinProblem problem)
                  Right recordSet -> pure recordSet
              p -> throwError (UnacceptableJoinProvenanceBUG p)
      saveRecordSet ref recordSet

unwrapAggs :: Text -> RecordSet -> Execute RecordSet
unwrapAggs aggField recordSet =
  pure
    (recordSet
       { rows =
           V.concatMap
             (\row ->
                let field = (Plan.FieldName aggField)
                 in case OMap.lookup field row of
                      Just (ArrayOutputValue subrows) -> do
                        let row' = OMap.delete field row
                        -- TODO: Be careful of using vector monad.
                        RecordOutputValue subrow <- subrows
                        pure (row' <> subrow)
                      _ -> pure row)
             (rows recordSet)
       })

makeRelationshipIn :: Plan.Relationship -> Execute [Expression]
makeRelationshipIn Plan.Relationship {leftRecordSet, onFields, rightTable} = do
  RecordSet {rows} <- getRecordSet leftRecordSet
  let inExpressions =
        map
          (\(leftField, rightField) ->
             InExpression
               (ColumnExpression
                  (planFieldNameToQueryFieldName rightTable rightField))
               (ArrayValue
                  (V.mapMaybe
                     (lookupField' leftField >=> outputValueToValue)
                     rows)))
          onFields
  pure inExpressions
  where
    lookupField' k row =
      case OMap.lookup k row of
        Nothing -> Nothing
        Just x -> Just x

planFieldNameToQueryFieldName :: EntityAlias -> Plan.FieldName -> FieldName
planFieldNameToQueryFieldName (EntityAlias fieldNameEntity) (Plan.FieldName fieldName) =
  FieldName {fieldNameEntity, fieldName}

outputValueToValue  :: OutputValue -> Maybe Value
outputValueToValue =
  \case
    IntegerOutputValue i -> pure (IntValue i)
    TextOutputValue i -> pure (TextValue i)
    BoolOutputValue i -> pure (BoolValue i)
    ArrayOutputValue v -> fmap ArrayValue (mapM outputValueToValue v)
    RecordOutputValue {} -> Nothing
    NullOutputValue -> Nothing

saveRecordSet :: Plan.Ref -> RecordSet -> Execute ()
saveRecordSet ref recordSet = do
  recordSetsRef <- asks recordSets
  liftIO (modifyIORef' recordSetsRef (OMap.insert ref recordSet))

getRecordSet :: Plan.Ref -> Execute RecordSet
getRecordSet ref = do
  recordSetsRef <- asks recordSets
  hash <- liftIO (readIORef recordSetsRef)
  case OMap.lookup ref hash of
    Nothing -> throwError (MissingRecordSetBUG ref)
    Just re -> pure re

getFinalRecordSet :: Plan.HeadAndTail -> Execute RecordSet
getFinalRecordSet Plan.HeadAndTail {..} = do
  headSet <- getRecordSet head
  tailSet <-
    if tail /= head
      then getRecordSet tail
      else pure headSet
  pure
    tailSet
      { rows =
          fmap
            (\row ->
               OMap.filterWithKey
                 (\(Plan.FieldName k) _ ->
                    maybe True (elem k) (wantedFields headSet))
                 row)
            (rows tailSet)
      }

--------------------------------------------------------------------------------
-- Array joins

leftArrayJoin ::
     Maybe [Text]
  -> Text
  -> [(Plan.FieldName, Plan.FieldName)]
  -> RecordSet
  -> RecordSet
  -> Either ExecuteProblem RecordSet
leftArrayJoin = leftArrayJoinViaIndex

-- | A naive, exponential reference implementation of a left join. It
-- serves as a trivial sample implementation for correctness checking
-- of more efficient ones.
_leftArrayJoinReferenceImpl ::
     Maybe [Text]
  -> Text
  -> [(Plan.FieldName, Plan.FieldName)]
  -> RecordSet
  -> RecordSet
  -> Either ExecuteProblem RecordSet
_leftArrayJoinReferenceImpl wantedFields joinAlias joinFields left right =
  pure
    RecordSet
      { origin = Nothing
      , wantedFields = Nothing
      , rows =
          V.fromList
            [ joinArrayRows wantedFields joinAlias leftRow rightRows
            | leftRow <- toList (rows left)
            , let rightRows =
                    V.fromList
                      [ rightRow
                      | rightRow <- toList (rows right)
                      , not (null joinFields)
                      , all
                          (\(leftField, rightField) ->
                             fromMaybe
                               False
                               (do leftValue <-
                                     lookupField "left" leftField leftRow
                                   rightValue <-
                                     lookupField "right" rightField rightRow
                                   pure (leftValue == rightValue)))
                          joinFields
                      ]
            ]
      }

-- | A more efficient left join implementation by indexing the
-- right-hand-side record set first.
leftArrayJoinViaIndex ::
     Maybe [Text]
  -> Text
  -> [(Plan.FieldName, Plan.FieldName)]
  -> RecordSet
  -> RecordSet
  -> Either ExecuteProblem RecordSet
leftArrayJoinViaIndex wantedFields joinAlias joinFields left right =
  pure
    RecordSet
      { origin = Nothing
      , wantedFields = Nothing
      , rows =
          V.mapMaybe
            (\leftRow ->
               let !key = makeKey joinFields "left" leftRow
                   !mrightRows = HM.lookup key rightIndex
                in let !row =
                         joinArrayRows
                           wantedFields
                           joinAlias
                           leftRow
                           (maybe mempty (V.fromList . toList) mrightRows)
                    in pure row)
            (rows left)
      }
  where
    !rightIndex = makeIndex joinFields (rows right)

-- | Do a single pass over the right-hand-side set of rows of a left
-- join. For each set of key/value pairs used in the join, produce a
-- sequence of rows corresponding to it.
--
-- Also, the field names in the @HashMap Plan.FieldName OutputValue@
-- are the left-hand side. Meaning to do a lookup we just produce a
-- value with the left-hand-side's fields, then we have an O(log n)
-- index lookup.
--
-- We build up the sequence because concatenation is
-- O(log(min(n1,n2))) for a sequence.
makeIndex ::
     [(Plan.FieldName, Plan.FieldName)]
  -> Vector (InsOrdHashMap Plan.FieldName OutputValue)
  -> HashMap (HashMap Plan.FieldName OutputValue) (Seq (InsOrdHashMap Plan.FieldName OutputValue))
makeIndex joinFields =
  V.foldl'
    (\hash row ->
       let !key = makeKey joinFields "right" row
        in HM.insertWith (flip (<>)) key (pure row) hash)
    mempty

-- So for every value in the right, here is the
-- left side's key and the right side's value.
makeKey :: (Eq k, Hashable k) => [(k, Plan.FieldName)] -> String -> InsOrdHashMap Plan.FieldName OutputValue -> HashMap k OutputValue
makeKey joinFields direction row =
  HM.fromList
    (mapMaybe
       (\(left, right) -> do
          value <- lookupField direction right row
          pure (left, value))
       joinFields)

-- | Join a row with another as an array join.
joinArrayRows ::
     Maybe [Text] -> Text
  -> InsOrdHashMap Plan.FieldName OutputValue
  -> Vector (InsOrdHashMap Plan.FieldName OutputValue)
  -> InsOrdHashMap Plan.FieldName OutputValue
joinArrayRows wantedFields fieldName leftRow rightRow =
  OMap.insert
    (Plan.FieldName fieldName)
    (ArrayOutputValue
       (fmap
          (RecordOutputValue .
           OMap.filterWithKey
             (\(Plan.FieldName k) _ -> maybe True (elem k) wantedFields))
          rightRow))
    leftRow

--------------------------------------------------------------------------------
-- Object joins

leftObjectJoin ::
     Maybe [Text] -> Text
  -> [(Plan.FieldName, Plan.FieldName)]
  -> RecordSet
  -> RecordSet
  -> Either ExecuteProblem RecordSet
leftObjectJoin wantedFields joinAlias joinFields left right =
  pure
    RecordSet
      { origin = Nothing
      , wantedFields = Nothing
      , rows =
          V.fromList
            [ joinObjectRows wantedFields joinAlias leftRow rightRows
            | leftRow <- toList (rows left)
            , let rightRows =
                    V.fromList
                      [ rightRow
                      | rightRow <- toList (rows right)
                      , not (null joinFields)
                      , all
                          (\(leftField, rightField) ->
                             fromMaybe
                               False
                               (do leftValue <-
                                     lookupField "left" leftField leftRow
                                   rightValue <-
                                     lookupField "right" rightField rightRow
                                   pure (leftValue == rightValue)))
                          joinFields
                      ]
            ]
      }

-- | Handy way to insert logging while debugging.
lookupField ::
     String
  -> Plan.FieldName
  -> InsOrdHashMap Plan.FieldName OutputValue
  -> Maybe OutputValue
lookupField _direction name hash = OMap.lookup name hash

-- | Join a row with another as an object join.
--
-- We expect rightRow to consist of a single row, but don't complain
-- if this is violated. TODO: Change?
joinObjectRows ::
     Maybe [Text] -> Text
  -> InsOrdHashMap Plan.FieldName OutputValue
  -> Vector (InsOrdHashMap Plan.FieldName OutputValue)
  -> InsOrdHashMap Plan.FieldName OutputValue
joinObjectRows wantedFields fieldName leftRow rightRows =
  foldl'
    (\left row ->
       OMap.insert
         (Plan.FieldName fieldName)
         (RecordOutputValue
            (OMap.filterWithKey
               (\(Plan.FieldName k) _ -> maybe True (elem k) wantedFields)
               row))
         left)
    leftRow
    rightRows

--------------------------------------------------------------------------------
-- Make a big query from a select

selectToBigQuery :: Plan.Select -> BigQuery
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
    }
  where
    (query, params) =
      ToQuery.renderBuilderPretty (ToQuery.fromSelect (Plan.selectQuery select))

--------------------------------------------------------------------------------
-- Type system

-- | Make a BigQuery type for the given value.
valueType :: Value -> BigQueryType
valueType =
  \case
    IntValue {} -> INT64
    TextValue {} -> STRING
    BoolValue {} -> BOOL
    ArrayValue values ->
      ARRAY
        (case values V.!? 0 of
           Just v -> valueType v
           -- Above: We base the type from the first element. Later,
           -- we could add some kind of sanity check that they are all
           -- the same type.
           Nothing -> INT64
           -- Above: If the array is null, it doesn't matter what type
           -- the element is. So we put STRING.
         )

--------------------------------------------------------------------------------
-- JSON serialization

-- | Make a JSON representation of the type of the given value.
valueToBigQueryJson :: Value -> Aeson.Value
valueToBigQueryJson = go
  where
    go =
      \case
        IntValue i -> Aeson.object ["value" .= show i]
        TextValue i -> Aeson.object ["value" .= Aeson.String i]
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
     MonadIO m => Credentials -> BigQuery -> m (Either ExecuteProblem RecordSet)
streamBigQuery credentials bigquery = do
  jobResult <- createQueryJob credentials bigquery
  case jobResult of
    Right job -> loop Nothing Nothing
      where loop pageToken mrecordSet = do
              results <- getJobResults credentials job Fetch {pageToken}
              case results of
                Left problem -> pure (Left problem)
                Right (JobComplete JobResults { pageToken = mpageToken'
                                              , recordSet = recordSet'@RecordSet {rows = rows'}
                                              }) ->
                  case mpageToken' of
                    Nothing -> pure (Right recordSet')
                    Just pageToken' ->
                      loop
                        (pure pageToken')
                        (case mrecordSet of
                           Nothing -> Just recordSet'
                           Just RecordSet {..} ->
                             Just (RecordSet {rows = rows <> rows', ..}))
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
         recordSet <- parseRecordSetPayload Nothing o
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
  => Credentials
  -> Job
  -> Fetch
  -> m (Either ExecuteProblem JobResultsResponse)
getJobResults Credentials {..} Job {jobId} Fetch {pageToken} =
  liftIO (catchAny run (pure . Left . GetJobResultsProblem))
  where
    url =
      ("https://bigquery.googleapis.com/bigquery/v2/projects/" <>
       T.unpack projectName <>
       "/queries/" <>
       T.unpack jobId <>
       "?alt=json&key=" <>
       T.unpack apiToken <>
       "&" <>
       T.unpack (encodeParams extraParameters))
    run = do
      let
      req <- parseRequest url
      let request =
            req
              { requestHeaders =
                  [ ("Authorization", "Bearer " <> T.encodeUtf8 accessToken)
                  , ("Content-Type", "application/json")
                  , ("User-Agent", "curl/7.54")
                  ]
              , checkResponse = \_ _resp -> pure ()
              , method = "GET"
              }
      mgr <- newManager tlsManagerSettings
      resp <- httpLbs request mgr
      case statusCode (responseStatus resp) of
        200 -> case Aeson.eitherDecode (responseBody resp) of
                 Left e -> pure (Left (GetJobDecodeProblem e))
                 Right results -> pure (Right results)
        _ -> pure (Left (RESTRequestNonOK (responseStatus resp)))
    extraParameters = pageTokenParam
      where
        pageTokenParam =
          case pageToken of
            Nothing -> []
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
createQueryJob :: MonadIO m => Credentials -> BigQuery -> m (Either ExecuteProblem Job)
createQueryJob Credentials {..} BigQuery {..} =
  liftIO (catchAny run (pure . Left . CreateQueryJobProblem))
  where
    run = do
      req <-
        parseRequest
          ("https://content-bigquery.googleapis.com/bigquery/v2/projects/" <>
           T.unpack projectName <>
           "/jobs?alt=json&key=" <>
           T.unpack apiToken)
      let request =
            req
              { requestHeaders =
                  [ ("Authorization", "Bearer " <> T.encodeUtf8 accessToken)
                  , ("Content-Type", "application/json")
                  , ("User-Agent", "curl/7.54")
                  ]
              , checkResponse = \_ _resp -> pure ()
              , method = "POST"
              , requestBody = RequestBodyLBS body
              }
      mgr <- newManager tlsManagerSettings
      resp <- httpLbs request mgr
      case statusCode (responseStatus resp) of
        200 ->
          case Aeson.eitherDecode (responseBody resp) of
            Left e -> pure (Left (CreateQueryJobDecodeProblem e))
            Right job -> pure (Right job)
        _ -> pure (Left (RESTRequestNonOK (responseStatus resp)))
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

parseRecordSetPayload :: Maybe Plan.PlannedAction -> Aeson.Object -> Aeson.Parser RecordSet
parseRecordSetPayload origin resp = do
  schema <- resp .: "schema"
  columns <- schema .: "fields" :: Aeson.Parser (Vector BigQueryField)
  rowsJSON <- fmap (fromMaybe mempty) (resp .:? "rows" :: Aeson.Parser (Maybe (Vector Aeson.Value)))
  rows <-
    V.imapM
      (\i row -> parseRow columns row Aeson.<?> Aeson.Index i)
      rowsJSON Aeson.<?> Aeson.Key "rows"
  pure RecordSet {origin, wantedFields = Nothing, rows}

--------------------------------------------------------------------------------
-- Schema-driven JSON deserialization

parseRow :: Vector BigQueryField -> Aeson.Value -> Aeson.Parser (InsOrdHashMap Plan.FieldName OutputValue)
parseRow columnTypes value = do
  result <- parseBigQueryRow columnTypes value
  case result of
    RecordOutputValue row -> pure row
    _ -> fail ("Expected a record when parsing a top-level row: " ++ show value)

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
    FieldRECORD types ->
      has_v isNullable (parseBigQueryRow types) object Aeson.<?> Aeson.Key "RECORD"
    FieldINTEGER ->
      has_v isNullable (fmap IntegerOutputValue . parseBigQueryIntegral) object Aeson.<?>
      Aeson.Key "INTEGER"
    FieldBOOL ->
      has_v isNullable (fmap BoolOutputValue . Aeson.parseJSON) object Aeson.<?>
      Aeson.Key "BOOL"
    FieldSTRING ->
      has_v isNullable (fmap TextOutputValue . Aeson.parseJSON) object Aeson.<?>
      Aeson.Key "STRING"

-- | Numbers in BigQuery are given as strings.
--
-- TODO: Look at large numbers. @IntegerOutputValue@ is currently an Int[64].
parseBigQueryIntegral :: (Integral v) => Aeson.Value -> Aeson.Parser v
parseBigQueryIntegral j = do
  text <- Aeson.parseJSON j
  case T.signed T.decimal text of
    Left err -> fail err
    Right (v,"") -> pure v
    _ -> fail ("extraneous input for number " <> show text)

parseBigQueryField :: BigQueryField -> Aeson.Value -> Aeson.Parser (Plan.FieldName, OutputValue)
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
      ARRAY t -> Aeson.object ["type" .= ("ARRAY" :: Text), "arrayType" .= t]
      INT64 -> atomic "INT64"
      STRING -> atomic "STRING"
      BOOL -> atomic "BOOL"
    where
      atomic ty = Aeson.object ["type" .= (ty :: Text)]

instance Aeson.FromJSON BigQueryField where
  parseJSON =
    Aeson.withObject
      "BigQueryField"
      (\o -> do
         name <- o .: "name"
         typ <-
           do flag <- o .: "type"
              case flag :: Text of
                "INTEGER" -> pure FieldINTEGER
                "STRING" -> pure FieldSTRING
                "BOOL" -> pure FieldBOOL
                "RECORD" -> do
                  fields <- o .: "fields"
                  pure (FieldRECORD fields)
                _ -> fail ("Unsupported field type: " ++ show flag) -- TODO: completeness.
         mode <- o .: "mode"
         pure BigQueryField {..})

instance Aeson.FromJSON Mode where
  parseJSON j = do
    s <- Aeson.parseJSON j
    case s :: Text of
      "NULLABLE" -> pure Nullable
      "REPEATED" -> pure Repeated
      _ -> pure NotNullable
