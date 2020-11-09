{-# LANGUAGE DuplicateRecordFields #-}

-- | Execute the plan given from .Plan.

module Hasura.SQL.BigQuery.Execute
  ( execute
  , runExecute
  , getCredentialsEnv
  , parseSampleResponse
  , Credentials(..)
  , RecordSet(..)
  , Execute
  , Value(..)
  ) where

import           Control.Applicative
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
import           Data.Maybe
import           Data.Sequence (Seq)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.Text.Read as T
import           Data.Tree
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Debug.Trace
import           GHC.Generics
import qualified Hasura.SQL.BigQuery.Plan as Plan
import qualified Hasura.SQL.BigQuery.Plan as Select (Select (..))
import qualified Hasura.SQL.BigQuery.ToQuery as ToQuery
import           Hasura.SQL.BigQuery.Types as BigQuery
import           Network.HTTP.Conduit
import           Prelude hiding (head,tail)
import           System.Environment

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

-- | Execute monad; as queries are performed, the record sets are
-- stored in the map.
newtype Execute a = Execute
  { unExecute :: ReaderT ExecuteReader IO a
  } deriving (Functor, Applicative, Monad, MonadReader ExecuteReader, MonadIO)

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

data Credentials = Credentials
  { accessToken :: !Text
  , apiToken :: !Text
  , projectName :: !Text
  }

instance Show Credentials where
  show Credentials {projectName} =
    "Credentials { accessToken = _, apiToken = _, projectName = " <>
    show projectName <>
    " }"

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

data JoinProblem =
  JoinProblem
  deriving (Show)

--------------------------------------------------------------------------------
-- Handy testing

getCredentialsEnv :: IO Credentials
getCredentialsEnv = do
  accessToken <- getEnvUnline "BIGQUERYACCESSTOKEN"
  apiToken <- getEnvUnline "BIGQUERYAPITOKEN"
  projectName <- getEnvUnline "BIGQUERYPROJECTNAME"
  pure Credentials {..}
  where
    getEnvUnline key = do
      value <- fmap (concat . take 1 . lines) (getEnv key)
      -- putStrLn $ "Got credential from environment: " <> show key
      pure (T.pack value)

parseSampleResponse :: FilePath -> IO RecordSet
parseSampleResponse fp = do
  bytes <- L.readFile fp
  case Aeson.eitherDecode bytes >>= Aeson.parseEither (parseRecordSet Nothing) of
    Left e -> error ("Bad parse: " ++ e)
    Right v -> pure v

--------------------------------------------------------------------------------
-- Executing the planned actions forest

runExecute :: MonadIO m => Credentials -> Plan.HeadAndTail -> Execute a -> m RecordSet
runExecute credentials headAndTail m = do
  recordSets <- liftIO (newIORef mempty)
  liftIO
    (runReaderT
       (unExecute (m >> getFinalRecordSet headAndTail))
       (ExecuteReader {credentials, recordSets}))

execute :: Forest Plan.PlannedAction -> Execute ()
execute = traverse_ (traverse_ executePlannedAction)

executePlannedAction :: Plan.PlannedAction -> Execute ()
executePlannedAction =
  \case
    planned@Plan.PlannedAction {ref, action} -> do
      liftIO (putStrLn ("\n" ++ replicate 80 '-' ++ "\n" ++ show ref))
      liftIO (putStrLn ("\nexecuting = " <> show planned))
      recordSet <-
        case action of
          Plan.SelectAction select -> do
            credentials <- asks credentials
            relationshipIn <-
              maybe
                (pure [])
                makeRelationshipIn
                (Plan.selectRelationship select)
            liftIO
              (LT.putStrLn ("\nquery = " <>
               LT.toLazyText (fst (ToQuery.renderBuilderPretty
                   (ToQuery.fromSelect (Plan.selectQuery select))))))
            recordSet <-
              runBigQuery
                credentials
                (selectToBigQuery
                   select
                     { Plan.selectWhere =
                         Plan.selectWhere select <> Where relationshipIn
                     })
            maybe
              pure
              unwrapAggs
              (Plan.selectAggUnwrap select)
              recordSet {wantedFields = Select.wantedFields select}
          Plan.JoinAction Plan.Join {..} -> do
            liftIO (putStrLn ("\nleft ref = " <> show leftRecordSet))
            liftIO (putStrLn ("\nright ref = " <> show rightRecordSet))
            left <- getRecordSet leftRecordSet
            right <- getRecordSet rightRecordSet
            liftIO (putStrLn ("\nleft record set = " <> show left))
            liftIO (putStrLn ("\nright record set = " <> show right))
            case joinProvenance of
              ArrayJoinProvenance ->
                case leftArrayJoin wantedFields joinFieldName joinOn left right of
                  Left problem -> error ("Join problem: " ++ show problem)
                  Right recordSet -> pure recordSet
              ObjectJoinProvenance ->
                case leftObjectJoin wantedFields joinFieldName joinOn left right of
                  Left problem -> error ("Join problem: " ++ show problem)
                  Right recordSet -> pure recordSet
              ArrayAggregateJoinProvenance ->
                case leftObjectJoin wantedFields joinFieldName joinOn left right of
                  Left problem -> error ("Join problem: " ++ show problem)
                  Right recordSet -> pure recordSet
              p -> error ("Unacceptable join provenance: " ++ show p)
      liftIO (putStrLn ("\nresult record set = " <> show recordSet))
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
        {-columns =
           V.concatMap
             (\original@BigQueryField {..} ->
                if name == Plan.FieldName aggField
                  then case typ of
                         FieldRECORD fs -> fs
                         _ -> pure original
                  else pure original)
             (columns recordSet)-}
       })

makeRelationshipIn :: Plan.Relationship -> Execute [Expression]
makeRelationshipIn Plan.Relationship {leftRecordSet, onFields, rightTable} = do
  liftIO (putStrLn ("\nrelationship left = " <> show leftRecordSet))
  liftIO (putStrLn ("\nrelationship fields = " <> show onFields))
  recordset@RecordSet {rows} <- getRecordSet leftRecordSet
  liftIO (putStrLn ("\nrelationship record set = " <> show recordset))
  let inExpressions = map
                        (\(leftField, rightField) ->
                           InExpression
                             (ColumnExpression
                                (planFieldNameToQueryFieldName rightTable rightField))
                             (ArrayValue
                                (V.mapMaybe (lookupField' leftField >=> outputValueToValue) rows)))
                        onFields
  liftIO (putStrLn ("\nrelationship in_expressions = " <> show inExpressions))
  pure
    inExpressions
  where
    lookupField' k row =
      case OMap.lookup k row of
        Nothing -> trace ("WARNING: Couldn't find key " <> show k) Nothing
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
    RecordOutputValue r ->
      trace ("WARNING: Can't use record in BigQuery: " <> show r) Nothing
    NullOutputValue ->
      trace ("WARNING: Shouldn't be using NULL in BigQuery input (?)") Nothing

saveRecordSet :: Plan.Ref -> RecordSet -> Execute ()
saveRecordSet ref recordSet = do
  recordSetsRef <- asks recordSets
  liftIO (modifyIORef' recordSetsRef (OMap.insert ref recordSet))

getRecordSet :: Plan.Ref -> Execute RecordSet
getRecordSet ref = do
  recordSetsRef <- asks recordSets
  hash <- liftIO (readIORef recordSetsRef)
  case OMap.lookup ref hash of
    Nothing -> error ("Missing ref! " ++ show ref) -- TODO: Change to real type.
    Just re -> pure re

getFinalRecordSet :: Plan.HeadAndTail -> Execute RecordSet
getFinalRecordSet Plan.HeadAndTail {..} = do
  headSet <- getRecordSet head
  tailSet <-
    if tail /= head
      then getRecordSet tail
      else pure headSet
  trace
    (unlines
       [ "\n" ++ replicate 80 '-'
       , "\nfinal record head = " <> show head
       , "\nfinal record tail = " <> show tail
       , "\nwanted fields = " <> show (wantedFields headSet)
       ])
    (pure
       tailSet
         { rows =
             fmap
               (\row ->
                  OMap.filterWithKey
                    (\(Plan.FieldName k) _ ->
                       maybe True (elem k) (wantedFields headSet))
                    row)
               (rows tailSet)
         })

--------------------------------------------------------------------------------
-- Array joins

leftArrayJoin ::
     Maybe [Text]
  -> Text
  -> [(Plan.FieldName, Plan.FieldName)]
  -> RecordSet
  -> RecordSet
  -> Either JoinProblem RecordSet
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
  -> Either JoinProblem RecordSet
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
  -> Either JoinProblem RecordSet
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
  -> Either JoinProblem RecordSet
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

lookupField ::
     String -> Plan.FieldName -> InsOrdHashMap Plan.FieldName OutputValue -> Maybe OutputValue
lookupField direction name hash =
  case OMap.lookup name hash of
    Nothing ->
      trace
        ("WARNING: Missing fieldname on " ++ direction ++ ": " ++ show name)
        Nothing
    Just v -> pure v

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
-- HTTP request

runBigQuery :: MonadIO m => Credentials -> BigQuery -> m RecordSet
runBigQuery Credentials {..} BigQuery {..} =
  liftIO $ do
    req <-
      parseRequest
        ("https://content-bigquery.googleapis.com/bigquery/v2/projects/" <>
         T.unpack projectName <>
         "/queries?alt=json&key=" <>
         T.unpack apiToken)
    let body =
          Aeson.encode
            (Aeson.object
               [ "query" .= query
               , "useLegacySql" .= False -- Important, it makes `quotes` work properly.
               , "parameterMode" .= ("NAMED" :: Text)
               , "queryParameters" .=
                 map
                   (\(name, Parameter {..}) ->
                      Aeson.object
                        [ "name" .= Aeson.toJSON name
                        , "parameterType" .= Aeson.toJSON typ
                        , "parameterValue" .= valueToBigQueryJson value
                        ])
                   (OMap.toList parameters)
               ])
    -- L8.putStrLn ("\nRequest body:\n" <> body)
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
    putStrLn ("\nparameters = " <> show parameters)
    do mgr <- newManager tlsManagerSettings
       resp <- httpLbs request mgr
       -- L.putStr (responseBody resp)
       case Aeson.eitherDecode (responseBody resp) >>=
            Aeson.parseEither (parseRecordSet Nothing) of
         Left e -> do
           error ("Bad parse: " ++ e ++ "\n" <> show (responseBody resp))
         Right v
           -- print v
          -> do
           pure v

--------------------------------------------------------------------------------
-- Consuming recordset from big query

parseRecordSet :: Maybe Plan.PlannedAction -> Aeson.Value -> Aeson.Parser RecordSet
parseRecordSet origin =
  Aeson.withObject
    "BigQuery response"
    (\resp -> do
       kind <- resp .: "kind"
       if kind == ("bigquery#queryResponse" :: Text)
         then do
           schema <- resp .: "schema"
           columns <- schema .: "fields" :: Aeson.Parser (Vector BigQueryField)
           rowsJSON <- fmap (fromMaybe mempty) (resp .:? "rows" :: Aeson.Parser (Maybe (Vector Aeson.Value)))
           rows <-
             V.imapM
               (\i row -> parseRow columns row Aeson.<?> Aeson.Index i)
               rowsJSON Aeson.<?> Aeson.Key "rows"
           pure RecordSet {origin, wantedFields = Nothing, {-columns-} rows}
         else fail ("Invalid response kind: " ++ show kind))

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
