{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.BigQuery.Instances.Execute () where

import Data.Aeson qualified as J
import Data.Aeson.Text qualified as J
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder qualified as LT
import Data.Vector qualified as V
import Hasura.Backends.BigQuery.Execute qualified as DataLoader
import Hasura.Backends.BigQuery.FromIr qualified as BigQuery
import Hasura.Backends.BigQuery.Plan
import Hasura.Backends.BigQuery.ToQuery qualified as ToQuery
import Hasura.Backends.BigQuery.Types qualified as BigQuery
import Hasura.Base.Error
import Hasura.Base.Error qualified as E
import Hasura.EncJSON
import Hasura.Function.Cache
import Hasura.GraphQL.Execute.Backend
import Hasura.GraphQL.Namespace (RootFieldAlias)
import Hasura.GraphQL.Parser.Variable qualified as G
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.QueryTags
  ( emptyQueryTagsComment,
  )
import Hasura.RQL.IR
import Hasura.RQL.IR.ModelInformation
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.IR.Value qualified as IR
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Session
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Client as HTTP
import Network.HTTP.Types qualified as HTTP

instance BackendExecute 'BigQuery where
  type PreparedQuery 'BigQuery = Text
  type MultiplexedQuery 'BigQuery = Void
  type ExecutionMonad 'BigQuery = IdentityT

  mkDBQueryPlan = bqDBQueryPlan
  mkDBMutationPlan = bqDBMutationPlan
  mkLiveQuerySubscriptionPlan _ _ _ _ _ _ _ =
    throw500 "Cannot currently perform subscriptions on BigQuery sources."
  mkDBStreamingSubscriptionPlan _ _ _ _ _ _ =
    throw500 "Cannot currently perform subscriptions on BigQuery sources."
  mkDBQueryExplain = bqDBQueryExplain
  mkSubscriptionExplain _ =
    throw500 "Cannot currently retrieve query execution plans on BigQuery sources."

  -- NOTE: Currently unimplemented!.
  --
  -- This function is just a stub for future implementation; for now it just
  -- throws a 500 error.
  mkDBRemoteRelationshipPlan =
    bqDBRemoteRelationshipPlan

-- query

bqDBQueryPlan ::
  forall m.
  ( MonadError E.QErr m
  ) =>
  UserInfo ->
  SourceName ->
  SourceConfig 'BigQuery ->
  QueryDB 'BigQuery Void (UnpreparedValue 'BigQuery) ->
  [HTTP.Header] ->
  Maybe G.Name ->
  m (DBStepInfo 'BigQuery, [ModelInfoPart])
bqDBQueryPlan userInfo sourceName sourceConfig qrf _ _ = do
  -- TODO (naveen): Append query tags to the query
  select <- planNoPlan (BigQuery.bigQuerySourceConfigToFromIrConfig sourceConfig) userInfo qrf
  let action = OnBaseMonad do
        result <-
          DataLoader.runExecute
            sourceConfig
            (DataLoader.executeSelect select)
        case result of
          Left err -> throw500WithDetail (DataLoader.executeProblemMessage DataLoader.HideDetails err) $ J.toJSON err
          Right (job, recordSet) -> pure ActionResult {arStatistics = Just BigQuery.ExecutionStatistics {_esJob = job}, arResult = recordSetToEncJSON (BigQuery.selectCardinality select) recordSet}
  modelNames <- irToModelInfoGen sourceName ModelSourceTypeBigQuery qrf
  let modelInfo = getModelInfoPartfromModelNames modelNames (ModelOperationType G.OperationTypeQuery)

  pure $ (DBStepInfo @'BigQuery sourceName sourceConfig (Just (selectSQLTextForExplain select)) action (), modelInfo)

-- | Convert the dataloader's 'RecordSet' type to JSON.
recordSetToEncJSON :: BigQuery.Cardinality -> DataLoader.RecordSet -> EncJSON
recordSetToEncJSON cardinality DataLoader.RecordSet {rows} =
  case cardinality of
    BigQuery.One
      | Just row <- rows V.!? 0 -> encJFromRecord row
      | otherwise -> encJFromList (toList (fmap encJFromRecord rows))
    BigQuery.Many -> encJFromList (toList (fmap encJFromRecord rows))
  where
    encJFromRecord =
      encJFromInsOrdHashMap . fmap encJFromOutputValue . InsOrdHashMap.mapKeys coerce
    encJFromOutputValue outputValue =
      case outputValue of
        DataLoader.NullOutputValue -> encJFromJValue J.Null
        DataLoader.DecimalOutputValue i -> encJFromJValue i
        DataLoader.BigDecimalOutputValue i -> encJFromJValue i
        DataLoader.FloatOutputValue i -> encJFromJValue i
        DataLoader.TextOutputValue i -> encJFromJValue i
        DataLoader.BytesOutputValue i -> encJFromJValue i
        DataLoader.DateOutputValue i -> encJFromJValue i
        DataLoader.TimestampOutputValue i -> encJFromJValue i
        DataLoader.TimeOutputValue i -> encJFromJValue i
        DataLoader.DatetimeOutputValue i -> encJFromJValue i
        DataLoader.GeographyOutputValue i -> encJFromJValue i
        DataLoader.BoolOutputValue i -> encJFromJValue i
        DataLoader.IntegerOutputValue i -> encJFromJValue i
        DataLoader.JsonOutputValue i -> encJFromJValue i
        DataLoader.ArrayOutputValue vector ->
          encJFromList (toList (fmap encJFromOutputValue vector))
        -- Really, the case below shouldn't be happening. But if it
        -- does, it's not a problem either. The output will just have
        -- a record in it.
        DataLoader.RecordOutputValue record -> encJFromRecord record

-- mutation

bqDBMutationPlan ::
  forall m.
  ( MonadError E.QErr m
  ) =>
  Env.Environment ->
  HTTP.Manager ->
  L.Logger L.Hasura ->
  UserInfo ->
  Options.StringifyNumbers ->
  SourceName ->
  SourceConfig 'BigQuery ->
  MutationDB 'BigQuery Void (UnpreparedValue 'BigQuery) ->
  [HTTP.Header] ->
  Maybe G.Name ->
  Maybe (HashMap G.Name (G.Value G.Variable)) ->
  m (DBStepInfo 'BigQuery, [ModelInfoPart])
bqDBMutationPlan _env _manager _logger _userInfo _stringifyNum _sourceName _sourceConfig _mrf _headers _gName _maybeSelSetArgs =
  throw500 "mutations are not supported in BigQuery; this should be unreachable"

-- explain

bqDBQueryExplain ::
  (MonadError E.QErr m) =>
  RootFieldAlias ->
  UserInfo ->
  SourceName ->
  SourceConfig 'BigQuery ->
  QueryDB 'BigQuery Void (UnpreparedValue 'BigQuery) ->
  [HTTP.Header] ->
  Maybe G.Name ->
  m (AB.AnyBackend DBStepInfo)
bqDBQueryExplain fieldName userInfo sourceName sourceConfig qrf _ _ = do
  select <- planNoPlan (BigQuery.bigQuerySourceConfigToFromIrConfig sourceConfig) userInfo qrf
  let textSQL = selectSQLTextForExplain select
  pure
    $ AB.mkAnyBackend
    $ DBStepInfo @'BigQuery
      sourceName
      sourceConfig
      Nothing
      ( OnBaseMonad
          $ pure
          $ withNoStatistics
          $ encJFromJValue
          $ ExplainPlan
            fieldName
            (Just $ textSQL)
            (Just $ T.lines $ textSQL)
      )
      ()

-- | Get the SQL text for a select, with parameters left as $1, $2, .. holes.
selectSQLTextForExplain :: BigQuery.Select -> Text
selectSQLTextForExplain =
  LT.toStrict
    . LT.toLazyText
    . fst
    . ToQuery.renderBuilderPretty
    . ToQuery.fromSelect

--------------------------------------------------------------------------------
-- Remote Relationships (e.g. DB-to-DB Joins, remote schema joins, etc.)
--------------------------------------------------------------------------------

-- | Construct an action (i.e. 'DBStepInfo') which can marshal some remote
-- relationship information into a form that BigQuery can query against.
--
-- XXX: Currently unimplemented; the Postgres implementation uses
-- @jsonb_to_recordset@ to query the remote relationship, however this
-- functionality doesn't exist in BigQuery.
--
-- NOTE: The following typeclass constraints will be necessary when implementing
-- this function for real:
--
-- @
--   MonadQueryTags m
--   Backend 'BigQuery
-- @
bqDBRemoteRelationshipPlan ::
  forall m.
  ( MonadError QErr m
  ) =>
  UserInfo ->
  SourceName ->
  SourceConfig 'BigQuery ->
  -- | List of json objects, each of which becomes a row of the table.
  NonEmpty J.Object ->
  -- | The above objects have this schema
  --
  -- XXX: What is this for/what does this mean?
  HashMap FieldName (Column 'BigQuery, ScalarType 'BigQuery) ->
  -- | This is a field name from the lhs that *has* to be selected in the
  -- response along with the relationship.
  FieldName ->
  (FieldName, SourceRelationshipSelection 'BigQuery Void UnpreparedValue) ->
  [HTTP.Header] ->
  Maybe G.Name ->
  Options.StringifyNumbers ->
  m (DBStepInfo 'BigQuery, [ModelInfoPart])
bqDBRemoteRelationshipPlan userInfo sourceName sourceConfig lhs lhsSchema argumentId relationship reqHeaders operationName stringifyNumbers = do
  (dbStepInfo, modelInfo) <- flip runReaderT emptyQueryTagsComment $ bqDBQueryPlan userInfo sourceName sourceConfig rootSelection reqHeaders operationName
  pure (dbStepInfo, modelInfo)
  where
    coerceToColumn = BigQuery.ColumnName . getFieldNameTxt
    joinColumnMapping = mapKeys coerceToColumn lhsSchema

    rowsArgument :: UnpreparedValue 'BigQuery
    rowsArgument =
      UVParameter IR.FreshVar
        $ ColumnValue (ColumnScalar BigQuery.StringScalarType)
        $ BigQuery.StringValue
        . LT.toStrict
        $ J.encodeToLazyText lhs

    recordSetDefinitionList =
      (coerceToColumn argumentId, BigQuery.IntegerScalarType) : HashMap.toList (fmap snd joinColumnMapping)

    jsonToRecordSet :: IR.SelectFromG ('BigQuery) (UnpreparedValue 'BigQuery)
    jsonToRecordSet =
      IR.FromFunction
        (BigQuery.FunctionName "unnest" Nothing)
        ( FunctionArgsExp
            [BigQuery.AEInput rowsArgument]
            mempty
        )
        (Just recordSetDefinitionList)

    rootSelection =
      convertRemoteSourceRelationship
        (fst <$> joinColumnMapping)
        jsonToRecordSet
        (BigQuery.ColumnName $ getFieldNameTxt argumentId)
        (ColumnScalar BigQuery.IntegerScalarType)
        relationship
        stringifyNumbers
