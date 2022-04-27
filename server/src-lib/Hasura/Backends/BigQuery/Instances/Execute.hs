{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.BigQuery.Instances.Execute () where

import Data.Aeson qualified as Aeson
import Data.Aeson.Text qualified as Aeson
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder qualified as LT
import Data.Vector qualified as V
import Hasura.Backends.BigQuery.Execute (executeProblemMessage)
import Hasura.Backends.BigQuery.Execute qualified as DataLoader
import Hasura.Backends.BigQuery.FromIr qualified as BigQuery
import Hasura.Backends.BigQuery.Plan
import Hasura.Backends.BigQuery.ToQuery qualified as ToQuery
import Hasura.Backends.BigQuery.Types qualified as BigQuery
import Hasura.Base.Error
import Hasura.Base.Error qualified as E
import Hasura.EncJSON
import Hasura.GraphQL.Execute.Backend
import Hasura.GraphQL.Namespace (RootFieldAlias)
import Hasura.GraphQL.Parser
import Hasura.Prelude
import Hasura.QueryTags
  ( emptyQueryTagsComment,
  )
import Hasura.RQL.IR
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Backend
import Hasura.Session
import Hasura.Tracing qualified as Tracing

instance BackendExecute 'BigQuery where
  type PreparedQuery 'BigQuery = Text
  type MultiplexedQuery 'BigQuery = Void
  type ExecutionMonad 'BigQuery = Tracing.TraceT (ExceptT QErr IO)

  mkDBQueryPlan = bqDBQueryPlan
  mkDBMutationPlan = bqDBMutationPlan
  mkLiveQuerySubscriptionPlan _ _ _ _ _ =
    throw500 "Cannot currently perform subscriptions on BigQuery sources."
  mkDBStreamingSubscriptionPlan _ _ _ _ =
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
  m (DBStepInfo 'BigQuery)
bqDBQueryPlan userInfo sourceName sourceConfig qrf = do
  -- TODO (naveen): Append query tags to the query
  select <- planNoPlan (BigQuery.bigQuerySourceConfigToFromIrConfig sourceConfig) userInfo qrf
  let action = do
        result <-
          DataLoader.runExecute
            sourceConfig
            (DataLoader.executeSelect select)
        case result of
          Left err -> throw500WithDetail (executeProblemMessage err) $ Aeson.toJSON err
          Right recordSet -> pure $! recordSetToEncJSON (BigQuery.selectCardinality select) recordSet
  pure $ DBStepInfo @'BigQuery sourceName sourceConfig (Just (selectSQLTextForExplain select)) action

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
      encJFromInsOrdHashMap . fmap encJFromOutputValue . OMap.mapKeys coerce
    encJFromOutputValue outputValue =
      case outputValue of
        DataLoader.NullOutputValue -> encJFromJValue Aeson.Null
        DataLoader.DecimalOutputValue !i -> encJFromJValue i
        DataLoader.BigDecimalOutputValue !i -> encJFromJValue i
        DataLoader.FloatOutputValue !i -> encJFromJValue i
        DataLoader.TextOutputValue !i -> encJFromJValue i
        DataLoader.BytesOutputValue !i -> encJFromJValue i
        DataLoader.DateOutputValue !i -> encJFromJValue i
        DataLoader.TimestampOutputValue !i -> encJFromJValue i
        DataLoader.TimeOutputValue !i -> encJFromJValue i
        DataLoader.DatetimeOutputValue !i -> encJFromJValue i
        DataLoader.GeographyOutputValue !i -> encJFromJValue i
        DataLoader.BoolOutputValue !i -> encJFromJValue i
        DataLoader.IntegerOutputValue !i -> encJFromJValue i
        DataLoader.ArrayOutputValue !vector ->
          encJFromList (toList (fmap encJFromOutputValue vector))
        -- Really, the case below shouldn't be happening. But if it
        -- does, it's not a problem either. The output will just have
        -- a record in it.
        DataLoader.RecordOutputValue !record -> encJFromRecord record

-- mutation

bqDBMutationPlan ::
  forall m.
  ( MonadError E.QErr m
  ) =>
  UserInfo ->
  StringifyNumbers ->
  SourceName ->
  SourceConfig 'BigQuery ->
  MutationDB 'BigQuery Void (UnpreparedValue 'BigQuery) ->
  m (DBStepInfo 'BigQuery)
bqDBMutationPlan _userInfo _stringifyNum _sourceName _sourceConfig _mrf =
  throw500 "mutations are not supported in BigQuery; this should be unreachable"

-- explain

bqDBQueryExplain ::
  MonadError E.QErr m =>
  RootFieldAlias ->
  UserInfo ->
  SourceName ->
  SourceConfig 'BigQuery ->
  QueryDB 'BigQuery Void (UnpreparedValue 'BigQuery) ->
  m (AB.AnyBackend DBStepInfo)
bqDBQueryExplain fieldName userInfo sourceName sourceConfig qrf = do
  select <- planNoPlan (BigQuery.bigQuerySourceConfigToFromIrConfig sourceConfig) userInfo qrf
  let textSQL = selectSQLTextForExplain select
  pure $
    AB.mkAnyBackend $
      DBStepInfo @'BigQuery sourceName sourceConfig Nothing $
        pure $
          encJFromJValue $
            ExplainPlan
              fieldName
              (Just $ textSQL)
              (Just $ T.lines $ textSQL)

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
  NonEmpty Aeson.Object ->
  -- | The above objects have this schema
  --
  -- XXX: What is this for/what does this mean?
  HashMap FieldName (Column 'BigQuery, ScalarType 'BigQuery) ->
  -- | This is a field name from the lhs that *has* to be selected in the
  -- response along with the relationship.
  FieldName ->
  (FieldName, SourceRelationshipSelection 'BigQuery Void UnpreparedValue) ->
  m (DBStepInfo 'BigQuery)
bqDBRemoteRelationshipPlan userInfo sourceName sourceConfig lhs lhsSchema argumentId relationship = do
  flip runReaderT emptyQueryTagsComment $ bqDBQueryPlan userInfo sourceName sourceConfig rootSelection
  where
    coerceToColumn = BigQuery.ColumnName . getFieldNameTxt
    joinColumnMapping = mapKeys coerceToColumn lhsSchema

    rowsArgument :: UnpreparedValue 'BigQuery
    rowsArgument =
      UVParameter Nothing $
        ColumnValue (ColumnScalar BigQuery.StringScalarType) $
          BigQuery.StringValue . LT.toStrict $ Aeson.encodeToLazyText lhs

    recordSetDefinitionList =
      (coerceToColumn argumentId, BigQuery.IntegerScalarType) : Map.toList (fmap snd joinColumnMapping)

    jsonToRecordSet :: IR.SelectFromG ('BigQuery) (UnpreparedValue 'BigQuery)
    jsonToRecordSet =
      IR.FromFunction
        (BigQuery.FunctionName "unnest")
        ( IR.FunctionArgsExp
            [IR.AEInput rowsArgument]
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
