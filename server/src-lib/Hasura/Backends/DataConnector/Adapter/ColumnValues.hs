-- | Implements 'getColVals' for the Data Connector backend.
--
-- 'getColVals' is used when evaluating a /remote relationship predicate in a
-- permission/ (see 'Hasura.RQL.IR.BoolExp.AVRemoteRelationship'). Given a
-- target table/column on a remote source and a filter on some field of that
-- table, it fetches the matching values of the target column so that the
-- caller can build an @<lhs column> IN (<values>)@ expression against the
-- origin table.
--
-- Previously this threw @getColVals: not implemented for the Data Connector
-- backend@, which meant that any remote-relationship permission filter whose
-- target (RHS) source was a Data Connector (e.g. Snowflake) failed at runtime
-- with a 500. This module runs the equivalent @SELECT <col> FROM <table> WHERE
-- <field> <op> <value>@ against the agent and returns the column values as
-- 'Text', matching the contract of the Postgres implementation
-- ('Hasura.Backends.Postgres.Execute.Types.getPGColValues').
--
-- NOTE (type approximation): the 'getColVals' contract does not carry the
-- scalar type of the /filter field/ (only the type of the column being
-- selected). We therefore reuse the selected column's scalar type as a
-- best-effort type annotation for the WHERE comparison, mirroring the Postgres
-- implementation which emits untyped text literals and relies on implicit
-- casting. See GS-642 for the longer-term fix of threading proper type
-- information (and JSON values) through this API.
module Hasura.Backends.DataConnector.Adapter.ColumnValues
  ( getDataConnectorColumnValues,
  )
where

import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HashMap
import Data.Set qualified as Set
import Data.Text.Encoding qualified as TE
import Data.Text.Extended (toTxt)
import Hasura.Authentication.Session (SessionVariables, getSessionVariableValue)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Adapter.ConfigTransform (transformSourceConfig)
import Hasura.Backends.DataConnector.Adapter.Types qualified as DC
import Hasura.Backends.DataConnector.Agent.Client (AgentClientContext (..), runAgentClientT)
import Hasura.Backends.DataConnector.Agent.Client qualified as Client
import Hasura.Base.Error (Code (NotFound, NotSupported), QErr, throw400)
import Hasura.Logging (nullLogger)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp.RemoteRelationshipPredicate
  ( RemoteRelSessionVariableORLiteralValue (..),
    RemoteRelSupportedOp (..),
  )
import Hasura.RQL.Types.Common (SourceName)
import Hasura.Tracing (ignoreTraceT)
import Witch qualified

-- | See the module documentation. Runs a single-column, filtered query against
-- a Data Connector agent and returns the column's values as 'Text'.
getDataConnectorColumnValues ::
  (MonadIO m, MonadError QErr m) =>
  SessionVariables ->
  SourceName ->
  DC.SourceConfig ->
  DC.TableName ->
  (DC.ScalarType, DC.ColumnName) ->
  (DC.ColumnName, [RemoteRelSupportedOp RemoteRelSessionVariableORLiteralValue]) ->
  m [Text]
getDataConnectorColumnValues sessionVariables sourceName sourceConfig tableName (colType, col) (colFieldName, boolExps) = do
  let apiScalarType = Witch.from colType
      selectedFieldName = API.FieldName (DC.unColumnName col)
      selectedField = API.ColumnField (Witch.from col) apiScalarType Nothing

  -- Translate the filter operators into a Data Connector API 'where' expression.
  whereExprs <- traverse (translateSupportedOp sessionVariables apiScalarType colFieldName) boolExps
  let whereClause = case whereExprs of
        [] -> Nothing
        [single] -> Just single
        exprs -> Just $ API.And (Set.fromList exprs)

  let queryRequest =
        API.QueryRequest
          { API._qrTarget = API.TTable (API.TargetTable (Witch.from tableName)),
            API._qrRelationships = mempty,
            API._qrRedactionExpressions = mempty,
            API._qrInterpolatedQueries = mempty,
            API._qrQuery =
              API.Query
                { API._qFields = Just $ HashMap.singleton selectedFieldName selectedField,
                  API._qAggregates = Nothing,
                  API._qAggregatesLimit = Nothing,
                  API._qLimit = Nothing,
                  API._qOffset = Nothing,
                  API._qWhere = whereClause,
                  API._qOrderBy = Nothing
                },
            API._qrForeach = Nothing
          }

  -- Resolve any session-variable/template-driven config, then send the query to
  -- the agent using the same client machinery as the metadata/introspection
  -- paths.
  --
  -- The agent client needs 'MonadTrace' and 'MonadBaseControl IO' (the latter
  -- via 'traceHTTPRequest') in addition to 'MonadIO'/'MonadError QErr', but the
  -- 'getColVals' class method only provides 'MonadIO' + 'MonadError QErr'. So we
  -- run the client in a concrete monad stack ending in 'IO' —
  -- @TraceT (ExceptT QErr IO)@ — which satisfies all of them, and only lift the
  -- final result back into @m@: 'ignoreTraceT' discharges 'MonadTrace',
  -- 'runExceptT' discharges 'MonadError QErr' into an 'Either', the concrete
  -- 'IO' base provides 'MonadBaseControl IO', and 'liftEitherM' re-raises any
  -- agent error in @m@. ('nullLogger' discards agent request logging.)
  transformedSourceConfig <- transformSourceConfig sourceConfig (Just sessionVariables)
  queryResponse <-
    liftEitherM
      . liftIO
      . runExceptT
      . ignoreTraceT
      . flip
        runAgentClientT
        ( AgentClientContext
            nullLogger
            (DC._scEndpoint transformedSourceConfig)
            (DC._scManager transformedSourceConfig)
            (DC._scTimeoutMicroseconds transformedSourceConfig)
            Nothing
        )
      $ Client.query sourceName (DC._scConfig transformedSourceConfig) queryRequest

  let rows = fromMaybe mempty (API._qrRows queryResponse)
  pure $ mapMaybe (extractColumnText selectedFieldName) rows

-- | Extract the selected column's value from a response row and render it as
-- 'Text'. NULLs (and missing columns) are dropped, since they cannot
-- meaningfully participate in the resulting @IN@ comparison.
extractColumnText :: API.FieldName -> HashMap API.FieldName API.FieldValue -> Maybe Text
extractColumnText fieldName row =
  HashMap.lookup fieldName row >>= (jsonValueToText . API.deserializeAsColumnFieldValue)

jsonValueToText :: J.Value -> Maybe Text
jsonValueToText = \case
  J.Null -> Nothing
  J.String t -> Just t
  other -> Just $ TE.decodeUtf8 $ BL.toStrict $ J.encode other

-- | Translate a single supported remote-relationship predicate operator into a
-- Data Connector API 'API.Expression' over @colFieldName@.
translateSupportedOp ::
  (MonadError QErr m) =>
  SessionVariables ->
  API.ScalarType ->
  DC.ColumnName ->
  RemoteRelSupportedOp RemoteRelSessionVariableORLiteralValue ->
  m API.Expression
translateSupportedOp sessionVariables scalarType colFieldName op =
  case op of
    RemoteRelEqOp v -> binOp API.Equal <$> resolve v
    RemoteRelNeqOp v -> API.Not . binOp API.Equal <$> resolve v
    RemoteRelGtOp v -> binOp API.GreaterThan <$> resolve v
    RemoteRelLtOp v -> binOp API.LessThan <$> resolve v
    RemoteRelGteOp v -> binOp API.GreaterThanOrEqual <$> resolve v
    RemoteRelLteOp v -> binOp API.LessThanOrEqual <$> resolve v
    RemoteRelInOp vs -> arrayOp <$> traverse resolve vs
    RemoteRelNinOp vs -> API.Not . arrayOp <$> traverse resolve vs
    RemoteRelLikeOp _ ->
      throw400 NotSupported "The _like operator is not supported in remote relationship permission filters targeting a Data Connector source"
    RemoteRelNlikeOp _ ->
      throw400 NotSupported "The _nlike operator is not supported in remote relationship permission filters targeting a Data Connector source"
    RemoteRelIsNullOp True -> pure $ API.ApplyUnaryComparisonOperator API.IsNull comparisonColumn
    RemoteRelIsNullOp False -> pure $ API.Not $ API.ApplyUnaryComparisonOperator API.IsNull comparisonColumn
  where
    comparisonColumn :: API.ComparisonColumn
    comparisonColumn = API.ComparisonColumn API.CurrentTable (API.mkColumnSelector (Witch.from colFieldName)) scalarType Nothing

    binOp :: API.BinaryComparisonOperator -> Text -> API.Expression
    binOp operator value =
      API.ApplyBinaryComparisonOperator operator comparisonColumn (API.ScalarValueComparison (API.ScalarValue (J.String value) scalarType))

    arrayOp :: [Text] -> API.Expression
    arrayOp values =
      API.ApplyBinaryArrayComparisonOperator API.In comparisonColumn (J.String <$> values) scalarType

    resolve :: (MonadError QErr m') => RemoteRelSessionVariableORLiteralValue -> m' Text
    resolve = \case
      RemoteRelSessionVariable sessionVariable ->
        onNothing
          (getSessionVariableValue sessionVariable sessionVariables)
          (throw400 NotFound $ "Session variable " <> toTxt sessionVariable <> " not found")
      RemoteRelLiteralValue value -> pure value
