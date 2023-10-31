module Hasura.Backends.DataConnector.Plan.RemoteRelationshipPlan
  ( mkRemoteRelationshipPlan,
  )
where

--------------------------------------------------------------------------------

import Control.Lens ((?~))
import Data.Aeson qualified as J
import Data.Aeson.Encoding qualified as JE
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Has
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Extended (toTxt)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Adapter.Types
import Hasura.Backends.DataConnector.Plan.Common
import Hasura.Backends.DataConnector.Plan.QueryPlan qualified as QueryPlan
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.IR.ModelInformation
import Hasura.RQL.IR.Select
import Hasura.RQL.IR.Value
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.Session
import Language.GraphQL.Draft.Syntax qualified as G
import Witch qualified

--------------------------------------------------------------------------------

mkRemoteRelationshipPlan ::
  forall m r.
  ( MonadError QErr m,
    MonadReader r m,
    Has API.ScalarTypesCapabilities r,
    MonadIO m,
    Has SessionVariables r
  ) =>
  SourceName ->
  SourceConfig ->
  -- | List of join json objects, each of which contains IDs to be laterally-joined against
  -- as well as an argument ID that identifies the particular set of IDs (ie 'row' to join against).
  -- This the response to the query constructed by this function is expected to contain a list
  -- of result objects, one per item in this list.
  -- Each object looks like: @{"__argument_id__": 1, "SomeIdColumn": 123}@
  NonEmpty J.Object ->
  -- | Schema of the IDs in the join json objects, excluding the argument ID
  HashMap FieldName (ColumnName, ScalarType) ->
  -- | The name of the argument ID property in the join json object. This property name and associated
  -- value must be in the result object
  FieldName ->
  -- | The field name the result of the join should be stored in the result object
  FieldName ->
  SourceRelationshipSelection 'DataConnector Void UnpreparedValue ->
  m (Plan API.QueryRequest API.QueryResponse, [ModelInfoPart])
mkRemoteRelationshipPlan sourceName _sourceConfig joinIds joinIdsSchema argumentIdFieldName resultFieldName ir = do
  foreachRowFilter <- traverse (translateForeachRowFilter argumentIdFieldName joinIdsSchema) joinIds
  argumentIds <- extractArgumentIds argumentIdFieldName joinIds
  queryRequest <- translateSourceRelationshipSelection foreachRowFilter ir
  modelNames <- getRSModelInfoGen sourceName ModelSourceTypeDataConnector ir
  let modelInfo = getModelInfoPartfromModelNames modelNames (ModelOperationType G.OperationTypeQuery)

  pure $ (Plan queryRequest (reshapeResponseToRemoteRelationshipQueryShape argumentIdFieldName argumentIds resultFieldName ir), modelInfo)
  where
    translateSourceRelationshipSelection ::
      NonEmpty (HashMap API.ColumnName API.ScalarValue) ->
      SourceRelationshipSelection 'DataConnector Void UnpreparedValue ->
      m API.QueryRequest
    translateSourceRelationshipSelection foreachRowFilter = \case
      SourceRelationshipObject objectSelect ->
        translateAnnObjectSelectToQueryRequest foreachRowFilter objectSelect
      SourceRelationshipArray simpleSelect ->
        QueryPlan.translateAnnSimpleSelectToQueryRequest simpleSelect
          <&> (API.qrForeach ?~ foreachRowFilter)
      SourceRelationshipArrayAggregate aggregateSelect ->
        QueryPlan.translateAnnAggregateSelectToQueryRequest aggregateSelect
          <&> (API.qrForeach ?~ foreachRowFilter)

    translateAnnObjectSelectToQueryRequest ::
      NonEmpty (HashMap API.ColumnName API.ScalarValue) ->
      AnnObjectSelectG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
      m API.QueryRequest
    translateAnnObjectSelectToQueryRequest foreachRowFilter AnnObjectSelectG {..} = do
      let tableName = case _aosTarget of
            FromTable table -> Witch.from table
            other -> error $ "translateAnnObjectSelectToQueryRequest: " <> show other
      ((fields, whereClause), (TableRelationships tableRelationships, redactionExpressionState, interpolatedQueries)) <-
        flip runStateT (mempty, RedactionExpressionState mempty, mempty) do
          fields <- QueryPlan.translateAnnFields noPrefix (API.TNTable tableName) _aosFields
          whereClause <- translateBoolExpToExpression (API.TNTable tableName) _aosTargetFilter
          pure (fields, whereClause)
      let apiTableRelationships = Set.fromList $ tableRelationshipsToList tableRelationships
      pure
        $ API.QueryRequest
          { _qrTarget = API.TTable (API.TargetTable tableName),
            _qrRelationships = apiTableRelationships,
            _qrRedactionExpressions = translateRedactionExpressions redactionExpressionState,
            _qrInterpolatedQueries = interpolatedQueries,
            _qrQuery =
              API.Query
                { _qFields = Just $ mapFieldNameHashMap fields,
                  _qAggregates = Nothing,
                  _qAggregatesLimit = Nothing,
                  _qLimit = Nothing,
                  _qOffset = Nothing,
                  _qWhere = whereClause,
                  _qOrderBy = Nothing
                },
            _qrForeach = Just foreachRowFilter
          }

tableRelationshipsToList :: HashMap API.TargetName (HashMap API.RelationshipName API.Relationship) -> [API.Relationships]
tableRelationshipsToList m = map relationshipDispatch (HashMap.toList m)
  where
    relationshipDispatch (API.TNFunction f, x) = API.RFunction (API.FunctionRelationships f x)
    relationshipDispatch (API.TNTable t, x) = API.RTable (API.TableRelationships t x)
    relationshipDispatch (API.TNInterpolatedQuery q, x) = API.RInterpolated (API.InterpolatedRelationships q x)

translateForeachRowFilter :: (MonadError QErr m) => FieldName -> HashMap FieldName (ColumnName, ScalarType) -> J.Object -> m (HashMap API.ColumnName API.ScalarValue)
translateForeachRowFilter argumentIdFieldName joinIdsSchema joinIds =
  joinIds
    & KM.toList
    -- Exclude the argument ID field
    -- The argument ID field and its value is added back to the result during response reshaping
    & mapMaybe
      ( \(propertyKey, value) ->
          let propertyKeyText = Text.pack $ K.toString propertyKey
              joinIdField = FieldName propertyKeyText
           in if joinIdField == argumentIdFieldName
                then Nothing
                else Just (joinIdField, value)
      )
    & traverse
      ( \(joinIdField, value) -> do
          (columnName, scalarType) <-
            HashMap.lookup joinIdField joinIdsSchema
              `onNothing` throw500 ("translateForeachRowFilter: Unable to find join id field " <> toTxt joinIdField <> " in join id schema")

          let scalarValue = API.ScalarValue value (Witch.from scalarType)
          pure (Witch.from columnName, scalarValue)
      )
    & fmap HashMap.fromList

extractArgumentIds :: (MonadError QErr m) => FieldName -> NonEmpty J.Object -> m (NonEmpty J.Value)
extractArgumentIds argumentIdFieldName joinIds =
  let argumentIdPropertyKey = K.fromText $ getFieldNameTxt argumentIdFieldName
   in joinIds
        & traverse
          ( \joinIdsObj ->
              KM.lookup argumentIdPropertyKey joinIdsObj
                `onNothing` throw500 ("translateForeachRowFilter: Unable to find argument id field " <> toTxt argumentIdFieldName <> " in join id object")
          )

--------------------------------------------------------------------------------

reshapeResponseToRemoteRelationshipQueryShape ::
  (MonadError QErr m) =>
  FieldName ->
  NonEmpty J.Value ->
  FieldName ->
  SourceRelationshipSelection 'DataConnector Void v ->
  API.QueryResponse ->
  m J.Encoding
reshapeResponseToRemoteRelationshipQueryShape argumentIdFieldName argumentIdValues resultFieldName sourceRelationshipSelection API.QueryResponse {..} = do
  when (actualRowCount /= expectedRowCount)
    $ throw500 ("Data Connector agent returned " <> tshow actualRowCount <> " foreach query response rows, but " <> tshow expectedRowCount <> " were expected")

  argumentResultObjects <- forM (zip rows (NE.toList argumentIdValues)) $ \(row, argumentId) -> do
    queryFieldValue <-
      HashMap.lookup (API.FieldName "query") row
        `onNothing` throw500 "Data Connector agent returned foreach query response row without expected 'query' field"
    foreachQueryResponse <-
      API.deserializeAsRelationshipFieldValue queryFieldValue
        `onLeft` (\err -> throw500 $ "Found column field value where relationship field value was expected in foreach query response field returned by Data Connector agent: " <> err)

    reshapedForeachQueryResponse <- reshapeForeachQueryResponse sourceRelationshipSelection foreachQueryResponse

    let argumentResponseWrapperFields =
          [ (getFieldNameTxt resultFieldName, reshapedForeachQueryResponse),
            (getFieldNameTxt argumentIdFieldName, J.toEncoding argumentId)
          ]

    pure $ encodeAssocListAsObject argumentResponseWrapperFields

  pure $ JE.list id argumentResultObjects
  where
    rows = fromMaybe mempty _qrRows
    actualRowCount = length rows
    expectedRowCount = length argumentIdValues

reshapeForeachQueryResponse ::
  (MonadError QErr m) =>
  SourceRelationshipSelection 'DataConnector Void v ->
  API.QueryResponse ->
  m J.Encoding
reshapeForeachQueryResponse sourceRelationshipSelection response =
  case sourceRelationshipSelection of
    SourceRelationshipObject objectSelect -> QueryPlan.reshapeSimpleSelectRows Single (_aosFields objectSelect) response
    SourceRelationshipArray simpleSelect -> QueryPlan.reshapeSimpleSelectRows Many (_asnFields simpleSelect) response
    SourceRelationshipArrayAggregate aggregateSelect -> QueryPlan.reshapeTableAggregateFields (_asnFields aggregateSelect) response
