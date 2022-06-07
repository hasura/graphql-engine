module Hasura.Backends.DataConnector.Agent.Query
  ( queryHandler,
  )
where

import Autodocodec.Extended
import Control.Monad (forM)
import Control.Monad.Except (throwError)
import Data.Aeson (Object)
import Data.Aeson qualified as J
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Map
import Data.List (foldl', sortBy)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, maybeToList)
import Data.Monoid (All (..), Any (..))
import Data.Text (Text)
import Data.Vector qualified as V
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Agent.Data
import Servant.Server
import Prelude

getBinaryComparisonOperatorEvaluator :: API.BinaryComparisonOperator -> API.Value -> API.Value -> Bool
getBinaryComparisonOperatorEvaluator op left right =
  case op of
    API.LessThan -> (left /= API.Null) && (left /= API.Null) && (left < right)
    API.LessThanOrEqual -> (left /= API.Null) && (left /= API.Null) && (left <= right)
    API.GreaterThan -> (left /= API.Null) && (left /= API.Null) && (left > right)
    API.GreaterThanOrEqual -> (left /= API.Null) && (left /= API.Null) && (left >= right)
    API.Equal -> (left /= API.Null) && (left /= API.Null) && (left == right)

getBinaryArrayComparisonOperatorEvaluator :: API.BinaryArrayComparisonOperator -> API.Value -> [API.Value] -> Bool
getBinaryArrayComparisonOperatorEvaluator op left rights =
  case op of
    API.In -> (left /= API.Null) && (left `elem` rights)

getUnaryComparisonOperatorEvaluator :: API.UnaryComparisonOperator -> API.Value -> Bool
getUnaryComparisonOperatorEvaluator op value =
  case op of
    API.IsNull -> value == API.Null

makeFilterPredicate :: Maybe API.Expression -> Row -> Bool
makeFilterPredicate Nothing _row = True
makeFilterPredicate (Just e) row = evaluate e
  where
    evaluate :: API.Expression -> Bool
    evaluate = \case
      API.And (ValueWrapper exprs) ->
        getAll $ foldMap (All . evaluate) exprs
      API.Or (ValueWrapper exprs) ->
        getAny $ foldMap (Any . evaluate) exprs
      API.Not (ValueWrapper expr) ->
        not $ evaluate expr
      API.ApplyBinaryComparisonOperator (ValueWrapper3 op column comparisonValue) ->
        let columnValue = getColumnValue column row
            value = extractScalarComparisonValue comparisonValue
         in getBinaryComparisonOperatorEvaluator op columnValue value
      API.ApplyBinaryArrayComparisonOperator (ValueWrapper3 op column comparisonValues) ->
        let columnValue = getColumnValue column row
            values = extractScalarComparisonValue <$> comparisonValues
         in getBinaryArrayComparisonOperatorEvaluator op columnValue values
      API.ApplyUnaryComparisonOperator (ValueWrapper2 op column) ->
        let columnValue = getColumnValue column row
         in getUnaryComparisonOperatorEvaluator op columnValue

    getColumnValue :: API.ColumnName -> Row -> API.Value
    getColumnValue colName row' = fromMaybe API.Null . Map.lookup colName $ unRow row'

    extractScalarComparisonValue :: API.ComparisonValue -> API.Value
    extractScalarComparisonValue = \case
      (API.AnotherColumn (ValueWrapper colName)) -> getColumnValue colName row
      (API.ScalarValue (ValueWrapper value)) -> value

sortRows :: [Row] -> [API.OrderBy] -> [Row]
sortRows rows order =
  flip sortBy rows $ \(Row l) (Row r) ->
    let f acc API.OrderBy {..} =
          let leftScalar = Map.lookup column l
              rightScalar = Map.lookup column r
           in if
                  | acc /= EQ -> acc
                  | Nothing <- leftScalar -> GT
                  | Nothing <- rightScalar -> LT
                  | leftScalar == rightScalar -> EQ
                  | leftScalar < rightScalar -> LT
                  | otherwise -> GT
     in foldl' f EQ order

paginateRows :: [Row] -> Maybe Int -> Maybe Int -> [Row]
paginateRows rows offset limit =
  let dropRows = maybe id drop offset
      takeRows = maybe id take limit
   in takeRows $ dropRows rows

createSubqueryForRelationshipField :: Row -> API.RelField -> Maybe API.Query
createSubqueryForRelationshipField (Row row) API.RelField {..} =
  let filterConditions =
        Map.toList columnMapping
          & fmap (\(pk, fk) -> fmap (fk,) $ Map.lookup (API.unPrimaryKey pk) row)
          & traverse
            ( fmap \(pkColumn, fkValue) ->
                API.ApplyBinaryComparisonOperator
                  ( ValueWrapper3
                      API.Equal
                      (API.unForeignKey pkColumn)
                      (API.ScalarValue (ValueWrapper fkValue))
                  )
            )
   in case filterConditions of
        Just cnds@(_ : _) ->
          let existingFilters = maybeToList (API.where_ query)
           in Just $ query {API.where_ = Just (API.And (ValueWrapper (cnds <> existingFilters)))}
        _ -> Nothing

projectRow ::
  HashMap Text API.Field ->
  (API.Query -> Handler API.QueryResponse) ->
  Row ->
  Handler Object
projectRow fields performQuery r@(Row row) = forM fields $ \case
  API.ColumnField (ValueWrapper colName) -> pure $ maybe J.Null J.toJSON $ Map.lookup colName row
  API.RelationshipField relField ->
    let subquery = createSubqueryForRelationshipField r relField
     in case subquery of
          Just subQuery ->
            case API.relationType relField of
              API.ArrayRelationship -> do
                API.QueryResponse obj <- performQuery subQuery
                pure $ J.Array $ fmap J.Object $ V.fromList obj
              API.ObjectRelationship -> do
                API.QueryResponse obj <- performQuery subQuery
                if null obj
                  then pure J.Null
                  else pure $ J.Object $ head obj
          Nothing -> pure $ J.Array mempty

queryHandler :: StaticData -> API.SourceName -> API.Config -> API.Query -> Handler API.QueryResponse
queryHandler (StaticData staticData') _sourceName _config query =
  performQuery query
  where
    performQuery :: API.Query -> Handler API.QueryResponse
    performQuery API.Query {..} =
      case Map.lookup from staticData' of
        Nothing -> throwError $ err400 {errBody = "query.from is not a valid table"}
        Just rows -> do
          let filteredRows = filter (makeFilterPredicate where_) rows
              sortedRows = sortRows filteredRows $ maybe [] NE.toList orderBy
              slicedRows = paginateRows sortedRows offset limit
          projectedRows <- traverse (projectRow fields performQuery) slicedRows
          pure $ API.QueryResponse projectedRows
