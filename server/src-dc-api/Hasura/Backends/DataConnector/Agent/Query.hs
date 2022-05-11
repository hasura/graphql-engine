module Hasura.Backends.DataConnector.Agent.Query
  ( queryHandler,
  )
where

--------------------------------------------------------------------------------

import Autodocodec.Extended
import Control.Monad (forM)
import Control.Monad.Except (throwError)
import Data.Aeson (Object)
import Data.Aeson qualified as J
import Data.Coerce
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

--------------------------------------------------------------------------------

getOperatorEvaluator :: API.Operator -> API.Value -> API.Value -> API.Value
getOperatorEvaluator op left right =
  case op of
    API.LessThan -> API.Boolean $ left < right
    API.LessThanOrEqual -> API.Boolean $ left <= right
    API.GreaterThan -> API.Boolean $ left > right
    API.GreaterThanOrEqual -> API.Boolean $ left >= right
    API.Equal -> API.Boolean $ left == right

-- TODO(SOLOMON): Move into ExceptT
makeFilterPredicate :: Maybe API.Expression -> Row -> Bool
makeFilterPredicate Nothing _row = True
makeFilterPredicate (Just e) row = validateBoolean $ evaluate e
  where
    validateBoolean :: API.Value -> Bool
    validateBoolean (API.Boolean p) = p
    validateBoolean _ = error $ "Expression does not evaluate to a boolean type: " <> show e

    evaluate :: API.Expression -> API.Value
    evaluate = \case
      API.Literal (ValueWrapper val) -> val
      API.In (ValueWrapper2 expr values) ->
        let val = evaluate expr
         in API.Boolean $ elem val values
      API.And (ValueWrapper exprs) ->
        API.Boolean $ getAll $ foldMap (All . validateBoolean . evaluate) exprs
      API.Or (ValueWrapper exprs) ->
        API.Boolean $ getAny $ foldMap (Any . validateBoolean . evaluate) exprs
      API.Not (ValueWrapper expr) ->
        API.Boolean $ not $ validateBoolean $ evaluate expr
      API.IsNull (ValueWrapper expr) ->
        API.Boolean $ (==) API.Null $ evaluate expr
      API.Column (ValueWrapper colName) -> fromMaybe API.Null $ Map.lookup colName $ unRow row
      API.ApplyOperator (ValueWrapper3 op left right) ->
        getOperatorEvaluator op (evaluate left) (evaluate right)

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
  let start = fromMaybe 0 offset
      end :: Int
      end = maybe (length rows) (+ start) limit
   in take (end - start) $ drop start rows

createSubqueryForRelationshipField :: Row -> API.RelField -> Maybe API.Query
createSubqueryForRelationshipField (Row row) API.RelField {..} =
  let filterConditions =
        Map.toList columnMapping
          & fmap (\(fk, pk) -> fmap (pk,) $ Map.lookup (coerce fk) row)
          & traverse
            ( fmap \(pkColumn, fkValue) ->
                API.ApplyOperator
                  ( ValueWrapper3
                      API.Equal
                      (API.Column (ValueWrapper (coerce pkColumn)))
                      (API.Literal (ValueWrapper fkValue))
                  )
            )
   in case filterConditions of
        Just cnds@(_ : _) ->
          let existingFilters = maybeToList (API.where_ query)
           in Just $ query {API.where_ = Just (API.And (ValueWrapper (cnds <> existingFilters)))}
        _ -> Nothing

projectRow :: HashMap Text API.Field -> (API.Query -> Handler API.QueryResponse) -> Row -> Handler Object
projectRow fields k (Row row) = forM fields $ \case
  API.ColumnField (ValueWrapper colName) -> pure $ maybe J.Null J.toJSON $ Map.lookup colName row
  API.RelationshipField relField ->
    let subquery = createSubqueryForRelationshipField (Row row) relField
     in case subquery of
          Just subQuery -> do
            API.QueryResponse obj <- k subQuery
            pure $ J.Array $ fmap J.Object $ V.fromList obj
          Nothing -> pure $ J.Array mempty

queryHandler :: StaticData -> API.Config -> API.Query -> Handler API.QueryResponse
queryHandler sd@(StaticData staticData') config API.Query {..} =
  case Map.lookup from staticData' of
    Nothing -> throwError $ err400 {errBody = "query.from is not a valid table"}
    Just rows -> do
      let filteredRows = filter (makeFilterPredicate where_) rows
          sortedRows = sortRows filteredRows $ maybe [] NE.toList orderBy
          slicedRows = paginateRows sortedRows offset limit
      projectedRows <- traverse (projectRow fields (queryHandler sd config)) slicedRows
      pure $ API.QueryResponse projectedRows
