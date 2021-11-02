module Hasura.Backends.BigQuery.DDL.BoolExp where

import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as Map
import Data.Text.Extended
import Hasura.Backends.BigQuery.Types
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.SchemaCache
import Hasura.SQL.Backend
import Hasura.SQL.Types

parseBoolExpOperations ::
  forall m v.
  (MonadError QErr m) =>
  ValueParser 'BigQuery m v ->
  TableName ->
  FieldInfoMap (FieldInfo 'BigQuery) ->
  ColumnReference 'BigQuery ->
  J.Value ->
  m [OpExpG 'BigQuery v]
parseBoolExpOperations rhsParser _table _fields columnRef value =
  withPathK (toTxt columnRef) $ parseOperations (columnReferenceType columnRef) value
  where
    parseWithTy ty = rhsParser (CollectableTypeScalar ty)

    parseOperations :: ColumnType 'BigQuery -> J.Value -> m [OpExpG 'BigQuery v]
    parseOperations columnType = \case
      J.Object o -> mapM (parseOperation columnType) $ Map.toList o
      v -> pure . AEQ False <$> parseWithTy columnType v

    parseOperation :: ColumnType 'BigQuery -> (Text, J.Value) -> m (OpExpG 'BigQuery v)
    parseOperation columnType (opStr, val) = withPathK opStr $
      case opStr of
        "_eq" -> parseEq
        "$eq" -> parseEq
        "_neq" -> parseNeq
        "$neq" -> parseNeq
        "$in" -> parseIn
        "_in" -> parseIn
        "$nin" -> parseNin
        "_nin" -> parseNin
        "_gt" -> parseGt
        "$gt" -> parseGt
        "_lt" -> parseLt
        "$lt" -> parseLt
        "_gte" -> parseGte
        "$gte" -> parseGte
        "_lte" -> parseLte
        "$lte" -> parseLte
        -- TODO: support column operators

        x -> throw400 UnexpectedPayload $ "Unknown operator : " <> x
      where
        colTy = columnReferenceType columnRef
        parseOne = parseWithTy columnType val
        parseManyWithType ty = rhsParser (CollectableTypeArray ty) val

        parseEq = AEQ False <$> parseOne
        parseNeq = ANE False <$> parseOne
        parseIn = AIN <$> parseManyWithType colTy
        parseNin = ANIN <$> parseManyWithType colTy
        parseGt = AGT <$> parseOne
        parseLt = ALT <$> parseOne
        parseGte = AGTE <$> parseOne
        parseLte = ALTE <$> parseOne
