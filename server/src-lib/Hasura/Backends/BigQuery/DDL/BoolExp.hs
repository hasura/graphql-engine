module Hasura.Backends.BigQuery.DDL.BoolExp
  ( parseBoolExpOperations,
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Text.Extended
import Hasura.Backends.BigQuery.Types (ScalarType (StringScalarType))
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.SchemaCache
import Hasura.SQL.Types

parseBoolExpOperations ::
  forall m v.
  (MonadError QErr m) =>
  ValueParser 'BigQuery m v ->
  FieldInfoMap (FieldInfo 'BigQuery) ->
  FieldInfoMap (FieldInfo 'BigQuery) ->
  ColumnReference 'BigQuery ->
  J.Value ->
  m [OpExpG 'BigQuery v]
parseBoolExpOperations rhsParser _rootTableFieldInfoMap _fields columnRef value =
  withPathK (toTxt columnRef) $ parseOperations (columnReferenceType columnRef) value
  where
    parseWithTy ty = rhsParser (CollectableTypeScalar ty)

    parseOperations :: ColumnType 'BigQuery -> J.Value -> m [OpExpG 'BigQuery v]
    parseOperations columnType = \case
      J.Object o -> mapM (parseOperation columnType . first K.toText) $ KM.toList o
      v -> pure . AEQ NullableComparison <$> parseWithTy columnType v

    parseOperation :: ColumnType 'BigQuery -> (Text, J.Value) -> m (OpExpG 'BigQuery v)
    parseOperation columnType (opStr, val) = withPathK opStr
      $ case opStr of
        "_eq" -> parseEq
        "$eq" -> parseEq
        "_neq" -> parseNeq
        "$neq" -> parseNeq
        "_gt" -> parseGt
        "$gt" -> parseGt
        "_lt" -> parseLt
        "$lt" -> parseLt
        "_gte" -> parseGte
        "$gte" -> parseGte
        "_lte" -> parseLte
        "$lte" -> parseLte
        "_like" -> parseLike
        "$like" -> parseLike
        "_nlike" -> parseNlike
        "$nlike" -> parseNlike
        "_in" -> parseIn
        "$in" -> parseIn
        "_nin" -> parseNin
        "$nin" -> parseNin
        "_is_null" -> parseIsNull
        "$is_null" -> parseIsNull
        -- TODO: support column operators

        x -> throw400 UnexpectedPayload $ "Unknown operator: " <> x
      where
        colTy = columnReferenceType columnRef
        parseOne = parseWithTy columnType val
        parseManyWithType ty = rhsParser (CollectableTypeArray ty) val

        parseEq = AEQ NullableComparison <$> parseOne
        parseNeq = ANE NullableComparison <$> parseOne
        parseGt = AGT <$> parseOne
        parseLt = ALT <$> parseOne
        parseGte = AGTE <$> parseOne
        parseLte = ALTE <$> parseOne
        parseLike = guardType StringScalarType >> ALIKE <$> parseOne
        parseNlike = guardType StringScalarType >> ANLIKE <$> parseOne
        parseIn = AIN <$> parseManyWithType colTy
        parseNin = ANIN <$> parseManyWithType colTy
        parseIsNull = bool ANISNOTNULL ANISNULL <$> decodeValue val

        guardType validType =
          unless (isScalarColumnWhere (== validType) colTy)
            $ throwError
            $ err400 UnexpectedPayload
            $ " is of type "
            <> columnReferenceType columnRef
            <<> "; this operator works only on columns of type "
            <>> validType
