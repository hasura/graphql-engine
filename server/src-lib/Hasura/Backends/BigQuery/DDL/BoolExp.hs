module Hasura.Backends.BigQuery.DDL.BoolExp where

import           Hasura.Prelude

import qualified Data.Aeson                               as J
import qualified Data.HashMap.Strict                      as Map

import           Hasura.Backends.BigQuery.Instances.Types ()
import           Hasura.Backends.BigQuery.Types
import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.SchemaCache
import           Hasura.SQL.Backend
import           Hasura.SQL.Types

parseBoolExpOperations
  :: forall m v
   . (MonadError QErr m)
  => ValueParser 'BigQuery m v
  -> TableName
  -> FieldInfoMap (FieldInfo 'BigQuery)
  -> ColumnInfo 'BigQuery
  -> J.Value
  -> m [OpExpG 'BigQuery v]
parseBoolExpOperations rhsParser _table _fields columnInfo value =
  withPathK (columnName $ pgiColumn columnInfo) $
    parseOperations (pgiType columnInfo) value
  where
    parseWithTy ty = rhsParser (CollectableTypeScalar ty)

    parseOperations :: ColumnType 'BigQuery -> J.Value -> m [OpExpG 'BigQuery v]
    parseOperations columnType = \case
      J.Object o -> mapM (parseOperation columnType) $ Map.toList o
      v          -> pure . AEQ False <$> parseWithTy columnType v

    parseOperation :: ColumnType 'BigQuery -> (Text, J.Value) -> m (OpExpG 'BigQuery v)
    parseOperation columnType (opStr, val) = withPathK opStr $
      case opStr of
        "_eq"  -> parseEq
        "$eq"  -> parseEq

        "_neq" -> parseNeq
        "$neq" -> parseNeq

        "_gt"  -> parseGt
        "$gt"  -> parseGt

        "_lt"  -> parseLt
        "$lt"  -> parseLt

        "_gte" -> parseGte
        "$gte" -> parseGte

        "_lte" -> parseLte
        "$lte" -> parseLte

        -- TODO: support column operators

        x      -> throw400 UnexpectedPayload $ "Unknown operator : " <> x

      where
        parseOne = parseWithTy columnType val

        parseEq = AEQ False <$> parseOne
        parseNeq = ANE False <$> parseOne
        parseGt = AGT <$> parseOne
        parseLt = ALT <$> parseOne
        parseGte = AGTE <$> parseOne
        parseLte = ALTE <$> parseOne
