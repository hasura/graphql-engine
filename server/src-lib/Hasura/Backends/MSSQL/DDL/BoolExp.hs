module Hasura.Backends.MSSQL.DDL.BoolExp where

import           Hasura.Prelude

import qualified Data.Aeson                            as J
import qualified Data.HashMap.Strict                   as Map

import           Hasura.Backends.MSSQL.Instances.Types ()
import           Hasura.Backends.MSSQL.Types           hiding (ColumnType)
import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.SchemaCache
import           Hasura.SQL.Backend
import           Hasura.SQL.Types

parseBoolExpOperations
  :: forall m v
   . (MonadError QErr m) -- , TableCoreInfoRM 'MSSQL m)
  => ValueParser 'MSSQL m v
  -> FieldInfoMap (FieldInfo 'MSSQL)
  -> ColumnInfo 'MSSQL
  -> J.Value
  -> m [OpExpG 'MSSQL v]
parseBoolExpOperations rhsParser _fields columnInfo value =
  withPathK (columnNameText $ pgiColumn columnInfo) $
    parseOperations (pgiType columnInfo) value
  where
    parseWithTy ty = rhsParser (CollectableTypeScalar ty)

    parseOperations :: ColumnType 'MSSQL -> J.Value -> m [OpExpG 'MSSQL v]
    parseOperations columnType = \case
      J.Object o -> mapM (parseOperation columnType) $ Map.toList o
      v          -> pure . AEQ False <$> parseWithTy columnType v

    parseOperation :: ColumnType 'MSSQL -> (Text, J.Value) -> m (OpExpG 'MSSQL v)
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

        x      -> throw400 UnexpectedPayload $ "Unknown operator : " <> x

      where
        parseOne = parseWithTy columnType val

        parseEq = AEQ False <$> parseOne
        parseNeq = ANE False <$> parseOne
        parseGt = AGT <$> parseOne
        parseLt = ALT <$> parseOne
        parseGte = AGTE <$> parseOne
        parseLte = ALTE <$> parseOne
