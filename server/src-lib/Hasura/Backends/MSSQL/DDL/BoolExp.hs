module Hasura.Backends.MSSQL.DDL.BoolExp where

import           Hasura.Prelude

import qualified Data.Aeson                            as J
import qualified Data.HashMap.Strict                   as Map
import qualified Data.Text                             as T

import           Data.Text.Extended                    (dquote, (<<>))

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
  -> TableName
  -> FieldInfoMap (FieldInfo 'MSSQL)
  -> ColumnInfo 'MSSQL
  -> J.Value
  -> m [OpExpG 'MSSQL v]
parseBoolExpOperations rhsParser _table _fields columnInfo value =
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
        "_eq"            -> parseEq
        "$eq"            -> parseEq

        "_neq"           -> parseNeq
        "$neq"           -> parseNeq

        "$in"            -> parseIn
        "_in"            -> parseIn

        "$nin"           -> parseNin
        "_nin"           -> parseNin

        "_gt"            -> parseGt
        "$gt"            -> parseGt

        "_lt"            -> parseLt
        "$lt"            -> parseLt

        "_gte"           -> parseGte
        "$gte"           -> parseGte

        "_lte"           -> parseLte
        "$lte"           -> parseLte

        "$like"          -> parseLike
        "_like"          -> parseLike

        "$nlike"         -> parseNlike
        "_nlike"         -> parseNlike

        "_st_contains"   -> ABackendSpecific <$> parseGeometryOrGeographyOp ASTContains
        "$st_contains"   -> ABackendSpecific <$> parseGeometryOrGeographyOp ASTContains
        "_st_equals"     -> ABackendSpecific <$> parseGeometryOrGeographyOp ASTEquals
        "$st_equals"     -> ABackendSpecific <$> parseGeometryOrGeographyOp ASTEquals
        "_st_intersects" -> ABackendSpecific <$> parseGeometryOrGeographyOp ASTIntersects
        "$st_intersects" -> ABackendSpecific <$> parseGeometryOrGeographyOp ASTIntersects
        "_st_overlaps"   -> ABackendSpecific <$> parseGeometryOrGeographyOp ASTOverlaps
        "$st_overlaps"   -> ABackendSpecific <$> parseGeometryOrGeographyOp ASTOverlaps
        "_st_within"     -> ABackendSpecific <$> parseGeometryOrGeographyOp ASTWithin
        "$st_within"     -> ABackendSpecific <$> parseGeometryOrGeographyOp ASTWithin

        "_st_crosses"    -> ABackendSpecific <$> parseGeometryOp ASTCrosses
        "$st_crosses"    -> ABackendSpecific <$> parseGeometryOp ASTCrosses
        "_st_touches"    -> ABackendSpecific <$> parseGeometryOp ASTTouches
        "$st_touches"    -> ABackendSpecific <$> parseGeometryOp ASTTouches

        x                -> throw400 UnexpectedPayload $ "Unknown operator : " <> x

      where
        colTy = pgiType columnInfo

        parseOne = parseWithTy columnType val
        parseManyWithType ty = rhsParser (CollectableTypeArray ty) val

        parseEq    = AEQ False <$> parseOne
        parseNeq   = ANE False <$> parseOne
        parseIn    = AIN <$> parseManyWithType colTy
        parseNin   = ANIN <$> parseManyWithType colTy
        parseGt    = AGT <$> parseOne
        parseLt    = ALT <$> parseOne
        parseGte   = AGTE <$> parseOne
        parseLte   = ALTE <$> parseOne
        parseLike  = guardType stringTypes >> ALIKE  <$> parseOne
        parseNlike = guardType stringTypes >> ANLIKE <$> parseOne

        parseGeometryOp f =
          guardType [GeometryType] >> f <$> parseOneNoSess colTy val
        parseGeometryOrGeographyOp f =
          guardType geoTypes >> f <$> parseOneNoSess colTy val
        parseOneNoSess ty = rhsParser (CollectableTypeScalar ty)

        guardType validTys = unless (isScalarColumnWhere (`elem` validTys) colTy) $
          throwError $ buildMsg colTy validTys

        buildMsg ty expTys = err400 UnexpectedPayload
          $ " is of type " <> ty <<> "; this operator works only on columns of type "
          <> T.intercalate "/" (map dquote expTys)
