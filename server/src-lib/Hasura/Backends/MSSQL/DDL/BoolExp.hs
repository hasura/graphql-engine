-- | MSSQL DDL BoolExp
--
-- How to parse the boolean expressions and operations relevant for MSSQL.
module Hasura.Backends.MSSQL.DDL.BoolExp
  ( parseBoolExpOperations,
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Text qualified as T
import Data.Text.Extended (dquote, toTxt, (<<>))
import Hasura.Backends.MSSQL.Types.Internal hiding (ColumnType)
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.SchemaCache
import Hasura.SQL.Types

parseBoolExpOperations ::
  forall m v.
  (MonadError QErr m) => -- , TableCoreInfoRM 'MSSQL m)
  ValueParser 'MSSQL m v ->
  FieldInfoMap (FieldInfo 'MSSQL) ->
  FieldInfoMap (FieldInfo 'MSSQL) ->
  ColumnReference 'MSSQL ->
  J.Value ->
  m [OpExpG 'MSSQL v]
parseBoolExpOperations rhsParser _rootTableFieldInfoMap _fields columnRef value =
  withPathK (toTxt columnRef) $ parseOperations (columnReferenceType columnRef) value
  where
    parseWithTy ty = rhsParser (CollectableTypeScalar ty)

    parseOperations :: ColumnType 'MSSQL -> J.Value -> m [OpExpG 'MSSQL v]
    parseOperations columnType = \case
      J.Object o -> mapM (parseOperation columnType . first K.toText) $ KM.toList o
      v -> pure . AEQ NullableComparison <$> parseWithTy columnType v

    parseOperation :: ColumnType 'MSSQL -> (Text, J.Value) -> m (OpExpG 'MSSQL v)
    parseOperation columnType (opStr, val) = withPathK opStr
      $ case opStr of
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
        "$like" -> parseLike
        "_like" -> parseLike
        "$nlike" -> parseNlike
        "_nlike" -> parseNlike
        "$is_null" -> parseIsNull
        "_is_null" -> parseIsNull
        "_st_contains" -> ABackendSpecific <$> parseGeometryOrGeographyOp ASTContains
        "$st_contains" -> ABackendSpecific <$> parseGeometryOrGeographyOp ASTContains
        "_st_equals" -> ABackendSpecific <$> parseGeometryOrGeographyOp ASTEquals
        "$st_equals" -> ABackendSpecific <$> parseGeometryOrGeographyOp ASTEquals
        "_st_intersects" -> ABackendSpecific <$> parseGeometryOrGeographyOp ASTIntersects
        "$st_intersects" -> ABackendSpecific <$> parseGeometryOrGeographyOp ASTIntersects
        "_st_overlaps" -> ABackendSpecific <$> parseGeometryOrGeographyOp ASTOverlaps
        "$st_overlaps" -> ABackendSpecific <$> parseGeometryOrGeographyOp ASTOverlaps
        "_st_within" -> ABackendSpecific <$> parseGeometryOrGeographyOp ASTWithin
        "$st_within" -> ABackendSpecific <$> parseGeometryOrGeographyOp ASTWithin
        "_st_crosses" -> ABackendSpecific <$> parseGeometryOp ASTCrosses
        "$st_crosses" -> ABackendSpecific <$> parseGeometryOp ASTCrosses
        "_st_touches" -> ABackendSpecific <$> parseGeometryOp ASTTouches
        "$st_touches" -> ABackendSpecific <$> parseGeometryOp ASTTouches
        x -> throw400 UnexpectedPayload $ "Unknown operator: " <> x
      where
        colTy = columnReferenceType columnRef

        parseOne = parseWithTy columnType val
        parseManyWithType ty = rhsParser (CollectableTypeArray ty) val

        parseEq = AEQ NullableComparison <$> parseOne
        parseNeq = ANE NullableComparison <$> parseOne
        parseIn = AIN <$> parseManyWithType colTy
        parseNin = ANIN <$> parseManyWithType colTy
        parseGt = AGT <$> parseOne
        parseLt = ALT <$> parseOne
        parseGte = AGTE <$> parseOne
        parseLte = ALTE <$> parseOne
        parseLike = guardType stringTypes >> ALIKE <$> parseOne
        parseNlike = guardType stringTypes >> ANLIKE <$> parseOne
        parseIsNull = bool ANISNOTNULL ANISNULL <$> parseVal

        parseGeometryOp f =
          guardType [GeometryType] >> f <$> parseOneNoSess colTy val
        parseGeometryOrGeographyOp f =
          guardType geoTypes >> f <$> parseOneNoSess colTy val
        parseOneNoSess ty = rhsParser (CollectableTypeScalar ty)

        guardType validTys =
          unless (isScalarColumnWhere (`elem` validTys) colTy)
            $ throwError
            $ buildMsg colTy validTys

        buildMsg ty expTys =
          err400 UnexpectedPayload
            $ " is of type "
            <> ty
            <<> "; this operator works only on columns of type "
            <> T.intercalate "/" (map dquote expTys)

        parseVal :: (J.FromJSON a) => m a
        parseVal = decodeValue val
