module Hasura.Backends.Postgres.DDL.BoolExp
  (parseBoolExpOperations)
where

import qualified Data.HashMap.Strict                as Map
import qualified Data.Text                          as T

import           Data.Aeson
import           Data.Text.Extended

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Prelude
import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.SchemaCache
import           Hasura.RQL.Types.Table
import           Hasura.SQL.Backend
import           Hasura.SQL.Types

-- | Represents a reference to a Postgres column, possibly casted an arbitrary
-- number of times. Used within 'parseOperationsExpression' for bookkeeping.
data ColumnReference (b :: BackendType)
  = ColumnReferenceColumn !(ColumnInfo b)
  | ColumnReferenceCast !(ColumnReference b) !(ColumnType b)

columnReferenceType :: ColumnReference backend -> ColumnType backend
columnReferenceType = \case
  ColumnReferenceColumn column     -> pgiType column
  ColumnReferenceCast _ targetType -> targetType

instance ToTxt (ColumnReference 'Postgres) where
  toTxt = \case
    ColumnReferenceColumn column -> toTxt $ pgiColumn column
    ColumnReferenceCast reference targetType ->
      toTxt reference <> "::" <> toTxt targetType

parseBoolExpOperations
  :: forall m v
   . (MonadError QErr m)
  => ValueParser 'Postgres m v
  -> FieldInfoMap (FieldInfo 'Postgres)
  -> ColumnInfo 'Postgres
  -> Value
  -> m [OpExpG 'Postgres v]
parseBoolExpOperations rhsParser fim columnInfo value = do
  restrictJSONColumn
  withPathK (getPGColTxt $ pgiColumn columnInfo) $
    parseOperations (ColumnReferenceColumn columnInfo) value
  where
    restrictJSONColumn :: m ()
    restrictJSONColumn = case columnInfo of
      ColumnInfo _ _ _ (ColumnScalar PGJSON) _ _ ->
        throwError (err400 UnexpectedPayload "JSON column can not be part of boolean expression")
      _                                          -> pure ()

    parseOperations :: ColumnReference 'Postgres -> Value -> m [OpExpG 'Postgres v]
    parseOperations column = \case
      Object o -> mapM (parseOperation column) (Map.toList o)
      val      -> pure . AEQ False <$> rhsParser columnType val
      where
        columnType = CollectableTypeScalar $ columnReferenceType column

    parseOperation :: ColumnReference 'Postgres -> (Text, Value) -> m (OpExpG 'Postgres v)
    parseOperation column (opStr, val) = withPathK opStr $
      case opStr of
        "$cast"          -> parseCast
        "_cast"          -> parseCast

        "$eq"            -> parseEq
        "_eq"            -> parseEq

        "$ne"            -> parseNe
        "_ne"            -> parseNe
        "$neq"           -> parseNe
        "_neq"           -> parseNe

        "$in"            -> parseIn
        "_in"            -> parseIn

        "$nin"           -> parseNin
        "_nin"           -> parseNin

        "$gt"            -> parseGt
        "_gt"            -> parseGt

        "$lt"            -> parseLt
        "_lt"            -> parseLt

        "$gte"           -> parseGte
        "_gte"           -> parseGte

        "$lte"           -> parseLte
        "_lte"           -> parseLte

        "$like"          -> parseLike
        "_like"          -> parseLike

        "$nlike"         -> parseNlike
        "_nlike"         -> parseNlike

        "$ilike"         -> parseIlike
        "_ilike"         -> parseIlike

        "$nilike"        -> parseNilike
        "_nilike"        -> parseNilike

        "$similar"       -> parseSimilar
        "_similar"       -> parseSimilar
        "$nsimilar"      -> parseNsimilar
        "_nsimilar"      -> parseNsimilar

        "$regex"         -> parseRegex
        "_regex"         -> parseRegex
        "$iregex"        -> parseIRegex
        "_iregex"        -> parseIRegex
        "$nregex"        -> parseNRegex
        "_nregex"        -> parseNRegex
        "$niregex"       -> parseNIRegex
        "_niregex"       -> parseNIRegex

        "$is_null"       -> parseIsNull
        "_is_null"       -> parseIsNull

        -- jsonb type
        "_contains"      -> guardType [PGJSONB] >> AContains <$> parseOne
        "$contains"      -> guardType [PGJSONB] >> AContains <$> parseOne
        "_contained_in"  -> guardType [PGJSONB] >> AContainedIn <$> parseOne
        "$contained_in"  -> guardType [PGJSONB] >> AContainedIn <$> parseOne
        "_has_key"       -> guardType [PGJSONB] >> AHasKey <$> parseWithTy (ColumnScalar PGText)
        "$has_key"       -> guardType [PGJSONB] >> AHasKey <$> parseWithTy (ColumnScalar PGText)

        "_has_keys_any"  -> guardType [PGJSONB] >> AHasKeysAny <$> parseManyWithType (ColumnScalar PGText)
        "$has_keys_any"  -> guardType [PGJSONB] >> AHasKeysAny <$> parseManyWithType (ColumnScalar PGText)
        "_has_keys_all"  -> guardType [PGJSONB] >> AHasKeysAll <$> parseManyWithType (ColumnScalar PGText)
        "$has_keys_all"  -> guardType [PGJSONB] >> AHasKeysAll <$> parseManyWithType (ColumnScalar PGText)

        -- geometry types
        "_st_contains"   -> parseGeometryOp ASTContains
        "$st_contains"   -> parseGeometryOp ASTContains
        "_st_crosses"    -> parseGeometryOp ASTCrosses
        "$st_crosses"    -> parseGeometryOp ASTCrosses
        "_st_equals"     -> parseGeometryOp ASTEquals
        "$st_equals"     -> parseGeometryOp ASTEquals
        "_st_overlaps"   -> parseGeometryOp ASTOverlaps
        "$st_overlaps"   -> parseGeometryOp ASTOverlaps
        "_st_touches"    -> parseGeometryOp ASTTouches
        "$st_touches"    -> parseGeometryOp ASTTouches
        "_st_within"     -> parseGeometryOp ASTWithin
        "$st_within"     -> parseGeometryOp ASTWithin
        -- geometry and geography types
        "_st_intersects" -> parseGeometryOrGeographyOp ASTIntersects
        "$st_intersects" -> parseGeometryOrGeographyOp ASTIntersects
        "_st_d_within"   -> parseSTDWithinObj
        "$st_d_within"   -> parseSTDWithinObj

        "$ceq"           -> parseCeq
        "_ceq"           -> parseCeq

        "$cne"           -> parseCne
        "_cne"           -> parseCne
        "$cneq"          -> parseCne
        "_cneq"          -> parseCne

        "$cgt"           -> parseCgt
        "_cgt"           -> parseCgt

        "$clt"           -> parseClt
        "_clt"           -> parseClt

        "$cgte"          -> parseCgte
        "_cgte"          -> parseCgte

        "$clte"          -> parseClte
        "_clte"          -> parseClte

        x                -> throw400 UnexpectedPayload $ "Unknown operator : " <> x
      where
        colTy = columnReferenceType column

        parseEq       = AEQ False <$> parseOne -- equals
        parseNe       = ANE False <$> parseOne -- <>
        parseIn       = AIN <$> parseManyWithType colTy -- in an array
        parseNin      = ANIN <$> parseManyWithType colTy -- not in an array
        parseGt       = AGT <$> parseOne -- >
        parseLt       = ALT <$> parseOne -- <
        parseGte      = AGTE <$> parseOne -- >=
        parseLte      = ALTE <$> parseOne -- <=
        parseLike     = guardType stringTypes >> ALIKE <$> parseOne
        parseNlike    = guardType stringTypes >> ANLIKE <$> parseOne
        parseIlike    = guardType stringTypes >> AILIKE () <$> parseOne
        parseNilike   = guardType stringTypes >> ANILIKE () <$> parseOne
        parseSimilar  = guardType stringTypes >> ASIMILAR <$> parseOne
        parseNsimilar = guardType stringTypes >> ANSIMILAR <$> parseOne
        parseRegex    = guardType stringTypes >> AREGEX <$> parseOne
        parseIRegex   = guardType stringTypes >> AIREGEX <$> parseOne
        parseNRegex   = guardType stringTypes >> ANREGEX <$> parseOne
        parseNIRegex  = guardType stringTypes >> ANIREGEX <$> parseOne

        parseIsNull   = bool ANISNOTNULL ANISNULL -- is null
                        <$> parseVal

        parseCeq      = CEQ <$> decodeAndValidateRhsCol
        parseCne      = CNE <$> decodeAndValidateRhsCol
        parseCgt      = CGT <$> decodeAndValidateRhsCol
        parseClt      = CLT <$> decodeAndValidateRhsCol
        parseCgte     = CGTE <$> decodeAndValidateRhsCol
        parseClte     = CLTE <$> decodeAndValidateRhsCol

        parseCast = do
          castOperations <- parseVal
          parsedCastOperations <-
            forM (Map.toList castOperations) $ \(targetTypeName, castedComparisons) -> do
              let targetType = textToPGScalarType targetTypeName
                  castedColumn = ColumnReferenceCast column (ColumnScalar targetType)
              checkValidCast targetType
              parsedCastedComparisons <- withPathK targetTypeName $
                parseOperations castedColumn castedComparisons
              return (targetType, parsedCastedComparisons)
          return . ACast $ Map.fromList parsedCastOperations

        checkValidCast targetType = case (colTy, targetType) of
          (ColumnScalar PGGeometry, PGGeography) -> return ()
          (ColumnScalar PGGeography, PGGeometry) -> return ()
          _ -> throw400 UnexpectedPayload $
            "cannot cast column of type " <> colTy <<> " to type " <>> targetType

        parseGeometryOp f =
          guardType [PGGeometry] >> f <$> parseOneNoSess colTy val
        parseGeometryOrGeographyOp f =
          guardType geoTypes >> f <$> parseOneNoSess colTy val

        parseSTDWithinObj = case colTy of
          ColumnScalar PGGeometry -> do
            DWithinGeomOp distVal fromVal <- parseVal
            dist <- withPathK "distance" $ parseOneNoSess (ColumnScalar PGFloat) distVal
            from <- withPathK "from" $ parseOneNoSess colTy fromVal
            return $ ASTDWithinGeom $ DWithinGeomOp dist from
          ColumnScalar PGGeography -> do
            DWithinGeogOp distVal fromVal sphVal <- parseVal
            dist <- withPathK "distance" $ parseOneNoSess (ColumnScalar PGFloat) distVal
            from <- withPathK "from" $ parseOneNoSess colTy fromVal
            useSpheroid <- withPathK "use_spheroid" $ parseOneNoSess (ColumnScalar PGBoolean) sphVal
            return $ ASTDWithinGeog $ DWithinGeogOp dist from useSpheroid
          _ -> throwError $ buildMsg colTy [PGGeometry, PGGeography]

        decodeAndValidateRhsCol =
          parseVal >>= validateRhsCol

        validateRhsCol rhsCol = do
          let errMsg = "column operators can only compare postgres columns"
          rhsType <- askColumnType fim rhsCol errMsg
          if colTy /= rhsType
            then throw400 UnexpectedPayload $
                 "incompatible column types : " <> column <<> ", " <>> rhsCol
            else return rhsCol

        parseWithTy ty = rhsParser (CollectableTypeScalar ty) val

        -- parse one with the column's type
        parseOne = parseWithTy colTy
        parseOneNoSess ty = rhsParser (CollectableTypeScalar ty)

        parseManyWithType ty = rhsParser (CollectableTypeArray ty) val

        guardType validTys = unless (isScalarColumnWhere (`elem` validTys) colTy) $
          throwError $ buildMsg colTy validTys
        buildMsg ty expTys = err400 UnexpectedPayload
          $ " is of type " <> ty <<> "; this operator works only on columns of type "
          <> T.intercalate "/" (map dquote expTys)

        parseVal :: (FromJSON a) => m a
        parseVal = decodeValue val
