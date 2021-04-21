module Hasura.Backends.Postgres.DDL.BoolExp
  (parseBoolExpOperations)
where

import qualified Data.HashMap.Strict                    as Map
import qualified Data.Text                              as T

import           Data.Aeson
import           Data.Text.Extended

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Backends.Postgres.Types.BoolExp
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
   . ( MonadError QErr m
     , TableCoreInfoRM 'Postgres m
     )
  => ValueParser 'Postgres m v
  -> QualifiedTable
  -> FieldInfoMap (FieldInfo 'Postgres)
  -> ColumnInfo 'Postgres
  -> Value
  -> m [OpExpG 'Postgres v]
parseBoolExpOperations rhsParser rootTable fim columnInfo value = do
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
        "_contains"      -> guardType [PGJSONB] >> ABackendSpecific . AContains <$> parseOne
        "$contains"      -> guardType [PGJSONB] >> ABackendSpecific . AContains <$> parseOne
        "_contained_in"  -> guardType [PGJSONB] >> ABackendSpecific . AContainedIn <$> parseOne
        "$contained_in"  -> guardType [PGJSONB] >> ABackendSpecific . AContainedIn <$> parseOne
        "_has_key"       -> guardType [PGJSONB] >> ABackendSpecific . AHasKey <$> parseWithTy (ColumnScalar PGText)
        "$has_key"       -> guardType [PGJSONB] >> ABackendSpecific . AHasKey <$> parseWithTy (ColumnScalar PGText)

        "_has_keys_any"  -> guardType [PGJSONB] >> ABackendSpecific . AHasKeysAny <$> parseManyWithType (ColumnScalar PGText)
        "$has_keys_any"  -> guardType [PGJSONB] >> ABackendSpecific . AHasKeysAny <$> parseManyWithType (ColumnScalar PGText)
        "_has_keys_all"  -> guardType [PGJSONB] >> ABackendSpecific . AHasKeysAll <$> parseManyWithType (ColumnScalar PGText)
        "$has_keys_all"  -> guardType [PGJSONB] >> ABackendSpecific . AHasKeysAll <$> parseManyWithType (ColumnScalar PGText)

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
        "_st_3d_intersects" -> parseGeometryOp AST3DIntersects
        "$st_3d_intersects" -> parseGeometryOp AST3DIntersects
        "_st_d_within"   -> parseSTDWithinObj
        "$st_d_within"   -> parseSTDWithinObj
        "_st_3d_d_within" -> parseST3DDWithinObj
        "$st_3d_d_within" -> parseST3DDWithinObj

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

        -- ltree types
        "_ancestor"         -> guardType [PGLtree] >> ABackendSpecific . AAncestor <$> parseOne
        "$ancestor"         -> guardType [PGLtree] >> ABackendSpecific . AAncestor <$> parseOne
        "_ancestor_any"     -> guardType [PGLtree] >> ABackendSpecific . AAncestorAny <$> parseManyWithType (ColumnScalar PGLtree)
        "$ancestor_any"     -> guardType [PGLtree] >> ABackendSpecific . AAncestorAny <$> parseManyWithType (ColumnScalar PGLtree)
        "_descendant"       -> guardType [PGLtree] >> ABackendSpecific . ADescendant <$> parseOne
        "$descendant"       -> guardType [PGLtree] >> ABackendSpecific . ADescendant <$> parseOne
        "_descendant_any"   -> guardType [PGLtree] >> ABackendSpecific . ADescendantAny <$> parseManyWithType (ColumnScalar PGLtree)
        "$descendant_any"   -> guardType [PGLtree] >> ABackendSpecific . ADescendantAny <$> parseManyWithType (ColumnScalar PGLtree)
        "_matches"          -> guardType [PGLtree] >> ABackendSpecific . AMatches <$> parseWithTy (ColumnScalar PGLquery)
        "$matches"          -> guardType [PGLtree] >> ABackendSpecific . AMatches <$> parseWithTy (ColumnScalar PGLquery)
        "_matches_any"      -> guardType [PGLtree] >> ABackendSpecific . AMatchesAny <$> parseManyWithType (ColumnScalar PGLquery)
        "$matches_any"      -> guardType [PGLtree] >> ABackendSpecific . AMatchesAny <$> parseManyWithType (ColumnScalar PGLquery)
        "_matches_fulltext" -> guardType [PGLtree] >> ABackendSpecific . AMatchesFulltext <$> parseWithTy (ColumnScalar PGLtxtquery)
        "$matches_fulltext" -> guardType [PGLtree] >> ABackendSpecific . AMatchesFulltext <$> parseWithTy (ColumnScalar PGLtxtquery)

        x                -> throw400 UnexpectedPayload $ "Unknown operator : " <> x
      where
        colTy = columnReferenceType column

        parseIsNull   = bool ANISNOTNULL ANISNULL <$> parseVal -- is null

        parseEq       = AEQ False <$> parseOne -- equals
        parseNe       = ANE False <$> parseOne -- <>
        parseIn       = AIN  <$> parseManyWithType colTy -- in an array
        parseNin      = ANIN <$> parseManyWithType colTy -- not in an array
        parseGt       = AGT  <$> parseOne -- >
        parseLt       = ALT  <$> parseOne -- <
        parseGte      = AGTE <$> parseOne -- >=
        parseLte      = ALTE <$> parseOne -- <=

        parseCeq      = CEQ  <$> decodeAndValidateRhsCol val
        parseCne      = CNE  <$> decodeAndValidateRhsCol val
        parseCgt      = CGT  <$> decodeAndValidateRhsCol val
        parseClt      = CLT  <$> decodeAndValidateRhsCol val
        parseCgte     = CGTE <$> decodeAndValidateRhsCol val
        parseClte     = CLTE <$> decodeAndValidateRhsCol val

        parseLike     = guardType stringTypes >> ALIKE                      <$> parseOne
        parseNlike    = guardType stringTypes >> ANLIKE                     <$> parseOne
        parseIlike    = guardType stringTypes >> ABackendSpecific . AILIKE  <$> parseOne
        parseNilike   = guardType stringTypes >> ABackendSpecific . ANILIKE <$> parseOne

        parseRegex    = guardType stringTypes >> ABackendSpecific . AREGEX     <$> parseOne
        parseIRegex   = guardType stringTypes >> ABackendSpecific . AIREGEX    <$> parseOne
        parseNRegex   = guardType stringTypes >> ABackendSpecific . ANREGEX    <$> parseOne
        parseNIRegex  = guardType stringTypes >> ABackendSpecific . ANIREGEX   <$> parseOne
        parseSimilar  = guardType stringTypes >> ABackendSpecific . ASIMILAR   <$> parseOne
        parseNsimilar = guardType stringTypes >> ABackendSpecific . ANSIMILAR  <$> parseOne

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
          guardType [PGGeometry] >> ABackendSpecific . f <$> parseOneNoSess colTy val
        parseGeometryOrGeographyOp f =
          guardType geoTypes >> ABackendSpecific . f <$> parseOneNoSess colTy val

        parseSTDWithinObj = ABackendSpecific <$> case colTy of
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

        decodeAndValidateRhsCol :: Value -> m (PGCol, Maybe QualifiedTable)
        decodeAndValidateRhsCol v@(String _) = do
          j <- decodeValue v
          col <- validateRhsCol fim j
          pure (col, Nothing)
        decodeAndValidateRhsCol (Array path) = do
          case toList path of
            [] -> throw400 Unexpected "path cannot be empty"
            [col] -> do
              j <- decodeValue col
              col' <- validateRhsCol fim j
              pure $ (col', Nothing)
            (root : col : []) -> do
              root' :: Text <- decodeValue root
              unless (root' == "$") $
                throw400 NotSupported "Relationship references are not supported in column comparison RHS"
              rootTableInfo <-
                lookupTableCoreInfo rootTable
                >>= (`onNothing` (throw500 $ "unexpected: " <> rootTable <<> " doesn't exist"))
              j <- decodeValue col
              col' <- validateRhsCol (_tciFieldInfoMap rootTableInfo) j
              pure $ (col', Just rootTable)
            _ -> throw400 NotSupported "Relationship references are not supported in column comparison RHS"

        decodeAndValidateRhsCol _ =
          throw400 Unexpected "a boolean expression JSON can either be a string or an array"

        parseST3DDWithinObj = ABackendSpecific <$> do
          guardType [PGGeometry]
          DWithinGeomOp distVal fromVal <- parseVal
          dist <- withPathK "distance" $ parseOneNoSess (ColumnScalar PGFloat) distVal
          from <- withPathK "from" $ parseOneNoSess colTy fromVal
          return $ AST3DDWithinGeom $ DWithinGeomOp dist from

        validateRhsCol fieldInfoMap rhsCol = do
          let errMsg = "column operators can only compare postgres columns"
          rhsType <- askColumnType fieldInfoMap rhsCol errMsg
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
