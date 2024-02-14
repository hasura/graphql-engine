-- | Postgres DDL BoolExp
--
-- How to parse the boolean expressions, specifically for Postgres.
--
-- See 'Hasura.Eventing.Backend'.
module Hasura.Backends.Postgres.DDL.BoolExp
  ( parseBoolExpOperations,
    buildComputedFieldBooleanExp,
  )
where

import Data.Aeson
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.HashMap.Strict qualified as HashMap
import Data.Text.Extended
import Hasura.Backends.Postgres.SQL.Types hiding (TableName)
import Hasura.Backends.Postgres.Types.BoolExp
import Hasura.Backends.Postgres.Types.ComputedField as PG
import Hasura.Base.Error
import Hasura.Function.Cache
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.BoolExp
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.SchemaCache
import Hasura.SQL.Types
import Hasura.Table.Cache

parseBoolExpOperations ::
  forall pgKind m v.
  ( Backend ('Postgres pgKind),
    MonadError QErr m
  ) =>
  ValueParser ('Postgres pgKind) m v ->
  FieldInfoMap (FieldInfo ('Postgres pgKind)) ->
  FieldInfoMap (FieldInfo ('Postgres pgKind)) ->
  ColumnReference ('Postgres pgKind) ->
  Value ->
  m [OpExpG ('Postgres pgKind) v]
parseBoolExpOperations rhsParser rootFieldInfoMap fim columnRef value = do
  restrictJSONColumn
  withPathK (toTxt columnRef) $ parseOperations columnRef value
  where
    restrictJSONColumn :: m ()
    restrictJSONColumn = case columnReferenceType columnRef of
      ColumnScalar PGJSON ->
        throwError (err400 UnexpectedPayload "JSON column can not be part of boolean expression")
      _ -> pure ()

    parseOperations :: ColumnReference ('Postgres pgKind) -> Value -> m [OpExpG ('Postgres pgKind) v]
    parseOperations column = \case
      Object o -> mapM (parseOperation column . first K.toText) (KM.toList o)
      val -> pure . AEQ NullableComparison <$> rhsParser columnType val
      where
        columnType = CollectableTypeScalar $ columnReferenceType column

    parseOperation :: ColumnReference ('Postgres pgKind) -> (Text, Value) -> m (OpExpG ('Postgres pgKind) v)
    parseOperation column (opStr, val) = withPathK opStr
      $ case opStr of
        "$cast" -> parseCast
        "_cast" -> parseCast
        "$eq" -> parseEq
        "_eq" -> parseEq
        "$ne" -> parseNe
        "_ne" -> parseNe
        "$neq" -> parseNe
        "_neq" -> parseNe
        "$in" -> parseIn
        "_in" -> parseIn
        "$nin" -> parseNin
        "_nin" -> parseNin
        "$gt" -> parseGt
        "_gt" -> parseGt
        "$lt" -> parseLt
        "_lt" -> parseLt
        "$gte" -> parseGte
        "_gte" -> parseGte
        "$lte" -> parseLte
        "_lte" -> parseLte
        "$like" -> parseLike
        "_like" -> parseLike
        "$nlike" -> parseNlike
        "_nlike" -> parseNlike
        "$ilike" -> parseIlike
        "_ilike" -> parseIlike
        "$nilike" -> parseNilike
        "_nilike" -> parseNilike
        "$similar" -> parseSimilar
        "_similar" -> parseSimilar
        "$nsimilar" -> parseNsimilar
        "_nsimilar" -> parseNsimilar
        "$regex" -> parseRegex
        "_regex" -> parseRegex
        "$iregex" -> parseIRegex
        "_iregex" -> parseIRegex
        "$nregex" -> parseNRegex
        "_nregex" -> parseNRegex
        "$niregex" -> parseNIRegex
        "_niregex" -> parseNIRegex
        "$is_null" -> parseIsNull
        "_is_null" -> parseIsNull
        -- arrays and jsonb type
        "_contains" -> guardTypeToArrayOrJsonb >> ABackendSpecific . AContains <$> parseOne
        "$contains" -> guardTypeToArrayOrJsonb >> ABackendSpecific . AContains <$> parseOne
        "_contained_in" -> guardTypeToArrayOrJsonb >> ABackendSpecific . AContainedIn <$> parseOne
        "$contained_in" -> guardTypeToArrayOrJsonb >> ABackendSpecific . AContainedIn <$> parseOne
        -- jsonb type
        "_has_key" -> guardType [PGJSONB] >> ABackendSpecific . AHasKey <$> parseWithTy (ColumnScalar PGText)
        "$has_key" -> guardType [PGJSONB] >> ABackendSpecific . AHasKey <$> parseWithTy (ColumnScalar PGText)
        "_has_keys_any" -> guardType [PGJSONB] >> ABackendSpecific . AHasKeysAny <$> parseManyWithType (ColumnScalar PGText)
        "$has_keys_any" -> guardType [PGJSONB] >> ABackendSpecific . AHasKeysAny <$> parseManyWithType (ColumnScalar PGText)
        "_has_keys_all" -> guardType [PGJSONB] >> ABackendSpecific . AHasKeysAll <$> parseManyWithType (ColumnScalar PGText)
        "$has_keys_all" -> guardType [PGJSONB] >> ABackendSpecific . AHasKeysAll <$> parseManyWithType (ColumnScalar PGText)
        -- geometry types
        "_st_contains" -> parseGeometryOp ASTContains
        "$st_contains" -> parseGeometryOp ASTContains
        "_st_crosses" -> parseGeometryOp ASTCrosses
        "$st_crosses" -> parseGeometryOp ASTCrosses
        "_st_equals" -> parseGeometryOp ASTEquals
        "$st_equals" -> parseGeometryOp ASTEquals
        "_st_overlaps" -> parseGeometryOp ASTOverlaps
        "$st_overlaps" -> parseGeometryOp ASTOverlaps
        "_st_touches" -> parseGeometryOp ASTTouches
        "$st_touches" -> parseGeometryOp ASTTouches
        "_st_within" -> parseGeometryOp ASTWithin
        "$st_within" -> parseGeometryOp ASTWithin
        -- geometry and geography types
        "_st_intersects" -> parseGeometryOrGeographyOp ASTIntersects
        "$st_intersects" -> parseGeometryOrGeographyOp ASTIntersects
        "_st_3d_intersects" -> parseGeometryOp AST3DIntersects
        "$st_3d_intersects" -> parseGeometryOp AST3DIntersects
        "_st_d_within" -> parseSTDWithinObj
        "$st_d_within" -> parseSTDWithinObj
        "_st_3d_d_within" -> parseST3DDWithinObj
        "$st_3d_d_within" -> parseST3DDWithinObj
        "$ceq" -> parseCeq
        "_ceq" -> parseCeq
        "$cne" -> parseCne
        "_cne" -> parseCne
        "$cneq" -> parseCne
        "_cneq" -> parseCne
        "$cgt" -> parseCgt
        "_cgt" -> parseCgt
        "$clt" -> parseClt
        "_clt" -> parseClt
        "$cgte" -> parseCgte
        "_cgte" -> parseCgte
        "$clte" -> parseClte
        "_clte" -> parseClte
        -- ltree types
        "_ancestor" -> guardType [PGLtree] >> ABackendSpecific . AAncestor <$> parseOne
        "$ancestor" -> guardType [PGLtree] >> ABackendSpecific . AAncestor <$> parseOne
        "_ancestor_any" -> guardType [PGLtree] >> ABackendSpecific . AAncestorAny <$> parseManyWithType (ColumnScalar PGLtree)
        "$ancestor_any" -> guardType [PGLtree] >> ABackendSpecific . AAncestorAny <$> parseManyWithType (ColumnScalar PGLtree)
        "_descendant" -> guardType [PGLtree] >> ABackendSpecific . ADescendant <$> parseOne
        "$descendant" -> guardType [PGLtree] >> ABackendSpecific . ADescendant <$> parseOne
        "_descendant_any" -> guardType [PGLtree] >> ABackendSpecific . ADescendantAny <$> parseManyWithType (ColumnScalar PGLtree)
        "$descendant_any" -> guardType [PGLtree] >> ABackendSpecific . ADescendantAny <$> parseManyWithType (ColumnScalar PGLtree)
        "_matches" -> guardType [PGLtree] >> ABackendSpecific . AMatches <$> parseWithTy (ColumnScalar PGLquery)
        "$matches" -> guardType [PGLtree] >> ABackendSpecific . AMatches <$> parseWithTy (ColumnScalar PGLquery)
        "_matches_any" -> guardType [PGLtree] >> ABackendSpecific . AMatchesAny <$> parseManyWithType (ColumnScalar PGLquery)
        "$matches_any" -> guardType [PGLtree] >> ABackendSpecific . AMatchesAny <$> parseManyWithType (ColumnScalar PGLquery)
        "_matches_fulltext" -> guardType [PGLtree] >> ABackendSpecific . AMatchesFulltext <$> parseWithTy (ColumnScalar PGLtxtquery)
        "$matches_fulltext" -> guardType [PGLtree] >> ABackendSpecific . AMatchesFulltext <$> parseWithTy (ColumnScalar PGLtxtquery)
        x -> throw400 UnexpectedPayload $ "Unknown operator: " <> x
      where
        colTy = columnReferenceType column
        colNonNullable = case fromMaybe True $ columnReferenceNullable column of
          True -> NullableComparison
          False -> NonNullableComparison

        parseIsNull = bool ANISNOTNULL ANISNULL <$> parseVal -- is null
        parseEq = AEQ colNonNullable <$> parseOne -- equals
        parseNe = ANE colNonNullable <$> parseOne -- <>
        parseIn = AIN <$> parseManyWithType colTy -- in an array
        parseNin = ANIN <$> parseManyWithType colTy -- not in an array
        parseGt = AGT <$> parseOne -- >
        parseLt = ALT <$> parseOne -- <
        parseGte = AGTE <$> parseOne -- >=
        parseLte = ALTE <$> parseOne -- <=
        parseCeq = CEQ <$> decodeAndValidateRhsCol val
        parseCne = CNE <$> decodeAndValidateRhsCol val
        parseCgt = CGT <$> decodeAndValidateRhsCol val
        parseClt = CLT <$> decodeAndValidateRhsCol val
        parseCgte = CGTE <$> decodeAndValidateRhsCol val
        parseClte = CLTE <$> decodeAndValidateRhsCol val

        parseLike = guardType stringTypes >> ALIKE <$> parseOne
        parseNlike = guardType stringTypes >> ANLIKE <$> parseOne
        parseIlike = guardType stringTypes >> ABackendSpecific . AILIKE <$> parseOne
        parseNilike = guardType stringTypes >> ABackendSpecific . ANILIKE <$> parseOne

        parseRegex = guardType stringTypes >> ABackendSpecific . AREGEX <$> parseOne
        parseIRegex = guardType stringTypes >> ABackendSpecific . AIREGEX <$> parseOne
        parseNRegex = guardType stringTypes >> ABackendSpecific . ANREGEX <$> parseOne
        parseNIRegex = guardType stringTypes >> ABackendSpecific . ANIREGEX <$> parseOne
        parseSimilar = guardType stringTypes >> ABackendSpecific . ASIMILAR <$> parseOne
        parseNsimilar = guardType stringTypes >> ABackendSpecific . ANSIMILAR <$> parseOne

        parseCast = do
          castOperations <- parseVal
          parsedCastOperations <-
            forM (HashMap.toList castOperations) $ \(targetTypeName, castedComparisons) -> do
              let targetType = textToPGScalarType targetTypeName
                  castedColumn = ColumnReferenceCast column (ColumnScalar targetType)
              checkValidCast targetType
              parsedCastedComparisons <-
                withPathK targetTypeName
                  $ parseOperations castedColumn castedComparisons
              return (targetType, parsedCastedComparisons)
          return . ACast $ HashMap.fromList parsedCastOperations

        checkValidCast targetType = case (colTy, targetType) of
          (ColumnScalar PGGeometry, PGGeography) -> return ()
          (ColumnScalar PGGeography, PGGeometry) -> return ()
          (ColumnScalar PGJSONB, PGText) -> return ()
          _ ->
            throw400 UnexpectedPayload
              $ "cannot cast column of type "
              <> colTy
              <<> " to type "
              <>> targetType

        parseGeometryOp f =
          guardType [PGGeometry] >> ABackendSpecific . f <$> parseOneNoSess colTy val
        parseGeometryOrGeographyOp f =
          guardType geoTypes >> ABackendSpecific . f <$> parseOneNoSess colTy val

        parseSTDWithinObj =
          ABackendSpecific <$> case colTy of
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
            _ -> throwError $ invalidTypeMessage (dquoteList [PGGeometry, PGGeography])

        decodeAndValidateRhsCol :: Value -> m (RootOrCurrentColumn ('Postgres pgKind))
        decodeAndValidateRhsCol v = case v of
          String _ -> go IsCurrent fim v
          Array path -> case toList path of
            [] -> throw400 Unexpected "path cannot be empty"
            [col] -> go IsCurrent fim col
            [String "$", col] -> do
              go IsRoot rootFieldInfoMap col
            _ -> throw400 NotSupported "Relationship references are not supported in column comparison RHS"
          _ -> throw400 Unexpected "a boolean expression JSON must be either a string or an array"
          where
            go rootInfo fieldsInfoMap columnValue = do
              colName <- decodeValue columnValue
              colInfo <- validateRhsCol fieldsInfoMap colName
              pure $ RootOrCurrentColumn rootInfo colInfo

        parseST3DDWithinObj =
          ABackendSpecific <$> do
            guardType [PGGeometry]
            DWithinGeomOp distVal fromVal <- parseVal
            dist <- withPathK "distance" $ parseOneNoSess (ColumnScalar PGFloat) distVal
            from <- withPathK "from" $ parseOneNoSess colTy fromVal
            return $ AST3DDWithinGeom $ DWithinGeomOp dist from

        validateRhsCol fieldInfoMap rhsCol = do
          rhsType <- askColumnType fieldInfoMap rhsCol "column operators can only compare postgres columns"
          when (colTy /= rhsType)
            $ throw400 UnexpectedPayload
            $ "incompatible column types: "
            <> column
            <<> " has type "
            <> colTy
            <<> ", but "
            <> rhsCol
            <<> " has type "
            <>> rhsType
          pure rhsCol

        parseWithTy ty = rhsParser (CollectableTypeScalar ty) val

        -- parse one with the column's type
        parseOne = parseWithTy colTy
        parseOneNoSess ty = rhsParser (CollectableTypeScalar ty)

        parseManyWithType ty = rhsParser (CollectableTypeArray ty) val

        guardTypeToArrayOrJsonb = guardTypeWhere isArrayOrJsonb "an array or JSONB"
          where
            isArrayOrJsonb = \case
              PGArray _ -> True
              PGJSONB -> True
              _ -> False

        guardType validTypes = guardTypeWhere (`elem` validTypes) (dquoteList validTypes)

        guardTypeWhere isValid messageOnError =
          unless (isScalarColumnWhere isValid colTy)
            $ throwError
            $ invalidTypeMessage messageOnError

        invalidTypeMessage expectedColumnType =
          err400 UnexpectedPayload
            $ " is of type "
            <> colTy
            <<> "; this operator works only on columns of type "
            <> expectedColumnType

        parseVal :: (FromJSON a) => m a
        parseVal = decodeValue val

buildComputedFieldBooleanExp ::
  forall pgKind m v.
  ( MonadError QErr m,
    Backend ('Postgres pgKind),
    TableCoreInfoRM ('Postgres pgKind) m
  ) =>
  BoolExpResolver ('Postgres pgKind) m v ->
  BoolExpRHSParser ('Postgres pgKind) m v ->
  FieldInfoMap (FieldInfo ('Postgres pgKind)) ->
  FieldInfoMap (FieldInfo ('Postgres pgKind)) ->
  ComputedFieldInfo ('Postgres pgKind) ->
  Value ->
  m (AnnComputedFieldBoolExp ('Postgres pgKind) v)
buildComputedFieldBooleanExp boolExpResolver rhsParser rootFieldInfoMap colInfoMap ComputedFieldInfo {..} colVal = do
  let ComputedFieldFunction {..} = _cfiFunction
  case toList _cffInputArgs of
    [] -> do
      let hasuraSession = _berpSessionValue rhsParser
          computedFieldFunctionArgs = flip FunctionArgsExp mempty $ PG.fromComputedFieldImplicitArguments hasuraSession _cffComputedFieldImplicitArgs
      AnnComputedFieldBoolExp _cfiXComputedFieldInfo _cfiName _cffName computedFieldFunctionArgs
        <$> case _cfiReturnType of
          CFRScalar scalarType ->
            CFBEScalar NoRedaction
              <$> parseBoolExpOperations (_berpValueParser rhsParser) rootFieldInfoMap colInfoMap (ColumnReferenceComputedField _cfiName scalarType) colVal
          CFRSetofTable table -> do
            tableBoolExp <- decodeValue colVal
            tableFieldInfoMap <- askFieldInfoMapSource table
            annTableBoolExp <- (getBoolExpResolver boolExpResolver) rhsParser tableFieldInfoMap tableFieldInfoMap $ unBoolExp tableBoolExp
            pure $ CFBETable table annTableBoolExp
    _ ->
      throw400
        UnexpectedPayload
        "Computed columns with input arguments can not be part of the where clause"
