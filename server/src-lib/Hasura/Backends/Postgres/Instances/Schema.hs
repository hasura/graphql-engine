{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.Postgres.Instances.Schema () where

import           Hasura.Prelude

import qualified Data.Aeson                             as J
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashMap.Strict.Extended           as M
import qualified Data.HashMap.Strict.InsOrd.Extended    as OMap
import qualified Data.List.NonEmpty                     as NE
import qualified Data.Text                              as T
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G

import           Data.Has
import           Data.Parser.JSONPath
import           Data.Text.Extended

import qualified Hasura.GraphQL.Parser                  as P
import qualified Hasura.GraphQL.Schema.Backend          as BS
import qualified Hasura.GraphQL.Schema.Build            as GSB
import qualified Hasura.RQL.IR.Select                   as IR
import qualified Hasura.RQL.IR.Update                   as IR
import qualified Hasura.SQL.AnyBackend                  as AB

import           Hasura.Backends.Postgres.SQL.DML       as PG hiding (CountType)
import           Hasura.Backends.Postgres.SQL.Types     as PG hiding (FunctionName, TableName)
import           Hasura.Backends.Postgres.SQL.Value     as PG
import           Hasura.Backends.Postgres.Types.BoolExp
import           Hasura.Backends.Postgres.Types.Column
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Parser                  hiding (EnumValueInfo, field)
import           Hasura.GraphQL.Parser.Internal.Parser  hiding (field)
import           Hasura.GraphQL.Schema.Backend          (BackendSchema, ComparisonExp,
                                                         MonadBuildSchema)
import           Hasura.GraphQL.Schema.BoolExp
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.Select
import           Hasura.GraphQL.Schema.Table
import           Hasura.RQL.Types
import           Hasura.SQL.Types


----------------------------------------------------------------
-- BackendSchema instance

instance BackendSchema 'Postgres where
  -- top level parsers
  buildTableQueryFields          = GSB.buildTableQueryFields
  buildTableRelayQueryFields     = buildTableRelayQueryFields
  buildTableInsertMutationFields = GSB.buildTableInsertMutationFields
  buildTableUpdateMutationFields = GSB.buildTableUpdateMutationFields
  buildTableDeleteMutationFields = GSB.buildTableDeleteMutationFields
  buildFunctionQueryFields       = GSB.buildFunctionQueryFields
  buildFunctionRelayQueryFields  = buildFunctionRelayQueryFields
  buildFunctionMutationFields    = GSB.buildFunctionMutationFields
  -- backend extensions
  relayExtension    = const $ Just ()
  nodesAggExtension = const $ Just ()
  -- indivdual components
  columnParser              = columnParser
  jsonPathArg               = jsonPathArg
  orderByOperators          = orderByOperators
  comparisonExps            = comparisonExps
  updateOperators           = updateOperators
  offsetParser              = offsetParser
  mkCountType               = mkCountType
  aggregateOrderByCountType = PG.PGInteger
  computedField             = computedFieldPG
  node                      = nodePG
  tableDistinctOn           = tableDistinctOn
  remoteRelationshipField   = remoteRelationshipFieldPG
  -- SQL literals
  columnDefaultValue = const PG.columnDefaultValue


----------------------------------------------------------------
-- Top level parsers

buildTableRelayQueryFields
  :: MonadBuildSchema 'Postgres r m n
  => SourceName
  -> SourceConfig 'Postgres
  -> TableName    'Postgres
  -> TableInfo    'Postgres
  -> G.Name
  -> NESeq (ColumnInfo 'Postgres)
  -> SelPermInfo  'Postgres
  -> m (Maybe (FieldParser n (QueryRootField UnpreparedValue)))
buildTableRelayQueryFields sourceName sourceInfo tableName tableInfo gqlName pkeyColumns selPerms = do
  let
    mkRF = RFDB sourceName
             . AB.mkAnyBackend
             . SourceConfigWith sourceInfo
             . QDBR
    fieldName = gqlName <> $$(G.litName "_connection")
    fieldDesc = Just $ G.Description $ "fetch data from the table: " <>> tableName
  optionalFieldParser (mkRF . QDBConnection) $ selectTableConnection tableName fieldName fieldDesc pkeyColumns selPerms

buildFunctionRelayQueryFields
  :: MonadBuildSchema 'Postgres r m n
  => SourceName
  -> SourceConfig 'Postgres
  -> FunctionName 'Postgres
  -> FunctionInfo 'Postgres
  -> TableName    'Postgres
  -> NESeq (ColumnInfo 'Postgres)
  -> SelPermInfo  'Postgres
  -> m (Maybe (FieldParser n (QueryRootField UnpreparedValue)))
buildFunctionRelayQueryFields sourceName sourceInfo functionName functionInfo tableName pkeyColumns selPerms = do
  funcName <- functionGraphQLName @'Postgres functionName `onLeft` throwError
  let
    mkRF = RFDB sourceName
             . AB.mkAnyBackend
             . SourceConfigWith sourceInfo
             . QDBR
    fieldName = funcName <> $$(G.litName "_connection")
    fieldDesc = Just $ G.Description $ "execute function " <> functionName <<> " which returns " <>> tableName
  optionalFieldParser (mkRF . QDBConnection) $ selectFunctionConnection functionInfo fieldName fieldDesc pkeyColumns selPerms


----------------------------------------------------------------
-- Individual components

columnParser
  :: (MonadSchema n m, MonadError QErr m)
  => ColumnType 'Postgres
  -> G.Nullability
  -> m (Parser 'Both n (Opaque (ColumnValue 'Postgres)))
columnParser columnType (G.Nullability isNullable) =
  -- TODO(PDV): It might be worth memoizing this function even though it isn’t
  -- recursive simply for performance reasons, since it’s likely to be hammered
  -- during schema generation. Need to profile to see whether or not it’s a win.
  opaque . fmap (ColumnValue columnType) <$> case columnType of
    ColumnScalar scalarType -> possiblyNullable scalarType <$> case scalarType of
      PGInteger -> pure (PGValInteger <$> P.int)
      PGBoolean -> pure (PGValBoolean <$> P.boolean)
      PGFloat   -> pure (PGValDouble  <$> P.float)
      PGText    -> pure (PGValText    <$> P.string)
      PGVarchar -> pure (PGValVarchar <$> P.string)
      PGJSON    -> pure (PGValJSON  . Q.JSON  <$> P.json)
      PGJSONB   -> pure (PGValJSONB . Q.JSONB <$> P.jsonb)
      -- For all other scalars, we convert the value to JSON and use the
      -- FromJSON instance. The major upside is that this avoids having to write
      -- new parsers for each custom type: if the JSON parser is sound, so will
      -- this one, and it avoids the risk of having two separate ways of parsing
      -- a value in the codebase, which could lead to inconsistencies.
      _  -> do
        name <- mkScalarTypeName scalarType
        let schemaType = P.NonNullable $ P.TNamed $ P.mkDefinition name Nothing P.TIScalar
        pure $ Parser
          { pType = schemaType
          , pParser =
              valueToJSON (P.toGraphQLType schemaType) >=>
              either (parseErrorWith ParseFailed . qeError) pure . runAesonParser (parsePGValue scalarType)
          }
    ColumnEnumReference (EnumReference tableName enumValues) ->
      case nonEmpty (Map.toList enumValues) of
        Just enumValuesList -> do
          name <- qualifiedObjectToName tableName <&> (<> $$(G.litName "_enum"))
          pure $ possiblyNullable PGText $ P.enum name Nothing (mkEnumValue <$> enumValuesList)
        Nothing -> throw400 ValidationFailed "empty enum values"
  where
    -- Sadly, this combinator is not sound in general, so we can’t export it
    -- for general-purpose use. If we did, someone could write this:
    --
    --   mkParameter <$> opaque do
    --     n <- int
    --     pure (mkIntColumnValue (n + 1))
    --
    -- Now we’d end up with a UVParameter that has a variable in it, so we’d
    -- parameterize over it. But when we’d reuse the plan, we wouldn’t know to
    -- increment the value by 1, so we’d use the wrong value!
    --
    -- We could theoretically solve this by retaining a reference to the parser
    -- itself and re-parsing each new value, using the saved parser, which
    -- would admittedly be neat. But it’s more complicated, and it isn’t clear
    -- that it would actually be useful, so for now we don’t support it.
    opaque :: MonadParse m => Parser 'Both m a -> Parser 'Both m (Opaque a)
    opaque parser = parser
      { pParser = \case
          P.GraphQLValue (G.VVariable var@Variable{ vInfo, vValue }) -> do
            typeCheck False (P.toGraphQLType $ pType parser) var
            P.mkOpaque (Just vInfo) <$> pParser parser (absurd <$> vValue)
          value -> P.mkOpaque Nothing <$> pParser parser value
      }
    possiblyNullable scalarType
      | isNullable = fmap (fromMaybe $ PGNull scalarType) . P.nullable
      | otherwise  = id
    mkEnumValue :: (EnumValue, EnumValueInfo) -> (P.Definition P.EnumValueInfo, PGScalarValue)
    mkEnumValue (EnumValue value, EnumValueInfo description) =
      ( P.mkDefinition value (G.Description <$> description) P.EnumValueInfo
      , PGValText $ G.unName value
      )

jsonPathArg
  :: MonadParse n
  => ColumnType 'Postgres
  -> InputFieldsParser n (Maybe (IR.ColumnOp 'Postgres))
jsonPathArg columnType
  | isScalarColumnWhere PG.isJSONType columnType =
      P.fieldOptional fieldName description P.string `P.bindFields` fmap join . traverse toColExp
  | otherwise = pure Nothing
  where
    fieldName = $$(G.litName "path")
    description = Just "JSON select path"
    toColExp textValue = case parseJSONPath textValue of
      Left err     -> parseError $ T.pack $ "parse json path error: " ++ err
      Right []     -> pure Nothing
      Right jPaths -> pure $ Just $ IR.ColumnOp PG.jsonbPathOp $ PG.SEArray $ map elToColExp jPaths
    elToColExp (Key k)   = PG.SELit k
    elToColExp (Index i) = PG.SELit $ tshow i

orderByOperators
  :: NonEmpty (Definition P.EnumValueInfo, (BasicOrderType 'Postgres, NullsOrderType 'Postgres))
orderByOperators = NE.fromList
  [ ( define $$(G.litName "asc") "in ascending order, nulls last"
    , (PG.OTAsc, PG.NLast)
    )
  , ( define $$(G.litName "asc_nulls_first") "in ascending order, nulls first"
    , (PG.OTAsc, PG.NFirst)
    )
  , ( define $$(G.litName "asc_nulls_last") "in ascending order, nulls last"
    , (PG.OTAsc, PG.NLast)
    )
  , ( define $$(G.litName "desc") "in descending order, nulls first"
    , (PG.OTDesc, PG.NFirst)
    )
  , ( define $$(G.litName "desc_nulls_first") "in descending order, nulls first"
    , (PG.OTDesc, PG.NFirst)
    )
  , ( define $$(G.litName "desc_nulls_last") "in descending order, nulls last"
    , (PG.OTDesc, PG.NLast)
    )
  ]
  where
    define name desc = P.mkDefinition name (Just desc) P.EnumValueInfo

comparisonExps
  :: forall m n r
   . (BackendSchema 'Postgres
     , MonadSchema n m
     , MonadError QErr m
     , MonadReader r m
     , Has QueryContext r
     )
  => ColumnType 'Postgres -> m (Parser 'Input n [ComparisonExp 'Postgres])
comparisonExps = P.memoize 'comparisonExps \columnType -> do
  -- see Note [Columns in comparison expression are never nullable]
  collapseIfNull <- asks $ qcDangerousBooleanCollapse . getter

  -- parsers used for comparison arguments
  geogInputParser    <- geographyWithinDistanceInput
  geomInputParser    <- geometryWithinDistanceInput
  ignInputParser     <- intersectsGeomNbandInput
  ingInputParser     <- intersectsNbandGeomInput
  typedParser        <- columnParser columnType (G.Nullability False)
  nullableTextParser <- columnParser (ColumnScalar PGText) (G.Nullability True)
  textParser         <- columnParser (ColumnScalar PGText) (G.Nullability False)
  -- `lquery` represents a regular-expression-like pattern for matching `ltree` values.
  lqueryParser       <- columnParser (ColumnScalar PGLquery) (G.Nullability False)
  -- `ltxtquery` represents a full-text-search-like pattern for matching `ltree` values.
  ltxtqueryParser    <- columnParser (ColumnScalar PGLtxtquery) (G.Nullability False)
  maybeCastParser    <- castExp columnType
  let name = P.getName typedParser <> $$(G.litName "_comparison_exp")
      desc = G.Description $ "Boolean expression to compare columns of type "
        <>  P.getName typedParser
        <<> ". All fields are combined with logical 'AND'."
      textListParser   = P.list textParser  `P.bind` traverse P.openOpaque
      columnListParser = P.list typedParser `P.bind` traverse P.openOpaque

  pure $ P.object name (Just desc) $ fmap catMaybes $ sequenceA $ concat
    [ flip (maybe []) maybeCastParser $ \castParser ->
      [ P.fieldOptional $$(G.litName "_cast")    Nothing (ACast <$> castParser)
      ]

    -- Common ops for all types
    , equalityOperators
        collapseIfNull
        (mkParameter <$> typedParser)
        (mkListLiteral columnType <$> columnListParser)

    -- Comparison ops for non Raster types
    , guard (isScalarColumnWhere (/= PGRaster) columnType) *>
      comparisonOperators
        collapseIfNull
        (mkParameter <$> typedParser)

    -- Ops for Raster types
    , guard (isScalarColumnWhere (== PGRaster) columnType) *>
      [ P.fieldOptional $$(G.litName "_st_intersects_rast")
        Nothing
        (ABackendSpecific . ASTIntersectsRast . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_st_intersects_nband_geom")
        Nothing
        (ABackendSpecific . ASTIntersectsNbandGeom <$> ingInputParser)
      , P.fieldOptional $$(G.litName "_st_intersects_geom_nband")
        Nothing
        (ABackendSpecific . ASTIntersectsGeomNband <$> ignInputParser)
      ]

    -- Ops for String like types
    , guard (isScalarColumnWhere isStringType columnType) *>
      [ P.fieldOptional $$(G.litName "_like")
        (Just "does the column match the given pattern")
        (ALIKE     . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_nlike")
        (Just "does the column NOT match the given pattern")
        (ANLIKE    . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_ilike")
        (Just "does the column match the given case-insensitive pattern")
        (ABackendSpecific . AILIKE . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_nilike")
        (Just "does the column NOT match the given case-insensitive pattern")
        (ABackendSpecific . ANILIKE . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_similar")
        (Just "does the column match the given SQL regular expression")
        (ABackendSpecific . ASIMILAR  . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_nsimilar")
        (Just "does the column NOT match the given SQL regular expression")
        (ABackendSpecific . ANSIMILAR . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_regex")
        (Just "does the column match the given POSIX regular expression, case sensitive")
        (ABackendSpecific . AREGEX  . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_iregex")
        (Just "does the column match the given POSIX regular expression, case insensitive")
        (ABackendSpecific . AIREGEX . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_nregex")
        (Just "does the column NOT match the given POSIX regular expression, case sensitive")
        (ABackendSpecific . ANREGEX  . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_niregex")
        (Just "does the column NOT match the given POSIX regular expression, case insensitive")
        (ABackendSpecific . ANIREGEX . mkParameter <$> typedParser)
      ]

    -- Ops for JSONB type
    , guard (isScalarColumnWhere (== PGJSONB) columnType) *>
      [ P.fieldOptional $$(G.litName "_contains")
        (Just "does the column contain the given json value at the top level")
        (ABackendSpecific . AContains    . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_contained_in")
        (Just "is the column contained in the given json value")
        (ABackendSpecific . AContainedIn . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_has_key")
        (Just "does the string exist as a top-level key in the column")
        (ABackendSpecific . AHasKey      . mkParameter <$> nullableTextParser)
      , P.fieldOptional $$(G.litName "_has_keys_any")
        (Just "do any of these strings exist as top-level keys in the column")
        (ABackendSpecific . AHasKeysAny . mkListLiteral (ColumnScalar PGText) <$> textListParser)
      , P.fieldOptional $$(G.litName "_has_keys_all")
        (Just "do all of these strings exist as top-level keys in the column")
        (ABackendSpecific . AHasKeysAll . mkListLiteral (ColumnScalar PGText) <$> textListParser)
      ]

    -- Ops for Geography type
    , guard (isScalarColumnWhere (== PGGeography) columnType) *>
      [ P.fieldOptional $$(G.litName "_st_intersects")
        (Just "does the column spatially intersect the given geography value")
        (ABackendSpecific . ASTIntersects . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_st_d_within")
        (Just "is the column within a given distance from the given geography value")
        (ABackendSpecific . ASTDWithinGeog <$> geogInputParser)
      ]

    -- Ops for Geometry type
    , guard (isScalarColumnWhere (== PGGeometry) columnType) *>
      [ P.fieldOptional $$(G.litName "_st_contains")
        (Just "does the column contain the given geometry value")
        (ABackendSpecific . ASTContains   . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_st_crosses")
        (Just "does the column cross the given geometry value")
        (ABackendSpecific . ASTCrosses    . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_st_equals")
        (Just "is the column equal to given geometry value (directionality is ignored)")
        (ABackendSpecific . ASTEquals     . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_st_overlaps")
        (Just "does the column 'spatially overlap' (intersect but not completely contain) the given geometry value")
        (ABackendSpecific . ASTOverlaps   . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_st_touches")
        (Just "does the column have atleast one point in common with the given geometry value")
        (ABackendSpecific . ASTTouches    . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_st_within")
        (Just "is the column contained in the given geometry value")
        (ABackendSpecific . ASTWithin     . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_st_intersects")
        (Just "does the column spatially intersect the given geometry value")
        (ABackendSpecific . ASTIntersects . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_st_3d_intersects")
        (Just "does the column spatially intersect the given geometry value in 3D")
        (ABackendSpecific . AST3DIntersects . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_st_d_within")
        (Just "is the column within a given distance from the given geometry value")
        (ABackendSpecific . ASTDWithinGeom <$> geomInputParser)
      , P.fieldOptional $$(G.litName "_st_3d_d_within")
        (Just "is the column within a given 3D distance from the given geometry value")
        (ABackendSpecific . AST3DDWithinGeom <$> geomInputParser)
      ]

    -- Ops for Ltree type
    , guard (isScalarColumnWhere (== PGLtree) columnType) *>
      [ P.fieldOptional $$(G.litName "_ancestor")
        (Just "is the left argument an ancestor of right (or equal)?")
        (ABackendSpecific . AAncestor        . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_ancestor_any")
        (Just "does array contain an ancestor of `ltree`?")
        (ABackendSpecific . AAncestorAny     . mkListLiteral columnType <$> columnListParser)
      , P.fieldOptional $$(G.litName "_descendant")
        (Just "is the left argument a descendant of right (or equal)?")
        (ABackendSpecific . ADescendant      . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_descendant_any")
        (Just "does array contain a descendant of `ltree`?")
        (ABackendSpecific . ADescendantAny   . mkListLiteral columnType <$> columnListParser)
      , P.fieldOptional $$(G.litName "_matches")
        (Just "does `ltree` match `lquery`?")
        (ABackendSpecific . AMatches         . mkParameter <$> lqueryParser)
      , P.fieldOptional $$(G.litName "_matches_any")
        (Just "does `ltree` match any `lquery` in array?")
        (ABackendSpecific . AMatchesAny      . mkListLiteral (ColumnScalar PGLquery) <$> textListParser)
      , P.fieldOptional $$(G.litName "_matches_fulltext")
        (Just "does `ltree` match `ltxtquery`?")
        (ABackendSpecific . AMatchesFulltext . mkParameter <$> ltxtqueryParser)
      ]
    ]
  where
    mkListLiteral :: ColumnType 'Postgres -> [ColumnValue 'Postgres] -> UnpreparedValue 'Postgres
    mkListLiteral columnType columnValues = P.UVLiteral $ SETyAnn
      (SEArray $ txtEncoder . cvValue <$> columnValues)
      (mkTypeAnn $ CollectableTypeArray $ unsafePGColumnToBackend columnType)

    castExp :: ColumnType 'Postgres -> m (Maybe (Parser 'Input n (CastExp 'Postgres (UnpreparedValue 'Postgres))))
    castExp sourceType = do
      let maybeScalars = case sourceType of
            ColumnScalar PGGeography -> Just (PGGeography, PGGeometry)
            ColumnScalar PGGeometry  -> Just (PGGeometry, PGGeography)
            _                        -> Nothing

      forM maybeScalars $ \(sourceScalar, targetScalar) -> do
        sourceName   <- mkScalarTypeName sourceScalar <&> (<> $$(G.litName "_cast_exp"))
        targetName   <- mkScalarTypeName targetScalar
        targetOpExps <- comparisonExps $ ColumnScalar targetScalar
        let field = P.fieldOptional targetName Nothing $ (targetScalar, ) <$> targetOpExps
        pure $ P.object sourceName Nothing $ M.fromList . maybeToList <$> field

geographyWithinDistanceInput
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => m (Parser 'Input n (DWithinGeogOp (UnpreparedValue 'Postgres)))
geographyWithinDistanceInput = do
  geographyParser <- columnParser (ColumnScalar PGGeography) (G.Nullability False)
  -- FIXME
  -- It doesn't make sense for this value to be nullable; it only is for
  -- backwards compatibility; if an explicit Null value is given, it will be
  -- forwarded to the underlying SQL function, that in turns treat a null value
  -- as an error. We can fix this by rejecting explicit null values, by marking
  -- this field non-nullable in a future release.
  booleanParser   <- columnParser (ColumnScalar PGBoolean)   (G.Nullability True)
  floatParser     <- columnParser (ColumnScalar PGFloat)     (G.Nullability False)
  pure $ P.object $$(G.litName "st_d_within_geography_input") Nothing $
    DWithinGeogOp <$> (mkParameter <$> P.field $$(G.litName "distance") Nothing floatParser)
                  <*> (mkParameter <$> P.field $$(G.litName "from")     Nothing geographyParser)
                  <*> (mkParameter <$> P.fieldWithDefault $$(G.litName "use_spheroid") Nothing (G.VBoolean True) booleanParser)

geometryWithinDistanceInput
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => m (Parser 'Input n (DWithinGeomOp (UnpreparedValue 'Postgres)))
geometryWithinDistanceInput = do
  geometryParser <- columnParser (ColumnScalar PGGeometry) (G.Nullability False)
  floatParser    <- columnParser (ColumnScalar PGFloat)    (G.Nullability False)
  pure $ P.object $$(G.litName "st_d_within_input") Nothing $
    DWithinGeomOp <$> (mkParameter <$> P.field $$(G.litName "distance") Nothing floatParser)
                  <*> (mkParameter <$> P.field $$(G.litName "from")     Nothing geometryParser)

intersectsNbandGeomInput
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => m (Parser 'Input n (STIntersectsNbandGeommin (UnpreparedValue 'Postgres)))
intersectsNbandGeomInput = do
  geometryParser <- columnParser (ColumnScalar PGGeometry) (G.Nullability False)
  integerParser  <- columnParser (ColumnScalar PGInteger)  (G.Nullability False)
  pure $ P.object $$(G.litName "st_intersects_nband_geom_input") Nothing $
    STIntersectsNbandGeommin <$> (mkParameter <$> P.field $$(G.litName "nband")   Nothing integerParser)
                             <*> (mkParameter <$> P.field $$(G.litName "geommin") Nothing geometryParser)

intersectsGeomNbandInput
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => m (Parser 'Input n (STIntersectsGeomminNband (UnpreparedValue 'Postgres)))
intersectsGeomNbandInput = do
  geometryParser <- columnParser (ColumnScalar PGGeometry) (G.Nullability False)
  integerParser  <- columnParser (ColumnScalar PGInteger)  (G.Nullability False)
  pure $ P.object $$(G.litName "st_intersects_geom_nband_input") Nothing $ STIntersectsGeomminNband
    <$> (     mkParameter <$> P.field         $$(G.litName "geommin") Nothing geometryParser)
    <*> (fmap mkParameter <$> P.fieldOptional $$(G.litName "nband")   Nothing integerParser)

offsetParser :: MonadParse n => Parser 'Both n (SQLExpression 'Postgres)
offsetParser = PG.txtEncoder <$> Parser
  { pType = fakeBigIntSchemaType
  , pParser = peelVariable (Just $ P.toGraphQLType fakeBigIntSchemaType) >=> \case
      P.GraphQLValue (G.VInt    i) -> PG.PGValBigInt <$> convertWith PG.scientificToInteger (fromInteger i)
      P.JSONValue    (J.Number  n) -> PG.PGValBigInt <$> convertWith PG.scientificToInteger n
      P.GraphQLValue (G.VString s) -> pure $ PG.PGValUnknown s
      P.JSONValue    (J.String  s) -> pure $ PG.PGValUnknown s
      v ->  typeMismatch $$(G.litName "Int") "a 32-bit integer, or a 64-bit integer represented as a string" v
  }
  where
    fakeBigIntSchemaType = P.NonNullable $ P.TNamed $ P.mkDefinition $$(G.litName "Int") Nothing P.TIScalar
    convertWith f = either (parseErrorWith ParseFailed . qeError) pure . runAesonParser f

mkCountType :: Maybe Bool -> Maybe [Column 'Postgres] -> CountType 'Postgres
mkCountType _           Nothing     = PG.CTStar
mkCountType (Just True) (Just cols) = PG.CTDistinct cols
mkCountType _           (Just cols) = PG.CTSimple cols

-- | Argument to distinct select on columns returned from table selection
-- > distinct_on: [table_select_column!]
tableDistinctOn
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => TableName 'Postgres
  -> SelPermInfo 'Postgres
  -> m (InputFieldsParser n (Maybe (XDistinct 'Postgres, NonEmpty (Column 'Postgres))))
tableDistinctOn table selectPermissions = do
  columnsEnum   <- tableSelectColumnsEnum table selectPermissions
  pure $ do
    maybeDistinctOnColumns <- join.join <$> for columnsEnum
      (P.fieldOptional distinctOnName distinctOnDesc . P.nullable . P.list)
    pure $ maybeDistinctOnColumns >>= NE.nonEmpty <&> ((),)
  where
    distinctOnName = $$(G.litName "distinct_on")
    distinctOnDesc = Just $ G.Description "distinct select on columns"

-- | Various update operators
updateOperators
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m)
  => QualifiedTable         -- ^ qualified name of the table
  -> UpdPermInfo 'Postgres  -- ^ update permissions of the table
  -> m (Maybe (InputFieldsParser n [(Column 'Postgres, IR.UpdOpExpG (UnpreparedValue 'Postgres))]))
updateOperators table updatePermissions = do
  tableGQLName <- getTableGQLName @'Postgres table
  columns      <- tableUpdateColumns table updatePermissions
  let numericCols = onlyNumCols   columns
      jsonCols    = onlyJSONBCols columns
  parsers <- catMaybes <$> sequenceA
    [ updateOperator tableGQLName $$(G.litName "_set")
        typedParser IR.UpdSet columns
        "sets the columns of the filtered rows to the given values"
        (G.Description $ "input type for updating data in table " <>> table)

    , updateOperator tableGQLName $$(G.litName "_inc")
        typedParser IR.UpdInc numericCols
        "increments the numeric columns with given value of the filtered values"
        (G.Description $"input type for incrementing numeric columns in table " <>> table)

    , let desc = "prepend existing jsonb value of filtered columns with new jsonb value"
      in updateOperator tableGQLName $$(G.litName "_prepend")
         typedParser IR.UpdPrepend jsonCols desc desc

    , let desc = "append existing jsonb value of filtered columns with new jsonb value"
      in updateOperator tableGQLName $$(G.litName "_append")
         typedParser IR.UpdAppend jsonCols desc desc

    , let desc = "delete key/value pair or string element. key/value pairs are matched based on their key value"
      in updateOperator tableGQLName $$(G.litName "_delete_key")
         nullableTextParser IR.UpdDeleteKey jsonCols desc desc

    , let desc = "delete the array element with specified index (negative integers count from the end). "
                 <> "throws an error if top level container is not an array"
      in updateOperator tableGQLName $$(G.litName "_delete_elem")
         nonNullableIntParser IR.UpdDeleteElem jsonCols desc desc

    , let desc = "delete the field or element with specified path (for JSON arrays, negative integers count from the end)"
      in updateOperator tableGQLName $$(G.litName "_delete_at_path")
         (fmap P.list . nonNullableTextParser) IR.UpdDeleteAtPath jsonCols desc desc
    ]
  whenMaybe (not $ null parsers) do
    let allowedOperators = fst <$> parsers
    pure $ fmap catMaybes (sequenceA $ snd <$> parsers)
      `P.bindFields` \opExps -> do
        -- there needs to be at least one operator in the update, even if it is empty
        let presetColumns = Map.toList $ IR.UpdSet . partialSQLExpToUnpreparedValue <$> upiSet updatePermissions
        when (null opExps && null presetColumns) $ parseError $
          "at least any one of " <> commaSeparated allowedOperators <> " is expected"

        -- no column should appear twice
        let flattenedExps = concat opExps
            erroneousExps = OMap.filter ((>1) . length) $ OMap.groupTuples flattenedExps
        unless (OMap.null erroneousExps) $ parseError $
          "column found in multiple operators; " <>
          T.intercalate ". " [ dquote columnName <> " in " <> commaSeparated (IR.updateOperatorText <$> ops)
                             | (columnName, ops) <- OMap.toList erroneousExps
                             ]

        pure $ presetColumns <> flattenedExps
  where
    typedParser columnInfo  = fmap P.mkParameter <$> columnParser (pgiType columnInfo) (G.Nullability $ pgiIsNullable columnInfo)
    nonNullableTextParser _ = fmap P.mkParameter <$> columnParser (ColumnScalar PGText)    (G.Nullability False)
    nullableTextParser    _ = fmap P.mkParameter <$> columnParser (ColumnScalar PGText)    (G.Nullability True)
    nonNullableIntParser  _ = fmap P.mkParameter <$> columnParser (ColumnScalar PGInteger) (G.Nullability False)

    onlyJSONBCols = filter (isScalarColumnWhere (== PGJSONB) . pgiType)

    updateOperator
      :: G.Name
      -> G.Name
      -> (ColumnInfo b -> m (Parser 'Both n a))
      -> (a -> IR.UpdOpExpG (UnpreparedValue b))
      -> [ColumnInfo b]
      -> G.Description
      -> G.Description
      -> m (Maybe (Text, InputFieldsParser n (Maybe [(Column b, IR.UpdOpExpG (UnpreparedValue b))])))
    updateOperator tableGQLName opName mkParser updOpExp columns opDesc objDesc =
      whenMaybe (not $ null columns) do
        fields <- for columns \columnInfo -> do
          let fieldName = pgiName columnInfo
              fieldDesc = pgiDescription columnInfo
          fieldParser <- mkParser columnInfo
          pure $ P.fieldOptional fieldName fieldDesc fieldParser
            `mapField` \value -> (pgiColumn columnInfo, updOpExp value)
        let objName = tableGQLName <> opName <> $$(G.litName "_input")
        pure $ (G.unName opName,)
             $ P.fieldOptional opName (Just opDesc)
             $ P.object objName (Just objDesc)
             $ catMaybes <$> sequenceA fields
