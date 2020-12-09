-- | Postgres-specific schema combinators
module Hasura.GraphQL.Schema.Postgres
  ( columnParser
  , jsonPathArg
  , getTableGQLName
  , orderByOperators
  , comparisonExps
  , offsetParser
  , mkCountType
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                            as J
import qualified Data.HashMap.Strict                   as Map
import qualified Data.List.NonEmpty                    as NE
import qualified Data.Text                             as T
import qualified Database.PG.Query                     as Q
import qualified Language.GraphQL.Draft.Syntax         as G

import           Data.Parser.JSONPath

import qualified Data.HashMap.Strict.Extended          as M
import qualified Hasura.GraphQL.Parser                 as P
import qualified Hasura.RQL.IR.Select                  as IR

import           Data.Text.Extended
import           Hasura.Backends.Postgres.SQL.DML      as PG hiding (CountType)
import           Hasura.Backends.Postgres.SQL.Types    as PG hiding (TableName)
import           Hasura.Backends.Postgres.SQL.Value    as PG
import           Hasura.GraphQL.Parser                 (Definition, InputFieldsParser, Kind (..),
                                                        Opaque, Parser, UnpreparedValue (..),
                                                        Variable (..), mkParameter)
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Parser.Internal.Parser (Parser (..), peelVariable, typeCheck,
                                                        typeMismatch, valueToJSON)
import           Hasura.GraphQL.Schema.Backend         (BackendSchema, ComparisonExp)
import           Hasura.RQL.Types
import           Hasura.SQL.Types


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
      -- FromJSON instance. The major upside is that this avoids having to wri`te
      -- a new parsers for each custom type: if the JSON parser is sound, so
      -- will this one, and it avoids the risk of having two separate ways of
      -- parsing a value in the codebase, which could lead to inconsistencies.
      _  -> do
        name <- P.mkScalarTypeName scalarType
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
    elToColExp (Index i) = PG.SELit $ T.pack (show i)

getTableGQLName
  :: MonadTableInfo 'Postgres r m
  => TableName 'Postgres
  -> m G.Name
getTableGQLName table = do
  tableInfo <- askTableInfo @'Postgres table
  let tableCustomName = _tcCustomName . _tciCustomConfig . _tiCoreInfo $ tableInfo
  tableCustomName `onNothing` qualifiedObjectToName table

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
  :: forall m n. (BackendSchema 'Postgres, MonadSchema n m, MonadError QErr m)
  => ColumnType 'Postgres -> m (Parser 'Input n [ComparisonExp 'Postgres])
comparisonExps = P.memoize 'comparisonExps \columnType -> do
  geogInputParser <- geographyWithinDistanceInput
  geomInputParser <- geometryWithinDistanceInput
  ignInputParser  <- intersectsGeomNbandInput
  ingInputParser  <- intersectsNbandGeomInput
  -- see Note [Columns in comparison expression are never nullable]
  typedParser        <- columnParser columnType (G.Nullability False)
  nullableTextParser <- columnParser (ColumnScalar PGText) (G.Nullability True)
  textParser         <- columnParser (ColumnScalar PGText) (G.Nullability False)
  maybeCastParser    <- castExp columnType
  let name = P.getName typedParser <> $$(G.litName "_comparison_exp")
      desc = G.Description $ "Boolean expression to compare columns of type "
        <>  P.getName typedParser
        <<> ". All fields are combined with logical 'AND'."
      textListParser = P.list textParser `P.bind` traverse P.openOpaque
      columnListParser = P.list typedParser `P.bind` traverse P.openOpaque
  pure $ P.object name (Just desc) $ fmap catMaybes $ sequenceA $ concat
    [ flip (maybe []) maybeCastParser $ \castParser ->
      [ P.fieldOptional $$(G.litName "_cast")    Nothing (ACast <$> castParser)
      ]
    -- Common ops for all types
    , [ P.fieldOptional $$(G.litName "_is_null") Nothing (bool ANISNOTNULL ANISNULL <$> P.boolean)
      , P.fieldOptional $$(G.litName "_eq")      Nothing (AEQ True . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_neq")     Nothing (ANE True . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_in")      Nothing (AIN  . mkListLiteral columnType <$> columnListParser)
      , P.fieldOptional $$(G.litName "_nin")     Nothing (ANIN . mkListLiteral columnType <$> columnListParser)
      ]
    -- Comparison ops for non Raster types
    , guard (isScalarColumnWhere (/= PGRaster) columnType) *>
      [ P.fieldOptional $$(G.litName "_gt")  Nothing (AGT  . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_lt")  Nothing (ALT  . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_gte") Nothing (AGTE . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_lte") Nothing (ALTE . mkParameter <$> typedParser)
      ]
    -- Ops for Raster types
    , guard (isScalarColumnWhere (== PGRaster) columnType) *>
      [ P.fieldOptional $$(G.litName "_st_intersects_rast")
        Nothing
        (ASTIntersectsRast . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_st_intersects_nband_geom")
        Nothing
        (ASTIntersectsNbandGeom <$> ingInputParser)
      , P.fieldOptional $$(G.litName "_st_intersects_geom_nband")
        Nothing
        (ASTIntersectsGeomNband <$> ignInputParser)
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
        (AILIKE () . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_nilike")
        (Just "does the column NOT match the given case-insensitive pattern")
        (ANILIKE () . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_similar")
        (Just "does the column match the given SQL regular expression")
        (ASIMILAR  . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_nsimilar")
        (Just "does the column NOT match the given SQL regular expression")
        (ANSIMILAR . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_regex")
        (Just "does the column match the given POSIX regular expression, case sensitive")
        (AREGEX  . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_iregex")
        (Just "does the column match the given POSIX regular expression, case insensitive")
        (AIREGEX . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_nregex")
        (Just "does the column NOT match the given POSIX regular expression, case sensitive")
        (ANREGEX  . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_niregex")
        (Just "does the column NOT match the given POSIX regular expression, case insensitive")
        (ANIREGEX . mkParameter <$> typedParser)
      ]
    -- Ops for JSONB type
    , guard (isScalarColumnWhere (== PGJSONB) columnType) *>
      [ P.fieldOptional $$(G.litName "_contains")
        (Just "does the column contain the given json value at the top level")
        (AContains    . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_contained_in")
        (Just "is the column contained in the given json value")
        (AContainedIn . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_has_key")
        (Just "does the string exist as a top-level key in the column")
        (AHasKey      . mkParameter <$> nullableTextParser)
      , P.fieldOptional $$(G.litName "_has_keys_any")
        (Just "do any of these strings exist as top-level keys in the column")
        (AHasKeysAny . mkListLiteral (ColumnScalar PGText) <$> textListParser)
      , P.fieldOptional $$(G.litName "_has_keys_all")
        (Just "do all of these strings exist as top-level keys in the column")
        (AHasKeysAll . mkListLiteral (ColumnScalar PGText) <$> textListParser)
      ]
    -- Ops for Geography type
    , guard (isScalarColumnWhere (== PGGeography) columnType) *>
      [ P.fieldOptional $$(G.litName "_st_intersects")
        (Just "does the column spatially intersect the given geography value")
        (ASTIntersects . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_st_d_within")
        (Just "is the column within a given distance from the given geography value")
        (ASTDWithinGeog <$> geogInputParser)
      ]
    -- Ops for Geometry type
    , guard (isScalarColumnWhere (== PGGeometry) columnType) *>
      [ P.fieldOptional $$(G.litName "_st_contains")
        (Just "does the column contain the given geometry value")
        (ASTContains   . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_st_crosses")
        (Just "does the column cross the given geometry value")
        (ASTCrosses    . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_st_equals")
        (Just "is the column equal to given geometry value (directionality is ignored)")
        (ASTEquals     . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_st_overlaps")
        (Just "does the column 'spatially overlap' (intersect but not completely contain) the given geometry value")
        (ASTOverlaps   . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_st_touches")
        (Just "does the column have atleast one point in common with the given geometry value")
        (ASTTouches    . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_st_within")
        (Just "is the column contained in the given geometry value")
        (ASTWithin     . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_st_intersects")
        (Just "does the column spatially intersect the given geometry value")
        (ASTIntersects . mkParameter <$> typedParser)
      , P.fieldOptional $$(G.litName "_st_d_within")
        (Just "is the column within a given distance from the given geometry value")
        (ASTDWithinGeom <$> geomInputParser)
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
        sourceName   <- P.mkScalarTypeName sourceScalar <&> (<> $$(G.litName "_cast_exp"))
        targetName   <- P.mkScalarTypeName targetScalar
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
