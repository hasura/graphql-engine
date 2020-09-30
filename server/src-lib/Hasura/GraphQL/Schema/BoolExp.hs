module Hasura.GraphQL.Schema.BoolExp
  ( boolExp
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended  as M
import qualified Language.GraphQL.Draft.Syntax as G

import qualified Hasura.GraphQL.Parser         as P

import           Hasura.GraphQL.Parser         (InputFieldsParser, Kind (..), Parser,
                                                UnpreparedValue, mkParameter)
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Schema.Table
import           Hasura.RQL.Types
import           Hasura.SQL.DML
import           Hasura.SQL.Types
import           Hasura.SQL.Value

type ComparisonExp = OpExpG UnpreparedValue

-- |
-- > input type_bool_exp {
-- >   _or: [type_bool_exp!]
-- >   _and: [type_bool_exp!]
-- >   _not: type_bool_exp
-- >   column: type_comparison_exp
-- >   ...
-- > }
boolExp
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => QualifiedTable
  -> Maybe SelPermInfo
  -> m (Parser 'Input n (AnnBoolExp UnpreparedValue))
boolExp table selectPermissions = memoizeOn 'boolExp table $ do
  name <- qualifiedObjectToName table <&> (<> $$(G.litName "_bool_exp"))
  let description = G.Description $
        "Boolean expression to filter rows from the table " <> table <<>
        ". All fields are combined with a logical 'AND'."

  tableFieldParsers <- catMaybes <$> maybe
    (pure [])
    (traverse mkField <=< tableSelectFields table)
    selectPermissions
  recur <- boolExp table selectPermissions
  -- Bafflingly, ApplicativeDo doesn’t work if we inline this definition (I
  -- think the TH splices throw it off), so we have to define it separately.
  let specialFieldParsers =
        [ P.fieldOptional $$(G.litName "_or")  Nothing (BoolOr  <$> P.list recur)
        , P.fieldOptional $$(G.litName "_and") Nothing (BoolAnd <$> P.list recur)
        , P.fieldOptional $$(G.litName "_not") Nothing (BoolNot <$> recur)
        ]

  pure $ BoolAnd <$> P.object name (Just description) do
    tableFields <- map BoolFld . catMaybes <$> sequenceA tableFieldParsers
    specialFields <- catMaybes <$> sequenceA specialFieldParsers
    pure (tableFields ++ specialFields)
  where
    mkField
      :: FieldInfo -> m (Maybe (InputFieldsParser n (Maybe (AnnBoolExpFld UnpreparedValue))))
    mkField fieldInfo = runMaybeT do
      fieldName <- MaybeT $ pure $ fieldInfoGraphQLName fieldInfo
      P.fieldOptional fieldName Nothing <$> case fieldInfo of
        -- field_name: field_type_comparison_exp
        FIColumn columnInfo ->
          lift $ fmap (AVCol columnInfo) <$> comparisonExps (pgiType columnInfo)

        -- field_name: field_type_bool_exp
        FIRelationship relationshipInfo -> do
          let remoteTable = riRTable relationshipInfo
          remotePermissions <- lift $ tableSelectPermissions remoteTable
          lift $ fmap (AVRel relationshipInfo) <$> boolExp remoteTable remotePermissions

        -- Using computed fields in boolean expressions is not currently supported.
        FIComputedField _ -> empty

        -- Using remote relationship fields in boolean expressions is not supported.
        FIRemoteRelationship _ -> empty

comparisonExps
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => PGColumnType -> m (Parser 'Input n [ComparisonExp])
comparisonExps = P.memoize 'comparisonExps \columnType -> do
  geogInputParser <- geographyWithinDistanceInput
  geomInputParser <- geometryWithinDistanceInput
  ignInputParser  <- intersectsGeomNbandInput
  ingInputParser  <- intersectsNbandGeomInput
  -- see Note [Columns in comparison expression are never nullable]
  columnParser       <- P.column columnType (G.Nullability False)
  nullableTextParser <- P.column (PGColumnScalar PGText) (G.Nullability True)
  textParser         <- P.column (PGColumnScalar PGText) (G.Nullability False)
  maybeCastParser    <- castExp columnType
  let name = P.getName columnParser <> $$(G.litName "_comparison_exp")
      desc = G.Description $ "Boolean expression to compare columns of type "
        <>  P.getName columnParser
        <<> ". All fields are combined with logical 'AND'."
      textListParser = P.list textParser `P.bind` traverse P.openOpaque
      columnListParser = P.list columnParser `P.bind` traverse P.openOpaque
  pure $ P.object name (Just desc) $ fmap catMaybes $ sequenceA $ concat
    [ flip (maybe []) maybeCastParser $ \castParser ->
      [ P.fieldOptional $$(G.litName "_cast")    Nothing (ACast <$> castParser)
      ]
    -- Common ops for all types
    , [ P.fieldOptional $$(G.litName "_is_null") Nothing (bool ANISNOTNULL ANISNULL <$> P.boolean)
      , P.fieldOptional $$(G.litName "_eq")      Nothing (AEQ True . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_neq")     Nothing (ANE True . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_in")      Nothing (AIN  . mkListLiteral columnType <$> columnListParser)
      , P.fieldOptional $$(G.litName "_nin")     Nothing (ANIN . mkListLiteral columnType <$> columnListParser)
      ]
    -- Comparison ops for non Raster types
    , guard (isScalarColumnWhere (/= PGRaster) columnType) *>
      [ P.fieldOptional $$(G.litName "_gt")  Nothing (AGT  . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_lt")  Nothing (ALT  . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_gte") Nothing (AGTE . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_lte") Nothing (ALTE . mkParameter <$> columnParser)
      ]
    -- Ops for Raster types
    , guard (isScalarColumnWhere (== PGRaster) columnType) *>
      [ P.fieldOptional $$(G.litName "_st_intersects_rast")
        Nothing
        (ASTIntersectsRast . mkParameter <$> columnParser)
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
        (ALIKE     . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_nlike")
        (Just "does the column NOT match the given pattern")
        (ANLIKE    . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_ilike")
        (Just "does the column match the given case-insensitive pattern")
        (AILIKE    . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_nilike")
        (Just "does the column NOT match the given case-insensitive pattern")
        (ANILIKE   . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_similar")
        (Just "does the column match the given SQL regular expression")
        (ASIMILAR  . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_nsimilar")
        (Just "does the column NOT match the given SQL regular expression")
        (ANSIMILAR . mkParameter <$> columnParser)
      ]
    -- Ops for JSONB type
    , guard (isScalarColumnWhere (== PGJSONB) columnType) *>
      [ P.fieldOptional $$(G.litName "_contains")
        (Just "does the column contain the given json value at the top level")
        (AContains    . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_contained_in")
        (Just "is the column contained in the given json value")
        (AContainedIn . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_has_key")
        (Just "does the string exist as a top-level key in the column")
        (AHasKey      . mkParameter <$> nullableTextParser)
      , P.fieldOptional $$(G.litName "_has_keys_any")
        (Just "do any of these strings exist as top-level keys in the column")
        (AHasKeysAny . mkListLiteral (PGColumnScalar PGText) <$> textListParser)
      , P.fieldOptional $$(G.litName "_has_keys_all")
        (Just "do all of these strings exist as top-level keys in the column")
        (AHasKeysAll . mkListLiteral (PGColumnScalar PGText) <$> textListParser)
      ]
    -- Ops for Geography type
    , guard (isScalarColumnWhere (== PGGeography) columnType) *>
      [ P.fieldOptional $$(G.litName "_st_intersects")
        (Just "does the column spatially intersect the given geography value")
        (ASTIntersects . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_st_d_within")
        (Just "is the column within a given distance from the given geography value")
        (ASTDWithinGeog <$> geogInputParser)
      ]
    -- Ops for Geometry type
    , guard (isScalarColumnWhere (== PGGeometry) columnType) *>
      [ P.fieldOptional $$(G.litName "_st_contains")
        (Just "does the column contain the given geometry value")
        (ASTContains   . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_st_crosses")
        (Just "does the column cross the given geometry value")
        (ASTCrosses    . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_st_equals")
        (Just "is the column equal to given geometry value (directionality is ignored)")
        (ASTEquals     . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_st_overlaps")
        (Just "does the column 'spatially overlap' (intersect but not completely contain) the given geometry value")
        (ASTOverlaps   . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_st_touches")
        (Just "does the column have atleast one point in common with the given geometry value")
        (ASTTouches    . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_st_within")
        (Just "is the column contained in the given geometry value")
        (ASTWithin     . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_st_intersects")
        (Just "does the column spatially intersect the given geometry value")
        (ASTIntersects . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_st_d_within")
        (Just "is the column within a given distance from the given geometry value")
        (ASTDWithinGeom <$> geomInputParser)
      ]
    ]
  where
    mkListLiteral :: PGColumnType -> [P.PGColumnValue] -> UnpreparedValue
    mkListLiteral columnType columnValues = P.UVLiteral $ SETyAnn
      (SEArray $ txtEncoder . pstValue . P.pcvValue <$> columnValues)
      (mkTypeAnn $ PGTypeArray $ unsafePGColumnToRepresentation columnType)

    castExp :: PGColumnType -> m (Maybe (Parser 'Input n (CastExp UnpreparedValue)))
    castExp sourceType = do
      let maybeScalars = case sourceType of
            PGColumnScalar PGGeography -> Just (PGGeography, PGGeometry)
            PGColumnScalar PGGeometry  -> Just (PGGeometry, PGGeography)
            _                          -> Nothing

      forM maybeScalars $ \(sourceScalar, targetScalar) -> do
        sourceName   <- P.mkScalarTypeName sourceScalar <&> (<> $$(G.litName "_cast_exp"))
        targetName   <- P.mkScalarTypeName targetScalar
        targetOpExps <- comparisonExps $ PGColumnScalar targetScalar
        let field = P.fieldOptional targetName Nothing $ (targetScalar, ) <$> targetOpExps
        pure $ P.object sourceName Nothing $ M.fromList . maybeToList <$> field

geographyWithinDistanceInput
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => m (Parser 'Input n (DWithinGeogOp UnpreparedValue))
geographyWithinDistanceInput = do
  geographyParser <- P.column (PGColumnScalar PGGeography) (G.Nullability False)
  -- FIXME
  -- It doesn't make sense for this value to be nullable; it only is for
  -- backwards compatibility; if an explicit Null value is given, it will be
  -- forwarded to the underlying SQL function, that in turns treat a null value
  -- as an error. We can fix this by rejecting explicit null values, by marking
  -- this field non-nullable in a future release.
  booleanParser   <- P.column (PGColumnScalar PGBoolean)   (G.Nullability True)
  floatParser     <- P.column (PGColumnScalar PGFloat)     (G.Nullability False)
  pure $ P.object $$(G.litName "st_d_within_geography_input") Nothing $
    DWithinGeogOp <$> (mkParameter <$> P.field $$(G.litName "distance") Nothing floatParser)
                  <*> (mkParameter <$> P.field $$(G.litName "from")     Nothing geographyParser)
                  <*> (mkParameter <$> P.fieldWithDefault $$(G.litName "use_spheroid") Nothing (G.VBoolean True) booleanParser)

geometryWithinDistanceInput
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => m (Parser 'Input n (DWithinGeomOp UnpreparedValue))
geometryWithinDistanceInput = do
  geometryParser <- P.column (PGColumnScalar PGGeometry) (G.Nullability False)
  floatParser    <- P.column (PGColumnScalar PGFloat)    (G.Nullability False)
  pure $ P.object $$(G.litName "st_d_within_input") Nothing $
    DWithinGeomOp <$> (mkParameter <$> P.field $$(G.litName "distance") Nothing floatParser)
                  <*> (mkParameter <$> P.field $$(G.litName "from")     Nothing geometryParser)

intersectsNbandGeomInput
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => m (Parser 'Input n (STIntersectsNbandGeommin UnpreparedValue))
intersectsNbandGeomInput = do
  geometryParser <- P.column (PGColumnScalar PGGeometry) (G.Nullability False)
  integerParser  <- P.column (PGColumnScalar PGInteger)  (G.Nullability False)
  pure $ P.object $$(G.litName "st_intersects_nband_geom_input") Nothing $
    STIntersectsNbandGeommin <$> (mkParameter <$> P.field $$(G.litName "nband")   Nothing integerParser)
                             <*> (mkParameter <$> P.field $$(G.litName "geommin") Nothing geometryParser)

intersectsGeomNbandInput
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => m (Parser 'Input n (STIntersectsGeomminNband UnpreparedValue))
intersectsGeomNbandInput = do
  geometryParser <- P.column (PGColumnScalar PGGeometry) (G.Nullability False)
  integerParser  <- P.column (PGColumnScalar PGInteger)  (G.Nullability False)
  pure $ P.object $$(G.litName "st_intersects_geom_nband_input") Nothing $ STIntersectsGeomminNband
    <$> (     mkParameter <$> P.field         $$(G.litName "geommin") Nothing geometryParser)
    <*> (fmap mkParameter <$> P.fieldOptional $$(G.litName "nband")   Nothing integerParser)

{- Note [Columns in comparison expression are never nullable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In comparisonExps, we hardcode `Nullability False` when calling `column`, which
might seem a bit sketchy. Shouldn’t the nullability depend on the nullability of
the underlying Postgres column?

No. If we did that, then we would allow boolean expressions like this:

    delete_users(where: {status: {eq: null}})

The user expects this to generate SQL like

    DELETE FROM users WHERE users.status IS NULL

but it doesn’t. We treat null to mean “no condition was specified” (since
that’s how GraphQL indicates an optional field was omitted), and we actually
generate SQL like this:

    DELETE FROM users

Now we’ve gone and deleted every user in the database. Hopefully the user had
backups!

We avoid this problem by making the column value non-nullable (which is correct,
since we never treat a null value as a SQL NULL), then creating the field using
fieldOptional. This creates a parser that rejects nulls, but won’t be called at
all if the field is not specified, which is permitted by the GraphQL
specification. See Note [Optional fields and nullability] in
Hasura.GraphQL.Parser.Internal.Parser for more details. -}
