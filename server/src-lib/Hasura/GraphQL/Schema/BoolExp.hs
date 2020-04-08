module Hasura.GraphQL.Schema.BoolExp
  ( boolExp
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended  as M
import qualified Language.GraphQL.Draft.Syntax as G

import qualified Hasura.GraphQL.Parser         as P

import           Hasura.GraphQL.Parser         (FieldsParser, Kind (..), Parser, UnpreparedValue,
                                                mkParameter)
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Schema.Common  (qualifiedObjectToName)
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value
import           Hasura.SQL.DML

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
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable -> m (Parser 'Input n (AnnBoolExp UnpreparedValue))
boolExp = P.memoize 'boolExp \tableName -> do
  name <- qualifiedObjectToName tableName <&> (<> $$(G.litName "_bool_exp"))
  let description = G.Description $
        "Boolean expression to filter rows from the table " <> tableName <<>
        ". All fields are combined with a logical 'AND'."

  tableInfo <- _tiCoreInfo <$> askTableInfo tableName
  -- FIXME: permissions!
  tableFieldParsers <- catMaybes <$> traverse mkField (M.elems $ _tciFieldInfoMap tableInfo)

  recur <- boolExp tableName
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
      :: FieldInfo -> m (Maybe (FieldsParser 'Input n (Maybe (AnnBoolExpFld UnpreparedValue))))
    mkField fieldInfo = runMaybeT do
      fieldName <- MaybeT $ pure $ fieldInfoGraphQLName fieldInfo
      P.fieldOptional fieldName Nothing <$> case fieldInfo of
        -- field_name: field_type_comparison_exp
        FIColumn columnInfo -> lift $ fmap (AVCol columnInfo) <$>
          comparisonExps (pgiType columnInfo) (G.Nullability $ pgiIsNullable columnInfo)

        -- field_name: field_type_bool_exp
        FIRelationship relationshipInfo ->
          lift $ fmap (AVRel relationshipInfo) <$> boolExp (riRTable relationshipInfo)

        -- Using computed fields in boolean expressions is not currently supported.
        FIComputedField _ -> empty

comparisonExps
  :: (MonadSchema n m, MonadError QErr m)
  => PGColumnType -> G.Nullability -> m (Parser 'Input n [ComparisonExp])
comparisonExps = P.memoize2 'comparisonExps \columnType nullability -> do
  columnParser <- P.column columnType nullability
  castParser   <- castExp columnType nullability
  let name       = P.getName columnParser <> $$(G.litName "_comparison_exp")
      listParser = P.list columnParser `P.bind` traverse P.openOpaque
  pure $ P.object name Nothing $ fmap catMaybes $ sequenceA $ concat
    [ [ P.fieldOptional $$(G.litName "_cast")    Nothing (ACast <$> castParser)
      , P.fieldOptional $$(G.litName "_is_null") Nothing (bool ANISNOTNULL ANISNULL <$> P.boolean)
      , P.fieldOptional $$(G.litName "_eq")      Nothing (AEQ True . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_neq")     Nothing (ANE True . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_in")      Nothing (AIN  . mkListLiteral columnType <$> listParser)
      , P.fieldOptional $$(G.litName "_nin")     Nothing (ANIN . mkListLiteral columnType <$> listParser)
      ]
    , guard (isScalarColumnWhere (/= PGRaster) columnType) *>
      [ P.fieldOptional $$(G.litName "_gt")  Nothing (AGT  . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_lt")  Nothing (ALT  . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_gte") Nothing (AGTE . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_lte") Nothing (ALTE . mkParameter <$> columnParser)
      ]
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
    , guard (isScalarColumnWhere (== PGJSONB) columnType) *>
      [ P.fieldOptional $$(G.litName "_contains")
        (Just "does the column contain the given json value at the top level")
        (AContains    . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_contained_in")
        (Just "is the column contained in the given json value")
        (AContainedIn . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_has_key")
        (Just "does the string exist as a top-level key in the column")
        (AHasKey      . mkParameter <$> columnParser)
      , P.fieldOptional $$(G.litName "_has_key_any")
        (Just "do any of these strings exist as top-level keys in the column")
        (AHasKeysAny . mkListLiteral columnType <$> listParser)
      , P.fieldOptional $$(G.litName "_has_key_all")
        (Just "do all of these strings exist as top-level keys in the column")
        (AHasKeysAll . mkListLiteral columnType <$> listParser)
      ]
    ]
  where
    mkListLiteral :: PGColumnType -> [P.PGColumnValue] -> UnpreparedValue
    mkListLiteral columnType columnValues = P.UVLiteral $ SETyAnn
      (SEArray $ txtEncoder . pstValue . P.pcvValue <$> columnValues)
      (mkTypeAnn $ PGTypeArray $ unsafePGColumnToRepresentation columnType)

castExp
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => PGColumnType -> G.Nullability -> m (Parser 'Input n (CastExp UnpreparedValue))
castExp sourceType nullability = do
  sourceName <- P.mkColumnTypeName sourceType <&> (<> $$(G.litName "_cast_exp"))
  fields <- sequenceA <$> traverse mkField targetTypes
  pure $ P.object sourceName Nothing $ M.fromList . catMaybes <$> fields
  where
    targetTypes = case sourceType of
      PGColumnScalar PGGeometry  -> [PGGeography]
      PGColumnScalar PGGeography -> [PGGeometry]
      _                          -> []

    mkField :: PGScalarType -> m (FieldsParser 'Input n (Maybe (PGScalarType, [ComparisonExp])))
    mkField targetType = do
      targetName <- P.mkScalarTypeName targetType
      value <- comparisonExps (PGColumnScalar targetType) nullability
      pure $ P.fieldOptional targetName Nothing $ (targetType,) <$> value
