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
  let name = P.getName columnParser <> $$(G.litName "_comparison_exp")
  pure $ P.object name Nothing $ catMaybes <$> sequenceA
    [ P.fieldOptional $$(G.litName "_cast")    Nothing (ACast <$> castParser)
    , P.fieldOptional $$(G.litName "_eq")      Nothing (AEQ True . mkParameter <$> columnParser)
    , P.fieldOptional $$(G.litName "_neq")     Nothing (ANE True . mkParameter <$> columnParser)
    , P.fieldOptional $$(G.litName "_is_null") Nothing (bool ANISNOTNULL ANISNULL <$> P.boolean)
    -- TODO: the rest of the operators
    ]

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
