{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.GraphQL.Schema.BoolExp
  ( AggregationPredicatesSchema (..),
    tableBoolExp,
    logicalModelBoolExp,
    mkBoolOperator,
    equalityOperators,
    comparisonOperators,
  )
where

import Data.Has (getter)
import Data.HashMap.Strict qualified as HashMap
import Data.Text.Casing (GQLNameIdentifier)
import Data.Text.Casing qualified as C
import Data.Text.Extended
import Hasura.Base.Error (throw500)
import Hasura.Function.Cache
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Parser
  ( InputFieldsParser,
    Kind (..),
    Parser,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Table
import Hasura.GraphQL.Schema.Typename
import Hasura.LogicalModel.Cache (LogicalModelInfo (..))
import Hasura.LogicalModel.Common
import Hasura.LogicalModel.Types (LogicalModelName (..))
import Hasura.Name qualified as Name
import Hasura.NativeQuery.Cache (NativeQueryInfo (_nqiReturns))
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Value
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType (BackendType)
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.NamingCase
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.RQL.Types.SchemaCache hiding (askTableInfo)
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.Table.Cache
import Language.GraphQL.Draft.Syntax qualified as G
import Type.Reflection

-- | Backends implement this type class to specify the schema of
-- aggregation predicates.
--
-- The default implementation results in a parser that does not parse anything.
--
-- The scope of this class is local to the function 'boolExp'. In particular,
-- methods in `class BackendSchema` and `type MonadBuildSchema` should *NOT*
-- include this class as a constraint.
class AggregationPredicatesSchema (b :: BackendType) where
  aggregationPredicatesParser ::
    forall r m n.
    (MonadBuildSourceSchema b r m n) =>
    TableInfo b ->
    SchemaT r m (Maybe (InputFieldsParser n [AggregationPredicates b (UnpreparedValue b)]))

-- Overlapping instance for backends that do not implement Aggregation Predicates.
instance {-# OVERLAPPABLE #-} (AggregationPredicates b ~ Const Void) => AggregationPredicatesSchema (b :: BackendType) where
  aggregationPredicatesParser ::
    forall r m n.
    (MonadBuildSourceSchema b r m n) =>
    TableInfo b ->
    SchemaT r m (Maybe (InputFieldsParser n [AggregationPredicates b (UnpreparedValue b)]))
  aggregationPredicatesParser _ = return Nothing

-- |
-- > input type_bool_exp {
-- >   _or: [type_bool_exp!]
-- >   _and: [type_bool_exp!]
-- >   _not: type_bool_exp
-- >   column: type_comparison_exp
-- >   ...
-- > }
boolExpInternal ::
  forall b r m n name.
  ( Typeable name,
    Ord name,
    ToTxt name,
    MonadBuildSchema b r m n,
    AggregationPredicatesSchema b
  ) =>
  GQLNameIdentifier ->
  Maybe (SelPermInfo b) ->
  [FieldInfo b] ->
  G.Description ->
  name ->
  SchemaT r m (Maybe (InputFieldsParser n [AggregationPredicates b (UnpreparedValue b)])) ->
  SchemaT r m (Parser 'Input n (AnnBoolExp b (UnpreparedValue b)))
boolExpInternal gqlName selectPermissions fieldInfos description memoizeKey mkAggPredParser = do
  sourceInfo :: SourceInfo b <- asks getter
  P.memoizeOn 'boolExpInternal (_siName sourceInfo, memoizeKey) do
    let customization = _siCustomization sourceInfo
        tCase = _rscNamingConvention customization
        mkTypename = runMkTypename $ _rscTypeNames customization
        name = mkTypename $ applyTypeNameCaseIdentifier tCase $ mkTableBoolExpTypeName gqlName

    tableFieldParsers <- catMaybes <$> traverse mkField fieldInfos

    aggregationPredicatesParser' <- fromMaybe (pure []) <$> mkAggPredParser
    recur <- boolExpInternal gqlName selectPermissions fieldInfos description memoizeKey mkAggPredParser

    -- Bafflingly, ApplicativeDo doesn’t work if we inline this definition (I
    -- think the TH splices throw it off), so we have to define it separately.
    let connectiveFieldParsers =
          [ P.fieldOptional Name.__or Nothing (BoolOr <$> P.list recur),
            P.fieldOptional Name.__and Nothing (BoolAnd <$> P.list recur),
            P.fieldOptional Name.__not Nothing (BoolNot <$> recur)
          ]

    pure
      $ BoolAnd
      <$> P.object name (Just description) do
        tableFields <- map BoolField . catMaybes <$> sequenceA tableFieldParsers
        specialFields <- catMaybes <$> sequenceA connectiveFieldParsers
        aggregationPredicateFields <- map (BoolField . AVAggregationPredicates) <$> aggregationPredicatesParser'
        pure (tableFields ++ specialFields ++ aggregationPredicateFields)
  where
    mkField ::
      FieldInfo b ->
      SchemaT r m (Maybe (InputFieldsParser n (Maybe (AnnBoolExpFld b (UnpreparedValue b)))))
    mkField fieldInfo = runMaybeT do
      selectPermissions' <- hoistMaybe selectPermissions
      !roleName <- retrieve scRole
      fieldName <- hoistMaybe $ fieldInfoGraphQLName fieldInfo
      P.fieldOptional fieldName Nothing <$> case fieldInfo of
        -- field_name: field_type_comparison_exp
        FIColumn (SCIScalarColumn columnInfo) ->
          let redactionExp = fromMaybe NoRedaction $ getRedactionExprForColumn selectPermissions' (ciColumn columnInfo)
           in lift $ fmap (AVColumn columnInfo redactionExp) <$> comparisonExps @b (ciType columnInfo)
        FIColumn (SCIObjectColumn nestedObjectInfo@NestedObjectInfo {..}) -> do
          SourceInfo {..} <- asks getter
          logicalModelInfo <-
            HashMap.lookup _noiType _siLogicalModels
              `onNothing` throw500 ("Logical model " <> _noiType <<> " not found in source " <>> _siName)
          lift $ fmap (AVNestedObject nestedObjectInfo) <$> logicalModelBoolExp logicalModelInfo
        FIColumn (SCIArrayColumn _) -> empty -- TODO(dmoverton)
        -- field_name: field_type_bool_exp
        FIRelationship relationshipInfo -> do
          case riTarget relationshipInfo of
            RelTargetNativeQuery nativeQueryName -> do
              logicalModelInfo <- _nqiReturns <$> askNativeQueryInfo nativeQueryName
              let remoteLogicalModelPermissions =
                    (fmap . fmap) (partialSQLExpToUnpreparedValue)
                      $ maybe annBoolExpTrue spiFilter
                      $ getSelPermInfoForLogicalModel roleName logicalModelInfo
              remoteBoolExp <- lift $ logicalModelBoolExp logicalModelInfo
              pure $ fmap (AVRelationship relationshipInfo . RelationshipFilters remoteLogicalModelPermissions) remoteBoolExp
            RelTargetTable remoteTable -> do
              remoteTableInfo <- askTableInfo $ remoteTable
              let remoteTablePermissions =
                    (fmap . fmap) (partialSQLExpToUnpreparedValue)
                      $ maybe annBoolExpTrue spiFilter
                      $ tableSelectPermissions roleName remoteTableInfo
              remoteBoolExp <- lift $ tableBoolExp remoteTableInfo
              pure $ fmap (AVRelationship relationshipInfo . RelationshipFilters remoteTablePermissions) remoteBoolExp
        FIComputedField ComputedFieldInfo {..} -> do
          let ComputedFieldFunction {..} = _cfiFunction
          -- For a computed field to qualify in boolean expression it shouldn't have any input arguments
          case toList _cffInputArgs of
            [] -> do
              let functionArgs =
                    flip FunctionArgsExp mempty
                      $ fromComputedFieldImplicitArguments @b UVSession _cffComputedFieldImplicitArgs

              fmap (AVComputedField . AnnComputedFieldBoolExp _cfiXComputedFieldInfo _cfiName _cffName functionArgs)
                <$> case computedFieldReturnType @b _cfiReturnType of
                  ReturnsScalar scalarType ->
                    let redactionExp = fromMaybe NoRedaction $ getRedactionExprForComputedField selectPermissions' _cfiName
                     in lift $ fmap (CFBEScalar redactionExp) <$> comparisonExps @b (ColumnScalar scalarType)
                  ReturnsTable table -> do
                    info <- askTableInfo table
                    lift $ fmap (CFBETable table) <$> tableBoolExp info
                  ReturnsOthers -> hoistMaybe Nothing
            _ -> hoistMaybe Nothing

        -- Using remote relationship fields in boolean expressions is not supported.
        FIRemoteRelationship _ -> empty

-- |
-- > input type_bool_exp {
-- >   _or: [type_bool_exp!]
-- >   _and: [type_bool_exp!]
-- >   _not: type_bool_exp
-- >   column: type_comparison_exp
-- >   ...
-- > }
-- | Boolean expression for logical models
logicalModelBoolExp ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    AggregationPredicatesSchema b
  ) =>
  LogicalModelInfo b ->
  SchemaT r m (Parser 'Input n (AnnBoolExp b (UnpreparedValue b)))
logicalModelBoolExp logicalModel = do
  roleName <- retrieve scRole
  let fieldInfos = HashMap.elems $ logicalModelFieldsToFieldInfo $ _lmiFields logicalModel
      name = getLogicalModelName (_lmiName logicalModel)
      gqlName = mkTableBoolExpTypeName (C.fromCustomName name)
      selectPermissions = getSelPermInfoForLogicalModel roleName logicalModel

      -- Aggregation parsers let us say things like, "select all authors
      -- with at least one article": they are predicates based on the
      -- object's relationship with some other entity.
      --
      -- Currently, logical models can't be defined to have
      -- relationships to other entities, and so they don't support
      -- aggregation predicates.
      --
      -- If you're here because you've been asked to implement them, this
      -- is where you want to put the parser.
      mkAggPredParser = pure (pure mempty)

      memoizeKey = name
      description =
        G.Description
          $ "Boolean expression to filter rows from the logical model for "
          <> name
          <<> ". All fields are combined with a logical 'AND'."
  boolExpInternal gqlName selectPermissions fieldInfos description memoizeKey mkAggPredParser

-- |
-- > input type_bool_exp {
-- >   _or: [type_bool_exp!]
-- >   _and: [type_bool_exp!]
-- >   _not: type_bool_exp
-- >   column: type_comparison_exp
-- >   ...
-- > }
-- | Booleans expressions for tables
tableBoolExp ::
  forall b r m n.
  (MonadBuildSchema b r m n, AggregationPredicatesSchema b) =>
  TableInfo b ->
  SchemaT r m (Parser 'Input n (AnnBoolExp b (UnpreparedValue b)))
tableBoolExp tableInfo = do
  roleName <- retrieve scRole
  let selectPermissions = tableSelectPermissions roleName tableInfo
  gqlName <- getTableIdentifierName tableInfo
  fieldInfos <- tableSelectFields tableInfo
  let mkAggPredParser = aggregationPredicatesParser tableInfo
  let description =
        G.Description
          $ "Boolean expression to filter rows from the table "
          <> tableInfoName tableInfo
          <<> ". All fields are combined with a logical 'AND'."

  let memoizeKey = tableInfoName tableInfo
  boolExpInternal gqlName selectPermissions fieldInfos description memoizeKey mkAggPredParser

{- Note [Nullability in comparison operators]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In comparisonExps, we hardcode most operators with `Nullability False` when
calling `column`, which might seem a bit sketchy. Shouldn’t the nullability
depend on the nullability of the underlying Postgres column?

No. If we did that, then we would allow boolean expressions like this:

    delete_users(where: {status: {eq: null}})

which in turn would generate a SQL query along the lines of:

    DELETE FROM users WHERE users.status = NULL

but `= NULL` might not do what they expect. For instance, on Postgres, it always
evaluates to False!

Even operators for which `null` is a valid value must be careful in their
implementation. An explicit `null` must always be handled explicitly! If,
instead, an explicit null is ignored:

    foo <- fmap join $ fieldOptional "_foo_level" $ nullable int

then

       delete_users(where: {_foo_level: null})
    => delete_users(where: {})
    => delete_users()

Now we’ve gone and deleted every user in the database. Whoops! Hopefully the
user had backups!

In most cases, as mentioned above, we avoid this problem by making the column
value non-nullable (which is correct, since we never treat a null value as a SQL
NULL), then creating the field using 'fieldOptional'. This creates a parser that
rejects nulls, but won’t be called at all if the field is not specified, which
is permitted by the GraphQL specification. See Note [The value of omitted
fields] in Hasura.GraphQL.Parser.Internal.Parser for more details.

Additionally, it is worth nothing that the `column` parser *does* handle
explicit nulls, by creating a Null column value.

But... the story doesn't end there. Some of our users WANT this peculiar
behaviour. For instance, they want to be able to express the following:

    query($isVerified: Boolean) {
      users(where: {_isVerified: {_eq: $isVerified}}) {
        name
      }
    }

    $isVerified is True  -> return users who are verified
    $isVerified is False -> return users who aren't
    $isVerified is null  -> return all users

In the future, we will likely introduce a separate group of operators that do
implement this particular behaviour explicitly; but for now we have an option that
reverts to the previous behaviour.

To do so, we have to treat explicit nulls as implicit one: this is what the
'nullable' combinator does: it treats an explicit null as if the field has never
been called at all.
-}

-- This is temporary, and should be removed as soon as possible.
mkBoolOperator ::
  (MonadParse n, 'Input P.<: k) =>
  -- | Naming convention for the field
  NamingCase ->
  -- | shall this be collapsed to True when null is given?
  Options.DangerouslyCollapseBooleans ->
  -- | name of this operator
  GQLNameIdentifier ->
  -- | optional description
  Maybe G.Description ->
  -- | parser for the underlying value
  Parser k n a ->
  InputFieldsParser n (Maybe a)
mkBoolOperator tCase Options.DangerouslyCollapseBooleans name desc = fmap join . P.fieldOptional (applyFieldNameCaseIdentifier tCase name) desc . P.nullable
mkBoolOperator tCase Options.Don'tDangerouslyCollapseBooleans name desc = P.fieldOptional (applyFieldNameCaseIdentifier tCase name) desc

equalityOperators ::
  (MonadParse n, 'Input P.<: k) =>
  NamingCase ->
  -- | shall this be collapsed to True when null is given?
  Options.DangerouslyCollapseBooleans ->
  -- | parser for one column value
  Parser k n (UnpreparedValue b) ->
  -- | parser for a list of column values
  Parser k n (UnpreparedValue b) ->
  [InputFieldsParser n (Maybe (OpExpG b (UnpreparedValue b)))]
equalityOperators tCase collapseIfNull valueParser valueListParser =
  [ mkBoolOperator tCase collapseIfNull (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_is", "null"])) Nothing $ bool ANISNOTNULL ANISNULL <$> P.boolean,
    mkBoolOperator tCase collapseIfNull (C.fromAutogeneratedName Name.__eq) Nothing $ AEQ NonNullableComparison <$> valueParser,
    mkBoolOperator tCase collapseIfNull (C.fromAutogeneratedName Name.__neq) Nothing $ ANE NonNullableComparison <$> valueParser,
    mkBoolOperator tCase collapseIfNull (C.fromAutogeneratedName Name.__in) Nothing $ AIN <$> valueListParser,
    mkBoolOperator tCase collapseIfNull (C.fromAutogeneratedName Name.__nin) Nothing $ ANIN <$> valueListParser
  ]

comparisonOperators ::
  (MonadParse n, 'Input P.<: k) =>
  NamingCase ->
  -- | shall this be collapsed to True when null is given?
  Options.DangerouslyCollapseBooleans ->
  -- | parser for one column value
  Parser k n (UnpreparedValue b) ->
  [InputFieldsParser n (Maybe (OpExpG b (UnpreparedValue b)))]
comparisonOperators tCase collapseIfNull valueParser =
  [ mkBoolOperator tCase collapseIfNull (C.fromAutogeneratedName Name.__gt) Nothing $ AGT <$> valueParser,
    mkBoolOperator tCase collapseIfNull (C.fromAutogeneratedName Name.__lt) Nothing $ ALT <$> valueParser,
    mkBoolOperator tCase collapseIfNull (C.fromAutogeneratedName Name.__gte) Nothing $ AGTE <$> valueParser,
    mkBoolOperator tCase collapseIfNull (C.fromAutogeneratedName Name.__lte) Nothing $ ALTE <$> valueParser
  ]
