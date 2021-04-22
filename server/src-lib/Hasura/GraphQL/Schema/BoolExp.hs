module Hasura.GraphQL.Schema.BoolExp
  ( boolExp
  , mkBoolOperator
  , equalityOperators
  , comparisonOperators
  ) where

import           Hasura.Prelude

import qualified Language.GraphQL.Draft.Syntax as G

import           Data.Text.Extended

import qualified Hasura.GraphQL.Parser         as P

import           Hasura.GraphQL.Parser         (InputFieldsParser, Kind (..), Parser,
                                                UnpreparedValue)
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Schema.Backend
import           Hasura.GraphQL.Schema.Table
import           Hasura.RQL.Types


-- |
-- > input type_bool_exp {
-- >   _or: [type_bool_exp!]
-- >   _and: [type_bool_exp!]
-- >   _not: type_bool_exp
-- >   column: type_comparison_exp
-- >   ...
-- > }
boolExp
  :: forall b r m n. MonadBuildSchema b r m n
  => TableName b
  -> Maybe (SelPermInfo b)
  -> m (Parser 'Input n (AnnBoolExp b (UnpreparedValue b)))
boolExp table selectPermissions = memoizeOn 'boolExp table $ do
  tableGQLName <- getTableGQLName @b table
  let name = tableGQLName <> $$(G.litName "_bool_exp")
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
      :: FieldInfo b
      -> m (Maybe (InputFieldsParser n (Maybe (AnnBoolExpFld b (UnpreparedValue b)))))
    mkField fieldInfo = runMaybeT do
      fieldName <- hoistMaybe $ fieldInfoGraphQLName fieldInfo
      P.fieldOptional fieldName Nothing <$> case fieldInfo of
        -- field_name: field_type_comparison_exp
        FIColumn columnInfo ->
          lift $ fmap (AVCol columnInfo) <$> comparisonExps @b (pgiType columnInfo)

        -- field_name: field_type_bool_exp
        FIRelationship relationshipInfo -> do
          let remoteTable = riRTable relationshipInfo
          remotePermissions <- lift $ tableSelectPermissions remoteTable
          lift $ fmap (AVRel relationshipInfo) <$> boolExp remoteTable remotePermissions

        -- Using computed fields in boolean expressions is not currently supported.
        FIComputedField _ -> empty

        -- Using remote relationship fields in boolean expressions is not supported.
        FIRemoteRelationship _ -> empty


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
is permitted by the GraphQL specification. See Note [Optional fields and
nullability] in Hasura.GraphQL.Parser.Internal.Parser for more details.

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
mkBoolOperator
  :: (MonadParse n, 'Input P.<: k)
  => Bool
  -- ^ shall this be collapsed to True when null is given?
  -> G.Name
  -- ^ name of this operator
  -> Maybe G.Description
  -- ^ optional description
  -> Parser k n a
  -- ^ parser for the underlying value
  -> InputFieldsParser n (Maybe a)
mkBoolOperator True  name desc = fmap join . P.fieldOptional name desc . P.nullable
mkBoolOperator False name desc = P.fieldOptional name desc

equalityOperators
  :: (MonadParse n, 'Input P.<: k)
  => Bool
  -- ^ shall this be collapsed to True when null is given?
  -> Parser k n (UnpreparedValue b)
  -- ^ parser for one column value
  -> Parser k n (UnpreparedValue b)
  -- ^ parser for a list of column values
  -> [InputFieldsParser n (Maybe (OpExpG b (UnpreparedValue b)))]
equalityOperators collapseIfNull valueParser valueListParser =
  [ mkBoolOperator collapseIfNull $$(G.litName "_is_null") Nothing $ bool ANISNOTNULL ANISNULL <$> P.boolean
  , mkBoolOperator collapseIfNull $$(G.litName "_eq")      Nothing $ AEQ True <$> valueParser
  , mkBoolOperator collapseIfNull $$(G.litName "_neq")     Nothing $ ANE True <$> valueParser
  , mkBoolOperator collapseIfNull $$(G.litName "_in")      Nothing $ AIN  <$> valueListParser
  , mkBoolOperator collapseIfNull $$(G.litName "_nin")     Nothing $ ANIN <$> valueListParser
  ]

comparisonOperators
  :: (MonadParse n, 'Input P.<: k)
  => Bool
  -- ^ shall this be collapsed to True when null is given?
  -> Parser k n (UnpreparedValue b)
  -- ^ parser for one column value
  -> [InputFieldsParser n (Maybe (OpExpG b (UnpreparedValue b)))]
comparisonOperators collapseIfNull valueParser =
  [ mkBoolOperator collapseIfNull $$(G.litName "_gt")  Nothing $ AGT  <$> valueParser
  , mkBoolOperator collapseIfNull $$(G.litName "_lt")  Nothing $ ALT  <$> valueParser
  , mkBoolOperator collapseIfNull $$(G.litName "_gte") Nothing $ AGTE <$> valueParser
  , mkBoolOperator collapseIfNull $$(G.litName "_lte") Nothing $ ALTE <$> valueParser
  ]
