module Hasura.GraphQL.Schema.BoolExp
  ( boolExp
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
  :: forall m n r b. (BackendSchema b, MonadSchema n m, MonadTableInfo b r m, MonadRole r m)
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
      fieldName <- MaybeT $ pure $ fieldInfoGraphQLName fieldInfo
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
