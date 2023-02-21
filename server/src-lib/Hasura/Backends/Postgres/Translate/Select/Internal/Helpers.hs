-- | Stuff gutted from Translate.Select.
module Hasura.Backends.Postgres.Translate.Select.Internal.Helpers
  ( mkFirstElementExp,
    mkLastElementExp,
    cursorIdentifier,
    startCursorIdentifier,
    endCursorIdentifier,
    hasNextPageIdentifier,
    hasPreviousPageIdentifier,
    pageInfoSelectAlias,
    pageInfoSelectAliasIdentifier,
    cursorsSelectAlias,
    cursorsSelectAliasIdentifier,
    encodeBase64,
    fromTableRowArgs,
    selectFromToFromItem,
    functionToIdentifier,
    withJsonBuildObj,
    withForceAggregation,
    selectToSelectWith,
    customSQLToTopLevelCTEs,
    logicalModelNameToAlias,
    toQuery,
  )
where

import Control.Monad.Writer (Writer, runWriter)
import Data.Bifunctor (bimap)
import Data.HashMap.Strict qualified as Map
import Data.Text.Extended (toTxt)
import Database.PG.Query (Query, fromBuilder)
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.RenameIdentifiers
import Hasura.Backends.Postgres.SQL.Types
  ( Identifier (..),
    QualifiedFunction,
    TableIdentifier (..),
    qualifiedObjectToText,
    tableIdentifierToIdentifier,
  )
import Hasura.Backends.Postgres.Translate.Select.Internal.Aliases
import Hasura.Backends.Postgres.Translate.Types (CustomSQLCTEs (..))
import Hasura.Backends.Postgres.Types.Function
import Hasura.NativeQuery.IR (NativeQuery (..))
import Hasura.NativeQuery.Metadata (LogicalModelName (..))
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.Types.Common (FieldName)
import Hasura.RQL.Types.Function
import Hasura.SQL.Backend
import Hasura.SQL.Types (ToSQL (toSQL))

-- | First element extractor expression from given record set
-- For example:- To get first "id" column from given row set,
-- the function generates the SQL expression AS `(array_agg("id"))[1]`
mkFirstElementExp :: S.SQLExp -> S.SQLExp
mkFirstElementExp expIdentifier =
  -- For Example
  S.SEArrayIndex (S.SEFnApp "array_agg" [expIdentifier] Nothing) (S.intToSQLExp 1)

-- | Last element extractor expression from given record set.
-- For example:- To get first "id" column from given row set,
-- the function generates the SQL expression AS `(array_agg("id"))[array_length(array_agg("id"), 1)]`
mkLastElementExp :: S.SQLExp -> S.SQLExp
mkLastElementExp expIdentifier =
  let arrayExp = S.SEFnApp "array_agg" [expIdentifier] Nothing
   in S.SEArrayIndex arrayExp $
        S.SEFnApp "array_length" [arrayExp, S.intToSQLExp 1] Nothing

cursorIdentifier :: Identifier
cursorIdentifier = Identifier "__cursor"

startCursorIdentifier :: Identifier
startCursorIdentifier = Identifier "__start_cursor"

endCursorIdentifier :: Identifier
endCursorIdentifier = Identifier "__end_cursor"

hasPreviousPageIdentifier :: Identifier
hasPreviousPageIdentifier = Identifier "__has_previous_page"

hasNextPageIdentifier :: Identifier
hasNextPageIdentifier = Identifier "__has_next_page"

pageInfoSelectAlias :: S.TableAlias
pageInfoSelectAlias = S.mkTableAlias "__page_info"

pageInfoSelectAliasIdentifier :: TableIdentifier
pageInfoSelectAliasIdentifier = S.tableAliasToIdentifier pageInfoSelectAlias

cursorsSelectAlias :: S.TableAlias
cursorsSelectAlias = S.mkTableAlias "__cursors_select"

cursorsSelectAliasIdentifier :: TableIdentifier
cursorsSelectAliasIdentifier = S.tableAliasToIdentifier cursorsSelectAlias

encodeBase64 :: S.SQLExp -> S.SQLExp
encodeBase64 =
  removeNewline . bytesToBase64Text . convertToBytes
  where
    convertToBytes e =
      S.SEFnApp "convert_to" [e, S.SELit "UTF8"] Nothing
    bytesToBase64Text e =
      S.SEFnApp "encode" [e, S.SELit "base64"] Nothing
    removeNewline e =
      S.SEFnApp "regexp_replace" [e, S.SELit "\\n", S.SELit "", S.SELit "g"] Nothing

fromTableRowArgs ::
  TableIdentifier -> FunctionArgsExpG (ArgumentExp S.SQLExp) -> S.FunctionArgs
fromTableRowArgs prefix = toFunctionArgs . fmap toSQLExp
  where
    toFunctionArgs (FunctionArgsExp positional named) =
      S.FunctionArgs positional named
    toSQLExp =
      onArgumentExp
        (S.SERowIdentifier (tableIdentifierToIdentifier baseTableIdentifier))
        (S.mkQIdenExp baseTableIdentifier . Identifier)
    baseTableIdentifier = mkBaseTableIdentifier prefix

selectFromToFromItem :: TableIdentifier -> SelectFrom ('Postgres pgKind) -> S.FromItem
selectFromToFromItem prefix = \case
  FromTable tn -> S.FISimple tn Nothing
  FromIdentifier i -> S.FIIdentifier $ TableIdentifier $ unFIIdentifier i
  FromFunction qf args defListM ->
    S.FIFunc $
      S.FunctionExp qf (fromTableRowArgs prefix args) $
        Just $
          S.mkFunctionAlias
            qf
            (fmap (fmap (first S.toColumnAlias)) defListM)
  FromNativeQuery nq ->
    S.FIIdentifier (S.tableAliasToIdentifier $ logicalModelNameToAlias (nqRootFieldName nq))

-- | Given a @LogicalModelName@, what should we call the CTE generated for it?
logicalModelNameToAlias :: LogicalModelName -> S.TableAlias
logicalModelNameToAlias nqName = S.mkTableAlias ("cte_" <> toTxt (getLogicalModelName nqName))

-- | Converts a function name to an 'Identifier'.
--
-- If the schema name is public, it will just use its name, otherwise it will
-- prefix it by the schema name.
functionToIdentifier :: QualifiedFunction -> Identifier
functionToIdentifier = Identifier . qualifiedObjectToText

-- uses json_build_object to build a json object
withJsonBuildObj ::
  FieldName -> [S.SQLExp] -> (S.ColumnAlias, S.SQLExp)
withJsonBuildObj parAls exps =
  (S.toColumnAlias parAls, jsonRow)
  where
    jsonRow = S.applyJsonBuildObj exps

-- | Forces aggregation
withForceAggregation :: S.TypeAnn -> S.SQLExp -> S.SQLExp
withForceAggregation tyAnn e =
  -- bool_or to force aggregation
  S.SEFnApp "coalesce" [e, S.SETyAnn (S.SEUnsafe "bool_or('true')") tyAnn] Nothing

-- | unwrap any emitted TopLevelCTEs for custom sql from the Writer and combine
-- them with a @Select@ to create a @SelectWith@
selectToSelectWith :: Writer CustomSQLCTEs S.Select -> S.SelectWith
selectToSelectWith action =
  let (selectSQL, customSQLCTEs) = runWriter action
   in S.SelectWith (customSQLToTopLevelCTEs customSQLCTEs) selectSQL

-- | convert map of CustomSQL CTEs into named TopLevelCTEs
customSQLToTopLevelCTEs :: CustomSQLCTEs -> [(S.TableAlias, S.TopLevelCTE)]
customSQLToTopLevelCTEs =
  fmap (bimap S.toTableAlias S.CTEUnsafeRawSQL) . Map.toList . getCustomSQLCTEs

toQuery :: S.SelectWithG S.TopLevelCTE -> Query
toQuery = fromBuilder . toSQL . renameIdentifiersSelectWithTopLevelCTE
