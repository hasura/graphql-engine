{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module provides common building blocks for composing Schema Parsers
-- used in the schema of Update Mutations.
module Hasura.GraphQL.Schema.Update
  ( UpdateOperator (..),
    updateOperator,
    buildUpdateOperators,
    presetColumns,
    setOp,
    incOp,
    updateTable,
    updateTableByPk,
    mkUpdateObject,
  )
where

import Data.Has (Has (getter))
import Data.HashMap.Strict qualified as M
import Data.HashMap.Strict.Extended qualified as M
import Data.List.NonEmpty qualified as NE
import Data.Text.Extended ((<>>))
import Hasura.Base.Error (QErr)
import Hasura.Base.ToErrorValue
import Hasura.GraphQL.Schema.Backend (BackendSchema (..), BackendTableSelectSchema (..), MonadBuildSchema, columnParser)
import Hasura.GraphQL.Schema.BoolExp (boolExp)
import Hasura.GraphQL.Schema.Common (Scenario (..), SchemaContext (..), mapField, partialSQLExpToUnpreparedValue, retrieve)
import Hasura.GraphQL.Schema.Mutation (mutationSelectionSet, primaryKeysArguments)
import Hasura.GraphQL.Schema.NamingCase
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Table (getTableGQLName, tableColumns, tableUpdateColumns)
import Hasura.GraphQL.Schema.Typename
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (AnnBoolExp, annBoolExpTrue)
import Hasura.RQL.IR.Returning (MutationOutputG (..))
import Hasura.RQL.IR.Root (RemoteRelationshipField)
import Hasura.RQL.IR.Update (AnnotatedUpdateG (..))
import Hasura.RQL.IR.Value
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.RQL.Types.Column (ColumnInfo (..), isNumCol)
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.Table
import Language.GraphQL.Draft.Syntax (Description (..), Name (..), Nullability (..), litName)

-- | @UpdateOperator b m n op@ represents one single update operator for a
-- backend @b@.
--
-- The type variable @op@ is the backend-specific data type that represents
-- update operators, typically in the form of a sum-type with an
-- @UnpreparedValue b@ in each constructor.
--
-- The @UpdateOperator b m n@ is a @Functor@. There exist building blocks of
-- common update operators (such as 'setOp', etc.) which have @op ~
-- UnpreparedValue b@. The Functor instance lets you wrap the generic update
-- operators in backend-specific tags.
data UpdateOperator b m n op = UpdateOperator
  { updateOperatorApplicableColumn :: ColumnInfo b -> Bool,
    updateOperatorParser ::
      Name ->
      TableName b ->
      NonEmpty (ColumnInfo b) ->
      m (P.InputFieldsParser n (HashMap (Column b) op))
  }
  deriving (Functor)

-- | The top-level component for building update operators parsers.
--
-- * It implements the @preset@ functionality from Update Permissions (see
--   <https://hasura.io/docs/latest/graphql/core/auth/authorization/permission-rules.html#column-presets
--   Permissions user docs>). Use the 'presetColumns' function to extract those from the update permissions.
-- * It validates that that the update fields parsed are sound when taken as a
--   whole, i.e. that some changes are actually specified (either in the
--   mutation query text or in update preset columns) and that each column is
--   only used in one operator.
buildUpdateOperators ::
  forall b r m n op.
  MonadBuildSchema b r m n =>
  -- | Columns with @preset@ expressions
  (HashMap (Column b) op) ->
  -- | Update operators to include in the Schema
  [UpdateOperator b m n op] ->
  TableInfo b ->
  m (P.InputFieldsParser n (HashMap (Column b) op))
buildUpdateOperators presetCols ops tableInfo = do
  parsers :: P.InputFieldsParser n [HashMap (Column b) op] <-
    sequenceA . catMaybes <$> traverse (runUpdateOperator tableInfo) ops
  pure $
    parsers
      `P.bindFields` ( \opExps -> do
                         let withPreset = presetCols : opExps
                         mergeDisjoint @b withPreset
                     )

-- | The columns that have 'preset' definitions applied to them. (see
-- <https://hasura.io/docs/latest/graphql/core/auth/authorization/permission-rules.html#column-presets
-- Permissions user docs>)
presetColumns :: UpdPermInfo b -> HashMap (Column b) (UnpreparedValue b)
presetColumns = fmap partialSQLExpToUnpreparedValue . upiSet

-- | Produce an InputFieldsParser from an UpdateOperator, but only if the operator
-- applies to the table (i.e., it admits a non-empty column set).
runUpdateOperator ::
  forall b r m n op.
  MonadBuildSchema b r m n =>
  TableInfo b ->
  UpdateOperator b m n op ->
  m
    ( Maybe
        ( P.InputFieldsParser
            n
            (HashMap (Column b) op)
        )
    )
runUpdateOperator tableInfo UpdateOperator {..} = do
  let tableName = tableInfoName tableInfo
  tableGQLName <- getTableGQLName tableInfo
  roleName <- retrieve scRole
  let columns = tableUpdateColumns roleName tableInfo

  let applicableCols :: Maybe (NonEmpty (ColumnInfo b)) =
        nonEmpty . filter updateOperatorApplicableColumn $ columns

  (sequenceA :: Maybe (m a) -> m (Maybe a))
    (applicableCols <&> updateOperatorParser tableGQLName tableName)

-- | Merge the results of parsed update operators. Throws an error if the same
-- column has been specified in multiple operators.
mergeDisjoint ::
  forall b m t.
  (Backend b, P.MonadParse m) =>
  [HashMap (Column b) t] ->
  m (HashMap (Column b) t)
mergeDisjoint parsedResults = do
  let unioned = M.unionsAll parsedResults
      duplicates = M.keys $ M.filter (not . null . NE.tail) unioned

  unless (null duplicates) $
    P.parseError
      ( "Column found in multiple operators: "
          <> toErrorValue duplicates
          <> "."
      )

  return $ M.map NE.head unioned

-- | Construct a parser for a single update operator.
--
-- @updateOperator _ "op" fp MkOp ["col1","col2"]@ gives a parser that accepts
-- objects in the shape of:
--
-- > op: {
-- >   col1: "x",
-- >   col2: "y"
-- > }
--
-- And (morally) parses into values:
--
-- > M.fromList [("col1", MkOp (fp "x")), ("col2", MkOp (fp "y"))]
updateOperator ::
  forall n r m b a.
  (P.MonadParse n, MonadReader r m, Has MkTypename r, Backend b) =>
  Name ->
  Name ->
  (ColumnInfo b -> m (P.Parser 'P.Both n a)) ->
  NonEmpty (ColumnInfo b) ->
  Description ->
  Description ->
  m (P.InputFieldsParser n (HashMap (Column b) a))
updateOperator tableGQLName opName mkParser columns opDesc objDesc = do
  fieldParsers :: NonEmpty (P.InputFieldsParser n (Maybe (Column b, a))) <-
    for columns \columnInfo -> do
      let fieldName = ciName columnInfo
          fieldDesc = ciDescription columnInfo
      fieldParser <- mkParser columnInfo
      pure $
        P.fieldOptional fieldName fieldDesc fieldParser
          `mapField` \value -> (ciColumn columnInfo, value)

  objName <- mkTypename $ tableGQLName <> opName <> $$(litName "_input")

  pure $
    fmap (M.fromList . (fold :: Maybe [(Column b, a)] -> [(Column b, a)])) $
      P.fieldOptional opName (Just opDesc) $
        P.object objName (Just objDesc) $
          (catMaybes . toList) <$> sequenceA fieldParsers
{-# ANN updateOperator ("HLint: ignore Use tuple-section" :: String) #-}

setOp ::
  forall b n r m.
  ( BackendSchema b,
    MonadReader r m,
    Has MkTypename r,
    Has NamingCase r,
    MonadError QErr m,
    P.MonadParse n
  ) =>
  UpdateOperator b m n (UnpreparedValue b)
setOp = UpdateOperator {..}
  where
    updateOperatorApplicableColumn = const True

    updateOperatorParser tableGQLName tableName columns = do
      let typedParser columnInfo =
            fmap mkParameter
              <$> columnParser
                (ciType columnInfo)
                (Nullability $ ciIsNullable columnInfo)

      updateOperator
        tableGQLName
        $$(litName "_set")
        typedParser
        columns
        "sets the columns of the filtered rows to the given values"
        (Description $ "input type for updating data in table " <>> tableName)

incOp ::
  forall b m n r.
  ( Backend b,
    MonadReader r m,
    MonadError QErr m,
    P.MonadParse n,
    BackendSchema b,
    Has MkTypename r,
    Has NamingCase r
  ) =>
  UpdateOperator b m n (UnpreparedValue b)
incOp = UpdateOperator {..}
  where
    updateOperatorApplicableColumn = isNumCol

    updateOperatorParser tableGQLName tableName columns = do
      let typedParser columnInfo =
            fmap mkParameter
              <$> columnParser
                (ciType columnInfo)
                (Nullability $ ciIsNullable columnInfo)

      updateOperator
        tableGQLName
        $$(litName "_inc")
        typedParser
        columns
        "increments the numeric columns with given value of the filtered values"
        (Description $ "input type for incrementing numeric columns in table " <>> tableName)

-- | Construct a root field, normally called update_tablename, that can be used
-- to update rows in a DB table specified by filters. Only returns a parser if
-- there are columns the user is allowed to update; otherwise returns Nothing.
updateTable ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    BackendTableSelectSchema b
  ) =>
  -- | backend-specific data needed to perform an update mutation
  P.InputFieldsParser n (BackendUpdate b (UnpreparedValue b)) ->
  Scenario ->
  -- | table source
  SourceInfo b ->
  -- | table info
  TableInfo b ->
  -- | field display name
  Name ->
  -- | field description, if any
  Maybe Description ->
  m (Maybe (P.FieldParser n (AnnotatedUpdateG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))))
updateTable backendUpdate scenario sourceInfo tableInfo fieldName description = runMaybeT do
  let tableName = tableInfoName tableInfo
      columns = tableColumns tableInfo
      whereName = $$(litName "where")
      whereDesc = "filter the rows which have to be updated"
      viewInfo = _tciViewInfo $ _tiCoreInfo tableInfo
  guard $ isMutable viIsUpdatable viewInfo
  roleName <- retrieve scRole
  updatePerms <- hoistMaybe $ _permUpd $ getRolePermInfo roleName tableInfo
  -- If we're in a frontend scenario, we should not include backend_only updates
  -- For more info see Note [Backend only permissions]
  guard $ not $ scenario == Frontend && upiBackendOnly updatePerms
  whereArg <- lift $ P.field whereName (Just whereDesc) <$> boolExp sourceInfo tableInfo
  selection <- lift $ mutationSelectionSet sourceInfo tableInfo
  tCase <- asks getter
  let argsParser = liftA2 (,) backendUpdate whereArg
  pure $
    P.subselection fieldName description argsParser selection
      <&> mkUpdateObject tableName columns updatePerms (Just tCase) . fmap MOutMultirowFields

-- | Construct a root field, normally called 'update_tablename_by_pk', that can be used
-- to update a single in a DB table, specified by primary key. Only returns a
-- parser if there are columns the user is allowed to update and if the user has
-- select permissions on all primary keys; otherwise returns Nothing.
updateTableByPk ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  BackendTableSelectSchema b =>
  -- | backend-specific data needed to perform an update mutation
  P.InputFieldsParser n (BackendUpdate b (UnpreparedValue b)) ->
  Scenario ->
  -- | table source
  SourceInfo b ->
  -- | table info
  TableInfo b ->
  -- | field display name
  Name ->
  -- | field description, if any
  Maybe Description ->
  m (Maybe (P.FieldParser n (AnnotatedUpdateG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))))
updateTableByPk backendUpdate scenario sourceInfo tableInfo fieldName description = runMaybeT $ do
  let columns = tableColumns tableInfo
      tableName = tableInfoName tableInfo
      viewInfo = _tciViewInfo $ _tiCoreInfo tableInfo
  guard $ isMutable viIsUpdatable viewInfo
  roleName <- retrieve scRole
  updatePerms <- hoistMaybe $ _permUpd $ getRolePermInfo roleName tableInfo
  -- If we're in a frontend scenario, we should not include backend_only updates
  -- For more info see Note [Backend only permissions]
  guard $ not $ scenario == Frontend && upiBackendOnly updatePerms
  pkArgs <- MaybeT $ primaryKeysArguments tableInfo
  selection <- MaybeT $ tableSelectionSet sourceInfo tableInfo
  tCase <- asks getter
  lift $ do
    tableGQLName <- getTableGQLName tableInfo
    pkObjectName <- mkTypename $ tableGQLName <> $$(litName "_pk_columns_input")
    let pkFieldName = $$(litName "pk_columns")
        pkObjectDesc = Description $ "primary key columns input for table: " <> unName tableGQLName
        pkParser = P.object pkObjectName (Just pkObjectDesc) pkArgs
        argsParser = (,) <$> backendUpdate <*> P.field pkFieldName Nothing pkParser
    pure $
      P.subselection fieldName description argsParser selection
        <&> mkUpdateObject tableName columns updatePerms (Just tCase) . fmap MOutSinglerowObject

mkUpdateObject ::
  Backend b =>
  TableName b ->
  [ColumnInfo b] ->
  UpdPermInfo b ->
  (Maybe NamingCase) ->
  ( ( BackendUpdate b (UnpreparedValue b),
      AnnBoolExp b (UnpreparedValue b)
    ),
    MutationOutputG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b)
  ) ->
  AnnotatedUpdateG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b)
mkUpdateObject _auTable _auAllCols updatePerms _auNamingConvention ((_auBackend, whereExp), _auOutput) =
  AnnotatedUpdateG {..}
  where
    permissionFilter = fmap partialSQLExpToUnpreparedValue <$> upiFilter updatePerms
    _auWhere = (permissionFilter, whereExp)
    _auCheck = maybe annBoolExpTrue ((fmap . fmap) partialSQLExpToUnpreparedValue) $ upiCheck updatePerms
