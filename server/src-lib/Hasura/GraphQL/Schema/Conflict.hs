{-# LANGUAGE ApplicativeDo #-}

-- | This module contains common schema parser building blocks pertaining to
-- parsing @_on_conflict@ clauses.
module Hasura.GraphQL.Schema.Conflict
  ( onConflictFieldParser,
  )
where

import Data.Text.Extended
import Hasura.GraphQL.Parser
  ( InputFieldsParser,
    Kind (..),
    Parser,
    UnpreparedValue (..),
  )
import Hasura.GraphQL.Parser qualified as P
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.BoolExp
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Table
import Hasura.Prelude
import Hasura.RQL.IR.Insert qualified as IR
import Hasura.RQL.Types
import Language.GraphQL.Draft.Syntax qualified as G

-- | Parser for a field name @on_conflict@ of type @<tablename>_on_conflict@.
onConflictFieldParser ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  TableInfo b ->
  Maybe (SelPermInfo b) ->
  Maybe (UpdPermInfo b) ->
  m (InputFieldsParser n (Maybe (IR.ConflictClauseP1 b (UnpreparedValue b))))
onConflictFieldParser sourceName tableInfo selectPerms updatePerms = do
  o <- withJust updatePerms $ defaultConflictObject sourceName tableInfo selectPerms
  return $ mkConflictArg o

-- | Creates a parser for the "_on_conflict" object of the given table.
--
-- This object is used to generate the "ON CONFLICT" SQL clause: what should be
-- done if an insert raises a conflict? It may not always exist: it can't be
-- created if there aren't any unique or primary keys constraints. However, if
-- there are no columns for which the current role has update permissions, we
-- must still accept an empty list for `update_columns`; we do this by adding a
-- placeholder value to the enum (see 'tableUpdateColumnsEnum').
defaultConflictObject ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  TableInfo b ->
  Maybe (SelPermInfo b) ->
  UpdPermInfo b ->
  m (Maybe (Parser 'Input n (IR.ConflictClauseP1 b (UnpreparedValue b))))
defaultConflictObject sourceName tableInfo selectPerms updatePerms = runMaybeT $ do
  tableGQLName <- getTableGQLName tableInfo
  columnsEnum <- lift $ tableUpdateColumnsEnum tableInfo updatePerms
  constraints <- hoistMaybe $ tciUniqueOrPrimaryKeyConstraints . _tiCoreInfo $ tableInfo
  constraintParser <- lift $ conflictConstraint constraints sourceName tableInfo
  whereExpParser <- lift $ boolExp sourceName tableInfo selectPerms
  objectName <- P.mkTypename $ tableGQLName <> $$(G.litName "_on_conflict")
  let presetColumns = partialSQLExpToUnpreparedValue <$> upiSet updatePerms
      updateFilter = fmap partialSQLExpToUnpreparedValue <$> upiFilter updatePerms
      objectDesc = G.Description $ "on conflict condition type for table " <>> tableInfoName tableInfo
      constraintName = $$(G.litName "constraint")
      columnsName = $$(G.litName "update_columns")
      whereExpName = $$(G.litName "where")
  pure $
    P.object objectName (Just objectDesc) $ do
      constraint <- IR.CTConstraint <$> P.field constraintName Nothing constraintParser
      whereExp <- P.fieldOptional whereExpName Nothing whereExpParser
      columns <-
        P.fieldWithDefault columnsName Nothing (G.VList []) (P.list columnsEnum) `P.bindFields` \cs ->
          -- this can only happen if the placeholder was used
          sequenceA cs `onNothing` parseError "erroneous column name"
      pure $
        case columns of
          [] -> IR.CP1DoNothing $ Just constraint
          _ -> IR.CP1Update constraint columns presetColumns $ BoolAnd $ updateFilter : maybeToList whereExp

-- | Creates a field parser for the "on_conflict" argument of insert fields.
--
-- The parser might not exist, as the current role might not have the
-- appropriate permissions, but insert fields can exist regardless. This
-- function creates a dummy 'InputFieldsParser' that always returns @Nothing@ in
-- such a case.
mkConflictArg ::
  MonadParse n =>
  Maybe (Parser 'Input n (IR.ConflictClauseP1 b (UnpreparedValue b))) ->
  InputFieldsParser n (Maybe (IR.ConflictClauseP1 b (UnpreparedValue b)))
mkConflictArg conflictParser' = withJust conflictParser' $ P.fieldOptional conflictName (Just conflictDesc)
  where
    conflictName = $$(G.litName "on_conflict")
    conflictDesc = "on conflict condition"

-- | Constructs a Parser for the name of the constraints on a given table.
--
-- The TableCoreInfo of a given table contains a list of unique or primary key
-- constraints. Given the list of such constraints, this function creates a
-- parser for an enum type that matches it. This function makes no attempt at
-- de-duplicating contraint names, and assumes they are correct.
--
-- This function can fail if a constraint has a name that cannot be translated
-- to a GraphQL name (see hasura/graphql-engine-mono#1748).
conflictConstraint ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  NonEmpty (Constraint b) ->
  SourceName ->
  TableInfo b ->
  m (Parser 'Both n (ConstraintName b))
conflictConstraint constraints sourceName tableInfo =
  memoizeOn 'conflictConstraint (sourceName, tableName) $ do
    tableGQLName <- getTableGQLName tableInfo
    constraintEnumValues <- for constraints \constraint -> do
      name <- textToName $ toTxt $ _cName constraint
      pure
        ( P.Definition name (Just "unique or primary key constraint") P.EnumValueInfo,
          _cName constraint
        )
    enumName <- P.mkTypename $ tableGQLName <> $$(G.litName "_constraint")
    let enumDesc = G.Description $ "unique or primary key constraints on table " <>> tableName
    pure $ P.enum enumName (Just enumDesc) constraintEnumValues
  where
    tableName = tableInfoName tableInfo
