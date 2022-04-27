{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Postgres Schema OnConflict
--
-- This module contains the building blocks for parsing @on_conflict@ clauses,
-- which in the Postgres backend are used to implement upsert functionality.
-- These are used by 'Hasura.Backends.Postgres.Instances.Schema.backendInsertParser' to
-- construct a postgres-specific schema parser for insert (and upsert) mutations.
module Hasura.Backends.Postgres.Schema.OnConflict
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
import Hasura.GraphQL.Parser.Constants qualified as G
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.BoolExp
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Table
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Insert qualified as IR
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.Table
import Hasura.SQL.Backend
import Language.GraphQL.Draft.Syntax qualified as G

-- | Parser for a field name @on_conflict@ of type @tablename_on_conflict@.
--
-- The @tablename_on_conflict@ object is used to generate the @ON CONFLICT@
-- SQL clause, indicating what should be done if an insert raises a conflict.
--
-- The types ordinarily produced by this parser are only created if the table has
-- unique or primary keys constraints.
--
-- If there are no columns for which the current role has update permissions, we
-- must still accept an empty list for @update_columns@ in the name of
-- backwards compatibility. We do this by adding a placeholder value to the
-- enum. See <https://github.com/hasura/graphql-engine/issues/6804>.
onConflictFieldParser ::
  forall pgKind r m n.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  SourceName ->
  TableInfo ('Postgres pgKind) ->
  m (InputFieldsParser n (Maybe (IR.OnConflictClause ('Postgres pgKind) (UnpreparedValue ('Postgres pgKind)))))
onConflictFieldParser sourceName tableInfo = do
  updatePerms <- _permUpd <$> tablePermissions tableInfo
  let maybeConstraints = tciUniqueOrPrimaryKeyConstraints . _tiCoreInfo $ tableInfo
  let maybeConflictObject = conflictObjectParser sourceName tableInfo <$> maybeConstraints <*> updatePerms
  case maybeConflictObject of
    Just conflictObject -> conflictObject <&> P.fieldOptional G._on_conflict (Just "upsert condition")
    Nothing -> return $ pure Nothing

-- | Create a parser for the @_on_conflict@ object of the given table.
conflictObjectParser ::
  forall pgKind r m n.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  SourceName ->
  TableInfo ('Postgres pgKind) ->
  NonEmpty (Constraint ('Postgres pgKind)) ->
  UpdPermInfo ('Postgres pgKind) ->
  m (Parser 'Input n (IR.OnConflictClause ('Postgres pgKind) (UnpreparedValue ('Postgres pgKind))))
conflictObjectParser sourceName tableInfo constraints updatePerms = do
  updateColumnsEnum <- updateColumnsPlaceholderParser tableInfo
  constraintParser <- conflictConstraint constraints sourceName tableInfo
  whereExpParser <- boolExp sourceName tableInfo
  tableGQLName <- getTableGQLName tableInfo
  objectName <- P.mkTypename $ tableGQLName <> G.__on_conflict

  let presetColumns = partialSQLExpToUnpreparedValue <$> upiSet updatePerms
      updateFilter = fmap partialSQLExpToUnpreparedValue <$> upiFilter updatePerms
      objectDesc = G.Description $ "on_conflict condition type for table " <>> tableName
      constraintName = G._constraint
      columnsName = G._update_columns
      whereExpName = G._where

  pure $
    P.object objectName (Just objectDesc) $ do
      constraint <- IR.CTConstraint <$> P.field constraintName Nothing constraintParser
      whereExp <- P.fieldOptional whereExpName Nothing whereExpParser
      updateColumns <-
        P.fieldWithDefault columnsName Nothing (G.VList []) (P.list updateColumnsEnum) `P.bindFields` \cs ->
          -- this can only happen if the placeholder was used
          sequenceA cs `onNothing` parseError "erroneous column name"
      pure $
        case updateColumns of
          [] -> IR.OCCDoNothing $ Just constraint
          _ -> IR.OCCUpdate $ IR.OnConflictClauseData constraint updateColumns presetColumns $ BoolAnd $ updateFilter : maybeToList whereExp
  where
    tableName = tableInfoName tableInfo

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
  forall pgKind r m n.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  NonEmpty (Constraint ('Postgres pgKind)) ->
  SourceName ->
  TableInfo ('Postgres pgKind) ->
  m (Parser 'Both n (ConstraintName ('Postgres pgKind)))
conflictConstraint constraints sourceName tableInfo =
  memoizeOn 'conflictConstraint (sourceName, tableName) $ do
    tableGQLName <- getTableGQLName tableInfo
    constraintEnumValues <- for constraints \constraint -> do
      name <- textToName $ toTxt $ _cName constraint
      pure
        ( P.Definition name (Just "unique or primary key constraint") P.EnumValueInfo,
          _cName constraint
        )
    enumName <- P.mkTypename $ tableGQLName <> G.__constraint
    let enumDesc = G.Description $ "unique or primary key constraints on table " <>> tableName
    pure $ P.enum enumName (Just enumDesc) constraintEnumValues
  where
    tableName = tableInfoName tableInfo
