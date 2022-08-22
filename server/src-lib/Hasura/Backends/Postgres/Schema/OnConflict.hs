{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE PatternGuards #-}
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

import Data.Has (getter)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Text.Extended
import Hasura.Backends.Postgres.SQL.Types (showPGCols)
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.BoolExp
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Parser
  ( InputFieldsParser,
    Kind (..),
    Parser,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Table
import Hasura.GraphQL.Schema.Typename (mkTypename)
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp qualified as IR
import Hasura.RQL.IR.Insert qualified as IR
import Hasura.RQL.IR.Value qualified as IR
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
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
-- must still accept an empty list for @update_columns@ to support the "ON
-- CONFLICT DO NOTHING" case. We do this by adding a placeholder value to the
-- enum. See <https://github.com/hasura/graphql-engine/issues/6804>.
onConflictFieldParser ::
  forall pgKind r m n.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  SourceInfo ('Postgres pgKind) ->
  TableInfo ('Postgres pgKind) ->
  m (InputFieldsParser n (Maybe (IR.OnConflictClause ('Postgres pgKind) (IR.UnpreparedValue ('Postgres pgKind)))))
onConflictFieldParser sourceInfo tableInfo = do
  tCase <- asks getter
  roleName <- retrieve scRole
  let permissions = getRolePermInfo roleName tableInfo
      maybeConstraints = tciUniqueOrPrimaryKeyConstraints . _tiCoreInfo $ tableInfo
      maybeConflictObject = conflictObjectParser sourceInfo tableInfo (_permUpd permissions) <$> maybeConstraints
  case maybeConflictObject of
    Just conflictObject -> conflictObject <&> P.fieldOptional (applyFieldNameCaseCust tCase Name._on_conflict) (Just "upsert condition")
    Nothing -> return $ pure Nothing

-- | Create a parser for the @_on_conflict@ object of the given table.
conflictObjectParser ::
  forall pgKind r m n.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  SourceInfo ('Postgres pgKind) ->
  TableInfo ('Postgres pgKind) ->
  Maybe (UpdPermInfo ('Postgres pgKind)) ->
  NonEmpty (UniqueConstraint ('Postgres pgKind)) ->
  m (Parser 'Input n (IR.OnConflictClause ('Postgres pgKind) (IR.UnpreparedValue ('Postgres pgKind))))
conflictObjectParser sourceInfo tableInfo maybeUpdatePerms constraints = do
  tCase <- asks getter
  updateColumnsEnum <- updateColumnsPlaceholderParser tableInfo
  constraintParser <- conflictConstraint constraints sourceInfo tableInfo
  whereExpParser <- boolExp sourceInfo tableInfo
  tableGQLName <- getTableIdentifierName tableInfo
  objectName <- mkTypename $ applyTypeNameCaseIdentifier tCase $ mkOnConflictTypeName tableGQLName
  let objectDesc = G.Description $ "on_conflict condition type for table " <>> tableName
      (presetColumns, updateFilter) = fromMaybe (HM.empty, IR.gBoolExpTrue) $ do
        UpdPermInfo {..} <- maybeUpdatePerms
        pure
          ( partialSQLExpToUnpreparedValue <$> upiSet,
            fmap partialSQLExpToUnpreparedValue <$> upiFilter
          )

  pure $
    P.object objectName (Just objectDesc) $ do
      constraintField <- P.field Name._constraint Nothing constraintParser
      let updateColumnsField = P.fieldWithDefault Name._update_columns Nothing (G.VList []) (P.list updateColumnsEnum)

      whereExp <- P.fieldOptional Name._where Nothing whereExpParser

      updateColumns <-
        updateColumnsField `P.bindFields` \updateColumnsMaybe ->
          onNothing
            (sequenceA @[] @Maybe updateColumnsMaybe)
            -- this can only happen if the placeholder was used
            (parseError "erroneous column name")

      pure $
        let UniqueConstraint (Constraint {_cName}) _ = constraintField
            constraintTarget = IR.CTConstraint _cName
         in case updateColumns of
              [] -> IR.OCCDoNothing $ Just constraintTarget
              _ ->
                IR.OCCUpdate $
                  IR.OnConflictClauseData constraintTarget updateColumns presetColumns $
                    IR.BoolAnd $ updateFilter : maybeToList whereExp
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
  NonEmpty (UniqueConstraint ('Postgres pgKind)) ->
  SourceInfo ('Postgres pgKind) ->
  TableInfo ('Postgres pgKind) ->
  m (Parser 'Both n (UniqueConstraint ('Postgres pgKind)))
conflictConstraint constraints sourceInfo tableInfo =
  P.memoizeOn 'conflictConstraint (_siName sourceInfo, tableName) $ do
    tCase <- asks getter
    tableGQLName <- getTableIdentifierName tableInfo
    constraintEnumValues <- for
      constraints
      \c@(UniqueConstraint (Constraint {_cName}) cCols) -> do
        name <- textToName $ toTxt $ _cName
        pure
          ( P.Definition
              name
              (Just $ "unique or primary key constraint on columns " <> coerce (showPGCols (HS.toList cCols)))
              Nothing
              []
              P.EnumValueInfo,
            c
          )
    enumName <- mkTypename $ applyTypeNameCaseIdentifier tCase $ mkTableConstraintTypeName tableGQLName
    let enumDesc = G.Description $ "unique or primary key constraints on table " <>> tableName
    pure $ P.enum enumName (Just enumDesc) constraintEnumValues
  where
    tableName = tableInfoName tableInfo
