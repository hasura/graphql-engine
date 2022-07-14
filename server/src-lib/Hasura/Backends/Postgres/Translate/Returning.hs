-- | Postgres Translate Returning
--
-- Combinators and helpers for dealing with GraphQL returning statements.
module Hasura.Backends.Postgres.Translate.Returning
  ( MutationCTE (..),
    getMutationCTE,
    checkPermissionRequired,
    mkMutFldExp,
    mkDefaultMutFlds,
    mkCheckErrorExp,
    mkMutationOutputExp,
    checkConstraintIdentifier,
    asCheckErrorExtractor,
  )
where

import Data.Coerce
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.Translate.Select
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Returning
import Hasura.RQL.IR.Select
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Table
import Hasura.SQL.Backend

-- | The postgres common table expression (CTE) for mutation queries.
-- This CTE expression is used to generate mutation field output expression,
-- see Note [Mutation output expression].
data MutationCTE
  = -- | A Mutation with check constraint validation (Insert or Update)
    MCCheckConstraint !S.CTE
  | -- | A Select statement which emits mutated table rows
    MCSelectValues !S.Select
  | -- | A Delete statement
    MCDelete !S.SQLDelete
  deriving (Show, Eq)

getMutationCTE :: MutationCTE -> S.CTE
getMutationCTE = \case
  MCCheckConstraint cte -> cte
  MCSelectValues select -> S.CTESelect select
  MCDelete delete -> S.CTEDelete delete

checkPermissionRequired :: MutationCTE -> Bool
checkPermissionRequired = \case
  MCCheckConstraint _ -> True
  MCSelectValues _ -> False
  MCDelete _ -> False

pgColsToSelFlds ::
  forall pgKind.
  Backend ('Postgres pgKind) =>
  [ColumnInfo ('Postgres pgKind)] ->
  [(FieldName, AnnField ('Postgres pgKind))]
pgColsToSelFlds cols =
  flip map cols $
    \pgColInfo ->
      ( fromCol @('Postgres pgKind) $ ciColumn pgColInfo,
        mkAnnColumnField (ciColumn pgColInfo) (ciType pgColInfo) Nothing Nothing
        --  ^^ Nothing because mutations aren't supported
        --  with inherited role
      )

mkDefaultMutFlds ::
  Backend ('Postgres pgKind) =>
  Maybe [ColumnInfo ('Postgres pgKind)] ->
  MutationOutput ('Postgres pgKind)
mkDefaultMutFlds =
  MOutMultirowFields . \case
    Nothing -> mutFlds
    Just cols -> ("returning", MRet $ pgColsToSelFlds cols) : mutFlds
  where
    mutFlds = [("affected_rows", MCount)]

mkMutFldExp ::
  ( Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind
  ) =>
  Identifier ->
  Maybe Int ->
  Options.StringifyNumbers ->
  MutFld ('Postgres pgKind) ->
  S.SQLExp
mkMutFldExp cteAlias preCalAffRows strfyNum = \case
  MCount ->
    let countExp =
          S.SESelect $
            S.mkSelect
              { S.selExtr = [S.Extractor S.countStar Nothing],
                S.selFrom = Just $ S.FromExp $ pure $ S.FIIdentifier cteAlias
              }
     in maybe countExp (S.SEUnsafe . tshow) preCalAffRows
  MExp t -> S.SELit t
  MRet selFlds ->
    let tabFrom = FromIdentifier $ toFIIdentifier cteAlias
        tabPerm = TablePerm annBoolExpTrue Nothing
     in S.SESelect $
          mkSQLSelect JASMultipleRows $
            AnnSelectG selFlds tabFrom tabPerm noSelectArgs strfyNum

toFIIdentifier :: Identifier -> FIIdentifier
toFIIdentifier = coerce . getIdenTxt
{-# INLINE toFIIdentifier #-}

{- Note [Mutation output expression]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An example output expression for INSERT mutation:

WITH "<table-name>__mutation_result_alias" AS (
  INSERT INTO <table-name> (<insert-column>[..])
  VALUES
    (<insert-value-row>[..])
    ON CONFLICT ON CONSTRAINT "<table-constraint-name>" DO NOTHING RETURNING *,
    -- An extra column expression which performs the 'CHECK' validation
    (<CHECK Condition>) AS "check__constraint"
),
"<table-name>__all_columns_alias" AS (
  -- Only extract columns from mutated rows. Columns sorted by ordinal position so that
  -- resulted rows can be casted to table type.
  SELECT (<table-column>[..])
  FROM
    "<table-name>__mutation_result_alias"
)
<SELECT statement to generate mutation response using '<table-name>__all_columns_alias' as FROM
 and bool_and("check__constraint") from "<table-name>__mutation_result_alias">
-}

-- | Generate mutation output expression with given mutation CTE statement.
-- See Note [Mutation output expression].
mkMutationOutputExp ::
  ( Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind
  ) =>
  QualifiedTable ->
  [ColumnInfo ('Postgres pgKind)] ->
  Maybe Int ->
  MutationCTE ->
  MutationOutput ('Postgres pgKind) ->
  Options.StringifyNumbers ->
  S.SelectWith
mkMutationOutputExp qt allCols preCalAffRows cte mutOutput strfyNum =
  S.SelectWith
    [ (S.Alias mutationResultAlias, getMutationCTE cte),
      (S.Alias allColumnsAlias, allColumnsSelect)
    ]
    sel
  where
    mutationResultAlias = Identifier $ snakeCaseQualifiedObject qt <> "__mutation_result_alias"
    allColumnsAlias = Identifier $ snakeCaseQualifiedObject qt <> "__all_columns_alias"
    allColumnsSelect =
      S.CTESelect $
        S.mkSelect
          { S.selExtr = map (S.mkExtr . ciColumn) (sortCols allCols),
            S.selFrom = Just $ S.mkIdenFromExp mutationResultAlias
          }

    sel =
      S.mkSelect
        { S.selExtr =
            S.Extractor extrExp Nothing :
            bool [] [S.Extractor checkErrorExp Nothing] (checkPermissionRequired cte)
        }
      where
        checkErrorExp = mkCheckErrorExp mutationResultAlias
        extrExp = case mutOutput of
          MOutMultirowFields mutFlds ->
            let jsonBuildObjArgs = flip concatMap mutFlds $
                  \(FieldName k, mutFld) ->
                    [ S.SELit k,
                      mkMutFldExp allColumnsAlias preCalAffRows strfyNum mutFld
                    ]
             in S.SEFnApp "json_build_object" jsonBuildObjArgs Nothing
          MOutSinglerowObject annFlds ->
            let tabFrom = FromIdentifier $ toFIIdentifier allColumnsAlias
                tabPerm = TablePerm annBoolExpTrue Nothing
             in S.SESelect $
                  mkSQLSelect JASSingleObject $
                    AnnSelectG annFlds tabFrom tabPerm noSelectArgs strfyNum

mkCheckErrorExp :: IsIdentifier a => a -> S.SQLExp
mkCheckErrorExp alias =
  let boolAndCheckConstraint =
        S.handleIfNull (S.SEBool $ S.BELit True) $
          S.SEFnApp "bool_and" [S.SEIdentifier checkConstraintIdentifier] Nothing
   in S.SESelect $
        S.mkSelect
          { S.selExtr = [S.Extractor boolAndCheckConstraint Nothing],
            S.selFrom = Just $ S.mkIdenFromExp alias
          }

checkConstraintIdentifier :: Identifier
checkConstraintIdentifier = Identifier "check__constraint"

asCheckErrorExtractor :: S.SQLExp -> S.Extractor
asCheckErrorExtractor s =
  S.Extractor s $ Just $ S.Alias checkConstraintIdentifier
