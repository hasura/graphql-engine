-- | Postgres Translate Insert
--
-- Translates IR inserts to Postgres-specific SQL INSERT statements.
module Hasura.Backends.Postgres.Translate.Insert
  ( mkInsertCTE,
    toSQLConflict,
    insertCheckConstraint,
    insertOrUpdateCheckExpr,
  )
where

import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.Translate.BoolExp
import Hasura.Backends.Postgres.Translate.Returning
import Hasura.Prelude
import Hasura.RQL.IR.Insert
import Hasura.RQL.Types.Backend
import Hasura.SQL.Backend

mkInsertCTE ::
  Backend ('Postgres pgKind) =>
  InsertQueryP1 ('Postgres pgKind) ->
  S.CTE
mkInsertCTE (InsertQueryP1 tn cols vals conflict (insCheck, updCheck) _ _) =
  S.CTEInsert insert
  where
    tupVals = S.ValuesExp $ map S.TupleExp vals
    insert =
      S.SQLInsert tn cols tupVals (toSQLConflict tn <$> conflict)
        . Just
        . S.RetExp
        $ [ S.selectStar,
            insertOrUpdateCheckExpr
              tn
              conflict
              (toSQLBool insCheck)
              (fmap toSQLBool updCheck)
          ]
    toSQLBool = toSQLBoolExp $ S.QualTable tn

toSQLConflict ::
  Backend ('Postgres pgKind) =>
  QualifiedTable ->
  OnConflictClause ('Postgres pgKind) S.SQLExp ->
  S.SQLConflict
toSQLConflict tableName = \case
  OCCDoNothing ct -> S.DoNothing $ toSQLCT <$> ct
  OCCUpdate OnConflictClauseData {..} ->
    S.Update
      (toSQLCT cp1udConflictTarget)
      (S.buildUpsertSetExp cp1udAffectedColumns cp1udValues)
      $ Just $ S.WhereFrag $ toSQLBoolExp (S.QualTable tableName) cp1udFilter
  where
    toSQLCT ct = case ct of
      CTColumn pgCols -> S.SQLColumn pgCols
      CTConstraint cn -> S.SQLConstraint cn

-- | Annotates the check constraint expression with @boolean@
-- (<check-condition>)::boolean
insertCheckConstraint :: S.BoolExp -> S.SQLExp
insertCheckConstraint boolExp =
  S.SETyAnn (S.SEBool boolExp) S.boolTypeAnn

-- | When inserting data, we might need to also enforce the update
-- check condition, because we might fall back to an update via an
-- @ON CONFLICT@ clause.
--
-- We generate something which looks like
--
-- > INSERT INTO
-- >   ...
-- > ON CONFLICT DO UPDATE SET
-- >   ...
-- > RETURNING
-- >   *,
-- >   CASE WHEN xmax = 0
-- >     THEN {insert_cond}
-- >     ELSE {update_cond}
-- >   END
-- >     AS "check__constraint"
--
-- See @https://stackoverflow.com/q/34762732@ for more information on the use of
-- the @xmax@ system column.
insertOrUpdateCheckExpr ::
  QualifiedTable ->
  Maybe (OnConflictClause ('Postgres pgKind) S.SQLExp) ->
  S.BoolExp ->
  Maybe S.BoolExp ->
  S.Extractor
insertOrUpdateCheckExpr qt (Just _conflict) insCheck (Just updCheck) =
  asCheckErrorExtractor $
    S.SECond
      ( S.BECompare
          S.SEQ
          (S.SEQIdentifier (S.QIdentifier (S.mkQual qt) (Identifier "xmax")))
          (S.SEUnsafe "0")
      )
      (insertCheckConstraint insCheck)
      (insertCheckConstraint updCheck)
insertOrUpdateCheckExpr _ _ insCheck _ =
  -- If we won't generate an ON CONFLICT clause, there is no point
  -- in testing xmax. In particular, views don't provide the xmax
  -- system column, but we don't provide ON CONFLICT for views,
  -- even if they are auto-updatable, so we can fortunately avoid
  -- having to test the non-existent xmax value.
  --
  -- Alternatively, if there is no update check constraint, we should
  -- use the insert check constraint, for backwards compatibility.
  asCheckErrorExtractor $ insertCheckConstraint insCheck
