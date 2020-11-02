module Hasura.Backends.Postgres.Translate.Insert
 ( mkInsertCTE
 , insertCheckExpr
 , buildConflictClause
 , toSQLConflict
 , insertOrUpdateCheckExpr
 ) where

import           Hasura.Prelude

import qualified Data.HashSet                               as HS

import           Data.Text.Extended
import           Instances.TH.Lift                          ()

import qualified Hasura.Backends.Postgres.SQL.DML           as S

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Backends.Postgres.Translate.BoolExp
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.IR.Insert
import           Hasura.RQL.Types


mkInsertCTE :: InsertQueryP1 'Postgres -> S.CTE
mkInsertCTE (InsertQueryP1 tn cols vals conflict (insCheck, updCheck) _ _) =
    S.CTEInsert insert
  where
    tupVals = S.ValuesExp $ map S.TupleExp vals
    insert =
      S.SQLInsert tn cols tupVals (toSQLConflict tn <$> conflict)
        . Just
        . S.RetExp
        $ [ S.selectStar
          , S.Extractor
              (insertOrUpdateCheckExpr tn conflict
                (toSQLBool insCheck)
                (fmap toSQLBool updCheck))
              Nothing
          ]
    toSQLBool = toSQLBoolExp $ S.QualTable tn


toSQLConflict :: QualifiedTable -> ConflictClauseP1 'Postgres S.SQLExp -> S.SQLConflict
toSQLConflict tableName = \case
  CP1DoNothing ct -> S.DoNothing $ toSQLCT <$> ct
  CP1Update ct inpCols preSet filtr -> S.Update
    (toSQLCT ct) (S.buildUpsertSetExp inpCols preSet) $
    Just $ S.WhereFrag $ toSQLBoolExp (S.QualTable tableName) filtr
  where
    toSQLCT ct = case ct of
      CTColumn pgCols -> S.SQLColumn pgCols
      CTConstraint cn -> S.SQLConstraint cn


validateInpCols :: (MonadError QErr m) => [PGCol] -> [PGCol] -> m ()
validateInpCols inpCols updColsPerm = forM_ inpCols $ \inpCol ->
  unless (inpCol `elem` updColsPerm) $ throw400 ValidationFailed $
    "column " <> inpCol <<> " is not updatable"

buildConflictClause
  :: (UserInfoM m, QErrM m)
  => SessVarBldr 'Postgres m
  -> TableInfo 'Postgres
  -> [PGCol]
  -> OnConflict
  -> m (ConflictClauseP1 'Postgres S.SQLExp)
buildConflictClause sessVarBldr tableInfo inpCols (OnConflict mTCol mTCons act) =
  case (mTCol, mTCons, act) of
    (Nothing, Nothing, CAIgnore)    -> return $ CP1DoNothing Nothing
    (Just col, Nothing, CAIgnore)   -> do
      validateCols col
      return $ CP1DoNothing $ Just $ CTColumn $ getPGCols col
    (Nothing, Just cons, CAIgnore)  -> do
      validateConstraint cons
      return $ CP1DoNothing $ Just $ CTConstraint cons
    (Nothing, Nothing, CAUpdate)    -> throw400 UnexpectedPayload
      "Expecting 'constraint' or 'constraint_on' when the 'action' is 'update'"
    (Just col, Nothing, CAUpdate)   -> do
      validateCols col
      (updFltr, preSet) <- getUpdPerm
      resolvedUpdFltr <- convAnnBoolExpPartialSQL sessVarBldr updFltr
      resolvedPreSet <- mapM (convPartialSQLExp sessVarBldr) preSet
      return $ CP1Update (CTColumn $ getPGCols col) inpCols resolvedPreSet resolvedUpdFltr
    (Nothing, Just cons, CAUpdate)  -> do
      validateConstraint cons
      (updFltr, preSet) <- getUpdPerm
      resolvedUpdFltr <- convAnnBoolExpPartialSQL sessVarBldr updFltr
      resolvedPreSet <- mapM (convPartialSQLExp sessVarBldr) preSet
      return $ CP1Update (CTConstraint cons) inpCols resolvedPreSet resolvedUpdFltr
    (Just _, Just _, _)             -> throw400 UnexpectedPayload
      "'constraint' and 'constraint_on' cannot be set at a time"
  where
    coreInfo = _tiCoreInfo tableInfo
    fieldInfoMap = _tciFieldInfoMap coreInfo
    -- toSQLBool = toSQLBoolExp (S.mkQual $ _tciName coreInfo)

    validateCols c = do
      let targetcols = getPGCols c
      void $ withPathK "constraint_on" $ indexedForM targetcols $
        \pgCol -> askPGType fieldInfoMap pgCol ""

    validateConstraint c = do
      let tableConsNames = maybe [] toList $
                           fmap _cName <$> tciUniqueOrPrimaryKeyConstraints coreInfo
      withPathK "constraint" $
       unless (c `elem` tableConsNames) $
       throw400 Unexpected $ "constraint " <> getConstraintTxt c
                   <<> " for table " <> _tciName coreInfo
                   <<> " does not exist"

    getUpdPerm = do
      upi <- askUpdPermInfo tableInfo
      let updFiltr = upiFilter upi
          preSet = upiSet upi
          updCols = HS.toList $ upiCols upi
      validateInpCols inpCols updCols
      return (updFiltr, preSet)

-- | Create an expression which will fail with a check constraint violation error
-- if the condition is not met on any of the inserted rows.
--
-- The resulting SQL will look something like this:
--
-- > INSERT INTO
-- >   ...
-- > RETURNING
-- >   *,
-- >   CASE WHEN {cond}
-- >     THEN NULL
-- >     ELSE hdb_catalog.check_violation('insert check constraint failed')
-- >   END
insertCheckExpr :: Text -> S.BoolExp -> S.SQLExp
insertCheckExpr errorMessage condExpr =
  S.SECond condExpr S.SENull
    (S.SEFunction
      (S.FunctionExp
        (QualifiedObject (SchemaName "hdb_catalog") (FunctionName "check_violation"))
        (S.FunctionArgs [S.SELit errorMessage] mempty)
        Nothing)
    )

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
-- >     THEN CASE WHEN {insert_cond}
-- >            THEN NULL
-- >            ELSE hdb_catalog.check_violation('insert check constraint failed')
-- >          END
-- >     ELSE CASE WHEN {update_cond}
-- >            THEN NULL
-- >            ELSE hdb_catalog.check_violation('update check constraint failed')
-- >          END
-- >   END
--
-- See @https://stackoverflow.com/q/34762732@ for more information on the use of
-- the @xmax@ system column.
insertOrUpdateCheckExpr
  :: QualifiedTable
  -> Maybe (ConflictClauseP1 'Postgres S.SQLExp)
  -> S.BoolExp
  -> Maybe S.BoolExp
  -> S.SQLExp
insertOrUpdateCheckExpr qt (Just _conflict) insCheck (Just updCheck) =
  S.SECond
    (S.BECompare
      S.SEQ
      (S.SEQIdentifier (S.QIdentifier (S.mkQual qt) (Identifier "xmax")))
      (S.SEUnsafe "0"))
    (insertCheckExpr "insert check constraint failed" insCheck)
    (insertCheckExpr "update check constraint failed" updCheck)
insertOrUpdateCheckExpr _ _ insCheck _ =
  -- If we won't generate an ON CONFLICT clause, there is no point
  -- in testing xmax. In particular, views don't provide the xmax
  -- system column, but we don't provide ON CONFLICT for views,
  -- even if they are auto-updatable, so we can fortunately avoid
  -- having to test the non-existent xmax value.
  --
  -- Alternatively, if there is no update check constraint, we should
  -- use the insert check constraint, for backwards compatibility.
  insertCheckExpr "insert check constraint failed" insCheck
