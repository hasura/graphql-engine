module Hasura.RQL.DML.Insert where

import           Data.Aeson.Types
import           Instances.TH.Lift        ()

import qualified Data.HashMap.Strict      as HM
import qualified Data.HashSet             as HS
import qualified Data.Sequence            as DS

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Mutation
import           Hasura.RQL.DML.Returning
import           Hasura.RQL.GBoolExp
import           Hasura.RQL.Instances     ()
import           Hasura.RQL.Types
import           Hasura.Server.Version    (HasVersion)
import           Hasura.Session
import           Hasura.SQL.Types

import qualified Database.PG.Query        as Q
import qualified Hasura.SQL.DML           as S

data ConflictTarget
  = CTColumn ![PGCol]
  | CTConstraint !ConstraintName
  deriving (Show, Eq)

data ConflictClauseP1
  = CP1DoNothing !(Maybe ConflictTarget)
  | CP1Update !ConflictTarget ![PGCol] !PreSetCols !S.BoolExp
  deriving (Show, Eq)

data InsertQueryP1
  = InsertQueryP1
  { iqp1Table     :: !QualifiedTable
  , iqp1Cols      :: ![PGCol]
  , iqp1Tuples    :: ![[S.SQLExp]]
  , iqp1Conflict  :: !(Maybe ConflictClauseP1)
  , iqp1CheckCond :: !(AnnBoolExpSQL, Maybe AnnBoolExpSQL)
  , iqp1Output    :: !MutationOutput
  , iqp1AllCols   :: ![PGColumnInfo]
  } deriving (Show, Eq)

mkInsertCTE :: InsertQueryP1 -> S.CTE
mkInsertCTE (InsertQueryP1 tn cols vals c (insCheck, updCheck) _ _) =
    S.CTEInsert insert
  where
    tupVals = S.ValuesExp $ map S.TupleExp vals
    insert =
      S.SQLInsert tn cols tupVals (toSQLConflict <$> c)
        . Just
        . S.RetExp
        $ [ S.selectStar
          , S.Extractor
              (insertOrUpdateCheckExpr tn c
                (toSQLBoolExp (S.QualTable tn) insCheck)
                (fmap (toSQLBoolExp (S.QualTable tn)) updCheck))
              Nothing
          ]

toSQLConflict :: ConflictClauseP1 -> S.SQLConflict
toSQLConflict conflict = case conflict of
  CP1DoNothing Nothing          -> S.DoNothing Nothing
  CP1DoNothing (Just ct)        -> S.DoNothing $ Just $ toSQLCT ct
  CP1Update ct inpCols preSet filtr    -> S.Update (toSQLCT ct)
    (S.buildUpsertSetExp inpCols preSet) $ Just $ S.WhereFrag filtr

  where
    toSQLCT ct = case ct of
      CTColumn pgCols -> S.SQLColumn pgCols
      CTConstraint cn -> S.SQLConstraint cn

convObj
  :: (UserInfoM m, QErrM m)
  => (PGColumnType -> Value -> m S.SQLExp)
  -> HM.HashMap PGCol S.SQLExp
  -> HM.HashMap PGCol S.SQLExp
  -> FieldInfoMap FieldInfo
  -> InsObj
  -> m ([PGCol], [S.SQLExp])
convObj prepFn defInsVals setInsVals fieldInfoMap insObj = do
  inpInsVals <- flip HM.traverseWithKey insObj $ \c val -> do
    let relWhenPGErr = "relationships can't be inserted"
    colType <- askPGType fieldInfoMap c relWhenPGErr
    -- if column has predefined value then throw error
    when (c `elem` preSetCols) $ throwNotInsErr c
    -- Encode aeson's value into prepared value
    withPathK (getPGColTxt c) $ prepFn colType val
  let insVals = HM.union setInsVals inpInsVals
      sqlExps = HM.elems $ HM.union insVals defInsVals
      inpCols = HM.keys inpInsVals

  return (inpCols, sqlExps)
  where
    preSetCols = HM.keys setInsVals

    throwNotInsErr c = do
      roleName <- _uiRole <$> askUserInfo
      throw400 NotSupported $ "column " <> c <<> " is not insertable"
        <> " for role " <>> roleName

validateInpCols :: (MonadError QErr m) => [PGCol] -> [PGCol] -> m ()
validateInpCols inpCols updColsPerm = forM_ inpCols $ \inpCol ->
  unless (inpCol `elem` updColsPerm) $ throw400 ValidationFailed $
    "column " <> inpCol <<> " is not updatable"

buildConflictClause
  :: (UserInfoM m, QErrM m)
  => SessVarBldr m
  -> TableInfo
  -> [PGCol]
  -> OnConflict
  -> m ConflictClauseP1
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
      return $ CP1Update (CTColumn $ getPGCols col) inpCols resolvedPreSet $
        toSQLBool resolvedUpdFltr
    (Nothing, Just cons, CAUpdate)  -> do
      validateConstraint cons
      (updFltr, preSet) <- getUpdPerm
      resolvedUpdFltr <- convAnnBoolExpPartialSQL sessVarBldr updFltr
      resolvedPreSet <- mapM (convPartialSQLExp sessVarBldr) preSet
      return $ CP1Update (CTConstraint cons) inpCols resolvedPreSet $
        toSQLBool resolvedUpdFltr
    (Just _, Just _, _)             -> throw400 UnexpectedPayload
      "'constraint' and 'constraint_on' cannot be set at a time"
  where
    coreInfo = _tiCoreInfo tableInfo
    fieldInfoMap = _tciFieldInfoMap coreInfo
    toSQLBool = toSQLBoolExp (S.mkQual $ _tciName coreInfo)

    validateCols c = do
      let targetcols = getPGCols c
      void $ withPathK "constraint_on" $ indexedForM targetcols $
        \pgCol -> askPGType fieldInfoMap pgCol ""

    validateConstraint c = do
      let tableConsNames = _cName <$> tciUniqueOrPrimaryKeyConstraints coreInfo
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


convInsertQuery
  :: (UserInfoM m, QErrM m, CacheRM m)
  => (Value -> m [InsObj])
  -> SessVarBldr m
  -> (PGColumnType -> Value -> m S.SQLExp)
  -> InsertQuery
  -> m InsertQueryP1
convInsertQuery objsParser sessVarBldr prepFn (InsertQuery tableName val oC mRetCols) = do

  insObjs <- objsParser val

  -- Get the current table information
  tableInfo <- askTabInfo tableName
  let coreInfo = _tiCoreInfo tableInfo

  -- If table is view then check if it is insertable
  mutableView tableName viIsInsertable
    (_tciViewInfo coreInfo) "insertable"

  -- Check if the role has insert permissions
  insPerm   <- askInsPermInfo tableInfo
  updPerm   <- askPermInfo' PAUpdate tableInfo

  -- Check if all dependent headers are present
  validateHeaders $ ipiRequiredHeaders insPerm

  let fieldInfoMap = _tciFieldInfoMap coreInfo
      setInsVals = ipiSet insPerm

  -- convert the returning cols into sql returing exp
  mAnnRetCols <- forM mRetCols $ \retCols -> do
    -- Check if select is allowed only if you specify returning
    selPerm <- modifyErr (<> selNecessaryMsg) $
               askSelPermInfo tableInfo

    withPathK "returning" $ checkRetCols fieldInfoMap selPerm retCols

  let mutOutput = mkDefaultMutFlds mAnnRetCols

  let defInsVals = S.mkColDefValMap $
                   map pgiColumn $ getCols fieldInfoMap
      allCols    = getCols fieldInfoMap
      insCols    = HM.keys defInsVals

  resolvedPreSet <- mapM (convPartialSQLExp sessVarBldr) setInsVals

  insTuples <- withPathK "objects" $ indexedForM insObjs $ \obj ->
    convObj prepFn defInsVals resolvedPreSet fieldInfoMap obj
  let sqlExps = map snd insTuples
      inpCols = HS.toList $ HS.fromList $ concatMap fst insTuples

  insCheck <- convAnnBoolExpPartialSQL sessVarFromCurrentSetting (ipiCheck insPerm)
  updCheck <- traverse (convAnnBoolExpPartialSQL sessVarFromCurrentSetting) (upiCheck =<< updPerm)

  conflictClause <- withPathK "on_conflict" $ forM oC $ \c -> do
      roleName <- askCurRole
      unless (isTabUpdatable roleName tableInfo) $ throw400 PermissionDenied $
        "upsert is not allowed for role " <> roleName
        <<> " since update permissions are not defined"

      buildConflictClause sessVarBldr tableInfo inpCols c
  return $ InsertQueryP1 tableName insCols sqlExps
           conflictClause (insCheck, updCheck) mutOutput allCols
  where
    selNecessaryMsg =
      "; \"returning\" can only be used if the role has "
      <> "\"select\" permission on the table"

decodeInsObjs :: (UserInfoM m, QErrM m) => Value -> m [InsObj]
decodeInsObjs v = do
  objs <- decodeValue v
  when (null objs) $ throw400 UnexpectedPayload "objects should not be empty"
  return objs

convInsQ
  :: (QErrM m, UserInfoM m, CacheRM m)
  => InsertQuery
  -> m (InsertQueryP1, DS.Seq Q.PrepArg)
convInsQ =
  runDMLP1T .
  convInsertQuery (withPathK "objects" . decodeInsObjs)
  sessVarFromCurrentSetting
  binRHSBuilder

execInsertQuery
  :: (HasVersion, MonadTx m, MonadIO m)
  => Bool
  -> Maybe MutationRemoteJoinCtx
  -> (InsertQueryP1, DS.Seq Q.PrepArg) -> m EncJSON
execInsertQuery strfyNum remoteJoinCtx (u, p) =
  runMutation $ mkMutation remoteJoinCtx (iqp1Table u) (insertCTE, p)
                (iqp1Output u) (iqp1AllCols u) strfyNum
  where
    insertCTE = mkInsertCTE u

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
  -> Maybe ConflictClauseP1
  -> S.BoolExp
  -> Maybe S.BoolExp
  -> S.SQLExp
insertOrUpdateCheckExpr qt (Just _conflict) insCheck (Just updCheck) =
  S.SECond
    (S.BECompare
      S.SEQ
      (S.SEQIden (S.QIden (S.mkQual qt) (Iden "xmax")))
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

runInsert
  :: ( HasVersion, QErrM m, UserInfoM m
     , CacheRM m, MonadTx m, HasSQLGenCtx m, MonadIO m
     )
  => InsertQuery -> m EncJSON
runInsert q = do
  res <- convInsQ q
  strfyNum <- stringifyNum <$> askSQLGenCtx
  execInsertQuery strfyNum Nothing res
