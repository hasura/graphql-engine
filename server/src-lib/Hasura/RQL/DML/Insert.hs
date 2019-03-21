module Hasura.RQL.DML.Insert where

import           Data.Aeson.Types
import           Instances.TH.Lift        ()

import qualified Data.Aeson.Text          as AT
import qualified Data.HashMap.Strict      as HM
import qualified Data.HashSet             as HS
import qualified Data.Sequence            as DS
import qualified Data.Text.Lazy           as LT

import           Hasura.Prelude
import           Hasura.EncJSON
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Mutation
import           Hasura.RQL.DML.Returning
import           Hasura.RQL.GBoolExp
import           Hasura.RQL.Instances     ()
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Database.PG.Query        as Q
import qualified Hasura.SQL.DML           as S

data ConflictTarget
  = Column ![PGCol]
  | Constraint !ConstraintName
  deriving (Show, Eq)

data ConflictClauseP1
  = CP1DoNothing !(Maybe ConflictTarget)
  | CP1Update !ConflictTarget ![PGCol] !PreSetCols !S.BoolExp
  deriving (Show, Eq)

data InsertQueryP1
  = InsertQueryP1
  { iqp1Table    :: !QualifiedTable
  , iqp1View     :: !QualifiedTable
  , iqp1Cols     :: ![PGCol]
  , iqp1Tuples   :: ![[S.SQLExp]]
  , iqp1Conflict :: !(Maybe ConflictClauseP1)
  , iqp1MutFlds  :: !MutFlds
  , iqp1UniqCols :: !(Maybe [PGColInfo])
  } deriving (Show, Eq)

mkInsertCTE :: InsertQueryP1 -> S.CTE
mkInsertCTE (InsertQueryP1 _ vn cols vals c _ _) =
  S.CTEInsert insert
  where
    insert =
      S.SQLInsert vn cols vals (toSQLConflict <$> c) $ Just S.returningStar

toSQLConflict :: ConflictClauseP1 -> S.SQLConflict
toSQLConflict conflict = case conflict of
  CP1DoNothing Nothing          -> S.DoNothing Nothing
  CP1DoNothing (Just ct)        -> S.DoNothing $ Just $ toSQLCT ct
  CP1Update ct inpCols preSet filtr    -> S.Update (toSQLCT ct)
    (S.buildUpsertSetExp inpCols preSet) $ Just $ S.WhereFrag filtr

  where
    toSQLCT ct = case ct of
      Column pgCols -> S.SQLColumn pgCols
      Constraint cn -> S.SQLConstraint cn

mkDefValMap :: FieldInfoMap -> HM.HashMap PGCol S.SQLExp
mkDefValMap cim =
  HM.fromList $ flip zip (repeat $ S.SEUnsafe "DEFAULT") $
  map (PGCol . getFieldNameTxt) $ HM.keys $ HM.filter isPGColInfo cim

getInsertDeps
  :: InsertQueryP1 -> [SchemaDependency]
getInsertDeps (InsertQueryP1 tn _ _ _ _ mutFlds _) =
  mkParentDep tn : retDeps
  where
    retDeps = map (mkColDep "untyped" tn . fst) $
              pgColsFromMutFlds mutFlds

convObj
  :: (UserInfoM m, QErrM m)
  => (PGColType -> Value -> m S.SQLExp)
  -> HM.HashMap PGCol S.SQLExp
  -> HM.HashMap PGCol S.SQLExp
  -> FieldInfoMap
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
      role <- userRole <$> askUserInfo
      throw400 NotSupported $ "column " <> c <<> " is not insertable"
        <> " for role " <>> role

validateInpCols :: (MonadError QErr m) => [PGCol] -> [PGCol] -> m ()
validateInpCols inpCols updColsPerm = forM_ inpCols $ \inpCol ->
  unless (inpCol `elem` updColsPerm) $ throw400 ValidationFailed $
    "column " <> inpCol <<> " is not updatable"

buildConflictClause
  :: (UserInfoM m, QErrM m)
  => TableInfo
  -> [PGCol]
  -> OnConflict
  -> m ConflictClauseP1
buildConflictClause tableInfo inpCols (OnConflict mTCol mTCons act) =
  case (mTCol, mTCons, act) of
    (Nothing, Nothing, CAIgnore)    -> return $ CP1DoNothing Nothing
    (Just col, Nothing, CAIgnore)   -> do
      validateCols col
      return $ CP1DoNothing $ Just $ Column $ getPGCols col
    (Nothing, Just cons, CAIgnore)  -> do
      validateConstraint cons
      return $ CP1DoNothing $ Just $ Constraint cons
    (Nothing, Nothing, CAUpdate)    -> throw400 UnexpectedPayload
      "Expecting 'constraint' or 'constraint_on' when the 'action' is 'update'"
    (Just col, Nothing, CAUpdate)   -> do
      validateCols col
      (updFiltr, preSet) <- getUpdPerm
      return $ CP1Update (Column $ getPGCols col) inpCols preSet $
        toSQLBool updFiltr
    (Nothing, Just cons, CAUpdate)  -> do
      validateConstraint cons
      (updFiltr, preSet) <- getUpdPerm
      return $ CP1Update (Constraint cons) inpCols preSet $
        toSQLBool updFiltr
    (Just _, Just _, _)             -> throw400 UnexpectedPayload
      "'constraint' and 'constraint_on' cannot be set at a time"
  where
    fieldInfoMap = tiFieldInfoMap tableInfo
    toSQLBool = toSQLBoolExp (S.mkQual $ tiName tableInfo)

    validateCols c = do
      let targetcols = getPGCols c
      void $ withPathK "constraint_on" $ indexedForM targetcols $
        \pgCol -> askPGType fieldInfoMap pgCol ""

    validateConstraint c = do
      let tableConsNames = map tcName $
            tiUniqOrPrimConstraints tableInfo
      withPathK "constraint" $
       unless (c `elem` tableConsNames) $
       throw400 Unexpected $ "constraint " <> getConstraintTxt c
                   <<> " for table " <> tiName tableInfo
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
  -> (PGColType -> Value -> m S.SQLExp)
  -> InsertQuery
  -> m InsertQueryP1
convInsertQuery objsParser prepFn (InsertQuery tableName val oC mRetCols) = do

  insObjs <- objsParser val

  -- Get the current table information
  tableInfo <- askTabInfo tableName

  -- If table is view then check if it is insertable
  mutableView tableName viIsInsertable
    (tiViewInfo tableInfo) "insertable"

  -- Check if the role has insert permissions
  insPerm   <- askInsPermInfo tableInfo

  -- Check if all dependent headers are present
  validateHeaders $ ipiRequiredHeaders insPerm

  let fieldInfoMap = tiFieldInfoMap tableInfo
      setInsVals = ipiSet insPerm
      uniqCols = getUniqCols (getCols fieldInfoMap) $
                 tiUniqOrPrimConstraints tableInfo

  -- convert the returning cols into sql returing exp
  mAnnRetCols <- forM mRetCols $ \retCols -> do
    -- Check if select is allowed only if you specify returning
    selPerm <- modifyErr (<> selNecessaryMsg) $
               askSelPermInfo tableInfo

    withPathK "returning" $ checkRetCols fieldInfoMap selPerm retCols

  let mutFlds = mkDefaultMutFlds mAnnRetCols

  let defInsVals = mkDefValMap fieldInfoMap
      insCols    = HM.keys defInsVals
      insView    = ipiView insPerm

  insTuples <- withPathK "objects" $ indexedForM insObjs $ \obj ->
    convObj prepFn defInsVals setInsVals fieldInfoMap obj
  let sqlExps = map snd insTuples
      inpCols = HS.toList $ HS.fromList $ concatMap fst insTuples

  conflictClause <- withPathK "on_conflict" $ forM oC $ \c -> do
      roleName <- askCurRole
      unless (isTabUpdatable roleName tableInfo) $ throw400 PermissionDenied $
        "upsert is not allowed for role " <> roleName
        <<> " since update permissions are not defined"
      buildConflictClause tableInfo inpCols c

  return $ InsertQueryP1 tableName insView insCols sqlExps
           conflictClause mutFlds uniqCols

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
  :: (QErrM m, UserInfoM m, CacheRM m, HasSQLGenCtx m)
  => InsertQuery
  -> m (InsertQueryP1, DS.Seq Q.PrepArg)
convInsQ =
  liftDMLP1 .
  convInsertQuery (withPathK "objects" . decodeInsObjs) binRHSBuilder

insertP2 :: Bool -> (InsertQueryP1, DS.Seq Q.PrepArg) -> Q.TxE QErr EncJSON
insertP2 strfyNum (u, p) =
  runMutation $ Mutation (iqp1Table u) (insertCTE, p)
                (iqp1MutFlds u) (iqp1UniqCols u) strfyNum
  where
    insertCTE = mkInsertCTE u

data ConflictCtx
  = CCUpdate !ConstraintName ![PGCol] !PreSetCols !S.BoolExp
  | CCDoNothing !(Maybe ConstraintName)
  deriving (Show, Eq)

nonAdminInsert :: Bool -> (InsertQueryP1, DS.Seq Q.PrepArg) -> Q.TxE QErr EncJSON
nonAdminInsert strfyNum (insQueryP1, args) = do
  conflictCtxM <- mapM extractConflictCtx conflictClauseP1
  setConflictCtx conflictCtxM
  insertP2 strfyNum (withoutConflictClause, args)
  where
    withoutConflictClause = insQueryP1{iqp1Conflict=Nothing}
    conflictClauseP1 = iqp1Conflict insQueryP1

extractConflictCtx :: (MonadError QErr m) => ConflictClauseP1 -> m ConflictCtx
extractConflictCtx cp =
  case cp of
    (CP1DoNothing mConflictTar) -> do
      mConstraintName <- mapM extractConstraintName mConflictTar
      return $ CCDoNothing mConstraintName
    (CP1Update conflictTar inpCols preSet filtr) -> do
      constraintName <- extractConstraintName conflictTar
      return $ CCUpdate constraintName inpCols preSet filtr
  where
    extractConstraintName (Constraint cn) = return cn
    extractConstraintName _ = throw400 NotSupported
      "\"constraint_on\" not supported for non admin insert. use \"constraint\" instead"

setConflictCtx :: Maybe ConflictCtx -> Q.TxE QErr ()
setConflictCtx conflictCtxM = do
  let t = maybe "null" conflictCtxToJSON conflictCtxM
      setVal = toSQL $ S.SELit t
      setVar = "SET LOCAL hasura.conflict_clause = "
      q = Q.fromBuilder $ setVar <> setVal
  Q.unitQE defaultTxErrorHandler q () False
  where
    encToText = LT.toStrict . AT.encodeToLazyText

    conflictCtxToJSON (CCDoNothing constrM) =
        encToText $ InsertTxConflictCtx CAIgnore constrM Nothing
    conflictCtxToJSON (CCUpdate constr updCols preSet filtr) =
        encToText $ InsertTxConflictCtx CAUpdate (Just constr) $
        Just $ toSQLTxt (S.buildUpsertSetExp updCols preSet)
               <> " " <> toSQLTxt (S.WhereFrag filtr)

runInsert
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m, HasSQLGenCtx m)
  => InsertQuery
  -> m EncJSON
runInsert q = do
  res <- convInsQ q
  role <- userRole <$> askUserInfo
  strfyNum <- stringifyNum <$> askSQLGenCtx
  liftTx $ bool (nonAdminInsert strfyNum res) (insertP2 strfyNum res) $ isAdmin role
