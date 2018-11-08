{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Hasura.RQL.DML.Insert where

import           Data.Aeson.Types
import           Instances.TH.Lift        ()

import qualified Data.Aeson.Text          as AT
import qualified Data.HashMap.Strict      as HM
import qualified Data.HashSet             as HS
import qualified Data.Sequence            as DS
import qualified Data.Text.Lazy           as LT

import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Returning
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
  | CP1Update !ConflictTarget ![PGCol]
  deriving (Show, Eq)

data InsertQueryP1
  = InsertQueryP1
  { iqp1Table    :: !QualifiedTable
  , iqp1View     :: !QualifiedTable
  , iqp1Cols     :: ![PGCol]
  , iqp1Tuples   :: ![[S.SQLExp]]
  , iqp1Conflict :: !(Maybe ConflictClauseP1)
  , iqp1MutFlds  :: !MutFlds
  } deriving (Show, Eq)

mkSQLInsert :: InsertQueryP1 -> S.SelectWith
mkSQLInsert (InsertQueryP1 tn vn cols vals c mutFlds) =
  mkSelWith tn (S.CTEInsert insert) mutFlds False
  where
    insert =
      S.SQLInsert vn cols vals (toSQLConflict <$> c) $ Just S.returningStar

toSQLConflict :: ConflictClauseP1 -> S.SQLConflict
toSQLConflict conflict = case conflict of
  (CP1DoNothing Nothing)   -> S.DoNothing Nothing
  (CP1DoNothing (Just ct)) -> S.DoNothing $ Just $ toSQLCT ct
  (CP1Update ct inpCols)    -> S.Update (toSQLCT ct)
    (S.buildSEWithExcluded inpCols)

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
getInsertDeps (InsertQueryP1 tn _ _ _ _ mutFlds) =
  mkParentDep tn : retDeps
  where
    retDeps = map (mkColDep "untyped" tn . fst) $
              pgColsFromMutFlds mutFlds

convObj
  :: (P1C m)
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

buildConflictClause
  :: (P1C m)
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
      return $ CP1Update (Column $ getPGCols col) inpCols
    (Nothing, Just cons, CAUpdate)  -> do
      validateConstraint cons
      return $ CP1Update (Constraint cons) inpCols
    (Just _, Just _, _)             -> throw400 UnexpectedPayload
      "'constraint' and 'constraint_on' cannot be set at a time"
  where
    fieldInfoMap = tiFieldInfoMap tableInfo

    validateCols c = do
      let targetcols = getPGCols c
      void $ withPathK "constraint_on" $ indexedForM targetcols $
        \pgCol -> askPGType fieldInfoMap pgCol ""

    validateConstraint c = do
      let tableConsNames = map tcName $ tiConstraints tableInfo
      withPathK "constraint" $
       unless (c `elem` tableConsNames) $
       throw400 Unexpected $ "constraint " <> getConstraintTxt c
                   <<> " for table " <> tiName tableInfo
                   <<> " does not exist"


convInsertQuery
  :: (P1C m)
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
      unless (ipiAllowUpsert insPerm) $ throw400 PermissionDenied $
        "upsert is not allowed for role" <>> roleName
      buildConflictClause tableInfo inpCols c

  return $ InsertQueryP1 tableName insView insCols sqlExps
           conflictClause mutFlds

  where
    selNecessaryMsg =
      "; \"returning\" can only be used if the role has "
      <> "\"select\" permission on the table"

decodeInsObjs :: (P1C m) => Value -> m [InsObj]
decodeInsObjs v = do
  objs <- decodeValue v
  when (null objs) $ throw400 UnexpectedPayload "objects should not be empty"
  return objs

convInsQ :: InsertQuery -> P1 (InsertQueryP1, DS.Seq Q.PrepArg)
convInsQ insQ =
  flip runStateT DS.empty $ convInsertQuery
  (withPathK "objects" . decodeInsObjs) binRHSBuilder insQ

insertP2 :: (InsertQueryP1, DS.Seq Q.PrepArg) -> Q.TxE QErr RespBody
insertP2 (u, p) =
  runIdentity . Q.getRow
  <$> Q.rawQE dmlTxErrorHandler (Q.fromBuilder insertSQL) (toList p) True
  where
    insertSQL = toSQL $ mkSQLInsert u

data ConflictCtx
  = CCUpdate !ConstraintName ![PGCol]
  | CCDoNothing !(Maybe ConstraintName)
  deriving (Show, Eq)

nonAdminInsert :: (InsertQueryP1, DS.Seq Q.PrepArg) -> Q.TxE QErr RespBody
nonAdminInsert (insQueryP1, args) = do
  conflictCtxM <- mapM extractConflictCtx conflictClauseP1
  setConflictCtx conflictCtxM
  insertP2 (withoutConflictClause, args)
  where
    withoutConflictClause = insQueryP1{iqp1Conflict=Nothing}
    conflictClauseP1 = iqp1Conflict insQueryP1

extractConflictCtx :: (MonadError QErr m) => ConflictClauseP1 -> m ConflictCtx
extractConflictCtx cp =
  case cp of
    (CP1DoNothing mConflictTar) -> do
      mConstraintName <- mapM extractConstraintName mConflictTar
      return $ CCDoNothing mConstraintName
    (CP1Update conflictTar inpCols) -> do
      constraintName <- extractConstraintName conflictTar
      return $ CCUpdate constraintName inpCols
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
    conflictCtxToJSON (CCUpdate constr updCols) =
        encToText $ InsertTxConflictCtx CAUpdate (Just constr) $
        Just $ toSQLTxt $ S.buildSEWithExcluded updCols

instance HDBQuery InsertQuery where

  type Phase1Res InsertQuery = (InsertQueryP1, DS.Seq Q.PrepArg)
  phaseOne = convInsQ

  phaseTwo _ p1Res = do
    role <- userRole <$> ask
    liftTx $
      bool (nonAdminInsert p1Res) (insertP2 p1Res) $ isAdmin role

  schemaCachePolicy = SCPNoChange
