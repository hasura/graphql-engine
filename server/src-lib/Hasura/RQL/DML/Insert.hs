{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Hasura.RQL.DML.Insert where

import           Data.Aeson.Types
import           Instances.TH.Lift        ()

import qualified Data.HashMap.Strict      as HM
import qualified Data.Sequence            as DS

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
mkSQLInsert (InsertQueryP1 _ vn cols vals c mutFlds) =
  mkSelWith (S.CTEInsert insert) mutFlds
  where
    insert =
      S.SQLInsert vn cols vals (toSQLConflict c) $ Just S.returningStar
    toSQLConflict conflict = case conflict of
      Nothing -> Nothing
      Just (CP1DoNothing Nothing)   -> Just $ S.DoNothing Nothing
      Just (CP1DoNothing (Just ct)) -> Just $ S.DoNothing $ Just $ toSQLCT ct
      Just (CP1Update ct pgCols)    -> Just $ S.Update (toSQLCT ct)
        (S.SetExp $ toSQLSetExps pgCols)

    toSQLCT ct = case ct of
      Column pgCols -> S.SQLColumn pgCols
      Constraint cn -> S.SQLConstraint cn

    toSQLSetExps = map $ \col
      -> S.SetExpItem (col, S.SEExcluded $ getPGColTxt col)

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
  -> FieldInfoMap
  -> InsObj
  -> m [S.SQLExp]
convObj prepFn defInsVals fieldInfoMap insObj = do
  inpInsVals <- flip HM.traverseWithKey insObj $ \c val -> do
    let relWhenPGErr = "relationships can't be inserted"
    colType <- askPGType fieldInfoMap c relWhenPGErr
    -- Encode aeson's value into prepared value
    withPathK (getPGColTxt c) $ prepFn colType val

  return $ HM.elems $ HM.union inpInsVals defInsVals

buildConflictClause
  :: (P1C m)
  => TableInfo
  -> OnConflict
  -> m ConflictClauseP1
buildConflictClause tableInfo (OnConflict mTCol mTCons act) = case (mTCol, mTCons, act) of
  (Nothing, Nothing, CAIgnore)    -> return $ CP1DoNothing Nothing
  (Just col, Nothing, CAIgnore)   -> do
    validateCols col
    return $ CP1DoNothing $ Just $ Column $ getPGCols col
  (Nothing, Just cons, CAIgnore)  -> return $ CP1DoNothing $ Just $ Constraint cons
  (Nothing, Nothing, CAUpdate)    -> throw400 UnexpectedPayload
    "Expecting 'constraint' or 'constraint_on' when the 'action' is 'update'"
  (Just col, Nothing, CAUpdate)   -> do
    validateCols col
    return $ CP1Update (Column $ getPGCols col) columns
  (Nothing, Just cons, CAUpdate)  -> return $ CP1Update (Constraint cons) columns
  (Just _, Just _, _)             -> throw400 UnexpectedPayload
    "'constraint' and 'constraint_on' cannot be set at a time"
  where
    fieldInfoMap = tiFieldInfoMap tableInfo
    columns = map pgiName $ getCols fieldInfoMap
    validateCols c = do
      let targetcols = getPGCols c
      void $ withPathK "constraint_on" $ indexedForM targetcols $
        \pgCol -> askPGType fieldInfoMap pgCol ""

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

  -- Check if the role has insert permissions
  insPerm   <- askInsPermInfo tableInfo

  -- Check if all dependent headers are present
  validateHeaders $ ipiRequiredHeaders insPerm

  let fieldInfoMap = tiFieldInfoMap tableInfo

  -- convert the returning cols into sql returing exp
  mAnnRetCols <- forM mRetCols $ \retCols -> do
    -- Check if select is allowed only if you specify returning
    selPerm <- modifyErr (<> selNecessaryMsg) $
               askSelPermInfo tableInfo

    withPathK "returning" $
      zip retCols <$> checkRetCols fieldInfoMap selPerm retCols

  let mutFlds = mkDefaultMutFlds mAnnRetCols

  let defInsVals = mkDefValMap fieldInfoMap
      insCols    = HM.keys defInsVals
      insView    = ipiView insPerm

  insTuples <- withPathK "objects" $ indexedForM insObjs $ \obj ->
    convObj prepFn defInsVals fieldInfoMap obj

  conflictClause <- withPathK "on_conflict" $ forM oC $ \c -> do
      roleName <- askCurRole
      unless (ipiAllowUpsert insPerm) $ throw400 PermissionDenied $
        "upsert is not allowed for role" <>> roleName
      buildConflictClause tableInfo c
  return $ InsertQueryP1 tableName insView insCols insTuples
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

instance HDBQuery InsertQuery where

  type Phase1Res InsertQuery = (InsertQueryP1, DS.Seq Q.PrepArg)
  phaseOne = convInsQ

  phaseTwo _ = liftTx . insertP2

  schemaCachePolicy = SCPNoChange
