module Hasura.RQL.DML.Insert
 ( runInsert
 ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                          as HM
import qualified Data.HashSet                                 as HS
import qualified Data.Sequence                                as DS
import qualified Database.PG.Query                            as Q

import           Control.Monad.Trans.Control                  (MonadBaseControl)
import           Data.Aeson.Types
import           Data.Text.Extended

import qualified Hasura.Backends.Postgres.SQL.DML             as S

import           Hasura.Backends.Postgres.Connection
import           Hasura.Backends.Postgres.Execute.Mutation
import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Backends.Postgres.Translate.Returning
import           Hasura.EncJSON
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Types
import           Hasura.RQL.IR.Insert
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Run
import           Hasura.Server.Version                        (HasVersion)
import           Hasura.Session


import qualified Data.Environment                             as Env
import qualified Hasura.Tracing                               as Tracing

convObj
  :: (UserInfoM m, QErrM m)
  => (ColumnType 'Postgres -> Value -> m S.SQLExp)
  -> HM.HashMap PGCol S.SQLExp
  -> HM.HashMap PGCol S.SQLExp
  -> FieldInfoMap (FieldInfo 'Postgres)
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


convInsertQuery
  :: (UserInfoM m, QErrM m, TableInfoRM 'Postgres m)
  => (Value -> m [InsObj])
  -> SessVarBldr 'Postgres m
  -> (ColumnType 'Postgres -> Value -> m S.SQLExp)
  -> InsertQuery
  -> m (InsertQueryP1 'Postgres)
convInsertQuery objsParser sessVarBldr prepFn (InsertQuery tableName _ val oC mRetCols) = do

  insObjs <- objsParser val

  -- Get the current table information
  tableInfo <- askTabInfoSource tableName
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

convInsQ
  :: (QErrM m, UserInfoM m, CacheRM m)
  => InsertQuery
  -> m (InsertQueryP1 'Postgres, DS.Seq Q.PrepArg)
convInsQ query = do
  let source = iqSource query
  tableCache <- askTableCache source
  flip runTableCacheRT (source, tableCache) $ runDMLP1T $
    convInsertQuery (withPathK "objects" . decodeInsObjs)
    sessVarFromCurrentSetting binRHSBuilder query

runInsert
  :: ( HasVersion, QErrM m, UserInfoM m
     , CacheRM m, HasSQLGenCtx m
     , MonadIO m, Tracing.MonadTrace m
     , MonadBaseControl IO m
     )
  => Env.Environment -> InsertQuery -> m EncJSON
runInsert env q = do
  sourceConfig <- _pcConfiguration <$> askPGSourceCache (iqSource q)
  res <- convInsQ q
  strfyNum <- stringifyNum <$> askSQLGenCtx
  runQueryLazyTx (_pscExecCtx sourceConfig) Q.ReadWrite $
    execInsertQuery env strfyNum Nothing res

decodeInsObjs :: (UserInfoM m, QErrM m) => Value -> m [InsObj]
decodeInsObjs v = do
  objs <- decodeValue v
  when (null objs) $ throw400 UnexpectedPayload "objects should not be empty"
  return objs
