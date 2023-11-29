module Hasura.RQL.DML.Insert
  ( runInsert,
  )
where

import Control.Lens ((^?))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson.Types
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HS
import Data.Sequence qualified as DS
import Data.Text.Extended
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection
import Hasura.Backends.Postgres.Execute.Mutation
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.Translate.Returning
import Hasura.Backends.Postgres.Types.Table
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.QueryTags
import Hasura.RQL.DML.Internal
import Hasura.RQL.DML.Types
import Hasura.RQL.IR.Insert
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.SchemaCache
import Hasura.Session
import Hasura.Table.Cache
import Hasura.Tracing qualified as Tracing

convObj ::
  (UserInfoM m, QErrM m) =>
  (ColumnType ('Postgres 'Vanilla) -> Value -> m S.SQLExp) ->
  HashMap.HashMap PGCol S.SQLExp ->
  HashMap.HashMap PGCol S.SQLExp ->
  FieldInfoMap (FieldInfo ('Postgres 'Vanilla)) ->
  InsObj ('Postgres 'Vanilla) ->
  m ([PGCol], [S.SQLExp])
convObj prepFn defInsVals setInsVals fieldInfoMap insObj = do
  inpInsVals <- flip HashMap.traverseWithKey insObj $ \c val -> do
    let relWhenPGErr = "relationships can't be inserted"
    colType <- askColumnType fieldInfoMap c relWhenPGErr
    -- if column has predefined value then throw error
    when (c `elem` preSetCols) $ throwNotInsErr c
    -- Encode aeson's value into prepared value
    withPathK (getPGColTxt c) $ prepFn colType val
  let insVals = HashMap.union setInsVals inpInsVals
      sqlExps = HashMap.elems $ HashMap.union insVals defInsVals
      inpCols = HashMap.keys inpInsVals

  return (inpCols, sqlExps)
  where
    preSetCols = HashMap.keys setInsVals

    throwNotInsErr c = do
      roleName <- _uiRole <$> askUserInfo
      throw400 NotSupported
        $ "column "
        <> c
        <<> " is not insertable"
        <> " for role "
        <>> roleName

validateInpCols :: (MonadError QErr m) => [PGCol] -> [PGCol] -> m ()
validateInpCols inpCols updColsPerm = forM_ inpCols $ \inpCol ->
  unless (inpCol `elem` updColsPerm)
    $ throw400 ValidationFailed
    $ "column "
    <> inpCol
    <<> " is not updatable"

buildConflictClause ::
  (UserInfoM m, QErrM m) =>
  SessionVariableBuilder m ->
  TableInfo ('Postgres 'Vanilla) ->
  [PGCol] ->
  OnConflict ->
  m (OnConflictClause ('Postgres 'Vanilla) S.SQLExp)
buildConflictClause sessVarBldr tableInfo inpCols (OnConflict mTCol mTCons act) =
  case (mTCol, mTCons, act) of
    (Nothing, Nothing, CAIgnore) -> return $ OCCDoNothing Nothing
    (Just col, Nothing, CAIgnore) -> do
      validateCols col
      return $ OCCDoNothing $ Just $ CTColumn $ getPGCols col
    (Nothing, Just cons, CAIgnore) -> do
      validateConstraint cons
      return $ OCCDoNothing $ Just $ CTConstraint cons
    (Nothing, Nothing, CAUpdate) ->
      throw400
        UnexpectedPayload
        "Expecting 'constraint' or 'constraint_on' when the 'action' is 'update'"
    (Just col, Nothing, CAUpdate) -> do
      validateCols col
      (updFltr, preSet) <- getUpdPerm
      resolvedUpdFltr <- convAnnBoolExpPartialSQL sessVarBldr updFltr
      resolvedPreSet <- mapM (convPartialSQLExp sessVarBldr) preSet
      return $ OCCUpdate $ OnConflictClauseData (CTColumn $ getPGCols col) inpCols resolvedPreSet resolvedUpdFltr
    (Nothing, Just cons, CAUpdate) -> do
      validateConstraint cons
      (updFltr, preSet) <- getUpdPerm
      resolvedUpdFltr <- convAnnBoolExpPartialSQL sessVarBldr updFltr
      resolvedPreSet <- mapM (convPartialSQLExp sessVarBldr) preSet
      return $ OCCUpdate $ OnConflictClauseData (CTConstraint cons) inpCols resolvedPreSet resolvedUpdFltr
    (Just _, Just _, _) ->
      throw400
        UnexpectedPayload
        "'constraint' and 'constraint_on' cannot be set at a time"
  where
    coreInfo = _tiCoreInfo tableInfo
    fieldInfoMap = _tciFieldInfoMap coreInfo
    -- toSQLBool = toSQLBoolExp (S.mkQual $ _tciName coreInfo)

    validateCols c = do
      let targetcols = getPGCols c
      void
        $ withPathK "constraint_on"
        $ indexedForM targetcols
        $ \pgCol -> askColumnType fieldInfoMap pgCol ""

    validateConstraint c = do
      let tableConsNames =
            maybe [] (toList . fmap (_cName . _ucConstraint)) (tciUniqueOrPrimaryKeyConstraints coreInfo)
      withPathK "constraint"
        $ unless (c `elem` tableConsNames)
        $ throw400 Unexpected
        $ "constraint "
        <> getConstraintTxt c
        <<> " for table "
        <> _tciName coreInfo
        <<> " does not exist"

    getUpdPerm = do
      upi <- askUpdPermInfo tableInfo
      let updFiltr = upiFilter upi
          preSet = upiSet upi
          updCols = HS.toList $ upiCols upi
      validateInpCols inpCols updCols
      return (updFiltr, preSet)

convInsertQuery ::
  (UserInfoM m, QErrM m, TableInfoRM ('Postgres 'Vanilla) m) =>
  (Value -> m [InsObj ('Postgres 'Vanilla)]) ->
  SessionVariableBuilder m ->
  (ColumnType ('Postgres 'Vanilla) -> Value -> m S.SQLExp) ->
  InsertQuery ->
  m (InsertQueryP1 ('Postgres 'Vanilla))
convInsertQuery objsParser sessVarBldr prepFn (InsertQuery tableName _ val oC mRetCols) = do
  insObjs <- objsParser val

  -- Get the current table information
  tableInfo <- askTableInfoSource tableName
  let coreInfo = _tiCoreInfo tableInfo

  -- If table is view then check if it is insertable
  mutableView
    tableName
    viIsInsertable
    (_tciViewInfo coreInfo)
    "insertable"

  -- Check if the role has insert permissions
  insPerm <- askInsPermInfo tableInfo
  updPerm <- askPermInfo permUpd tableInfo

  -- Check if all dependent headers are present
  validateHeaders $ ipiRequiredHeaders insPerm

  let fieldInfoMap = _tciFieldInfoMap coreInfo
      setInsVals = ipiSet insPerm

  -- convert the returning cols into sql returing exp
  mAnnRetCols <- forM mRetCols $ \retCols -> do
    -- Check if select is allowed only if you specify returning
    selPerm <-
      modifyErr (<> selNecessaryMsg)
        $ askSelPermInfo tableInfo

    withPathK "returning" $ checkRetCols fieldInfoMap selPerm retCols

  let mutOutput = mkDefaultMutFlds mAnnRetCols

  let defInsVals =
        HashMap.fromList
          [ (structuredColumnInfoColumn column, S.columnDefaultValue)
            | column <- getCols fieldInfoMap,
              _cmIsInsertable (structuredColumnInfoMutability column)
          ]
      allCols = mapMaybe (^? _SCIScalarColumn) $ getCols fieldInfoMap
      insCols = HashMap.keys defInsVals

  resolvedPreSet <- mapM (convPartialSQLExp sessVarBldr) setInsVals

  insTuples <- withPathK "objects"
    $ indexedForM insObjs
    $ \obj ->
      convObj prepFn defInsVals resolvedPreSet fieldInfoMap obj
  let sqlExps = map snd insTuples
      inpCols = HS.toList $ HS.fromList $ concatMap fst insTuples

  insCheck <- convAnnBoolExpPartialSQL sessVarFromCurrentSetting (ipiCheck insPerm)
  updCheck <- traverse (convAnnBoolExpPartialSQL sessVarFromCurrentSetting) (upiCheck =<< updPerm)

  conflictClause <- withPathK "on_conflict"
    $ forM oC
    $ \c -> do
      role <- askCurRole
      unless (isTabUpdatable role tableInfo)
        $ throw400 PermissionDenied
        $ "upsert is not allowed for role "
        <> role
        <<> " since update permissions are not defined"
      buildConflictClause sessVarBldr tableInfo inpCols c
  return
    $ InsertQueryP1
      tableName
      insCols
      sqlExps
      conflictClause
      (insCheck, updCheck)
      mutOutput
      allCols
  where
    selNecessaryMsg =
      "; \"returning\" can only be used if the role has "
        <> "\"select\" permission on the table"

convInsQ ::
  (QErrM m, UserInfoM m, CacheRM m) =>
  InsertQuery ->
  m (InsertQueryP1 ('Postgres 'Vanilla), DS.Seq PG.PrepArg)
convInsQ query = do
  let source = iqSource query
  tableCache :: TableCache ('Postgres 'Vanilla) <- fold <$> askTableCache source
  flip runTableCacheRT tableCache
    $ runDMLP1T
    $ convInsertQuery
      (withPathK "objects" . decodeInsObjs)
      sessVarFromCurrentSetting
      binRHSBuilder
      query

runInsert ::
  forall m.
  ( QErrM m,
    UserInfoM m,
    CacheRM m,
    MonadIO m,
    Tracing.MonadTrace m,
    MonadBaseControl IO m,
    MetadataM m
  ) =>
  SQLGenCtx ->
  InsertQuery ->
  m EncJSON
runInsert sqlGen q = do
  sourceConfig <- askSourceConfig @('Postgres 'Vanilla) (iqSource q)
  userInfo <- askUserInfo
  res <- convInsQ q
  let strfyNum = stringifyNum sqlGen
  runTxWithCtx (_pscExecCtx sourceConfig) (Tx PG.ReadWrite Nothing) LegacyRQLQuery
    $ flip runReaderT emptyQueryTagsComment
    $ execInsertQuery strfyNum Nothing userInfo res

decodeInsObjs :: (UserInfoM m, QErrM m) => Value -> m [InsObj ('Postgres 'Vanilla)]
decodeInsObjs v = do
  objs <- decodeValue v
  when (null objs) $ throw400 UnexpectedPayload "objects should not be empty"
  return objs
