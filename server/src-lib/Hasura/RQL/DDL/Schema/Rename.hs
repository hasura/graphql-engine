{-# LANGUAGE QuasiQuotes #-}
module Hasura.RQL.DDL.Schema.Rename
  ( renameTableInCatalog
  , renameColInCatalog
  , renameRelInCatalog
  )
where

import           Hasura.Prelude
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.DDL.Permission.Internal
import           Hasura.RQL.DDL.Relationship.Types
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Data.HashMap.Strict                as M
import qualified Data.Map.Strict                    as Map
import qualified Database.PG.Query                  as Q

import           Data.Aeson

data RenameItem a
  = RenameItem
  { _riTable :: !QualifiedTable
  , _riOld   :: !a
  , _riNew   :: !a
  } deriving (Show, Eq)

data RenameField
  = RFCol !(RenameItem PGCol)
  | RFRel !(RenameItem RelName)
  deriving (Show, Eq)

data RenameTableCtx
  = RenameTableCtx
  { _rtcOldName  :: !QualifiedTable
  , _rtcNewName  :: !QualifiedTable
  , _rtcRefTable :: !QualifiedTable
  , _rtcRelName  :: !RelName
  } deriving (Show, Eq)

otherDeps :: QErrM m => Text -> SchemaObjId -> m ()
otherDeps errMsg = \case
  SOQTemplate name ->
    throw400 NotSupported $
      "found dependant query template " <> name <<> "; " <> errMsg
  d                ->
      throw500 $ "unexpected dependancy "
        <> reportSchemaObj d <> "; " <> errMsg


renameTableInCatalog
  :: (MonadTx m, CacheRM m)
  => QualifiedTable -> QualifiedTable -> m ()
renameTableInCatalog newQT oldQT = do
  sc <- askSchemaCache
  let allDeps = getDependentObjs sc $ SOTable oldQT
  -- update all dependant schema objects
  forM_ allDeps $ \case
    SOTableObj refQT (TORel rn) ->
      updateRelDefs $ RenameTableCtx oldQT newQT refQT rn
    -- table names are not specified in permission definitions
    SOTableObj _ (TOPerm _ _)   -> return ()
    d -> otherDeps errMsg d
  -- -- Update table name in hdb_catalog
  liftTx $ Q.catchE defaultTxErrorHandler updateTableInCatalog
  where
    QualifiedObject nsn ntn = newQT
    QualifiedObject osn otn = oldQT
    errMsg = "cannot rename table " <> oldQT <<> " to " <>> newQT
    updateTableInCatalog =
      Q.unitQ [Q.sql|
           UPDATE "hdb_catalog"."hdb_table"
              SET table_schema = $1, table_name = $2
            WHERE table_schema = $3 AND table_name = $4
                |] (nsn, ntn, osn, otn) False

renameColInCatalog
  :: (MonadTx m, CacheRM m)
  => PGCol -> PGCol -> QualifiedTable -> TableInfo -> m ()
renameColInCatalog oCol nCol qt ti = do
  sc <- askSchemaCache
  -- Check if any relation exists with new column name
  assertFldNotExists
  -- Fetch dependent objects
  let depObjs = getDependentObjs sc $ SOTableObj qt $ TOCol oCol
      renameFld = RFCol $ RenameItem qt oCol nCol
  -- Update dependent objects
  forM_ depObjs $ \case
    SOTableObj refQT (TOPerm role pt) ->
      updatePermFlds refQT role pt renameFld
    SOTableObj refQT (TORel rn) ->
      if qt == refQT
        then updateRelNativeCols oCol nCol qt rn
      else updateRelRemoteCols oCol nCol refQT rn
    d -> otherDeps errMsg d
  where
    errMsg = "cannot rename column " <> oCol <<> " to " <>> nCol
    assertFldNotExists =
      case M.lookup (fromPGCol oCol) $ tiFieldInfoMap ti of
        Just (FIRelationship _) ->
          throw400 AlreadyExists $ "cannot rename column " <> oCol
          <<> " to " <> nCol <<> " in table " <> qt <<>
          " as a relationship with the name already exists"
        _ -> return ()

renameRelInCatalog
  :: (MonadTx m, CacheRM m)
  => QualifiedTable -> RelName -> RelName -> m ()
renameRelInCatalog qt oldRN newRN = do
  sc <- askSchemaCache
  let depObjs = getDependentObjs sc $ SOTableObj qt $ TORel oldRN
      renameFld = RFRel $ RenameItem qt oldRN newRN

  forM_ depObjs $ \case
    SOTableObj refQT (TOPerm role pt) ->
      updatePermFlds refQT role pt renameFld
    d -> otherDeps errMsg d
  liftTx updateRelName
  where
    errMsg = "cannot rename relationship " <> oldRN <<> " to " <>> newRN
    QualifiedObject sn tn = qt
    updateRelName =
      Q.unitQE defaultTxErrorHandler [Q.sql|
        UPDATE hdb_catalog.hdb_relationship
           SET rel_name = $1
         WHERE table_schema = $2
           AND table_name = $3
           AND rel_name = $4
      |] (newRN, sn, tn, oldRN) True


-- update table names in relationship definition
updateRelDefs :: (MonadTx m, CacheRM m) => RenameTableCtx -> m ()
updateRelDefs ctx = do
  fim <- askFieldInfoMap $ _rtcRefTable ctx
  ri <- askRelType fim (_rtcRelName ctx) ""
  case riType ri of
    ObjRel -> updateObjRelDef ctx
    ArrRel -> updateArrRelDef ctx


updateObjRelDef :: (MonadTx m) => RenameTableCtx -> m ()
updateObjRelDef ctx = do
  oldDefV <- liftTx $ getRelDef qt rn
  oldDef :: ObjRelUsing <- decodeValue oldDefV
  case oldDef of
    RUFKeyOn _ -> return ()
    RUManual (ObjRelManualConfig (RelManualConfig dbQT rmCols)) -> do
      let newDef = mkObjRelUsing rmCols
      when (dbQT == oldQT ) $
        liftTx $ updateRel qt rn $ toJSON (newDef :: ObjRelUsing)
  where
    RenameTableCtx oldQT newQT qt rn = ctx
    mkObjRelUsing colMap = RUManual $ ObjRelManualConfig $
      RelManualConfig newQT colMap

updateArrRelDef :: (MonadTx m) => RenameTableCtx -> m ()
updateArrRelDef ctx = do
  oldDefV <- liftTx $ getRelDef qt rn
  oldDef  <- decodeValue oldDefV
  let (newDef, doUpdate) = mkNewArrRelUsing oldDef
  when doUpdate $ liftTx $ updateRel qt rn $ toJSON newDef
  where
    RenameTableCtx oldQT newQT qt rn = ctx
    mkNewArrRelUsing arrRelUsing = case arrRelUsing of
      RUFKeyOn (ArrRelUsingFKeyOn dbQT c) ->
        ( RUFKeyOn $ ArrRelUsingFKeyOn newQT c
        , oldQT == dbQT
        )
      RUManual (ArrRelManualConfig (RelManualConfig dbQT rmCols)) ->
        ( RUManual $ ArrRelManualConfig $ RelManualConfig newQT rmCols
        , oldQT == dbQT
        )

getRelDef :: QualifiedTable -> RelName -> Q.TxE QErr Value
getRelDef (QualifiedObject sn tn) rn =
  Q.getAltJ . runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler
    [Q.sql|
     SELECT rel_def::json FROM hdb_catalog.hdb_relationship
      WHERE table_schema = $1 AND table_name = $2
        AND rel_name = $3
    |] (sn, tn, rn) True

updateRel :: QualifiedTable -> RelName -> Value -> Q.TxE QErr ()
updateRel (QualifiedObject sn tn) rn relDef =
  Q.unitQE defaultTxErrorHandler [Q.sql|
           UPDATE hdb_catalog.hdb_relationship
              SET rel_def = $1 :: jsonb
            WHERE table_schema = $2
              AND table_name = $3
              AND rel_name = $4
                |] (Q.AltJ relDef, sn , tn, rn) True


-- | update fields in premissions
updatePermFlds :: (MonadTx m, CacheRM m)
  => QualifiedTable -> RoleName -> PermType -> RenameField -> m ()
updatePermFlds refQT rn pt rf = do
  Q.AltJ pDef <- liftTx fetchPermDef
  case pt of
    PTInsert -> do
      perm <- decodeValue pDef
      updateInsPermFlds rf refQT rn perm
    PTSelect -> do
      perm <- decodeValue pDef
      updateSelPermFlds rf refQT rn perm
    PTUpdate -> do
      perm <- decodeValue pDef
      updateUpdPermFlds rf refQT rn perm
    PTDelete -> do
      perm <- decodeValue pDef
      updateDelPermFlds rf refQT rn perm
  where
    QualifiedObject sn tn = refQT
    fetchPermDef =
      runIdentity . Q.getRow <$>
        Q.withQE defaultTxErrorHandler [Q.sql|
                  SELECT perm_def::json
                    FROM hdb_catalog.hdb_permission
                   WHERE table_schema = $1
                     AND table_name = $2
                     AND role_name = $3
                     AND perm_type = $4
                 |] (sn, tn, rn, permTypeToCode pt) True

type PermModifier m a = RenameField -> QualifiedTable -> RoleName -> a -> m ()

updateInsPermFlds :: (MonadTx m, CacheRM m) => PermModifier m InsPerm
updateInsPermFlds rf refQT rn (InsPerm chk preset cols) = do
  updBoolExp <- updateBoolExp refQT rf chk
  liftTx $ updatePermDefInCatalog PTInsert refQT rn $
    InsPerm updBoolExp updPresetM updColsM
  where
    updPresetM = updatePreset refQT rf <$> preset
    updColsM = updateCols refQT rf <$> cols

updateSelPermFlds :: (MonadTx m, CacheRM m) => PermModifier m SelPerm
updateSelPermFlds rf refQT rn (SelPerm cols fltr limit aggAllwd) = do
  updBoolExp <- updateBoolExp refQT rf fltr
  liftTx $ updatePermDefInCatalog PTSelect refQT rn $
      SelPerm updCols updBoolExp limit aggAllwd
  where
    updCols = updateCols refQT rf cols

updateUpdPermFlds :: (MonadTx m, CacheRM m) => PermModifier m UpdPerm
updateUpdPermFlds rf refQT rn (UpdPerm cols preset fltr) = do
  updBoolExp <- updateBoolExp refQT rf fltr
  liftTx $ updatePermDefInCatalog PTUpdate refQT rn $
      UpdPerm updCols updPresetM updBoolExp
  where
    updCols = updateCols refQT rf cols
    updPresetM = updatePreset refQT rf <$> preset

updateDelPermFlds :: (MonadTx m, CacheRM m) => PermModifier m DelPerm
updateDelPermFlds rf refQT rn (DelPerm fltr) = do
  updBoolExp <- updateBoolExp refQT rf fltr
  liftTx $ updatePermDefInCatalog PTDelete refQT rn $
    DelPerm updBoolExp

updatePreset
  :: QualifiedTable -> RenameField -> ColVals -> ColVals
updatePreset refQT rf obj =
   case rf of
     RFCol (RenameItem qt oCol nCol) ->
       if qt == refQT then updatePreset' oCol nCol obj
       else obj
     _                              -> obj

updatePreset' :: PGCol -> PGCol -> ColVals -> ColVals
updatePreset' oCol nCol obj =
  M.fromList updItems
  where
    updItems= map procObjItem $ M.toList obj
    procObjItem (pgCol, v) =
      let isUpdated = pgCol == oCol
          updCol = bool pgCol nCol isUpdated
      in (PGCol $ getPGColTxt updCol, v)

updateCols
  :: QualifiedTable -> RenameField -> PermColSpec -> PermColSpec
updateCols refQT rf permSpec =
  case rf of
    RFCol (RenameItem qt oCol nCol) ->
      if qt == refQT then updateCols' oCol nCol permSpec
      else permSpec
    _                              -> permSpec
  where
    updateCols' oCol nCol cols = case cols of
      PCStar -> cols
      PCCols c -> PCCols $ flip map c $
        \col -> if col == oCol then nCol else col

updateBoolExp
  :: (QErrM m, CacheRM m)
  => QualifiedTable -> RenameField -> BoolExp -> m BoolExp
updateBoolExp refQT rf be =
  case rf of
    RFCol (RenameItem qt oCol nCol) ->
      updateBoolExp' qt refQT (fromPGCol oCol) (fromPGCol nCol) be
    RFRel (RenameItem qt oRN nRN)   ->
      updateBoolExp' qt refQT (fromRel oRN) (fromRel nRN) be

updateBoolExp'
  :: (QErrM m, CacheRM m)
  => QualifiedTable -> QualifiedTable
  -> FieldName -> FieldName -> BoolExp -> m BoolExp
updateBoolExp' qt refQT oFld nFld boolExp =
  BoolExp <$> case unBoolExp boolExp of
    BoolAnd exps -> procBoolExps BoolAnd exps

    BoolOr exps -> procBoolExps BoolOr exps

    BoolFld (ColExp fld v) -> do
      let newFld = bool fld nFld $ refQT == qt && oFld == fld
      (BoolFld . ColExp newFld) <$> do
        fim <- askFieldInfoMap refQT
        fi <- askFieldInfo fim fld
        case fi of
          FIColumn _ -> return v
          FIRelationship ri -> do
            let remTable = riRTable ri
            colExp <- decodeValue v
            ube <- updateBoolExp' qt remTable oFld nFld colExp
            return $ toJSON ube

    BoolNot be -> do
      updatedExp <- updateBoolExp' qt refQT oFld nFld $ BoolExp be
      return $ BoolNot $ unBoolExp updatedExp

  where
    updateExps exps = forM (map BoolExp exps) $
                      updateBoolExp' qt refQT oFld nFld
    procBoolExps f bExps = (f . map unBoolExp) <$> updateExps bExps

-- update columns in relations
type RelColsModifier m = PGCol -> PGCol -> QualifiedTable -> RelName -> m ()

updateRelRemoteCols :: (MonadTx m, CacheRM m) => RelColsModifier m
updateRelRemoteCols oCol nCol refQT relName = do
  fim <- askFieldInfoMap refQT
  ri <- askRelType fim relName ""
  case riType ri of
    ObjRel -> updateObjRelRemoteCol oCol nCol refQT relName
    ArrRel -> updateArrRelRemoteCol oCol nCol refQT relName

updateObjRelRemoteCol :: (MonadTx m) => RelColsModifier m
updateObjRelRemoteCol oCol nCol qt rn = do
  oldDefV <- liftTx $ getRelDef qt rn
  oldDef :: ObjRelUsing <- decodeValue oldDefV
  case oldDef of
    RUFKeyOn _ -> return ()
    RUManual (ObjRelManualConfig manConf) -> do
      let (updatedManualConf, updNeeded) =
            updateColForManualConfig oCol nCol Map.map snd manConf
      when updNeeded $
        liftTx $ updateRel qt rn $ toJSON
          (RUManual $ ObjRelManualConfig updatedManualConf :: ObjRelUsing)

updateArrRelRemoteCol :: (MonadTx m) => RelColsModifier m
updateArrRelRemoteCol oCol nCol qt rn = do
  oldDefV <- liftTx $ getRelDef qt rn
  oldDef <- decodeValue oldDefV
  updateArrRel oldDef
  where
    updateArrRel arrRelUsing = case arrRelUsing of
      RUFKeyOn (ArrRelUsingFKeyOn t c) -> when (c == oCol) $
          liftTx $ updateRel qt rn $ toJSON
            (RUFKeyOn (ArrRelUsingFKeyOn t nCol) :: ArrRelUsing)
      RUManual (ArrRelManualConfig manConf) -> do
        let (updatedManualConf, updNeeded) =
              updateColForManualConfig oCol nCol Map.map snd manConf
        when updNeeded $
          liftTx $ updateRel qt rn $ toJSON
            (RUManual $ ArrRelManualConfig updatedManualConf :: ArrRelUsing)

updateRelNativeCols :: (MonadTx m, CacheRM m) => RelColsModifier m
updateRelNativeCols oCol nCol qt relName = do
  fim <- askFieldInfoMap qt
  ri <- askRelType fim relName ""
  case riType ri of
    ObjRel -> updateObjRelNativeCol oCol nCol qt relName
    ArrRel -> updateArrRelNativeCol oCol nCol qt relName

updateObjRelNativeCol :: (MonadTx m) => RelColsModifier m
updateObjRelNativeCol oCol nCol qt rn = do
  oldDefV <- liftTx $ getRelDef qt rn
  oldDef :: ObjRelUsing <- decodeValue oldDefV
  case oldDef of
    RUFKeyOn c -> when (c == oCol) $
      liftTx $ updateRel qt rn $ toJSON
        (RUFKeyOn nCol :: ObjRelUsing)
    RUManual (ObjRelManualConfig manConf) -> do
      let (updatedManualConf, updNeeded) =
            updateColForManualConfig oCol nCol Map.mapKeys fst manConf
      when updNeeded $
        liftTx $ updateRel qt rn $ toJSON
          (RUManual $ ObjRelManualConfig updatedManualConf :: ObjRelUsing)

updateArrRelNativeCol :: (MonadTx m) => RelColsModifier m
updateArrRelNativeCol oCol nCol qt rn = do
  oldDefV <- liftTx $ getRelDef qt rn
  oldDef :: ArrRelUsing <- decodeValue oldDefV
  updateArrRel oldDef
  where
    updateArrRel arrRelUsing = case arrRelUsing of
      RUFKeyOn _ -> return ()
      RUManual (ArrRelManualConfig manConf) -> do
        let (updatedManualConf, updNeeded) =
              updateColForManualConfig oCol nCol Map.mapKeys fst manConf
        when updNeeded $
          liftTx $ updateRel qt rn $ toJSON
            (RUManual $ ArrRelManualConfig updatedManualConf :: ArrRelUsing)

type ColMapModifier = (PGCol -> PGCol) -> Map.Map PGCol PGCol -> Map.Map PGCol PGCol
type ColAccessor = (PGCol, PGCol) -> PGCol

updateColForManualConfig
  :: PGCol -> PGCol
  -> ColMapModifier -> ColAccessor
  -> RelManualConfig -> (RelManualConfig, Bool)
updateColForManualConfig oCol nCol modFn accFn (RelManualConfig tn rmCols) =
  let updatedColMap =
        flip modFn rmCols $ \col -> bool col nCol $ col == oCol
  in
  ( RelManualConfig tn updatedColMap
  , oCol `elem` map accFn (Map.toList rmCols)
  )
