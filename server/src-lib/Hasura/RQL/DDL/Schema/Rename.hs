-- | Functions for mutating the catalog (with integrity checking) to incorporate schema changes
-- discovered after applying a user-supplied SQL query. None of these functions modify the schema
-- cache, so it must be reloaded after the catalog is updated.
module Hasura.RQL.DDL.Schema.Rename
  ( renameTableInCatalog
  , renameColInCatalog
  , renameRelInCatalog
  )
where

import           Control.Lens.Combinators
import           Control.Lens.Operators
import           Hasura.Prelude
import qualified Hasura.RQL.DDL.EventTrigger        as DS
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.DDL.Permission.Internal
import           Hasura.RQL.DDL.Relationship.Types
import           Hasura.RQL.DDL.Schema.Catalog
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Data.HashMap.Strict                as M
import qualified Database.PG.Query                  as Q

import           Data.Aeson

data RenameItem a
  = RenameItem
  { _riTable :: !QualifiedTable
  , _riOld   :: !a
  , _riNew   :: !a
  } deriving (Show, Eq)

type RenameCol = RenameItem PGCol
data RenameField
  = RFCol !RenameCol
  | RFRel !(RenameItem RelName)
  deriving (Show, Eq)

type RenameTable = (QualifiedTable, QualifiedTable)

data Rename
  = RTable !RenameTable
  | RField !RenameField
  deriving (Show, Eq)

otherDeps :: QErrM m => Text -> SchemaObjId -> m ()
otherDeps errMsg d =
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
      updateRelDefs refQT rn (oldQT, newQT)
    SOTableObj refQT (TOPerm rn pt)   ->
      updatePermFlds refQT rn pt $ RTable (oldQT, newQT)
    -- A trigger's definition is not dependent on the table directly
    SOTableObj _ (TOTrigger _)   -> return ()
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
  => PGCol -> PGCol -> QualifiedTable -> FieldInfoMap FieldInfo -> m ()
renameColInCatalog oCol nCol qt fieldInfo = do
  sc <- askSchemaCache
  -- Check if any relation exists with new column name
  assertFldNotExists
  -- Fetch dependent objects
  let depObjs = getDependentObjs sc $ SOTableObj qt $ TOCol oCol
      renameFld = RFCol $ RenameItem qt oCol nCol
  -- Update dependent objects
  forM_ depObjs $ \case
    SOTableObj refQT (TOPerm role pt) ->
      updatePermFlds refQT role pt $ RField renameFld
    SOTableObj refQT (TORel rn) ->
      updateColInRel refQT rn $ RenameItem qt oCol nCol
    SOTableObj _ (TOTrigger triggerName) ->
      updateColInEventTriggerDef triggerName $ RenameItem qt oCol nCol
    d -> otherDeps errMsg d
  -- Update custom column names
  possiblyUpdateCustomColumnNames qt oCol nCol
  where
    errMsg = "cannot rename column " <> oCol <<> " to " <>> nCol
    assertFldNotExists =
      case M.lookup (fromPGCol oCol) fieldInfo of
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
      updatePermFlds refQT role pt $ RField renameFld
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
updateRelDefs
  :: (MonadTx m, CacheRM m)
  => QualifiedTable -> RelName ->  RenameTable -> m ()
updateRelDefs qt rn renameTable = do
  fim <- askFieldInfoMap qt
  ri <- askRelType fim rn ""
  case riType ri of
    ObjRel -> updateObjRelDef qt rn renameTable
    ArrRel -> updateArrRelDef qt rn renameTable


updateObjRelDef
  :: (MonadTx m)
  => QualifiedTable -> RelName ->  RenameTable -> m ()
updateObjRelDef qt rn (oldQT, newQT) = do
  oldDefV <- liftTx $ getRelDef qt rn
  oldDef :: ObjRelUsing <- decodeValue oldDefV
  let newDef = case oldDef of
        RUFKeyOn _ -> oldDef
        RUManual (RelManualConfig dbQT rmCols) ->
          let updQT = bool oldQT newQT $ oldQT == dbQT
          in RUManual $ RelManualConfig updQT rmCols
  liftTx $ updateRel qt rn $ toJSON newDef

updateArrRelDef
  :: (MonadTx m)
  => QualifiedTable -> RelName ->  RenameTable -> m ()
updateArrRelDef qt rn (oldQT, newQT) = do
  oldDefV <- liftTx $ getRelDef qt rn
  oldDef  <- decodeValue oldDefV
  let newDef = case oldDef of
        RUFKeyOn (ArrRelUsingFKeyOn dbQT c) ->
          let updQT = getUpdQT dbQT
          in RUFKeyOn $ ArrRelUsingFKeyOn updQT c
        RUManual (RelManualConfig dbQT rmCols) ->
          let updQT = getUpdQT dbQT
          in RUManual $ RelManualConfig updQT rmCols
  liftTx $ updateRel qt rn $ toJSON newDef
  where
    getUpdQT dbQT = bool oldQT newQT $ oldQT == dbQT

-- | update fields in premissions
updatePermFlds :: (MonadTx m, CacheRM m)
  => QualifiedTable -> RoleName -> PermType -> Rename -> m ()
updatePermFlds refQT rn pt rename = do
  pDef <- fmap fst $ liftTx $ fetchPermDef refQT rn pt
  case pt of
    PTInsert -> do
      perm <- decodeValue pDef
      updateInsPermFlds refQT rename rn perm
    PTSelect -> do
      perm <- decodeValue pDef
      updateSelPermFlds refQT rename rn perm
    PTUpdate -> do
      perm <- decodeValue pDef
      updateUpdPermFlds refQT rename rn perm
    PTDelete -> do
      perm <- decodeValue pDef
      updateDelPermFlds refQT rename rn perm

updateInsPermFlds
  :: (MonadTx m, CacheRM m)
  => QualifiedTable -> Rename -> RoleName -> InsPerm -> m ()
updateInsPermFlds refQT rename rn (InsPerm chk preset cols) = do
  updatedPerm <- case rename of
    RTable rt -> do
      let updChk = updateTableInBoolExp rt chk
      return $ InsPerm updChk preset cols
    RField rf -> do
      updChk <- updateFieldInBoolExp refQT rf chk
      let updPresetM = updatePreset refQT rf <$> preset
          updColsM = updateCols refQT rf <$> cols
      return $ InsPerm updChk updPresetM updColsM
  liftTx $ updatePermDefInCatalog PTInsert refQT rn updatedPerm

updateSelPermFlds
  :: (MonadTx m, CacheRM m)
  => QualifiedTable -> Rename -> RoleName -> SelPerm -> m ()
updateSelPermFlds refQT rename rn (SelPerm cols fltr limit aggAllwd computedFields) = do
  updatedPerm <- case rename of
    RTable rt -> do
      let updFltr = updateTableInBoolExp rt fltr
      return $ SelPerm cols updFltr limit aggAllwd computedFields
    RField rf -> do
      updFltr <- updateFieldInBoolExp refQT rf fltr
      let updCols = updateCols refQT rf cols
      return $ SelPerm updCols updFltr limit aggAllwd computedFields
  liftTx $ updatePermDefInCatalog PTSelect refQT rn updatedPerm

updateUpdPermFlds
  :: (MonadTx m, CacheRM m)
  => QualifiedTable -> Rename -> RoleName -> UpdPerm -> m ()
updateUpdPermFlds refQT rename rn (UpdPerm cols preset fltr check) = do
  updatedPerm <- case rename of
    RTable rt -> do
      let updFltr = updateTableInBoolExp rt fltr
          updCheck = fmap (updateTableInBoolExp rt) check
      return $ UpdPerm cols preset updFltr updCheck
    RField rf -> do
      updFltr <- updateFieldInBoolExp refQT rf fltr
      updCheck <- traverse (updateFieldInBoolExp refQT rf) check
      let updCols = updateCols refQT rf cols
          updPresetM = updatePreset refQT rf <$> preset
      return $ UpdPerm updCols updPresetM updFltr updCheck
  liftTx $ updatePermDefInCatalog PTUpdate refQT rn updatedPerm

updateDelPermFlds
  :: (MonadTx m, CacheRM m)
  => QualifiedTable -> Rename -> RoleName -> DelPerm -> m ()
updateDelPermFlds refQT rename rn (DelPerm fltr) = do
  updFltr <- case rename of
    RTable rt -> return $ updateTableInBoolExp rt fltr
    RField rf -> updateFieldInBoolExp refQT rf fltr
  liftTx $ updatePermDefInCatalog PTDelete refQT rn $ DelPerm updFltr

updatePreset
  :: QualifiedTable -> RenameField -> (ColumnValues Value) -> (ColumnValues Value)
updatePreset qt rf obj =
   case rf of
     RFCol (RenameItem opQT oCol nCol) ->
       if qt == opQT then updatePreset' oCol nCol
       else obj
     _                              -> obj

  where
    updatePreset' oCol nCol =
      M.fromList updItems
      where
        updItems= map procObjItem $ M.toList obj
        procObjItem (pgCol, v) =
          let isUpdated = pgCol == oCol
              updCol = bool pgCol nCol isUpdated
          in (updCol, v)

updateCols
  :: QualifiedTable -> RenameField -> PermColSpec -> PermColSpec
updateCols qt rf permSpec =
  case rf of
    RFCol (RenameItem opQT oCol nCol) ->
      if qt == opQT then updateCols' oCol nCol permSpec
      else permSpec
    _                              -> permSpec
  where
    updateCols' oCol nCol cols = case cols of
      PCStar -> cols
      PCCols c -> PCCols $ flip map c $
        \col -> if col == oCol then nCol else col

updateTableInBoolExp :: RenameTable -> BoolExp -> BoolExp
updateTableInBoolExp (oldQT, newQT) =
  over _Wrapped . transform $ (_BoolExists . geTable) %~ \rqfQT ->
    if rqfQT == oldQT then newQT else rqfQT

updateFieldInBoolExp
  :: (QErrM m, CacheRM m)
  => QualifiedTable -> RenameField -> BoolExp -> m BoolExp
updateFieldInBoolExp qt rf be = BoolExp <$>
  case unBoolExp be of
    BoolAnd exps -> BoolAnd <$> procExps exps
    BoolOr  exps -> BoolOr <$> procExps exps
    BoolNot e    -> BoolNot <$> updateBoolExp' e
    BoolExists (GExists refqt wh) ->
      (BoolExists . GExists refqt . unBoolExp)
      <$> updateFieldInBoolExp refqt rf (BoolExp wh)
    BoolFld fld  -> BoolFld <$> updateColExp qt rf fld
  where
    procExps = mapM updateBoolExp'
    updateBoolExp' =
      fmap unBoolExp . updateFieldInBoolExp qt rf . BoolExp

updateColExp
  :: (QErrM m, CacheRM m)
  => QualifiedTable -> RenameField -> ColExp-> m ColExp
updateColExp qt rf (ColExp fld val) =
  ColExp updatedFld <$> updatedVal
  where
    updatedFld = bool fld nFld $ opQT == qt && oFld == fld
    updatedVal = do
      fim <- askFieldInfoMap qt
      fi <- askFieldInfo fim fld
      case fi of
        FIColumn _         -> return val
        FIComputedField _ -> return val
        FIRelationship ri  -> do
          let remTable = riRTable ri
          be <- decodeValue val
          ube <- updateFieldInBoolExp remTable rf be
          return $ toJSON ube
        FIRemoteRelationship {} ->
          throw500 "cannot update remote field" -- TODO: determine the proper behavior here (from master).

    (oFld, nFld, opQT) = case rf of
      RFCol (RenameItem tn oCol nCol) -> (fromPGCol oCol, fromPGCol nCol, tn)
      RFRel (RenameItem tn oRel nRel) -> (fromRel oRel, fromRel nRel, tn)

-- rename columns in relationship definitions
updateColInRel
  :: (MonadTx m, CacheRM m)
  => QualifiedTable -> RelName -> RenameCol -> m ()
updateColInRel fromQT rn rnCol = do
  fim <- askFieldInfoMap fromQT
  ri <- askRelType fim rn ""
  let toQT = riRTable ri
  oldDefV <- liftTx $ getRelDef fromQT rn
  newDefV <- case riType ri of
    ObjRel -> fmap toJSON $
      updateColInObjRel fromQT toQT rnCol <$> decodeValue oldDefV
    ArrRel -> fmap toJSON $
      updateColInArrRel fromQT toQT rnCol <$> decodeValue oldDefV
  liftTx $ updateRel fromQT rn newDefV

-- rename columns in relationship definitions
updateColInEventTriggerDef
  :: (MonadTx m)
  => TriggerName -> RenameCol -> m ()
updateColInEventTriggerDef trigName rnCol = do
  (trigTab, trigDef) <- liftTx $ DS.getEventTriggerDef trigName
  void $ liftTx $ DS.updateEventTriggerInCatalog $
    rewriteEventTriggerConf trigTab trigDef
  where
    rewriteSubsCols trigTab = \case
      SubCStar       -> SubCStar
      SubCArray cols -> SubCArray $
                        map (getNewCol rnCol trigTab) cols
    rewriteOpSpec trigTab (SubscribeOpSpec cols payload) =
      SubscribeOpSpec
      (rewriteSubsCols trigTab cols)
      (rewriteSubsCols trigTab <$> payload)
    rewriteTrigOpsDef trigTab (TriggerOpsDef ins upd del man) =
      TriggerOpsDef
      (rewriteOpSpec trigTab <$> ins)
      (rewriteOpSpec trigTab <$> upd)
      (rewriteOpSpec trigTab <$> del)
      man
    rewriteEventTriggerConf trigTab etc =
      etc { etcDefinition =
            rewriteTrigOpsDef trigTab $ etcDefinition etc
          }

updateColInObjRel
  :: QualifiedTable -> QualifiedTable
  -> RenameCol -> ObjRelUsing -> ObjRelUsing
updateColInObjRel fromQT toQT rnCol = \case
  RUFKeyOn col -> RUFKeyOn $ getNewCol rnCol fromQT col
  RUManual manConfig -> RUManual $ updateRelManualConfig fromQT toQT rnCol manConfig

updateColInArrRel
  :: QualifiedTable -> QualifiedTable
  -> RenameCol -> ArrRelUsing -> ArrRelUsing
updateColInArrRel fromQT toQT rnCol = \case
  RUFKeyOn (ArrRelUsingFKeyOn t c) ->
    let updCol = getNewCol rnCol toQT c
    in RUFKeyOn $ ArrRelUsingFKeyOn t updCol
  RUManual manConfig -> RUManual $ updateRelManualConfig fromQT toQT rnCol manConfig

type ColMap = HashMap PGCol PGCol

getNewCol
  :: RenameCol -> QualifiedTable -> PGCol -> PGCol
getNewCol rnCol qt col =
  if opQT == qt && col == oCol then nCol else col
  where
    RenameItem opQT oCol nCol = rnCol

updateRelManualConfig
  :: QualifiedTable -> QualifiedTable
  -> RenameCol -> RelManualConfig -> RelManualConfig
updateRelManualConfig fromQT toQT rnCol manConfig =
  RelManualConfig tn $ updateColMap fromQT toQT rnCol colMap
  where
    RelManualConfig tn colMap = manConfig

updateColMap
  :: QualifiedTable -> QualifiedTable
  -> RenameCol -> ColMap -> ColMap
updateColMap fromQT toQT rnCol =
  M.fromList . map (modCol fromQT *** modCol toQT) . M.toList
  where
    RenameItem qt oCol nCol = rnCol
    modCol colQt col = if colQt == qt && col == oCol then nCol else col

possiblyUpdateCustomColumnNames
  :: MonadTx m => QualifiedTable -> PGCol -> PGCol -> m ()
possiblyUpdateCustomColumnNames qt oCol nCol = do
  TableConfig customRootFields customColumns <- getTableConfig qt
  let updatedCustomColumns =
        M.fromList $ flip map (M.toList customColumns) $
        \(dbCol, val) -> (, val) $ if dbCol == oCol then nCol else dbCol
  when (updatedCustomColumns /= customColumns) $
    updateTableConfig qt $ TableConfig customRootFields updatedCustomColumns

-- database functions for relationships
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
