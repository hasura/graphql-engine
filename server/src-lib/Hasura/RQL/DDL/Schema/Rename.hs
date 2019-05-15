module Hasura.RQL.DDL.Schema.Rename
  ( renameTableInCatalog
  , renameColInCatalog
  , renameRelInCatalog
  )
where

import           Control.Arrow                      ((***))
import           Hasura.Prelude
import qualified Hasura.RQL.DDL.EventTrigger        as DS
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

type RenameCol = RenameItem PGCol
data RenameField
  = RFCol !RenameCol
  | RFRel !(RenameItem RelName)
  deriving (Show, Eq)

type RenameTable = (QualifiedTable, QualifiedTable)

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
      updateRelDefs refQT rn (oldQT, newQT)
    -- table names are not specified in permission definitions
    SOTableObj _ (TOPerm _ _)   -> return ()
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
      updateColInRel refQT rn $ RenameItem qt oCol nCol
    SOTableObj _ (TOTrigger triggerName) ->
      updateColInEventTriggerDef triggerName $ RenameItem qt oCol nCol
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
        RUManual (ObjRelManualConfig (RelManualConfig dbQT rmCols)) ->
          let updQT = bool oldQT newQT $ oldQT == dbQT
          in RUManual $ ObjRelManualConfig $ RelManualConfig updQT rmCols
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
        RUManual (ArrRelManualConfig (RelManualConfig dbQT rmCols)) ->
          let updQT = getUpdQT dbQT
          in RUManual $ ArrRelManualConfig $ RelManualConfig updQT rmCols
  liftTx $ updateRel qt rn $ toJSON newDef
  where
    getUpdQT dbQT = bool oldQT newQT $ oldQT == dbQT

-- | update fields in premissions
updatePermFlds :: (MonadTx m, CacheRM m)
  => QualifiedTable -> RoleName -> PermType -> RenameField -> m ()
updatePermFlds refQT rn pt rf = do
  Q.AltJ pDef <- liftTx fetchPermDef
  case pt of
    PTInsert -> do
      perm <- decodeValue pDef
      updateInsPermFlds refQT rf rn perm
    PTSelect -> do
      perm <- decodeValue pDef
      updateSelPermFlds refQT rf rn perm
    PTUpdate -> do
      perm <- decodeValue pDef
      updateUpdPermFlds refQT rf rn perm
    PTDelete -> do
      perm <- decodeValue pDef
      updateDelPermFlds refQT rf rn perm
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

updateInsPermFlds
  :: (MonadTx m, CacheRM m)
  => QualifiedTable -> RenameField -> RoleName -> InsPerm -> m ()
updateInsPermFlds qt rf rn (InsPerm chk preset cols) = do
  updBoolExp <- updateBoolExp qt rf chk
  liftTx $ updatePermDefInCatalog PTInsert qt rn $
    InsPerm updBoolExp updPresetM updColsM
  where
    updPresetM = updatePreset qt rf <$> preset
    updColsM = updateCols qt rf <$> cols

updateSelPermFlds
  :: (MonadTx m, CacheRM m)
  => QualifiedTable -> RenameField -> RoleName -> SelPerm -> m ()
updateSelPermFlds refQT rf rn (SelPerm cols fltr limit aggAllwd) = do
  updBoolExp <- updateBoolExp refQT rf fltr
  liftTx $ updatePermDefInCatalog PTSelect refQT rn $
      SelPerm updCols updBoolExp limit aggAllwd
  where
    updCols = updateCols refQT rf cols

updateUpdPermFlds
  :: (MonadTx m, CacheRM m)
  => QualifiedTable -> RenameField -> RoleName -> UpdPerm -> m ()
updateUpdPermFlds refQT rf rn (UpdPerm cols preset fltr) = do
  updBoolExp <- updateBoolExp refQT rf fltr
  liftTx $ updatePermDefInCatalog PTUpdate refQT rn $
      UpdPerm updCols updPresetM updBoolExp
  where
    updCols = updateCols refQT rf cols
    updPresetM = updatePreset refQT rf <$> preset

updateDelPermFlds
  :: (MonadTx m, CacheRM m)
  => QualifiedTable -> RenameField -> RoleName -> DelPerm -> m ()
updateDelPermFlds refQT rf rn (DelPerm fltr) = do
  updBoolExp <- updateBoolExp refQT rf fltr
  liftTx $ updatePermDefInCatalog PTDelete refQT rn $
    DelPerm updBoolExp

updatePreset
  :: QualifiedTable -> RenameField -> ColVals -> ColVals
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

updateBoolExp
  :: (QErrM m, CacheRM m)
  => QualifiedTable -> RenameField -> BoolExp -> m BoolExp
updateBoolExp qt rf =
  fmap BoolExp . traverse (updateColExp qt rf) . unBoolExp

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
        FIColumn _ -> return val
        FIRelationship ri -> do
          let remTable = riRTable ri
          be <- decodeValue val
          ube <- updateBoolExp remTable rf be
          return $ toJSON ube

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
  void $ liftTx $ DS.updateEventTriggerDef trigName $
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
  RUManual (ObjRelManualConfig manConfig) ->
    RUManual $ ObjRelManualConfig $
    updateRelManualConfig fromQT toQT rnCol manConfig

updateColInArrRel
  :: QualifiedTable -> QualifiedTable
  -> RenameCol -> ArrRelUsing -> ArrRelUsing
updateColInArrRel fromQT toQT rnCol = \case
  RUFKeyOn (ArrRelUsingFKeyOn t c) ->
    let updCol = getNewCol rnCol toQT c
    in RUFKeyOn $ ArrRelUsingFKeyOn t updCol
  RUManual (ArrRelManualConfig manConfig) ->
    RUManual $ ArrRelManualConfig $
    updateRelManualConfig fromQT toQT rnCol manConfig

type ColMap = Map.Map PGCol PGCol

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
updateColMap fromQT toQT rnCol colMap =
  Map.fromList $ map (modCol fromQT *** modCol toQT) (Map.toList colMap)
  where
    RenameItem qt oCol nCol = rnCol
    modCol colQt col = if colQt == qt && col == oCol then nCol else col

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
