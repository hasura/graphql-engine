module Hasura.RQL.DDL.Schema.Rename
  ( renameTableInCatalog
  , renameColumnInCatalog
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

import           Control.Arrow                      (first, (***))
import           Data.Aeson

data RenameField a
  = RenameField
  { rfOld :: !a
  , rfNew :: !a
  } deriving (Show, Eq)

data TableRenameField
  = TRFCol !(RenameField PGCol)
  | TRFRel !(RenameField RelName)
  deriving (Show, Eq)

renameTableInCatalog
  :: (MonadTx m)
  => SchemaCache -> QualifiedTable -> QualifiedTable -> m ()
renameTableInCatalog sc newTN oldTN = do
  let allRels = getAllRelations $ scTables sc
  -- Update depended relations on this table with new name
  forM_ allRels $ \rel -> updateRelDefs newTN oldTN rel
  -- Update table name in hdb_catalog
  liftTx $ Q.catchE defaultTxErrorHandler updateTableInCatalog

  where
    QualifiedObject nsn ntn = newTN
    QualifiedObject osn otn = oldTN
    updateTableInCatalog =
      Q.unitQ [Q.sql|
           UPDATE "hdb_catalog"."hdb_table"
              SET table_schema = $1, table_name = $2
            WHERE table_schema = $3 AND table_name = $4
                |] (nsn, ntn, osn, otn) False

renameColumnInCatalog
  :: (MonadTx m)
  => SchemaCache -> PGCol -> PGCol
  -> QualifiedTable -> TableInfo -> m ()
renameColumnInCatalog sc oCol nCol qt ti = do
  -- Check if any relation exists with new column name
  assertFldNotExists
  -- Update cols in permissions
  updatePermFlds qt $ TRFCol $ RenameField oCol nCol
  -- Update right cols in relations
  let allRels = getAllRelations $ scTables sc
  forM_ allRels $ \r -> updateRelRemoteCols oCol nCol qt r
  -- Update left cols in table's relations
  let rels = getRels $ tiFieldInfoMap ti
  updateRelNativeCols oCol nCol rels qt
  where
    assertFldNotExists =
      case M.lookup (fromPGCol oCol) $ tiFieldInfoMap ti of
        Just (FIRelationship _) ->
          throw400 AlreadyExists $ "cannot rename column " <> oCol
          <<> " to " <> nCol <<> " in table " <> qt <<>
          " as a relationship with the name already exists"
        _ -> return ()

renameRelInCatalog
  :: (MonadTx m) => QualifiedTable -> RelName -> RelName -> m ()
renameRelInCatalog qt oldRN newRN = do
  liftTx updateRelName
  updatePermFlds qt $ TRFRel $ RenameField oldRN newRN
  where
    QualifiedObject sn tn = qt
    updateRelName =
      Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.hdb_relationship
             SET rel_name = $1
           WHERE table_schema = $2
             AND table_name = $3
             AND rel_name = $4
          |] (newRN, sn, tn, oldRN) True

-- helper functions for rename table
getRelDef :: QualifiedTable -> RelName -> Q.TxE QErr Value
getRelDef (QualifiedObject sn tn) rn =
  Q.getAltJ . runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler
    [Q.sql|
     SELECT rel_def::json FROM hdb_catalog.hdb_relationship
      WHERE table_schema = $1 AND table_name = $2
        AND rel_name = $3
    |] (sn, tn, rn) True

updateRel :: QualifiedTable
          -> RelName
          -> Value
          -> Q.TxE QErr ()
updateRel (QualifiedObject sn tn) rn relDef =
  Q.unitQE defaultTxErrorHandler [Q.sql|
           UPDATE hdb_catalog.hdb_relationship
              SET rel_def = $1 :: jsonb
            WHERE table_schema = $2
              AND table_name = $3
              AND rel_name = $4
                |] (Q.AltJ relDef, sn , tn, rn) True

updateRelDefs
  :: (MonadTx m)
  => QualifiedTable
  -> QualifiedTable
  -> (QualifiedTable, [RelInfo])
  -> m ()
updateRelDefs newTN oldTN (qt, rels) =
  forM_ rels $ \rel -> when (oldTN == riRTable rel) $
    case riType rel of
      ObjRel -> updateObjRelDef newTN qt $ riName rel
      ArrRel -> updateArrRelDef newTN qt $ riName rel

updateObjRelDef :: (MonadTx m) => QualifiedTable
                -> QualifiedTable -> RelName -> m ()
updateObjRelDef newTN qt rn = do
  oldDefV <- liftTx $ getRelDef qt rn
  oldDef :: ObjRelUsing <- decodeValue oldDefV
  case oldDef of
    RUFKeyOn _ -> return ()
    RUManual (ObjRelManualConfig (RelManualConfig _ rmCols)) -> do
      let newDef = mkObjRelUsing rmCols
      liftTx $ updateRel qt rn $ toJSON (newDef :: ObjRelUsing)
  where
    mkObjRelUsing colMap = RUManual $ ObjRelManualConfig $
      RelManualConfig newTN colMap

updateArrRelDef :: (MonadTx m) => QualifiedTable
                -> QualifiedTable -> RelName -> m ()
updateArrRelDef newTN qt rn = do
  oldDefV <- liftTx $ getRelDef qt rn
  oldDef  <- decodeValue oldDefV
  liftTx $ updateRel qt rn $ toJSON $ mkNewArrRelUsing oldDef
  where
    mkNewArrRelUsing arrRelUsing = case arrRelUsing of
      RUFKeyOn (ArrRelUsingFKeyOn _ c) ->
        RUFKeyOn $ ArrRelUsingFKeyOn newTN c
      RUManual (ArrRelManualConfig (RelManualConfig _ rmCols)) ->
        RUManual $ ArrRelManualConfig $ RelManualConfig newTN rmCols

-- helper functions for rename fields

-- | update fields in premissions
updatePermFlds :: (MonadTx m)
               => QualifiedTable -> TableRenameField -> m ()
updatePermFlds qt@(QualifiedObject sn tn) trf = do
  perms <- liftTx fetchPerms
  forM_ perms $ \(rn, ty, Q.AltJ (pDef :: Value)) ->
    case ty of
      PTInsert -> do
        perm <- decodeValue pDef
        updateInsPermCols trf qt rn perm
      PTSelect -> do
        perm <- decodeValue pDef
        updateSelPermCols trf qt rn perm
      PTUpdate -> do
        perm <- decodeValue pDef
        updateUpdPermCols trf qt rn perm
      PTDelete -> do
        perm <- decodeValue pDef
        updateDelPermCols trf qt rn perm
  where
    fetchPerms = Q.listQE defaultTxErrorHandler [Q.sql|
                  SELECT role_name, perm_type, perm_def::json
                    FROM hdb_catalog.hdb_permission
                   WHERE table_schema = $1
                     AND table_name = $2
                 |] (sn, tn) True

updateInsPermCols
  :: (MonadTx m)
  => TableRenameField -> QualifiedTable -> RoleName -> InsPerm -> m ()
updateInsPermCols trf qt rn (InsPerm chk preset cols) =
  when updNeeded $
    liftTx $ updatePermDefInCatalog PTInsert qt rn $
      InsPerm updBoolExp updPreset updCols
  where
    updNeeded = updNeededFromPreset || updNeededFromBoolExp || updNeededFromCols
    (updPreset, updNeededFromPreset) = fromM $ updatePreset trf <$> preset
    (updCols, updNeededFromCols) = fromM $ updateCols trf <$> cols
    (updBoolExp, updNeededFromBoolExp) = updateBoolExp trf chk

    fromM = maybe (Nothing, False) (first Just)

updateSelPermCols
  :: (MonadTx m)
  => TableRenameField -> QualifiedTable -> RoleName -> SelPerm -> m ()
updateSelPermCols trf qt rn (SelPerm cols fltr limit aggAllwd) =
  when ( updNeededFromCols || updNeededFromBoolExp) $
    liftTx $ updatePermDefInCatalog PTSelect qt rn $
      SelPerm updCols updBoolExp limit aggAllwd
  where
    (updCols, updNeededFromCols) = updateCols trf cols
    (updBoolExp, updNeededFromBoolExp) = updateBoolExp trf fltr

updateUpdPermCols
  :: (MonadTx m)
  => TableRenameField -> QualifiedTable -> RoleName -> UpdPerm -> m ()
updateUpdPermCols trf qt rn (UpdPerm cols fltr) =
  when ( updNeededFromCols || updNeededFromBoolExp) $
    liftTx $ updatePermDefInCatalog PTUpdate qt rn $
      UpdPerm updCols updBoolExp
  where
    (updCols, updNeededFromCols) = updateCols trf cols
    (updBoolExp, updNeededFromBoolExp) = updateBoolExp trf fltr

updateDelPermCols
  :: (MonadTx m)
  => TableRenameField -> QualifiedTable -> RoleName -> DelPerm -> m ()
updateDelPermCols trf qt rn (DelPerm fltr) =
  when updNeededFromBoolExp $
    liftTx $ updatePermDefInCatalog PTDelete qt rn $
      DelPerm updBoolExp
  where
    (updBoolExp, updNeededFromBoolExp) = updateBoolExp trf fltr

updatePreset :: TableRenameField -> Object -> (Object, Bool)
updatePreset trf obj =
  case trf of
    TRFCol (RenameField oCol nCol) -> updatePreset' oCol nCol obj
    TRFRel _                       -> (obj, False)

updatePreset' :: PGCol -> PGCol -> Object -> (Object, Bool)
updatePreset' oCol nCol obj =
  (M.fromList updItems, or isUpds)
  where
    (updItems, isUpds) = unzip $ map procObjItem $ M.toList obj
    procObjItem (k, v) =
      let pgCol = PGCol k
          isUpdated = pgCol == oCol
          updCol = bool pgCol nCol isUpdated
      in ((getPGColTxt updCol, v), isUpdated)

updateCols :: TableRenameField -> PermColSpec -> (PermColSpec, Bool)
updateCols trf permSpec =
  case trf of
    TRFCol (RenameField oCol nCol) -> updateCols' oCol nCol permSpec
    TRFRel _                       -> (permSpec, False)

updateCols' :: PGCol -> PGCol -> PermColSpec -> (PermColSpec, Bool)
updateCols' oCol nCol cols = case cols of
  PCStar -> (cols, False)
  PCCols c -> ( PCCols $ flip map c $ \col -> if col == oCol then nCol else col
              , oCol `elem` c
              )

updateBoolExp :: TableRenameField -> BoolExp -> (BoolExp, Bool)
updateBoolExp trf (BoolExp boolExp) =
  first BoolExp $ updateBoolExp' oFld nFld boolExp
  where
    (oFld, nFld) = case trf of
      TRFCol (RenameField oCol nCol) -> (fromPGCol oCol, fromPGCol nCol)
      TRFRel (RenameField oRN nRN)   -> (fromRel oRN, fromRel nRN)

updateBoolExp' :: FieldName -> FieldName -> GBoolExp ColExp -> (GBoolExp ColExp, Bool)
updateBoolExp' oFld nFld boolExp = case boolExp of
  BoolAnd exps -> (BoolAnd *** or) (updateExps exps)

  BoolOr exps -> (BoolOr *** or) (updateExps exps)

  be@(BoolFld (ColExp fld v)) -> if oFld == fld
                               then ( BoolFld $ ColExp nFld v
                                    , True
                                    )
                               else (be, False)
  BoolNot be -> let updatedExp = updateBoolExp' oFld nFld be
                in ( BoolNot $ fst updatedExp
                   , snd updatedExp
                   )
  where
    updateExps exps = unzip $ flip map exps $ updateBoolExp' oFld nFld

-- | update remote columns of relationships
updateRelRemoteCols
  :: (MonadTx m)
  => PGCol -> PGCol
  -> QualifiedTable
  -> (QualifiedTable, [RelInfo])
  -> m ()
updateRelRemoteCols oCol nCol table (qt, rels) =
  forM_ rels $ \rel -> when (table == riRTable rel) $
    case riType rel of
      ObjRel -> updateObjRelRemoteCol oCol nCol qt $ riName rel
      ArrRel -> updateArrRelRemoteCol oCol nCol qt $ riName rel

updateObjRelRemoteCol :: (MonadTx m) => PGCol -> PGCol
                 -> QualifiedTable -> RelName -> m ()
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

updateArrRelRemoteCol :: (MonadTx m) => PGCol -> PGCol
                -> QualifiedTable -> RelName -> m ()
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

-- | update native columns in relationships
updateRelNativeCols
  :: (MonadTx m) => PGCol -> PGCol -> [RelInfo] -> QualifiedTable -> m ()
updateRelNativeCols oCol nCol rels qt =
  forM_ rels $ \rel -> case riType rel of
    ObjRel -> updateObjRelNativeCol oCol nCol qt $ riName rel
    ArrRel -> updateArrRelNativeCol oCol nCol qt $ riName rel

updateObjRelNativeCol :: (MonadTx m) => PGCol -> PGCol
                 -> QualifiedTable -> RelName -> m ()
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

updateArrRelNativeCol :: (MonadTx m) => PGCol -> PGCol
                -> QualifiedTable -> RelName -> m ()
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

-- | update columns in manual_configuration
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
