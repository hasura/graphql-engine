{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hasura.RQL.DDL.Schema.Table.Internal
  ( renameTable
  , renameColumn
  )
where

import           Hasura.Prelude
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.DDL.Permission.Internal
import           Hasura.RQL.DDL.Relationship
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Data.Map.Strict                    as Map
import qualified Database.PG.Query                  as Q

import           Control.Arrow                      ((***))
import           Data.Aeson

renameTable :: (P2C m) => QualifiedTable -> QualifiedTable -> m ()
renameTable newQT oldQT = do
   allRels <- getAllRelations . scTables <$> askSchemaCache
   -- Update depended relations on this table with new name
   forM_ allRels $ \rel -> updateRelDefs newQT oldQT rel
   -- Update table name in hdb_catalog
   liftTx $ Q.catchE defaultTxErrorHandler $
     updateTableInCatalog oldQT newQT

renameColumn :: (P2C m) => PGCol -> PGCol -> QualifiedTable -> TableInfo -> m ()
renameColumn oCol nCol qt ti = do
  sc <- askSchemaCache
  -- Update cols in permissions
  updatePermCols oCol nCol qt
  -- Update right cols in relations
  let allRels = getAllRelations $ scTables sc
  forM_ allRels $ \r -> updateRelRCols oCol nCol qt r
  -- Update left cols in table's relations
  let rels = getRels $ tiFieldInfoMap ti
  updateRelLCols oCol nCol rels qt

-- helper functions for rename table
getRelDef :: QualifiedTable -> RelName -> Q.TxE QErr Value
getRelDef (QualifiedTable sn tn) rn =
  Q.getAltJ . runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler
    [Q.sql|
     SELECT rel_def::json FROM hdb_catalog.hdb_relationship
      WHERE table_schema = $1 AND table_name = $2
        AND rel_name = $3
    |] (sn, tn, rn) True

updateRelDefs
  :: (P2C m)
  => QualifiedTable
  -> QualifiedTable
  -> (QualifiedTable, [RelInfo])
  -> m ()
updateRelDefs newQT oldQT (qt, rels) =
  forM_ rels $ \rel -> when (oldQT == riRTable rel) $
    case riType rel of
      ObjRel -> updateObjRelDef newQT qt $ riName rel
      ArrRel -> updateArrRelDef newQT qt $ riName rel

updateObjRelDef :: (P2C m) => QualifiedTable
                -> QualifiedTable -> RelName -> m ()
updateObjRelDef newQT qt rn = do
  oldDefV <- liftTx $ getRelDef qt rn
  oldDef :: ObjRelUsing <- decodeValue oldDefV
  case oldDef of
    RUFKeyOn _ -> return ()
    RUManual (ObjRelManualConfig (RelManualConfig _ rmCols)) -> do
      let newDef = mkObjRelUsing rmCols
      liftTx $ updateRel qt rn $ toJSON (newDef :: ObjRelUsing)
  where
    mkObjRelUsing colMap = RUManual $ ObjRelManualConfig $
      RelManualConfig newQT colMap

updateArrRelDef :: (P2C m) => QualifiedTable
                -> QualifiedTable -> RelName -> m ()
updateArrRelDef newQT qt rn = do
  oldDefV <- liftTx $ getRelDef qt rn
  oldDef  <- decodeValue oldDefV
  liftTx $ updateRel qt rn $ toJSON $ mkNewArrRelUsing oldDef
  where
    mkNewArrRelUsing arrRelUsing = case arrRelUsing of
      RUFKeyOn (ArrRelUsingFKeyOn _ c) ->
        RUFKeyOn $ ArrRelUsingFKeyOn newQT c
      RUManual (ArrRelManualConfig (RelManualConfig _ rmCols)) ->
        RUManual $ ArrRelManualConfig $ RelManualConfig newQT rmCols

-- helper functions for rename column
updatePermCols :: (P2C m) => PGCol -> PGCol -> QualifiedTable -> m ()
updatePermCols oCol nCol qt@(QualifiedTable sn tn) = do
  perms <- liftTx fetchPerms
  forM_ perms $ \(rn, ty, Q.AltJ (pDef :: Value)) ->
    case ty of
      PTInsert -> do
        perm <- decodeValue pDef
        updateInsPermCols oCol nCol $
          WithTable qt $ PermDef rn perm Nothing
      PTSelect -> do
        perm <- decodeValue pDef
        updateSelPermCols oCol nCol $
          WithTable qt $ PermDef rn perm Nothing
      PTUpdate -> do
        perm <- decodeValue pDef
        updateUpdPermCols oCol nCol $
          WithTable qt $ PermDef rn perm Nothing
      PTDelete -> do
        perm <- decodeValue pDef
        updateDelPermCols oCol nCol $
          WithTable qt $ PermDef rn perm Nothing
  where
    fetchPerms = Q.listQE defaultTxErrorHandler [Q.sql|
                  SELECT role_name, perm_type, perm_def::json
                    FROM hdb_catalog.hdb_permission
                   WHERE table_schema = $1
                     AND table_name = $2
                 |] (sn, tn) True

updateInsPermCols :: (P2C m) => PGCol -> PGCol -> CreateInsPerm -> m ()
updateInsPermCols oCol nCol (WithTable qt (PermDef rn (InsPerm chk _) _)) = do
  let updatedBoolExp = updateBoolExp oCol nCol chk
  when (snd updatedBoolExp) $
    liftTx $ updatePermInCatalog PTInsert qt $
      PermDef rn (InsPerm (fst updatedBoolExp) Nothing) Nothing

updateSelPermCols :: (P2C m) => PGCol -> PGCol -> CreateSelPerm -> m ()
updateSelPermCols oCol nCol (WithTable qt (PermDef rn (SelPerm cols fltr) _)) =
  when ( updNeededFromCols || updNeededFromBoolExp) $
    liftTx $ updatePermInCatalog PTSelect qt $
      PermDef rn (SelPerm updCols updBoolExp) Nothing
  where
    (updCols, updNeededFromCols) = updateCols oCol nCol cols
    (updBoolExp, updNeededFromBoolExp) = updateBoolExp oCol nCol fltr

updateUpdPermCols :: (P2C m) => PGCol -> PGCol -> CreateUpdPerm -> m ()
updateUpdPermCols oCol nCol (WithTable qt (PermDef rn (UpdPerm cols fltr) _)) =
  when ( updNeededFromCols || updNeededFromBoolExp) $
    liftTx $ updatePermInCatalog PTUpdate qt $
      PermDef rn (UpdPerm updCols updBoolExp) Nothing
  where
    (updCols, updNeededFromCols) = updateCols oCol nCol cols
    (updBoolExp, updNeededFromBoolExp) = updateBoolExp oCol nCol fltr

updateDelPermCols :: (P2C m) => PGCol -> PGCol -> CreateDelPerm -> m ()
updateDelPermCols oCol nCol (WithTable qt (PermDef rn (DelPerm fltr)_)) = do
  let updatedFltrExp = updateBoolExp oCol nCol fltr
  when (snd updatedFltrExp) $
    liftTx $ updatePermInCatalog PTDelete qt $
      PermDef rn (DelPerm $ fst updatedFltrExp) Nothing

updateTableInCatalog :: QualifiedTable -> QualifiedTable -> Q.Tx ()
updateTableInCatalog oldTable newTable =
  Q.unitQ [Q.sql|
           UPDATE "hdb_catalog"."hdb_table"
              SET table_schema = $1, table_name = $2
            WHERE table_schema = $3 AND table_name = $4
                |] (nsn, ntn, osn, otn) False
  where
    QualifiedTable osn otn = oldTable
    QualifiedTable nsn ntn = newTable

updateCols :: PGCol -> PGCol -> PermColSpec -> (PermColSpec, Bool)
updateCols oCol nCol cols = case cols of
  PCStar -> (cols, False)
  PCCols c -> ( PCCols $ flip map c $ \col -> if col == oCol then nCol else col
              , oCol `elem` c
              )

updateBoolExp :: PGCol -> PGCol -> BoolExp -> (BoolExp, Bool)
updateBoolExp oCol nCol boolExp = case boolExp of
  BoolAnd exps -> (BoolAnd *** or) (updateExps exps)

  BoolOr exps -> (BoolOr *** or) (updateExps exps)

  be@(BoolCol (ColExp c v)) -> if oCol == PGCol (getFieldNameTxt c)
                               then ( BoolCol $ ColExp (fromPGCol nCol) v
                                    , True
                                    )
                               else (be, False)
  BoolNot be -> let updatedExp = updateBoolExp oCol nCol be
                in ( BoolNot $ fst updatedExp
                   , snd updatedExp
                   )
  where
    updateExps exps = unzip $ flip map exps $ updateBoolExp oCol nCol

-- update right columns
updateRelRCols
  :: (P2C m)
  => PGCol -> PGCol
  -> QualifiedTable
  -> (QualifiedTable, [RelInfo])
  -> m ()
updateRelRCols oCol nCol table (qt, rels) =
  forM_ rels $ \rel -> when (table == riRTable rel) $
    case riType rel of
      ObjRel -> updateObjRelRCol oCol nCol qt $ riName rel
      ArrRel -> updateArrRelRCol oCol nCol qt $ riName rel

updateObjRelRCol :: (P2C m) => PGCol -> PGCol
                 -> QualifiedTable -> RelName -> m ()
updateObjRelRCol oCol nCol qt rn = do
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

updateArrRelRCol :: (P2C m) => PGCol -> PGCol
                -> QualifiedTable -> RelName -> m ()
updateArrRelRCol oCol nCol qt rn = do
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

-- update left columns
updateRelLCols
  :: (P2C m) => PGCol -> PGCol -> [RelInfo] -> QualifiedTable -> m ()
updateRelLCols oCol nCol rels qt =
  forM_ rels $ \rel -> case riType rel of
    ObjRel -> updateObjRelLCol oCol nCol qt $ riName rel
    ArrRel -> updateArrRelLCol oCol nCol qt $ riName rel

updateObjRelLCol :: (P2C m) => PGCol -> PGCol
                 -> QualifiedTable -> RelName -> m ()
updateObjRelLCol oCol nCol qt rn = do
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

updateArrRelLCol :: (P2C m) => PGCol -> PGCol
                -> QualifiedTable -> RelName -> m ()
updateArrRelLCol oCol nCol qt rn = do
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

-- update columns in manual_configuration
type ColMapModifier = (PGCol -> PGCol) -> Map.Map PGCol PGCol -> Map.Map PGCol PGCol
type ColAccessor = (PGCol, PGCol) -> PGCol

updateColForManualConfig
  :: PGCol -> PGCol
  -> ColMapModifier -> ColAccessor
  -> RelManualConfig -> (RelManualConfig, Bool)
updateColForManualConfig oCol nCol modFn accFn (RelManualConfig tn rmCols) =
  let updatedColMap =
        flip modFn rmCols $ \col -> if col == oCol then nCol else col
  in
  ( RelManualConfig tn updatedColMap
  , oCol `elem` map accFn (Map.toList rmCols)
  )
