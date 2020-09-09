{-# LANGUAGE ViewPatterns #-}

-- | Functions for mutating the catalog (with integrity checking) to incorporate schema changes
-- discovered after applying a user-supplied SQL query. None of these functions modify the schema
-- cache, so it must be reloaded after the catalog is updated.
module Hasura.RQL.DDL.Schema.Rename
  ( renameTableInMetadata
  , renameColumnInMetadata
  , renameRelationshipInMetadata
  )
where

import           Control.Lens.Combinators
import           Control.Lens.Operators
import           Hasura.Prelude
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.Types
import           Hasura.Session
import           Hasura.SQL.Types

import           Data.Aeson
import qualified Data.HashMap.Strict           as M
import qualified Data.List.NonEmpty            as NE
import qualified Data.Set                      as Set
import qualified Language.GraphQL.Draft.Syntax as G

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

renameTableInMetadata
  :: ( MonadError QErr m
     , CacheRM m
     , MonadWriter MetadataModifier m
     )
  => SourceName -> QualifiedTable -> QualifiedTable -> m ()
renameTableInMetadata source newQT oldQT = do
  sc <- askSchemaCache
  let allDeps = getDependentObjs sc $ SOTable oldQT

  -- update all dependant schema objects
  forM_ allDeps $ \case
    SOTableObj refQT (TORel rn _) ->
      updateRelDefs source refQT rn (oldQT, newQT)
    SOTableObj refQT (TOPerm rn pt)   ->
      updatePermFlds source refQT rn pt $ RTable (oldQT, newQT)
    -- A trigger's definition is not dependent on the table directly
    SOTableObj _ (TOTrigger _)   -> pure ()
    -- A remote relationship's definition is not dependent on the table directly
    SOTableObj _ (TORemoteRel _) -> pure ()

    d -> otherDeps errMsg d
  -- Update table name in metadata
  tell $ MetadataModifier $ metaSources.ix source.smTables %~ \tables ->
    flip (maybe tables) (M.lookup oldQT tables) $
    \tableMeta -> M.delete oldQT $ M.insert newQT tableMeta tables
  where
    errMsg = "cannot rename table " <> oldQT <<> " to " <>> newQT
    -- updateTableInCatalog =
    --   Q.unitQ [Q.sql|
    --        UPDATE "hdb_catalog"."hdb_table"
    --           SET table_schema = $1, table_name = $2
    --         WHERE table_schema = $3 AND table_name = $4
    --             |] (nsn, ntn, osn, otn) False

renameColumnInMetadata
  :: ( MonadError QErr m
     , CacheRM m
     , MonadWriter MetadataModifier m
     )
  => PGCol -> PGCol -> SourceName -> QualifiedTable -> FieldInfoMap FieldInfo -> m ()
renameColumnInMetadata oCol nCol sourceName qt fieldInfo = do
  sc <- askSchemaCache
  -- Check if any relation exists with new column name
  assertFldNotExists
  -- Fetch dependent objects
  let depObjs = getDependentObjs sc $ SOTableObj qt $ TOCol oCol
      renameFld = RFCol $ RenameItem qt oCol nCol
  -- Update dependent objects
  forM_ depObjs $ \case
    SOTableObj refQT (TOPerm role pt) ->
      updatePermFlds sourceName refQT role pt $ RField renameFld
    SOTableObj refQT (TORel rn _) ->
      updateColInRel sourceName refQT rn $ RenameItem qt oCol nCol
    SOTableObj refQT (TOTrigger triggerName) ->
      updateColInEventTriggerDef sourceName refQT triggerName $ RenameItem qt oCol nCol
    SOTableObj _ (TORemoteRel remoteRelName) ->
      updateColInRemoteRelationship sourceName remoteRelName $ RenameItem qt oCol nCol
    d -> otherDeps errMsg d
  -- Update custom column names
  possiblyUpdateCustomColumnNames sourceName qt oCol nCol
  where
    errMsg = "cannot rename column " <> oCol <<> " to " <>> nCol
    assertFldNotExists =
      case M.lookup (fromPGCol oCol) fieldInfo of
        Just (FIRelationship _) ->
          throw400 AlreadyExists $ "cannot rename column " <> oCol
          <<> " to " <> nCol <<> " in table " <> qt <<>
          " as a relationship with the name already exists"
        _ -> pure ()

renameRelationshipInMetadata
  :: ( MonadError QErr m
     , CacheRM m
     , MonadWriter MetadataModifier m
     )
  => SourceName -> QualifiedTable -> RelName -> RelType -> RelName -> m ()
renameRelationshipInMetadata source qt oldRN relType newRN = do
  sc <- askSchemaCache
  let depObjs = getDependentObjs sc $ SOTableObj qt $ TORel oldRN relType
      renameFld = RFRel $ RenameItem qt oldRN newRN

  forM_ depObjs $ \case
    SOTableObj refQT (TOPerm role pt) ->
      updatePermFlds source refQT role pt $ RField renameFld
    d -> otherDeps errMsg d
  tell $ MetadataModifier $ tableMetadataSetter source qt %~ case relType of
    ObjRel -> tmObjectRelationships %~ rewriteRelationships
    ArrRel -> tmArrayRelationships  %~ rewriteRelationships
  where
    errMsg = "cannot rename relationship " <> oldRN <<> " to " <>> newRN
    rewriteRelationships
      :: Relationships (RelDef a) -> Relationships (RelDef a)
    rewriteRelationships relationsMap =
      flip (maybe relationsMap) (M.lookup oldRN relationsMap) $
      \rd -> M.insert newRN rd{_rdName = newRN} $ M.delete oldRN relationsMap
    -- updateRelName =
    --   Q.unitQE defaultTxErrorHandler [Q.sql|
    --     UPDATE hdb_catalog.hdb_relationship
    --        SET rel_name = $1
    --      WHERE table_schema = $2
    --        AND table_name = $3
    --        AND rel_name = $4
    --   |] (newRN, sn, tn, oldRN) True

-- update table names in relationship definition
updateRelDefs
  :: ( MonadError QErr m
     , CacheRM m
     , MonadWriter MetadataModifier m
     )
  => SourceName -> QualifiedTable -> RelName ->  RenameTable -> m ()
updateRelDefs source qt rn renameTable = do
  fim <- askFieldInfoMap source qt
  ri <- askRelType fim rn ""
  tell $ MetadataModifier $ tableMetadataSetter source qt %~ case riType ri of
    ObjRel -> tmObjectRelationships.ix rn %~ updateObjRelDef renameTable
    ArrRel -> tmArrayRelationships.ix rn %~ updateArrRelDef renameTable
  where
    updateObjRelDef :: RenameTable -> ObjRelDef -> ObjRelDef
    updateObjRelDef (oldQT, newQT) =
      rdUsing %~ \case
      RUFKeyOn fk -> RUFKeyOn fk
      RUManual (RelManualConfig dbQT rmCols) ->
        let updQT = bool oldQT newQT $ oldQT == dbQT
        in RUManual $ RelManualConfig updQT rmCols

    updateArrRelDef :: RenameTable -> ArrRelDef -> ArrRelDef
    updateArrRelDef (oldQT, newQT) =
      rdUsing %~ \case
      RUFKeyOn (ArrRelUsingFKeyOn dbQT c) ->
        let updQT = getUpdQT dbQT
        in RUFKeyOn $ ArrRelUsingFKeyOn updQT c
      RUManual (RelManualConfig dbQT rmCols) ->
        let updQT = getUpdQT dbQT
        in RUManual $ RelManualConfig updQT rmCols
      where
        getUpdQT dbQT = bool oldQT newQT $ oldQT == dbQT

-- | update fields in premissions
updatePermFlds
  :: ( MonadError QErr m
     , CacheRM m
     , MonadWriter MetadataModifier m
     )
  => SourceName -> QualifiedTable -> RoleName -> PermType -> Rename -> m ()
updatePermFlds source refQT rn pt rename = do
  tables <- getSourceTables source
  let withTables :: Reader TableCache a -> a
      withTables = flip runReader tables
  tell $ MetadataModifier $
    tableMetadataSetter source refQT %~ case pt of
      PTInsert ->
        tmInsertPermissions.ix rn.pdPermission %~ \insPerm ->
          withTables $ updateInsPermFlds refQT rename insPerm
      PTSelect ->
        tmSelectPermissions.ix rn.pdPermission %~ \selPerm ->
          withTables $ updateSelPermFlds refQT rename selPerm
      PTUpdate ->
        tmUpdatePermissions.ix rn.pdPermission %~ \updPerm ->
          withTables $ updateUpdPermFlds refQT rename updPerm
      PTDelete ->
        tmDeletePermissions.ix rn.pdPermission %~ \delPerm ->
          withTables $ updateDelPermFlds refQT rename delPerm

updateInsPermFlds
  :: (MonadReader TableCache m)
  => QualifiedTable -> Rename -> InsPerm -> m InsPerm
updateInsPermFlds refQT rename (InsPerm chk preset cols mBackendOnly) =
  case rename of
    RTable rt -> do
      let updChk = updateTableInBoolExp rt chk
      pure $ InsPerm updChk preset cols mBackendOnly
    RField rf -> do
      updChk <- updateFieldInBoolExp refQT rf chk
      let updPresetM = updatePreset refQT rf <$> preset
          updColsM = updateCols refQT rf <$> cols
      pure $ InsPerm updChk updPresetM updColsM mBackendOnly

updateSelPermFlds
  :: (MonadReader TableCache m)
  => QualifiedTable -> Rename -> SelPerm -> m SelPerm
updateSelPermFlds refQT rename (SelPerm cols fltr limit aggAllwd computedFields) = do
  case rename of
    RTable rt -> do
      let updFltr = updateTableInBoolExp rt fltr
      pure $ SelPerm cols updFltr limit aggAllwd computedFields
    RField rf -> do
      updFltr <- updateFieldInBoolExp refQT rf fltr
      let updCols = updateCols refQT rf cols
      pure $ SelPerm updCols updFltr limit aggAllwd computedFields

updateUpdPermFlds
  :: (MonadReader TableCache m)
  => QualifiedTable -> Rename -> UpdPerm -> m (UpdPerm)
updateUpdPermFlds refQT rename (UpdPerm cols preset fltr check) = do
  case rename of
    RTable rt -> do
      let updFltr = updateTableInBoolExp rt fltr
          updCheck = fmap (updateTableInBoolExp rt) check
      pure $ UpdPerm cols preset updFltr updCheck
    RField rf -> do
      updFltr <- updateFieldInBoolExp refQT rf fltr
      updCheck <- traverse (updateFieldInBoolExp refQT rf) check
      let updCols = updateCols refQT rf cols
          updPresetM = updatePreset refQT rf <$> preset
      pure $ UpdPerm updCols updPresetM updFltr updCheck

updateDelPermFlds
  :: (MonadReader TableCache m)
  => QualifiedTable -> Rename -> DelPerm -> m DelPerm
updateDelPermFlds refQT rename (DelPerm fltr) = do
  DelPerm <$> case rename of
    RTable rt -> pure $ updateTableInBoolExp rt fltr
    RField rf -> updateFieldInBoolExp refQT rf fltr

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
  :: (MonadReader TableCache m)
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
  :: (MonadReader TableCache m)
  => QualifiedTable -> RenameField -> ColExp -> m ColExp
updateColExp qt rf (ColExp fld val) =
  ColExp updatedFld <$> updatedVal
  where
    updatedFld = bool fld nFld $ opQT == qt && oFld == fld
    updatedVal = do
      tables <- ask
      let maybeFieldInfo = M.lookup qt tables >>=
            M.lookup fld . _tciFieldInfoMap . _tiCoreInfo
      case maybeFieldInfo of
        Nothing -> pure val
        Just fi -> case fi of
          FIColumn _         -> pure val
          FIComputedField _  -> pure val
          FIRelationship ri  -> do
            let remTable = riRTable ri
            case decodeValue val of
              Left _   -> pure val
              Right be -> toJSON <$> updateFieldInBoolExp remTable rf be

          FIRemoteRelationship{} -> pure val

    (oFld, nFld, opQT) = case rf of
      RFCol (RenameItem tn oCol nCol) -> (fromPGCol oCol, fromPGCol nCol, tn)
      RFRel (RenameItem tn oRel nRel) -> (fromRel oRel, fromRel nRel, tn)

-- rename columns in relationship definitions
updateColInRel
  :: (CacheRM m, MonadWriter MetadataModifier m)
  => SourceName -> QualifiedTable -> RelName -> RenameCol -> m ()
updateColInRel source fromQT rn rnCol = do
  tables <- getSourceTables source
  let maybeRelInfo =
        tables ^? ix fromQT.tiCoreInfo.tciFieldInfoMap.ix (fromRel rn)._FIRelationship
  forM_ maybeRelInfo $ \relInfo ->
    tell $ MetadataModifier $ tableMetadataSetter source fromQT %~
    case riType relInfo of
      ObjRel -> tmObjectRelationships.ix rn.rdUsing %~
                updateColInObjRel fromQT (riRTable relInfo) rnCol
      ArrRel -> tmArrayRelationships.ix rn.rdUsing %~
                updateColInArrRel fromQT (riRTable relInfo) rnCol

updateColInRemoteRelationship
  :: ( MonadError QErr m
     , MonadWriter MetadataModifier m
     )
  => SourceName -> RemoteRelationshipName -> RenameCol -> m ()
updateColInRemoteRelationship source remoteRelationshipName renameCol = do
  oldColName <- parseGraphQLName $ getPGColTxt oldCol
  newColName <- parseGraphQLName $ getPGColTxt newCol
  tell $ MetadataModifier $
    tableMetadataSetter source qt.tmRemoteRelationships.ix remoteRelationshipName.rrmDefinition %~
      (rrdHasuraFields %~ modifyHasuraFields) .
      (rrdRemoteField %~ modifyFieldCalls oldColName newColName)
  -- (RemoteRelationshipDef remoteSchemaName hasuraFlds remoteFields) <-
  --   liftTx $ RR.getRemoteRelDefFromCatalog remoteRelationshipName qt
  -- let oldColPGTxt = getPGColTxt oldCol
  --     newColPGTxt = getPGColTxt newCol
  --     oldColFieldName = FieldName $ oldColPGTxt
  --     newColFieldName = FieldName $ newColPGTxt
  --     modifiedHasuraFlds = Set.insert newColFieldName $ Set.delete oldColFieldName hasuraFlds
  --     fieldCalls = unRemoteFields remoteFields
  -- oldColName <- parseGraphQLName oldColPGTxt
  -- newColName <- parseGraphQLName newColPGTxt
  -- let modifiedFieldCalls = NE.map (\(FieldCall name args) ->
  --                                    let remoteArgs = getRemoteArguments args
  --                                    in FieldCall name $ RemoteArguments $
  --                                        fmap (replaceVariableName oldColName newColName) remoteArgs
  --                                 ) $ fieldCalls
  -- liftTx $ RR.updateRemoteRelInCatalog (RemoteRelationship remoteRelationshipName source qt modifiedHasuraFlds remoteSchemaName (RemoteFields modifiedFieldCalls))
  where
    (RenameItem qt oldCol newCol) = renameCol
    modifyHasuraFields = Set.insert (fromPGCol newCol) . Set.delete (fromPGCol oldCol)
    modifyFieldCalls oldColName newColName =
      RemoteFields
      . NE.map (\(FieldCall name args) ->
                   let remoteArgs = getRemoteArguments args
                   in FieldCall name $ RemoteArguments $

                                      fmap (replaceVariableName oldColName newColName) remoteArgs
                      -- map (\(G.ObjectFieldG key val) ->
                      --         G.ObjectFieldG key $ replaceVariableName oldColName newColName val
                      --     ) remoteArgs
               )
      . unRemoteFields

    parseGraphQLName txt = maybe (throw400 ParseFailed errMsg) pure $ G.mkName txt
      where
        errMsg = txt <> " is not a valid GraphQL name"

    replaceVariableName :: G.Name -> G.Name -> G.Value G.Name -> G.Value G.Name
    replaceVariableName oldColName newColName = \case
      G.VVariable oldColName' ->
        G.VVariable $ bool oldColName newColName $ oldColName == oldColName'
      G.VList values -> G.VList $ map (replaceVariableName oldColName newColName) values
      G.VObject values ->
        G.VObject $ fmap (replaceVariableName oldColName newColName) values
      v -> v

-- rename columns in relationship definitions
updateColInEventTriggerDef
  :: (MonadWriter MetadataModifier m)
  => SourceName -> QualifiedTable -> TriggerName -> RenameCol -> m ()
updateColInEventTriggerDef source table trigName rnCol =
  tell $ MetadataModifier $
    tableMetadataSetter source table.tmEventTriggers.ix trigName %~ rewriteEventTriggerConf
  where
    rewriteSubsCols = \case
      SubCStar       -> SubCStar
      SubCArray cols -> SubCArray $
                        map (getNewCol rnCol table) cols
    rewriteOpSpec (SubscribeOpSpec cols payload) =
      SubscribeOpSpec
      (rewriteSubsCols cols)
      (rewriteSubsCols <$> payload)
    rewriteTrigOpsDef (TriggerOpsDef ins upd del man) =
      TriggerOpsDef
      (rewriteOpSpec <$> ins)
      (rewriteOpSpec <$> upd)
      (rewriteOpSpec <$> del)
      man
    rewriteEventTriggerConf etc =
      etc { etcDefinition =
            rewriteTrigOpsDef $ etcDefinition etc
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
  :: MonadWriter MetadataModifier m
  => SourceName -> QualifiedTable -> PGCol -> PGCol -> m ()
possiblyUpdateCustomColumnNames source qt oCol nCol = do
  let updateCustomColumns customColumns =
        M.fromList $ flip map (M.toList customColumns) $
        \(dbCol, val) -> (, val) $ if dbCol == oCol then nCol else dbCol
  tell $ MetadataModifier $
    tableMetadataSetter source qt.tmConfiguration.tcCustomColumnNames %~ updateCustomColumns

getSourceTables :: CacheRM m => SourceName -> m TableCache
getSourceTables source =
  (maybe mempty _pcTables . M.lookup source . scPostgres) <$> askSchemaCache

-- database functions for relationships
-- getRelDef :: QualifiedTable -> RelName -> Q.TxE QErr Value
-- getRelDef (QualifiedObject sn tn) rn =
--   Q.getAltJ . runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler
--     [Q.sql|
--      SELECT rel_def::json FROM hdb_catalog.hdb_relationship
--       WHERE table_schema = $1 AND table_name = $2
--         AND rel_name = $3
--     |] (sn, tn, rn) True

-- updateRel :: QualifiedTable -> RelName -> Value -> Q.TxE QErr ()
-- updateRel (QualifiedObject sn tn) rn relDef =
--   Q.unitQE defaultTxErrorHandler [Q.sql|
--            UPDATE hdb_catalog.hdb_relationship
--               SET rel_def = $1 :: jsonb
--             WHERE table_schema = $2
--               AND table_name = $3
--               AND rel_name = $4
--                 |] (Q.AltJ relDef, sn , tn, rn) True
