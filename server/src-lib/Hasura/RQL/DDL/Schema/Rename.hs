-- | Functions for updating the metadata (with integrity checking) to incorporate schema changes
-- discovered after applying a user-supplied SQL query. None of these functions modify the schema
-- cache, so it must be reloaded after the metadata is updated.
module Hasura.RQL.DDL.Schema.Rename
  ( renameTableInMetadata
  , renameColumnInMetadata
  , renameRelationshipInMetadata
  )
where

import           Control.Lens.Combinators
import           Control.Lens.Operators
import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Prelude
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.Types
import           Hasura.Session

import           Data.Aeson
import           Data.Text.Extended

import qualified Data.HashMap.Strict                as M
import qualified Data.HashMap.Strict.InsOrd         as OMap
import qualified Data.List.NonEmpty                 as NE
import qualified Data.Set                           as Set
import qualified Language.GraphQL.Draft.Syntax      as G

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
  let allDeps = getDependentObjs sc $ SOSourceObj source $ SOITable oldQT

  -- update all dependant schema objects
  forM_ allDeps $ \case
    (SOSourceObj _ (SOITableObj refQT (TORel rn))) ->
      updateRelDefs source refQT rn (oldQT, newQT)
    (SOSourceObj _ (SOITableObj refQT (TOPerm rn pt)))   ->
      updatePermFlds source refQT rn pt $ RTable (oldQT, newQT)
    -- A trigger's definition is not dependent on the table directly
    (SOSourceObj _ (SOITableObj _ (TOTrigger _)))   -> pure ()
    -- A remote relationship's definition is not dependent on the table directly
    (SOSourceObj _ (SOITableObj _ (TORemoteRel _))) -> pure ()

    d -> otherDeps errMsg d
  -- Update table name in metadata
  tell $ MetadataModifier $ metaSources.ix source.smTables %~ \tables ->
    flip (maybe tables) (OMap.lookup oldQT tables) $
    \tableMeta -> OMap.delete oldQT $ OMap.insert newQT tableMeta{_tmTable = newQT} tables
  where
    errMsg = "cannot rename table " <> oldQT <<> " to " <>> newQT

renameColumnInMetadata
  :: ( MonadError QErr m
     , CacheRM m
     , MonadWriter MetadataModifier m
     )
  => PGCol -> PGCol -> SourceName -> QualifiedTable -> FieldInfoMap (FieldInfo 'Postgres) -> m ()
renameColumnInMetadata oCol nCol source qt fieldInfo = do
  sc <- askSchemaCache
  -- Check if any relation exists with new column name
  assertFldNotExists
  -- Fetch dependent objects
  let depObjs = getDependentObjs sc $ SOSourceObj source $
                SOITableObj qt $ TOCol oCol
      renameFld = RFCol $ RenameItem qt oCol nCol
  -- Update dependent objects
  forM_ depObjs $ \case
    (SOSourceObj _ (SOITableObj refQT (TOPerm role pt))) ->
      updatePermFlds source refQT role pt $ RField renameFld
    (SOSourceObj _ (SOITableObj refQT (TORel rn))) ->
      updateColInRel source refQT rn $ RenameItem qt oCol nCol
    (SOSourceObj _ (SOITableObj refQT (TOTrigger triggerName))) ->
      updateColInEventTriggerDef source refQT triggerName $ RenameItem qt oCol nCol
    (SOSourceObj _ (SOITableObj _ (TORemoteRel remoteRelName))) ->
      updateColInRemoteRelationship source remoteRelName $ RenameItem qt oCol nCol
    d -> otherDeps errMsg d
  -- Update custom column names
  possiblyUpdateCustomColumnNames source qt oCol nCol
  where
    errMsg = "cannot rename column " <> oCol <<> " to " <>> nCol
    assertFldNotExists =
      case M.lookup (fromCol @'Postgres oCol) fieldInfo of
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
  let depObjs = getDependentObjs sc $ SOSourceObj source $
                SOITableObj qt $ TORel oldRN
      renameFld = RFRel $ RenameItem qt oldRN newRN

  forM_ depObjs $ \case
    (SOSourceObj _ (SOITableObj refQT (TOPerm role pt))) ->
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
      flip (maybe relationsMap) (OMap.lookup oldRN relationsMap) $
      \rd -> OMap.insert newRN rd{_rdName = newRN} $ OMap.delete oldRN relationsMap

-- update table names in relationship definition
updateRelDefs
  :: ( MonadError QErr m
     , CacheRM m
     , MonadWriter MetadataModifier m
     )
  => SourceName -> QualifiedTable -> RelName -> RenameTable -> m ()
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
      RUManual (RelManualConfig origQT rmCols) ->
        let updQT = bool origQT newQT $ oldQT == origQT
        in RUManual $ RelManualConfig updQT rmCols

    updateArrRelDef :: RenameTable -> ArrRelDef -> ArrRelDef
    updateArrRelDef (oldQT, newQT) =
      rdUsing %~ \case
      RUFKeyOn (ArrRelUsingFKeyOn origQT c) ->
        let updQT = getUpdQT origQT
        in RUFKeyOn $ ArrRelUsingFKeyOn updQT c
      RUManual (RelManualConfig origQT rmCols) ->
        let updQT = getUpdQT origQT
        in RUManual $ RelManualConfig updQT rmCols
      where
        getUpdQT origQT = bool origQT newQT $ oldQT == origQT

-- | update fields in premissions
updatePermFlds
  :: ( MonadError QErr m
     , CacheRM m
     , MonadWriter MetadataModifier m
     )
  => SourceName -> QualifiedTable -> RoleName -> PermType -> Rename -> m ()
updatePermFlds source refQT rn pt rename = do
  tables <- getSourceTables source
  let withTables :: Reader (TableCache 'Postgres) a -> a
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
  :: (MonadReader (TableCache 'Postgres) m)
  => QualifiedTable -> Rename -> InsPerm 'Postgres -> m (InsPerm 'Postgres)
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
  :: (MonadReader (TableCache 'Postgres) m)
  => QualifiedTable -> Rename -> SelPerm 'Postgres -> m (SelPerm 'Postgres)
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
  :: (MonadReader (TableCache 'Postgres) m)
  => QualifiedTable -> Rename -> UpdPerm 'Postgres -> m (UpdPerm 'Postgres)
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
  :: (MonadReader (TableCache 'Postgres) m)
  => QualifiedTable -> Rename -> DelPerm 'Postgres -> m (DelPerm 'Postgres)
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

updateTableInBoolExp :: RenameTable -> BoolExp 'Postgres -> BoolExp 'Postgres
updateTableInBoolExp (oldQT, newQT) =
  over _Wrapped . transform $ (_BoolExists . geTable) %~ \rqfQT ->
    if rqfQT == oldQT then newQT else rqfQT

updateFieldInBoolExp
  :: (MonadReader (TableCache 'Postgres) m)
  => QualifiedTable -> RenameField -> BoolExp 'Postgres -> m (BoolExp 'Postgres)
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
  :: (MonadReader (TableCache 'Postgres) m)
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
      RFCol (RenameItem tn oCol nCol) -> (fromCol @'Postgres oCol, fromCol @'Postgres nCol, tn)
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
  where
    (RenameItem qt oldCol newCol) = renameCol
    modifyHasuraFields = Set.insert (fromCol @'Postgres newCol) . Set.delete (fromCol @'Postgres oldCol)
    modifyFieldCalls oldColName newColName =
      RemoteFields
      . NE.map (\(FieldCall name args) ->
                   let remoteArgs = getRemoteArguments args
                   in FieldCall name $ RemoteArguments $

                                      fmap (replaceVariableName oldColName newColName) remoteArgs
               )
      . unRemoteFields

    parseGraphQLName txt = onNothing (G.mkName txt) $ throw400 ParseFailed errMsg
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

getSourceTables :: CacheRM m => SourceName -> m (TableCache 'Postgres)
getSourceTables source =
  (maybe mempty _pcTables . M.lookup source . scPostgres) <$> askSchemaCache
