-- | Functions for updating the metadata (with integrity checking) to incorporate schema changes
-- discovered after applying a user-supplied SQL query. None of these functions modify the schema
-- cache, so it must be reloaded after the metadata is updated.
module Hasura.RQL.DDL.Schema.Rename
  ( renameTableInMetadata
  , renameColumnInMetadata
  , renameRelationshipInMetadata
  )
where

import           Hasura.Prelude

import qualified Data.HashMap.Strict           as M
import qualified Data.HashMap.Strict.InsOrd    as OMap
import qualified Data.HashSet                  as Set
import qualified Data.List.NonEmpty            as NE
import qualified Language.GraphQL.Draft.Syntax as G

import           Control.Lens.Combinators
import           Control.Lens.Operators
import           Data.Aeson
import           Data.Text.Extended

import qualified Hasura.SQL.AnyBackend         as AB

import           Hasura.Base.Error
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.Types
import           Hasura.Session


data RenameItem (b :: BackendType) a
  = RenameItem
  { _riTable :: !(TableName b)
  , _riOld   :: !a
  , _riNew   :: !a
  }

type RenameCol (b :: BackendType) = RenameItem b (Column b)

data RenameField b
  = RFCol !(RenameCol b)
  | RFRel !(RenameItem b RelName)

type RenameTable b = (TableName b, TableName b)

data Rename b
  = RTable !(RenameTable b)
  | RField !(RenameField b)

otherDeps :: QErrM m => Text -> SchemaObjId -> m ()
otherDeps errMsg d =
  throw500 $ "unexpected dependancy "
    <> reportSchemaObj d <> "; " <> errMsg

renameTableInMetadata
  :: forall b m
   . ( MonadError QErr m
     , CacheRM m
     , MonadWriter MetadataModifier m
     , BackendMetadata b
     )
  => SourceName
  -> TableName b
  -> TableName b
  -> m ()
renameTableInMetadata source newQT oldQT = do
  sc <- askSchemaCache
  let allDeps = getDependentObjs sc
                  $ SOSourceObj source
                  $ AB.mkAnyBackend
                  $ SOITable @b oldQT

  -- update all dependant schema objects
  forM_ allDeps $ \case
    sobj@(SOSourceObj _ exists) -> case AB.unpackAnyBackend @b exists of
      Just (SOITableObj refQT (TORel rn)) ->
        updateRelDefs @b source refQT rn (oldQT, newQT)
      Just (SOITableObj refQT (TOPerm rn pt)) ->
        updatePermFlds @b source refQT rn pt $ RTable (oldQT, newQT)
      -- A trigger's definition is not dependent on the table directly
      Just (SOITableObj _ (TOTrigger _)) -> pure ()
      -- A remote relationship's definition is not dependent on the table directly
      Just (SOITableObj _ (TORemoteRel _)) -> pure ()
      _ -> otherDeps errMsg sobj

    d -> otherDeps errMsg d
  -- Update table name in metadata
  tell $ MetadataModifier $ metaSources.ix source.(toSourceMetadata @b).smTables %~ \tables ->
    flip (maybe tables) (OMap.lookup oldQT tables) $
    \tableMeta -> OMap.delete oldQT $ OMap.insert newQT tableMeta{_tmTable = newQT} tables
  where
    errMsg = "cannot rename table " <> oldQT <<> " to " <>> newQT

renameColumnInMetadata
  :: forall b m
   . ( MonadError QErr m
     , CacheRM m
     , MonadWriter MetadataModifier m
     , BackendMetadata b
     )
  => Column b -> Column b -> SourceName -> TableName b -> FieldInfoMap (FieldInfo b) -> m ()
renameColumnInMetadata oCol nCol source qt fieldInfo = do
  sc <- askSchemaCache
  -- Check if any relation exists with new column name
  assertFldNotExists
  -- Fetch dependent objects
  let depObjs = getDependentObjs sc
                  $ SOSourceObj source
                  $ AB.mkAnyBackend
                  $ SOITableObj @b qt
                  $ TOCol @b oCol
      renameFld = RFCol @b $ RenameItem qt oCol nCol
  -- Update dependent objects
  forM_ depObjs $ \case
    sobj@(SOSourceObj _ exists) -> case AB.unpackAnyBackend @b exists of
      Just (SOITableObj refQT (TOPerm role pt)) ->
        updatePermFlds @b source refQT role pt $ RField renameFld
      Just (SOITableObj refQT (TORel rn)) ->
        updateColInRel @b source refQT rn $ RenameItem qt oCol nCol
      Just (SOITableObj refQT (TOTrigger triggerName)) ->
        tell
          $ MetadataModifier
          $ tableMetadataSetter @b source refQT.tmEventTriggers.ix triggerName
          %~ updateColumnInEventTrigger @b refQT oCol nCol qt
      Just (SOITableObj _ (TORemoteRel remoteRelName)) ->
        updateColInRemoteRelationship source remoteRelName
          $ RenameItem @b qt oCol nCol
      _ -> otherDeps errMsg sobj
    d -> otherDeps errMsg d
  -- Update custom column names
  possiblyUpdateCustomColumnNames @b source qt oCol nCol
  where
    errMsg = "cannot rename column " <> oCol <<> " to " <>> nCol
    assertFldNotExists =
      case M.lookup (fromCol @b oCol) fieldInfo of
        Just (FIRelationship _) ->
          throw400 AlreadyExists $ "cannot rename column " <> oCol
          <<> " to " <> nCol <<> " in table " <> qt <<>
          " as a relationship with the name already exists"
        _ -> pure ()

renameRelationshipInMetadata
  :: forall b m
   . ( MonadError QErr m
     , CacheRM m
     , MonadWriter MetadataModifier m
     , BackendMetadata b
     )
  => SourceName -> TableName b -> RelName -> RelType -> RelName -> m ()
renameRelationshipInMetadata source qt oldRN relType newRN = do
  sc <- askSchemaCache
  let depObjs = getDependentObjs sc
                  $ SOSourceObj source
                  $ AB.mkAnyBackend
                  $ SOITableObj @b qt
                  $ TORel oldRN
      renameFld = RFRel $ RenameItem @b qt oldRN newRN

  forM_ depObjs $ \case
    sobj@(SOSourceObj _ exists) -> case AB.unpackAnyBackend @b exists of
      Just (SOITableObj refQT (TOPerm role pt)) ->
        updatePermFlds @b source refQT role pt $ RField renameFld
      _ -> otherDeps errMsg sobj
    d -> otherDeps errMsg d
  tell $ MetadataModifier $ tableMetadataSetter @b source qt %~ case relType of
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
  :: forall b m
   . ( MonadError QErr m
     , CacheRM m
     , MonadWriter MetadataModifier m
     , BackendMetadata b
     )
  => SourceName -> TableName b -> RelName -> RenameTable b -> m ()
updateRelDefs source qt rn renameTable = do
  fim <- askFieldInfoMap @b source qt
  ri <- askRelType fim rn ""
  tell $ MetadataModifier $ tableMetadataSetter source qt %~ case riType ri of
    ObjRel -> tmObjectRelationships.ix rn %~ updateObjRelDef renameTable
    ArrRel -> tmArrayRelationships.ix rn %~ updateArrRelDef renameTable
  where
    updateObjRelDef :: RenameTable b -> ObjRelDef b -> ObjRelDef b
    updateObjRelDef (oldQT, newQT) =
      rdUsing %~ \case
      RUFKeyOn fk -> RUFKeyOn fk
      RUManual (RelManualConfig origQT rmCols rmIO) ->
        let updQT = bool origQT newQT $ oldQT == origQT
        in RUManual $ RelManualConfig updQT rmCols rmIO

    updateArrRelDef :: RenameTable b -> ArrRelDef b -> ArrRelDef b
    updateArrRelDef (oldQT, newQT) =
      rdUsing %~ \case
      RUFKeyOn (ArrRelUsingFKeyOn origQT c) ->
        let updQT = getUpdQT origQT
        in RUFKeyOn $ ArrRelUsingFKeyOn updQT c
      RUManual (RelManualConfig origQT rmCols rmIO) ->
        let updQT = getUpdQT origQT
        in RUManual $ RelManualConfig updQT rmCols rmIO
      where
        getUpdQT origQT = bool origQT newQT $ oldQT == origQT

-- | update fields in permissions
updatePermFlds
  :: forall b m
   . ( MonadError QErr m
     , CacheRM m
     , MonadWriter MetadataModifier m
     , BackendMetadata b
     )
  => SourceName -> TableName b -> RoleName -> PermType -> Rename b -> m ()
updatePermFlds source refQT rn pt rename = do
  tables <- askTableCache source
  let withTables :: Reader (TableCache b) a -> a
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
  :: (MonadReader (TableCache b) m, Backend b)
  => TableName b -> Rename b -> InsPerm b -> m (InsPerm b)
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
  :: (MonadReader (TableCache b) m, Backend b)
  => TableName b -> Rename b -> SelPerm b -> m (SelPerm b)
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
  :: (MonadReader (TableCache b) m, Backend b)
  => TableName b -> Rename b -> UpdPerm b -> m (UpdPerm b)
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
  :: (MonadReader (TableCache b) m, Backend b)
  => TableName b -> Rename b -> DelPerm b -> m (DelPerm b)
updateDelPermFlds refQT rename (DelPerm fltr) = do
  DelPerm <$> case rename of
    RTable rt -> pure $ updateTableInBoolExp rt fltr
    RField rf -> updateFieldInBoolExp refQT rf fltr

updatePreset
  :: (Backend b)
  => TableName b -> RenameField b -> (ColumnValues b Value) -> (ColumnValues b Value)
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
  :: (Backend b) => TableName b -> RenameField b -> PermColSpec b -> PermColSpec b
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

updateTableInBoolExp :: (Backend b) => RenameTable b -> BoolExp b -> BoolExp b
updateTableInBoolExp (oldQT, newQT) =
  over _Wrapped . transform $ (_BoolExists . geTable) %~ \rqfQT ->
    if rqfQT == oldQT then newQT else rqfQT

updateFieldInBoolExp
  :: (MonadReader (TableCache b) m, Backend b)
  => TableName b -> RenameField b -> BoolExp b -> m (BoolExp b)
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
  :: forall b m. (MonadReader (TableCache b) m, Backend b)
  => TableName b -> RenameField b -> ColExp -> m ColExp
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
      RFCol (RenameItem tn oCol nCol) -> (fromCol @b oCol, fromCol @b nCol, tn)
      RFRel (RenameItem tn oRel nRel) -> (fromRel oRel, fromRel nRel, tn)

-- rename columns in relationship definitions
updateColInRel
  :: forall b m
   . (CacheRM m, MonadWriter MetadataModifier m, BackendMetadata b)
  => SourceName -> TableName b -> RelName -> RenameCol b -> m ()
updateColInRel source fromQT rn rnCol = do
  tables <- askSourceTables @b source
  let maybeRelInfo =
        tables ^? ix fromQT.tiCoreInfo.tciFieldInfoMap.ix (fromRel rn)._FIRelationship
  forM_ maybeRelInfo $ \relInfo ->
    let relTableName = riRTable relInfo
    in tell $ MetadataModifier $ tableMetadataSetter source fromQT %~
      case riType relInfo of
        ObjRel -> tmObjectRelationships.ix rn.rdUsing %~
                  updateColInObjRel fromQT relTableName rnCol
        ArrRel -> tmArrayRelationships.ix rn.rdUsing %~
                  updateColInArrRel fromQT relTableName rnCol

updateColInRemoteRelationship
  :: forall b m
   . ( MonadError QErr m
     , MonadWriter MetadataModifier m
     , BackendMetadata b
     )
  => SourceName -> RemoteRelationshipName -> RenameCol b -> m ()
updateColInRemoteRelationship source remoteRelationshipName renameCol = do
  oldColName <- parseGraphQLName $ toTxt oldCol
  newColName <- parseGraphQLName $ toTxt newCol
  tell $ MetadataModifier $
    tableMetadataSetter @b source qt.tmRemoteRelationships.ix remoteRelationshipName.rrmDefinition %~
      (rrdHasuraFields %~ modifyHasuraFields) .
      (rrdRemoteField %~ modifyFieldCalls oldColName newColName)
  where
    (RenameItem qt oldCol newCol) = renameCol
    modifyHasuraFields = Set.insert (fromCol @b newCol) . Set.delete (fromCol @b oldCol)
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

updateColInObjRel
  :: (Backend b)
  => TableName b -> TableName b -> RenameCol b -> ObjRelUsing b -> ObjRelUsing b
updateColInObjRel fromQT toQT rnCol = \case
  RUFKeyOn c         ->
    RUFKeyOn $ updateRelChoice fromQT toQT rnCol c
  RUManual manConfig ->
    RUManual $ updateRelManualConfig fromQT toQT rnCol manConfig

updateRelChoice
  :: Backend b
  => TableName b
  -> TableName b
  -> RenameCol b
  -> ObjRelUsingChoice b
  -> ObjRelUsingChoice b
updateRelChoice fromQT toQT rnCol =
  \case
    SameTable col   -> SameTable $ getNewCol rnCol fromQT col
    RemoteTable t c -> RemoteTable t (getNewCol rnCol toQT c)

updateColInArrRel
  :: (Backend b)
  => TableName b -> TableName b -> RenameCol b -> ArrRelUsing b -> ArrRelUsing b
updateColInArrRel fromQT toQT rnCol = \case
  RUFKeyOn (ArrRelUsingFKeyOn t c) ->
    let updCol = getNewCol rnCol toQT c
    in RUFKeyOn $ ArrRelUsingFKeyOn t updCol
  RUManual manConfig -> RUManual $ updateRelManualConfig fromQT toQT rnCol manConfig

type ColMap b = HashMap (Column b) (Column b)

getNewCol
  :: forall b f
   . Backend b
  => Functor f
  => RenameCol b
  -> TableName b
  -> f (Column b)
  -> f (Column b)
getNewCol rnCol qt cols =
  if qt == opQT then
    go <$> cols
  else
    cols
  where
    RenameItem opQT oCol nCol = rnCol
    go :: Column b -> Column b
    go col
      | col == oCol = nCol
      | otherwise   = col

updateRelManualConfig
  :: forall b
   . (Backend b)
  => TableName b
  -> TableName b
  -> RenameCol b
  -> RelManualConfig b
  -> RelManualConfig b
updateRelManualConfig fromQT toQT rnCol manConfig =
  RelManualConfig tn (updateColMap fromQT toQT rnCol colMap) io
  where
    RelManualConfig tn colMap io = manConfig

updateColMap
  :: forall b
   . (Backend b)
  => TableName b
  -> TableName b
  -> RenameCol b
  -> ColMap b
  -> ColMap b
updateColMap fromQT toQT rnCol =
  M.fromList . map (modCol fromQT *** modCol toQT) . M.toList
  where
    RenameItem qt oCol nCol = rnCol
    modCol colQt col = if colQt == qt && col == oCol then nCol else col

possiblyUpdateCustomColumnNames
  :: forall b m
   .(MonadWriter MetadataModifier m, BackendMetadata b)
  => SourceName
  -> TableName b
  -> Column b
  -> Column b
  -> m ()
possiblyUpdateCustomColumnNames source qt oCol nCol = do
  let updateCustomColumns customColumns =
        M.fromList $ flip map (M.toList customColumns) $
        \(dbCol, val) -> (, val) $ if dbCol == oCol then nCol else dbCol
  tell $ MetadataModifier $
    tableMetadataSetter @b source qt.tmConfiguration.tcCustomColumnNames %~ updateCustomColumns
