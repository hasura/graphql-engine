-- | Functions for updating the metadata (with integrity checking) to incorporate schema changes
-- discovered after applying a user-supplied SQL query. None of these functions modify the schema
-- cache, so it must be reloaded after the metadata is updated.
module Hasura.RQL.DDL.Schema.Rename
  ( renameTableInMetadata,
    renameColumnInMetadata,
    renameRelationshipInMetadata,
  )
where

import Control.Lens.Combinators
import Control.Lens.Operators
import Data.Aeson
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashSet qualified as Set
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.DDL.Permission
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.BoolExp.Lenses (geTable, _BoolExists)
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Permission
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.Relationships.Remote
import Hasura.RQL.Types.Relationships.ToSource
import Hasura.RQL.Types.Roles (RoleName)
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCacheTypes
import Hasura.RemoteSchema.Metadata
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Table.Cache
import Hasura.Table.Metadata
  ( Relationships,
    TableMetadata (..),
    tmArrayRelationships,
    tmConfiguration,
    tmDeletePermissions,
    tmEventTriggers,
    tmInsertPermissions,
    tmObjectRelationships,
    tmRemoteRelationships,
    tmSelectPermissions,
    tmUpdatePermissions,
  )
import Language.GraphQL.Draft.Syntax qualified as G

data RenameItem (b :: BackendType) a = RenameItem
  { _riTable :: TableName b,
    _riOld :: a,
    _riNew :: a
  }

type RenameCol (b :: BackendType) = RenameItem b (Column b)

data RenameField b
  = RFCol (RenameCol b)
  | RFRel (RenameItem b RelName)

type RenameTable b = (TableName b, TableName b)

data Rename b
  = RTable (RenameTable b)
  | RField (RenameField b)

otherDeps :: (QErrM m) => Text -> SchemaObjId -> m ()
otherDeps errMsg d =
  throw500
    $ "unexpected dependency "
    <> reportSchemaObj d
    <> "; "
    <> errMsg

-- | Replace all references to a given table name by its new name across the entire metadata.
--
-- This function will make use of the metadata dependency graph (see 'getDependentObjs') to identify
-- all places that refer to the old table name, and replace it accordingly. Most operations will
-- occur within the same source, such as table references in relationships and permissions.
-- Dependencies across sources can happen in the case of cross-source relationships.
--
-- This function will fail if it encounters a nonsensical dependency; for instance, if there's a
-- dependency from that table to a source.
--
-- For more information about the dependency graph, see 'SchemaObjId'.
renameTableInMetadata ::
  forall b m.
  ( MonadError QErr m,
    CacheRM m,
    MonadWriter MetadataModifier m,
    BackendMetadata b
  ) =>
  SourceName ->
  TableName b ->
  TableName b ->
  m ()
renameTableInMetadata source newQT oldQT = do
  sc <- askSchemaCache
  let allDeps =
        getDependentObjs sc
          $ SOSourceObj source
          $ AB.mkAnyBackend
          $ SOITable @b oldQT

  -- update all dependant schema objects
  forM_ allDeps $ \case
    -- the dependend object is a source object in the same source
    sobj@(SOSourceObj depSourceName exists)
      | depSourceName == source,
        Just sourceObjId <- AB.unpackAnyBackend @b exists ->
          case sourceObjId of
            SOITableObj refQT (TORel rn) ->
              updateRelDefs @b source refQT rn (oldQT, newQT)
            SOITableObj refQT (TOPerm rn pt) ->
              updatePermFlds @b source refQT rn pt $ RTable (oldQT, newQT)
            -- A trigger's definition is not dependent on the table directly
            SOITableObj _ (TOTrigger _) -> pure ()
            -- A remote relationship's definition is not dependent on the table directly
            SOITableObj _ (TORemoteRel _) -> pure ()
            _ -> otherDeps errMsg sobj
    -- the dependend object is a source object in a different source
    sobj@(SOSourceObj depSourceName exists) ->
      AB.dispatchAnyBackend @Backend exists \(sourceObjId :: SourceObjId b') ->
        case sourceObjId of
          SOITableObj tableName (TORemoteRel remoteRelationshipName) -> do
            updateTableInRemoteRelationshipRHS @b' @b depSourceName tableName remoteRelationshipName (oldQT, newQT)
          -- only remote relationships might create dependencies across sources
          _ -> otherDeps errMsg sobj
    -- any other kind of dependent object (erroneous)
    d -> otherDeps errMsg d
  -- Update table name in metadata
  tell
    $ MetadataModifier
    $ metaSources
    . ix source
    . (toSourceMetadata @b)
    . smTables
    %~ \tables ->
      flip (maybe tables) (InsOrdHashMap.lookup oldQT tables)
        $ \tableMeta -> InsOrdHashMap.delete oldQT $ InsOrdHashMap.insert newQT tableMeta {_tmTable = newQT} tables
  where
    errMsg = "cannot rename table " <> oldQT <<> " to " <>> newQT

-- | Replace all references to a given column name by its new name across the entire metadata.
--
-- This function will make use of the metadata dependency graph (see 'getDependentObjs') to identify
-- all places that refer to the old column name, and replace it accordingly. Most operations will
-- occur within the same source, such as column references in relationships and permissions.
-- Dependencies across sources can happen in the case of cross-source relationships.
--
-- This function will fail if it encounters a nonsensical dependency; for instance, if there's a
-- dependency from that table to a source.
--
-- For more information about the dependency graph, see 'SchemaObjId'.
renameColumnInMetadata ::
  forall b m.
  ( MonadError QErr m,
    CacheRM m,
    MonadWriter MetadataModifier m,
    BackendMetadata b,
    Column b ~ ColumnPath b
  ) =>
  Column b ->
  Column b ->
  SourceName ->
  TableName b ->
  FieldInfoMap (FieldInfo b) ->
  m ()
renameColumnInMetadata oCol nCol source qt fieldInfo = do
  sc <- askSchemaCache
  -- Check if any relation exists with new column name
  assertFldNotExists
  -- Fetch dependent objects
  let depObjs =
        getDependentObjs sc
          $ SOSourceObj source
          $ AB.mkAnyBackend
          $ SOITableObj @b qt
          $ TOCol @b oCol
      renameItem = RenameItem @b qt oCol nCol
      renameFld = RFCol renameItem
  -- Update dependent objects
  forM_ depObjs $ \case
    -- the dependend object is a source object in the same source
    sobj@(SOSourceObj depSourceName exists)
      | depSourceName == source,
        Just sourceObjId <- AB.unpackAnyBackend @b exists ->
          case sourceObjId of
            SOITableObj refQT (TOPerm role pt) ->
              updatePermFlds @b source refQT role pt $ RField renameFld
            SOITableObj refQT (TORel rn) ->
              updateColInRel @b source refQT rn renameItem
            SOITableObj refQT (TOTrigger triggerName) ->
              tell
                $ MetadataModifier
                $ tableMetadataSetter @b source refQT
                . tmEventTriggers
                . ix triggerName
                %~ updateColumnInEventTrigger @b refQT oCol nCol qt
            SOITableObj _ (TORemoteRel remoteRelName) ->
              updateColInRemoteRelationshipLHS source remoteRelName renameItem
            _ -> otherDeps errMsg sobj
    -- the dependend object is a source object in a different source
    sobj@(SOSourceObj depSourceName exists) ->
      AB.dispatchAnyBackend @Backend exists \(sourceObjId :: SourceObjId b') ->
        case sourceObjId of
          SOITableObj tableName (TORemoteRel remoteRelationshipName) -> do
            updateColInRemoteRelationshipRHS @b' @b depSourceName tableName remoteRelationshipName renameItem
          -- only remote relationships might create dependencies across sources
          _ -> otherDeps errMsg sobj
    -- any other kind of dependent object (erroneous)
    d -> otherDeps errMsg d
  -- Update custom column names
  possiblyUpdateCustomColumnNames @b source qt oCol nCol
  where
    errMsg = "cannot rename column " <> oCol <<> " to " <>> nCol
    assertFldNotExists =
      case HashMap.lookup (fromCol @b oCol) fieldInfo of
        Just (FIRelationship _) ->
          throw400 AlreadyExists
            $ "cannot rename column "
            <> oCol
            <<> " to "
            <> nCol
            <<> " in table "
            <> qt
            <<> " as a relationship with the name already exists"
        _ -> pure ()

renameRelationshipInMetadata ::
  forall b m.
  ( MonadError QErr m,
    CacheRM m,
    MonadWriter MetadataModifier m,
    BackendMetadata b
  ) =>
  SourceName ->
  TableName b ->
  RelName ->
  RelType ->
  RelName ->
  m ()
renameRelationshipInMetadata source qt oldRN relType newRN = do
  sc <- askSchemaCache
  let depObjs =
        getDependentObjs sc
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
  tell
    $ MetadataModifier
    $ tableMetadataSetter @b source qt
    %~ case relType of
      ObjRel -> tmObjectRelationships %~ rewriteRelationships
      ArrRel -> tmArrayRelationships %~ rewriteRelationships
  where
    errMsg = "cannot rename relationship " <> oldRN <<> " to " <>> newRN
    rewriteRelationships ::
      Relationships (RelDef a) -> Relationships (RelDef a)
    rewriteRelationships relationsMap =
      flip (maybe relationsMap) (InsOrdHashMap.lookup oldRN relationsMap)
        $ \rd -> InsOrdHashMap.insert newRN rd {_rdName = newRN} $ InsOrdHashMap.delete oldRN relationsMap

-- update table names in relationship definition
updateRelDefs ::
  forall b m.
  ( MonadError QErr m,
    CacheRM m,
    MonadWriter MetadataModifier m,
    BackendMetadata b
  ) =>
  SourceName ->
  TableName b ->
  RelName ->
  RenameTable b ->
  m ()
updateRelDefs source qt rn renameTable = do
  fim <- askTableFieldInfoMap @b source qt
  ri <- askRelType fim rn ""
  tell
    $ MetadataModifier
    $ tableMetadataSetter source qt
    %~ case riType ri of
      ObjRel -> tmObjectRelationships . ix rn %~ updateObjRelDef renameTable
      ArrRel -> tmArrayRelationships . ix rn %~ updateArrRelDef renameTable
  where
    updateObjRelDef :: RenameTable b -> ObjRelDef b -> ObjRelDef b
    updateObjRelDef (oldQT, newQT) =
      rdUsing %~ \case
        RUFKeyOn fk -> RUFKeyOn fk
        RUManual (RelManualTableConfig (RelManualTableConfigC origQT (RelManualCommon rmCols rmIO))) ->
          let updQT = bool origQT newQT $ oldQT == origQT
           in RUManual $ RelManualTableConfig $ RelManualTableConfigC updQT (RelManualCommon rmCols rmIO)
        RUManual (RelManualNativeQueryConfig nqc) ->
          RUManual (RelManualNativeQueryConfig nqc)

    updateArrRelDef :: RenameTable b -> ArrRelDef b -> ArrRelDef b
    updateArrRelDef (oldQT, newQT) =
      rdUsing %~ \case
        RUFKeyOn (ArrRelUsingFKeyOn origQT c) ->
          let updQT = getUpdQT origQT
           in RUFKeyOn $ ArrRelUsingFKeyOn updQT c
        RUManual (RelManualTableConfig (RelManualTableConfigC origQT (RelManualCommon rmCols rmIO))) ->
          let updQT = getUpdQT origQT
           in RUManual $ RelManualTableConfig $ RelManualTableConfigC updQT (RelManualCommon rmCols rmIO)
        RUManual (RelManualNativeQueryConfig nqc) ->
          RUManual (RelManualNativeQueryConfig nqc)
      where
        getUpdQT origQT = bool origQT newQT $ oldQT == origQT

-- | update fields in permissions
updatePermFlds ::
  forall b m.
  ( MonadError QErr m,
    CacheRM m,
    MonadWriter MetadataModifier m,
    BackendMetadata b
  ) =>
  SourceName ->
  TableName b ->
  RoleName ->
  PermType ->
  Rename b ->
  m ()
updatePermFlds source refQT rn pt rename = do
  tables <- fold <$> askTableCache source
  let withTables :: Reader (TableCache b) a -> a
      withTables = flip runReader tables
  tell
    $ MetadataModifier
    $ tableMetadataSetter source refQT
    %~ case pt of
      PTInsert ->
        tmInsertPermissions . ix rn . pdPermission %~ \insPerm ->
          withTables $ updateInsPermFlds refQT rename insPerm
      PTSelect ->
        tmSelectPermissions . ix rn . pdPermission %~ \selPerm ->
          withTables $ updateSelPermFlds refQT rename selPerm
      PTUpdate ->
        tmUpdatePermissions . ix rn . pdPermission %~ \updPerm ->
          withTables $ updateUpdPermFlds refQT rename updPerm
      PTDelete ->
        tmDeletePermissions . ix rn . pdPermission %~ \delPerm ->
          withTables $ updateDelPermFlds refQT rename delPerm

updateInsPermFlds ::
  (MonadReader (TableCache b) m, Backend b) =>
  TableName b ->
  Rename b ->
  PermDefPermission b InsPerm ->
  m (PermDefPermission b InsPerm)
updateInsPermFlds refQT rename (InsPerm' (InsPerm chk preset cols backendOnly validateInput)) =
  case rename of
    RTable rt -> do
      let updChk = updateTableInBoolExp rt chk
      pure $ InsPerm' $ InsPerm updChk preset cols backendOnly validateInput
    RField rf -> do
      updChk <- updateFieldInBoolExp refQT rf chk
      let updPresetM = updatePreset refQT rf <$> preset
          updColsM = updateCols refQT rf <$> cols
      pure $ InsPerm' $ InsPerm updChk updPresetM updColsM backendOnly validateInput

updateSelPermFlds ::
  (MonadReader (TableCache b) m, Backend b) =>
  TableName b ->
  Rename b ->
  PermDefPermission b SelPerm ->
  m (PermDefPermission b SelPerm)
updateSelPermFlds refQT rename (SelPerm' (SelPerm cols fltr limit aggAllwd computedFields allowedQueryRootFieldTypes allowedSubsRootFieldTypes)) = do
  case rename of
    RTable rt -> do
      let updFltr = updateTableInBoolExp rt fltr
      pure $ SelPerm' $ SelPerm cols updFltr limit aggAllwd computedFields allowedQueryRootFieldTypes allowedSubsRootFieldTypes
    RField rf -> do
      updFltr <- updateFieldInBoolExp refQT rf fltr
      let updCols = updateCols refQT rf cols
      pure $ SelPerm' $ SelPerm updCols updFltr limit aggAllwd computedFields allowedQueryRootFieldTypes allowedSubsRootFieldTypes

updateUpdPermFlds ::
  (MonadReader (TableCache b) m, Backend b) =>
  TableName b ->
  Rename b ->
  PermDefPermission b UpdPerm ->
  m (PermDefPermission b UpdPerm)
updateUpdPermFlds refQT rename (UpdPerm' (UpdPerm cols preset fltr check backendOnly validateInput)) = do
  case rename of
    RTable rt -> do
      let updFltr = updateTableInBoolExp rt fltr
          updCheck = fmap (updateTableInBoolExp rt) check
      pure $ UpdPerm' $ UpdPerm cols preset updFltr updCheck backendOnly validateInput
    RField rf -> do
      updFltr <- updateFieldInBoolExp refQT rf fltr
      updCheck <- traverse (updateFieldInBoolExp refQT rf) check
      let updCols = updateCols refQT rf cols
          updPresetM = updatePreset refQT rf <$> preset
      pure $ UpdPerm' $ UpdPerm updCols updPresetM updFltr updCheck backendOnly validateInput

updateDelPermFlds ::
  (MonadReader (TableCache b) m, Backend b) =>
  TableName b ->
  Rename b ->
  PermDefPermission b DelPerm ->
  m (PermDefPermission b DelPerm)
updateDelPermFlds refQT rename (DelPerm' (DelPerm fltr backendOnly validateInput)) = do
  case rename of
    RTable rt -> do
      let updFltr = updateTableInBoolExp rt fltr
      pure $ DelPerm' $ DelPerm updFltr backendOnly validateInput
    RField rf -> do
      updFltr <- updateFieldInBoolExp refQT rf fltr
      pure $ DelPerm' $ DelPerm updFltr backendOnly validateInput

updatePreset ::
  (Backend b) =>
  TableName b ->
  RenameField b ->
  ColumnValues b Value ->
  ColumnValues b Value
updatePreset qt rf obj =
  case rf of
    RFCol (RenameItem opQT oCol nCol) ->
      if qt == opQT
        then updatePreset' oCol nCol
        else obj
    _ -> obj
  where
    updatePreset' oCol nCol =
      HashMap.fromList updItems
      where
        updItems = map procObjItem $ HashMap.toList obj
        procObjItem (pgCol, v) =
          let isUpdated = pgCol == oCol
              updCol = bool pgCol nCol isUpdated
           in (updCol, v)

updateCols ::
  (Backend b) => TableName b -> RenameField b -> PermColSpec b -> PermColSpec b
updateCols qt rf permSpec =
  case rf of
    RFCol (RenameItem opQT oCol nCol) ->
      if qt == opQT
        then updateCols' oCol nCol permSpec
        else permSpec
    _ -> permSpec
  where
    updateCols' oCol nCol cols = case cols of
      PCStar -> cols
      PCCols c -> PCCols
        $ flip map c
        $ \col -> if col == oCol then nCol else col

updateTableInBoolExp :: (Backend b) => RenameTable b -> BoolExp b -> BoolExp b
updateTableInBoolExp (oldQT, newQT) =
  over _Wrapped
    . transform
    $ (_BoolExists . geTable)
    %~ \rqfQT ->
      if rqfQT == oldQT then newQT else rqfQT

updateFieldInBoolExp ::
  (MonadReader (TableCache b) m, Backend b) =>
  TableName b ->
  RenameField b ->
  BoolExp b ->
  m (BoolExp b)
updateFieldInBoolExp qt rf be =
  BoolExp
    <$> case unBoolExp be of
      BoolAnd exps -> BoolAnd <$> procExps exps
      BoolOr exps -> BoolOr <$> procExps exps
      BoolNot e -> BoolNot <$> updateBoolExp' e
      BoolExists (GExists refqt wh) ->
        BoolExists
          . GExists refqt
          . unBoolExp
          <$> updateFieldInBoolExp refqt rf (BoolExp wh)
      BoolField fld -> BoolField <$> updateColExp qt rf fld
  where
    procExps = mapM updateBoolExp'
    updateBoolExp' =
      fmap unBoolExp . updateFieldInBoolExp qt rf . BoolExp

updateColExp ::
  forall b m.
  (MonadReader (TableCache b) m, Backend b) =>
  TableName b ->
  RenameField b ->
  ColExp ->
  m ColExp
updateColExp qt rf (ColExp fld val) =
  ColExp updatedFld <$> updatedVal
  where
    updatedFld = bool fld nFld $ opQT == qt && oFld == fld
    updatedVal = do
      tables <- ask
      let maybeFieldInfo =
            HashMap.lookup qt tables
              >>= HashMap.lookup fld
              . _tciFieldInfoMap
              . _tiCoreInfo
      case maybeFieldInfo of
        Nothing -> pure val
        Just fi -> case fi of
          FIColumn _ -> pure val
          FIComputedField _ -> pure val
          FIRelationship ri -> do
            case riTarget ri of
              RelTargetNativeQuery _ -> error "updateColExp RelTargetNativeQuery"
              RelTargetTable remTable ->
                case decodeValue val of
                  Left _ -> pure val
                  Right be -> toJSON <$> updateFieldInBoolExp remTable rf be
          FIRemoteRelationship {} -> pure val

    (oFld, nFld, opQT) = case rf of
      RFCol (RenameItem tn oCol nCol) -> (fromCol @b oCol, fromCol @b nCol, tn)
      RFRel (RenameItem tn oRel nRel) -> (fromRel oRel, fromRel nRel, tn)

-- rename columns in relationship definitions
updateColInRel ::
  forall b m.
  (CacheRM m, MonadWriter MetadataModifier m, BackendMetadata b, Column b ~ ColumnPath b) =>
  SourceName ->
  TableName b ->
  RelName ->
  RenameCol b ->
  m ()
updateColInRel source fromQT rn rnCol = do
  tables <- fold <$> askTableCache @b source
  let maybeRelInfo =
        tables ^? ix fromQT . tiCoreInfo . tciFieldInfoMap . ix (fromRel rn) . _FIRelationship
  forM_ maybeRelInfo $ \relInfo ->
    case riTarget relInfo of
      RelTargetNativeQuery _ -> error "updateColInRel RelTargetNativeQuery"
      RelTargetTable relTableName ->
        tell
          $ MetadataModifier
          $ tableMetadataSetter source fromQT
          %~ case riType relInfo of
            ObjRel ->
              tmObjectRelationships . ix rn . rdUsing
                %~ updateColInObjRel fromQT relTableName rnCol
            ArrRel ->
              tmArrayRelationships . ix rn . rdUsing
                %~ updateColInArrRel fromQT relTableName rnCol

-- | Local helper: update a column's name in the left-hand side of a remote relationship.
--
-- There are two kinds or remote relationships: remote source relationships, across sources, and
-- remote schema relationships, on remote schemas. In both cases, we maintain a mapping from the
-- source table's colunns to what they should be joined against in the target; when a column is
-- renamed, those references must be renamed as well. This function handles both cases.
--
-- See 'renameColumnInMetadata'.
updateColInRemoteRelationshipLHS ::
  forall b m.
  ( MonadError QErr m,
    MonadWriter MetadataModifier m,
    BackendMetadata b
  ) =>
  SourceName ->
  RelName ->
  RenameCol b ->
  m ()
updateColInRemoteRelationshipLHS source remoteRelationshipName (RenameItem qt oldCol newCol) = do
  oldColName <- parseGraphQLName $ toTxt oldCol
  newColName <- parseGraphQLName $ toTxt newCol
  let oldFieldName = fromCol @b oldCol
      newFieldName = fromCol @b newCol

      updateSet =
        Set.insert newFieldName . Set.delete oldFieldName

      updateMapKey =
        -- mapKeys is not available in 0.2.13.0
        HashMap.fromList . map (\(key, value) -> (if key == oldFieldName then newFieldName else key, value)) . HashMap.toList

      updateFieldCalls (RemoteFields fields) =
        RemoteFields
          $ fields
          <&> \(FieldCall name (RemoteArguments args)) ->
            FieldCall name $ RemoteArguments $ updateVariableName <$> args

      updateVariableName =
        fmap \v -> if v == oldColName then newColName else v

      remoteRelationshipLens =
        tableMetadataSetter @b source qt . tmRemoteRelationships . ix remoteRelationshipName . rrDefinition

      remoteSchemaLHSModifier =
        remoteRelationshipLens . _RelationshipToSchema . _2
          %~ (trrdLhsFields %~ updateSet)
            . (trrdRemoteField %~ updateFieldCalls)

      remoteSourceLHSModifier =
        remoteRelationshipLens . _RelationshipToSource . tsrdFieldMapping %~ updateMapKey

  tell $ MetadataModifier $ remoteSchemaLHSModifier . remoteSourceLHSModifier
  where
    parseGraphQLName txt =
      G.mkName txt
        `onNothing` throw400 ParseFailed (txt <> " is not a valid GraphQL name")

-- | Local helper: update a column's name in the right-hand side of a remote relationship.
--
-- In the case of remote _source_ relationships, the mapping from column to column needs to be
-- updated if one of the rhs columns has been renamed. A dependency is tracked from the rhs source's
-- column to the lhs source's relationship: when a rhs source's column has been renamed, this
-- function performs the corresponding update in the lhs source's relationship definition.
--
-- See 'renameColumnInMetadata'.
updateColInRemoteRelationshipRHS ::
  forall source target m.
  ( MonadWriter MetadataModifier m,
    Backend source,
    Backend target
  ) =>
  SourceName ->
  TableName source ->
  RelName ->
  RenameCol target ->
  m ()
updateColInRemoteRelationshipRHS source tableName remoteRelationshipName (RenameItem _ oldCol newCol) =
  tell
    $ MetadataModifier
    $ tableMetadataSetter @source source tableName
    . tmRemoteRelationships
    . ix remoteRelationshipName
    . rrDefinition
    . _RelationshipToSource
    . tsrdFieldMapping
    %~ updateMapValue
  where
    oldFieldName = fromCol @target oldCol
    newFieldName = fromCol @target newCol
    updateMapValue =
      fmap \value -> if value == oldFieldName then newFieldName else value

-- | Local helper: update a table's name in the right-hand side of a remote relationship.
--
-- In the case of remote _source_ relationships, the relationship definition targets a specific
-- table in the rhs source, and that reference needs to be updated if the targeted table has been
-- renamed. A dependency is tracked from the rhs source's table to the lhs source's relationship:
-- when a rhs table has been renamed, this function performs the corresponding update in the lhs
-- source's relationship definition.
--
-- See 'renameTableInMetadata'.
updateTableInRemoteRelationshipRHS ::
  forall source target m.
  ( MonadWriter MetadataModifier m,
    Backend source,
    Backend target
  ) =>
  SourceName ->
  TableName source ->
  RelName ->
  RenameTable target ->
  m ()
updateTableInRemoteRelationshipRHS source tableName remoteRelationshipName (_, newTableName) =
  tell
    $ MetadataModifier
    $ tableMetadataSetter @source source tableName
    . tmRemoteRelationships
    . ix remoteRelationshipName
    . rrDefinition
    . _RelationshipToSource
    . tsrdTable
    .~ toJSON newTableName

updateColInObjRel ::
  (Backend b, Column b ~ ColumnPath b) =>
  TableName b ->
  TableName b ->
  RenameCol b ->
  ObjRelUsing b ->
  ObjRelUsing b
updateColInObjRel fromQT toQT rnCol = \case
  RUFKeyOn c ->
    RUFKeyOn $ updateRelChoice fromQT toQT rnCol c
  RUManual manConfig ->
    RUManual $ updateRelManualConfig fromQT toQT rnCol manConfig

updateRelChoice ::
  (Backend b, Column b ~ ColumnPath b) =>
  TableName b ->
  TableName b ->
  RenameCol b ->
  ObjRelUsingChoice b ->
  ObjRelUsingChoice b
updateRelChoice fromQT toQT rnCol =
  \case
    SameTable col -> SameTable $ getNewCol rnCol fromQT col
    RemoteTable t c -> RemoteTable t (getNewCol rnCol toQT c)

updateColInArrRel ::
  (Backend b, Column b ~ ColumnPath b) =>
  TableName b ->
  TableName b ->
  RenameCol b ->
  ArrRelUsing b ->
  ArrRelUsing b
updateColInArrRel fromQT toQT rnCol = \case
  RUFKeyOn (ArrRelUsingFKeyOn t c) ->
    let updCol = getNewCol rnCol toQT c
     in RUFKeyOn $ ArrRelUsingFKeyOn t updCol
  RUManual manConfig -> RUManual $ updateRelManualConfig fromQT toQT rnCol manConfig

getNewCol ::
  forall b f.
  (Backend b) =>
  (Functor f) =>
  RenameCol b ->
  TableName b ->
  f (Column b) ->
  f (Column b)
getNewCol rnCol qt cols =
  if qt == opQT
    then go <$> cols
    else cols
  where
    RenameItem opQT oCol nCol = rnCol
    go :: Column b -> Column b
    go col
      | col == oCol = nCol
      | otherwise = col

updateRelManualConfig ::
  forall b.
  (Backend b, Column b ~ ColumnPath b) =>
  TableName b ->
  TableName b ->
  RenameCol b ->
  RelManualConfig b ->
  RelManualConfig b
updateRelManualConfig fromQT toQT rnCol (RelManualTableConfig (RelManualTableConfigC tn (RelManualCommon colMap io))) =
  RelManualTableConfig (RelManualTableConfigC tn (RelManualCommon (updateColMap fromQT toQT rnCol colMap) io))
updateRelManualConfig fromQT toQT rnCol (RelManualNativeQueryConfig (RelManualNativeQueryConfigC nqn (RelManualCommon colMap io))) =
  RelManualNativeQueryConfig (RelManualNativeQueryConfigC nqn (RelManualCommon (updateColMap fromQT toQT rnCol colMap) io))

updateColMap ::
  forall b.
  (Backend b, Column b ~ ColumnPath b) =>
  TableName b ->
  TableName b ->
  RenameCol b ->
  RelMapping b ->
  RelMapping b
updateColMap fromQT toQT rnCol =
  RelMapping . HashMap.fromList . map (modCol fromQT *** modCol toQT) . HashMap.toList . unRelMapping
  where
    RenameItem qt oCol nCol = rnCol
    modCol colQt col = if colQt == qt && col == oCol then nCol else col

possiblyUpdateCustomColumnNames ::
  forall b m.
  (MonadWriter MetadataModifier m, BackendMetadata b) =>
  SourceName ->
  TableName b ->
  Column b ->
  Column b ->
  m ()
possiblyUpdateCustomColumnNames source tableName oldColumn newColumn = do
  tell
    $ MetadataModifier
    $ tableMetadataSetter @b source tableName
    . tmConfiguration
    . tcColumnConfig
    %~ swapOldColumnForNewColumn
  where
    swapOldColumnForNewColumn :: HashMap (Column b) columnData -> HashMap (Column b) columnData
    swapOldColumnForNewColumn customColumns =
      HashMap.fromList $ (\(dbCol, val) -> (,val) $ if dbCol == oldColumn then newColumn else dbCol) <$> HashMap.toList customColumns
