module Hasura.RQL.DDL.Relationship
  ( CreateArrRel (..),
    CreateObjRel (..),
    runCreateRelationship,
    objRelP2Setup,
    arrRelP2Setup,
    DropRel,
    runDropRel,
    dropRelationshipInMetadata,
    SetRelComment,
    runSetRelComment,
  )
where

import Control.Lens ((.~))
import Data.Aeson.Types
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.HashSet qualified as Set
import Data.Text.Extended
import Data.Tuple (swap)
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.RQL.DDL.Permission
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.SchemaCacheTypes
import Hasura.RQL.Types.Table
import Hasura.SQL.AnyBackend qualified as AB

--------------------------------------------------------------------------------
-- Create local relationship

newtype CreateArrRel b = CreateArrRel {unCreateArrRel :: WithTable b (ArrRelDef b)}
  deriving newtype (FromJSON)

newtype CreateObjRel b = CreateObjRel {unCreateObjRel :: WithTable b (ObjRelDef b)}
  deriving newtype (FromJSON)

runCreateRelationship ::
  forall m b a.
  (MonadError QErr m, CacheRWM m, ToJSON a, MetadataM m, Backend b, BackendMetadata b) =>
  RelType ->
  WithTable b (RelDef a) ->
  m EncJSON
runCreateRelationship relType (WithTable source tableName relDef) = do
  let relName = _rdName relDef
  -- Check if any field with relationship name already exists in the table
  tableFields <- _tciFieldInfoMap <$> askTableCoreInfo @b source tableName
  onJust (Map.lookup (fromRel relName) tableFields) $
    const $
      throw400 AlreadyExists $
        "field with name " <> relName <<> " already exists in table " <>> tableName

  tableCache <-
    askSchemaCache
      >>= flip onNothing (throw400 NotFound "Could not find source.")
        . unsafeTableCache source
        . scSources

  let comment = _rdComment relDef
      metadataObj =
        MOSourceObjId source $
          AB.mkAnyBackend $
            SMOTableObj @b tableName $
              MTORel relName relType
  addRelationshipToMetadata <- case relType of
    ObjRel -> do
      value <- decodeValue $ toJSON relDef
      validateRelationship @b
        tableCache
        tableName
        (Left value)
      pure $ tmObjectRelationships %~ OMap.insert relName (RelDef relName (_rdUsing value) comment)
    ArrRel -> do
      value <- decodeValue $ toJSON relDef
      validateRelationship @b
        tableCache
        tableName
        (Right value)
      pure $ tmArrayRelationships %~ OMap.insert relName (RelDef relName (_rdUsing value) comment)

  buildSchemaCacheFor metadataObj $
    MetadataModifier $
      tableMetadataSetter @b source tableName %~ addRelationshipToMetadata
  pure successMsg

objRelP2Setup ::
  forall b m.
  (QErrM m, Backend b) =>
  SourceName ->
  TableName b ->
  HashMap (TableName b) (HashSet (ForeignKey b)) ->
  RelDef (ObjRelUsing b) ->
  m (RelInfo b, [SchemaDependency])
objRelP2Setup source qt foreignKeys (RelDef rn ru _) = case ru of
  RUManual rm -> do
    let refqt = rmTable rm
        (lCols, rCols) = unzip $ Map.toList $ rmColumns rm
        io = fromMaybe BeforeParent $ rmInsertOrder rm
        mkDependency tableName reason col =
          SchemaDependency
            ( SOSourceObj source $
                AB.mkAnyBackend $
                  SOITableObj @b tableName $
                    TOCol @b col
            )
            reason
        dependencies =
          map (mkDependency qt DRLeftColumn) lCols
            <> map (mkDependency refqt DRRightColumn) rCols
    pure (RelInfo rn ObjRel (rmColumns rm) refqt True io, dependencies)
  RUFKeyOn (SameTable columns) -> do
    foreignTableForeignKeys <-
      Map.lookup qt foreignKeys
        `onNothing` throw400 NotFound ("table " <> qt <<> " does not exist in source: " <> sourceNameToText source)
    ForeignKey constraint foreignTable colMap <- getRequiredFkey columns (Set.toList foreignTableForeignKeys)
    let dependencies =
          [ SchemaDependency
              ( SOSourceObj source $
                  AB.mkAnyBackend $
                    SOITableObj @b qt $
                      TOForeignKey @b (_cName constraint)
              )
              DRFkey,
            -- this needs to be added explicitly to handle the remote table being untracked. In this case,
            -- neither the using_col nor the constraint name will help.
            SchemaDependency
              ( SOSourceObj source $
                  AB.mkAnyBackend $
                    SOITable @b foreignTable
              )
              DRRemoteTable
          ]
            <> fmap (drUsingColumnDep @b source qt) (toList columns)
    pure (RelInfo rn ObjRel colMap foreignTable False BeforeParent, dependencies)
  RUFKeyOn (RemoteTable remoteTable remoteCols) ->
    mkFkeyRel ObjRel AfterParent source rn qt remoteTable remoteCols foreignKeys

arrRelP2Setup ::
  forall b m.
  (QErrM m, Backend b) =>
  HashMap (TableName b) (HashSet (ForeignKey b)) ->
  SourceName ->
  TableName b ->
  ArrRelDef b ->
  m (RelInfo b, [SchemaDependency])
arrRelP2Setup foreignKeys source qt (RelDef rn ru _) = case ru of
  RUManual rm -> do
    let refqt = rmTable rm
        (lCols, rCols) = unzip $ Map.toList $ rmColumns rm
        deps =
          map
            ( \c ->
                SchemaDependency
                  ( SOSourceObj source $
                      AB.mkAnyBackend $
                        SOITableObj @b qt $
                          TOCol @b c
                  )
                  DRLeftColumn
            )
            lCols
            <> map
              ( \c ->
                  SchemaDependency
                    ( SOSourceObj source $
                        AB.mkAnyBackend $
                          SOITableObj @b refqt $
                            TOCol @b c
                    )
                    DRRightColumn
              )
              rCols
    pure (RelInfo rn ArrRel (rmColumns rm) refqt True AfterParent, deps)
  RUFKeyOn (ArrRelUsingFKeyOn refqt refCols) ->
    mkFkeyRel ArrRel AfterParent source rn qt refqt refCols foreignKeys

mkFkeyRel ::
  forall b m.
  QErrM m =>
  Backend b =>
  RelType ->
  InsertOrder ->
  SourceName ->
  RelName ->
  TableName b ->
  TableName b ->
  NonEmpty (Column b) ->
  HashMap (TableName b) (HashSet (ForeignKey b)) ->
  m (RelInfo b, [SchemaDependency])
mkFkeyRel relType io source rn sourceTable remoteTable remoteColumns foreignKeys = do
  foreignTableForeignKeys <-
    Map.lookup remoteTable foreignKeys
      `onNothing` throw400 NotFound ("table " <> remoteTable <<> " does not exist in source: " <> sourceNameToText source)
  let keysThatReferenceUs = filter ((== sourceTable) . _fkForeignTable) (Set.toList foreignTableForeignKeys)
  ForeignKey constraint _foreignTable colMap <- getRequiredFkey remoteColumns keysThatReferenceUs
  let dependencies =
        [ SchemaDependency
            ( SOSourceObj source $
                AB.mkAnyBackend $
                  SOITableObj @b remoteTable $
                    TOForeignKey @b (_cName constraint)
            )
            DRRemoteFkey,
          SchemaDependency
            ( SOSourceObj source $
                AB.mkAnyBackend $
                  SOITable @b remoteTable
            )
            DRRemoteTable
        ]
          <> fmap (drUsingColumnDep @b source remoteTable) (toList remoteColumns)
  pure (RelInfo rn relType (reverseMap colMap) remoteTable False io, dependencies)
  where
    reverseMap :: Eq y => Hashable y => HashMap x y -> HashMap y x
    reverseMap = Map.fromList . fmap swap . Map.toList

-- | Try to find a foreign key constraint, identifying a constraint by its set of columns
getRequiredFkey ::
  (QErrM m, Backend b) =>
  NonEmpty (Column b) ->
  [ForeignKey b] ->
  m (ForeignKey b)
getRequiredFkey cols fkeys =
  case filteredFkeys of
    [k] -> return k
    [] -> throw400 ConstraintError "no foreign constraint exists on the given column(s)"
    _ -> throw400 ConstraintError "more than one foreign key constraint exists on the given column(s)"
  where
    filteredFkeys = filter ((== Set.fromList (toList cols)) . Map.keysSet . _fkColumnMapping) fkeys

drUsingColumnDep ::
  forall b.
  Backend b =>
  SourceName ->
  TableName b ->
  Column b ->
  SchemaDependency
drUsingColumnDep source qt col =
  SchemaDependency
    ( SOSourceObj source $
        AB.mkAnyBackend $
          SOITableObj @b qt $
            TOCol @b col
    )
    DRUsingColumn

--------------------------------------------------------------------------------
-- Drop local relationship

data DropRel b = DropRel
  { _drSource :: !SourceName,
    _drTable :: !(TableName b),
    _drRelationship :: !RelName,
    _drCascade :: !Bool
  }

instance (Backend b) => FromJSON (DropRel b) where
  parseJSON = withObject "DropRel" $ \o ->
    DropRel
      <$> o .:? "source" .!= defaultSource
      <*> o .: "table"
      <*> o .: "relationship"
      <*> o .:? "cascade" .!= False

runDropRel ::
  forall b m.
  (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b) =>
  DropRel b ->
  m EncJSON
runDropRel (DropRel source qt rn cascade) = do
  depObjs <- collectDependencies
  withNewInconsistentObjsCheck do
    metadataModifiers <- traverse purgeRelDep depObjs
    buildSchemaCache $
      MetadataModifier $
        tableMetadataSetter @b source qt
          %~ dropRelationshipInMetadata rn . foldr (.) id metadataModifiers
  pure successMsg
  where
    collectDependencies = do
      tabInfo <- askTableCoreInfo @b source qt
      void $ askRelType (_tciFieldInfoMap tabInfo) rn ""
      sc <- askSchemaCache
      let depObjs =
            getDependentObjs
              sc
              ( SOSourceObj source $
                  AB.mkAnyBackend $
                    SOITableObj @b qt $
                      TORel rn
              )
      when (depObjs /= [] && not cascade) $ reportDependentObjectsExist depObjs
      pure depObjs

purgeRelDep ::
  forall b m.
  QErrM m =>
  Backend b =>
  SchemaObjId ->
  m (TableMetadata b -> TableMetadata b)
purgeRelDep (SOSourceObj _ exists)
  | Just (SOITableObj _ (TOPerm rn pt)) <- AB.unpackAnyBackend @b exists =
    pure $ dropPermissionInMetadata rn pt
purgeRelDep d =
  throw500 $
    "unexpected dependency of relationship : "
      <> reportSchemaObj d

--------------------------------------------------------------------------------
-- Set local relationship comment

data SetRelComment b = SetRelComment
  { arSource :: !SourceName,
    arTable :: !(TableName b),
    arRelationship :: !RelName,
    arComment :: !(Maybe Text)
  }
  deriving (Generic)

deriving instance (Backend b) => Show (SetRelComment b)

deriving instance (Backend b) => Eq (SetRelComment b)

instance (Backend b) => FromJSON (SetRelComment b) where
  parseJSON = withObject "SetRelComment" $ \o ->
    SetRelComment
      <$> o .:? "source" .!= defaultSource
      <*> o .: "table"
      <*> o .: "relationship"
      <*> o .:? "comment"

runSetRelComment ::
  forall m b.
  (CacheRWM m, MonadError QErr m, MetadataM m, BackendMetadata b) =>
  SetRelComment b ->
  m EncJSON
runSetRelComment defn = do
  tabInfo <- askTableCoreInfo @b source qt
  relType <- riType <$> askRelType (_tciFieldInfoMap tabInfo) rn ""
  let metadataObj =
        MOSourceObjId source $
          AB.mkAnyBackend $
            SMOTableObj @b qt $
              MTORel rn relType
  buildSchemaCacheFor metadataObj $
    MetadataModifier $
      tableMetadataSetter @b source qt %~ case relType of
        ObjRel -> tmObjectRelationships . ix rn . rdComment .~ comment
        ArrRel -> tmArrayRelationships . ix rn . rdComment .~ comment
  pure successMsg
  where
    SetRelComment source qt rn comment = defn
