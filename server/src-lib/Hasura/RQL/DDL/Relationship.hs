module Hasura.RQL.DDL.Relationship
  ( CreateArrRel (..),
    CreateObjRel (..),
    execCreateRelationship,
    runCreateRelationship,
    defaultBuildObjectRelationshipInfo,
    defaultBuildArrayRelationshipInfo,
    DropRel,
    execDropRel,
    runDropRel,
    dropRelationshipInMetadata,
    SetRelComment,
    runSetRelComment,
    nativeQueryRelationshipSetup,
  )
where

import Control.Lens ((.~))
import Data.Aeson.Types
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashMap.Strict.NonEmpty qualified as NEHashMap
import Data.HashSet qualified as Set
import Data.Sequence qualified as Seq
import Data.Text.Extended
import Data.Tuple (swap)
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.NativeQuery.Types (NativeQueryName)
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
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Table.Cache
import Hasura.Table.Metadata
  ( TableMetadata,
    tmArrayRelationships,
    tmObjectRelationships,
  )

--------------------------------------------------------------------------------
-- Create local relationship

newtype CreateArrRel b = CreateArrRel {unCreateArrRel :: WithTable b (ArrRelDef b)}
  deriving newtype (FromJSON)

newtype CreateObjRel b = CreateObjRel {unCreateObjRel :: WithTable b (ObjRelDef b)}
  deriving newtype (FromJSON)

execCreateRelationship ::
  forall b m a.
  ( BackendMetadata b,
    CacheRM m,
    MonadError QErr m,
    ToJSON a
  ) =>
  RelType ->
  WithTable b (RelDef a) ->
  Metadata ->
  m (MetadataObjId, MetadataModifier)
execCreateRelationship relType (WithTable source tableName relDef) _ = do
  let relName = _rdName relDef
  -- Check if any field with relationship name already exists in the table
  tableFields <- _tciFieldInfoMap <$> askTableCoreInfo @b source tableName
  for_ (HashMap.lookup (fromRel relName) tableFields)
    $ const
    $ throw400 AlreadyExists
    $ "field with name "
    <> relName
    <<> " already exists in table "
    <>> tableName

  tableCache <-
    askSchemaCache
      >>= flip onNothing (throw400 NotFound "Could not find source.")
      . unsafeTableCache source
      . scSources

  let comment = _rdComment relDef
      metadataObj =
        MOSourceObjId source
          $ AB.mkAnyBackend
          $ SMOTableObj @b tableName
          $ MTORel relName relType
  addRelationshipToMetadata <- case relType of
    ObjRel -> do
      value <- decodeValue $ toJSON relDef
      validateRelationship @b
        tableCache
        tableName
        (Left value)
      pure $ tmObjectRelationships %~ InsOrdHashMap.insert relName (RelDef relName (_rdUsing value) comment)
    ArrRel -> do
      value <- decodeValue $ toJSON relDef
      validateRelationship @b
        tableCache
        tableName
        (Right value)
      pure $ tmArrayRelationships %~ InsOrdHashMap.insert relName (RelDef relName (_rdUsing value) comment)

  let metadataModifier = MetadataModifier do
        tableMetadataSetter @b source tableName
          %~ addRelationshipToMetadata

  pure (metadataObj, metadataModifier)

runCreateRelationship ::
  forall m b a.
  (MonadError QErr m, CacheRWM m, ToJSON a, MetadataM m, BackendMetadata b) =>
  RelType ->
  WithTable b (RelDef a) ->
  m EncJSON
runCreateRelationship relType withTable = do
  (metadataObj, metadataModifier) <-
    getMetadata >>= execCreateRelationship relType withTable

  buildSchemaCacheFor metadataObj metadataModifier
  pure successMsg

defaultBuildObjectRelationshipInfo ::
  forall b m.
  (QErrM m, Backend b) =>
  SourceName ->
  HashMap (TableName b) (HashSet (ForeignKey b)) ->
  TableName b ->
  ObjRelDef b ->
  m (RelInfo b, Seq SchemaDependency)
defaultBuildObjectRelationshipInfo source foreignKeys qt (RelDef rn ru _) = case ru of
  RUManual (RelManualNativeQueryConfig (RelManualNativeQueryConfigC {rmnNativeQueryName = refqt, rmnCommon = common})) -> do
    let (lCols, rCols) = unzip $ HashMap.toList $ unRelMapping $ rmColumns common
        io = fromMaybe BeforeParent $ rmInsertOrder common
        mkNativeQueryDependency nativeQueryName reason col =
          SchemaDependency
            ( SOSourceObj source
                $ AB.mkAnyBackend
                $ SOINativeQueryObj @b nativeQueryName
                $ NQOCol @b
                $ getColumnPathColumn @b col
            )
            reason
        mkDependency tableName reason col =
          SchemaDependency
            ( SOSourceObj source
                $ AB.mkAnyBackend
                $ SOITableObj @b tableName
                $ TOCol @b
                $ getColumnPathColumn @b col
            )
            reason
        dependencies =
          (mkDependency qt DRLeftColumn <$> Seq.fromList lCols)
            <> (mkNativeQueryDependency refqt DRRightColumn <$> Seq.fromList rCols)
    pure (RelInfo rn ObjRel (rmColumns common) (RelTargetNativeQuery refqt) True io, dependencies)
  RUManual (RelManualTableConfig (RelManualTableConfigC {rmtTable = refqt, rmtCommon = common})) -> do
    let (lCols, rCols) = unzip $ HashMap.toList $ unRelMapping $ rmColumns common
        io = fromMaybe BeforeParent $ rmInsertOrder common
        mkDependency tableName reason col =
          SchemaDependency
            ( SOSourceObj source
                $ AB.mkAnyBackend
                $ SOITableObj @b tableName
                $ TOCol @b
                $ getColumnPathColumn @b col
            )
            reason
        dependencies =
          (mkDependency qt DRLeftColumn <$> Seq.fromList lCols)
            <> (mkDependency refqt DRRightColumn <$> Seq.fromList rCols)
    pure (RelInfo rn ObjRel (rmColumns common) (RelTargetTable refqt) True io, dependencies)
  RUFKeyOn (SameTable columns) -> do
    foreignTableForeignKeys <-
      HashMap.lookup qt foreignKeys
        `onNothing` throw400 NotFound ("table " <> qt <<> " does not exist in source: " <> sourceNameToText source)
    ForeignKey constraint foreignTable colMap <- getRequiredFkey columns (Set.toList foreignTableForeignKeys)
    let dependencies =
          Seq.fromList
            [ SchemaDependency
                ( SOSourceObj source
                    $ AB.mkAnyBackend
                    $ SOITableObj @b qt
                    $ TOForeignKey @b (_cName constraint)
                )
                DRFkey,
              -- this needs to be added explicitly to handle the remote table being untracked. In this case,
              -- neither the using_col nor the constraint name will help.
              SchemaDependency
                ( SOSourceObj source
                    $ AB.mkAnyBackend
                    $ SOITable @b foreignTable
                )
                DRRemoteTable
            ]
            <> (drUsingColumnDep @b source qt <$> Seq.fromList (toList $ getColumnPathColumn @b <$> columns))
    pure (RelInfo rn ObjRel (RelMapping $ NEHashMap.toHashMap colMap) (RelTargetTable foreignTable) False BeforeParent, dependencies)
  RUFKeyOn (RemoteTable remoteTable remoteCols) ->
    mkFkeyRel ObjRel AfterParent source rn qt remoteTable remoteCols foreignKeys

-- | set up a relationship from a Native Query onto another data source
nativeQueryRelationshipSetup ::
  forall b m.
  (QErrM m, Backend b) =>
  SourceName ->
  NativeQueryName ->
  RelType ->
  RelDef (RelManualConfig b) ->
  m (RelInfo b, Seq SchemaDependency)
nativeQueryRelationshipSetup sourceName nativeQueryName relType (RelDef relName relConfig _) = do
  let (relTarget, common, createRHSSchemaDependency) = case relConfig of
        RelManualNativeQueryConfig (RelManualNativeQueryConfigC {rmnNativeQueryName = refqt, rmnCommon}) ->
          ( RelTargetNativeQuery refqt,
            rmnCommon,
            \c ->
              SchemaDependency
                ( SOSourceObj sourceName
                    $ AB.mkAnyBackend
                    $ SOINativeQueryObj @b refqt
                    $ NQOCol @b
                    $ getColumnPathColumn @b c
                )
                DRRightColumn
          )
        RelManualTableConfig (RelManualTableConfigC {rmtTable = refqt, rmtCommon}) ->
          ( RelTargetTable refqt,
            rmtCommon,
            \c ->
              SchemaDependency
                ( SOSourceObj sourceName
                    $ AB.mkAnyBackend
                    $ SOITableObj @b refqt
                    $ TOCol @b
                    $ getColumnPathColumn @b c
                )
                DRRightColumn
          )

  let (lCols, rCols) = unzip $ HashMap.toList $ unRelMapping $ rmColumns common
      io = case relType of
        ObjRel -> fromMaybe BeforeParent $ rmInsertOrder common
        ArrRel -> AfterParent
      deps =
        ( fmap
            ( \c ->
                SchemaDependency
                  ( SOSourceObj sourceName
                      $ AB.mkAnyBackend
                      $ SOINativeQueryObj @b nativeQueryName
                      $ NQOCol @b
                      $ getColumnPathColumn @b c
                  )
                  DRLeftColumn
            )
            (Seq.fromList lCols)
        )
          <> fmap createRHSSchemaDependency (Seq.fromList rCols)
  pure (RelInfo relName relType (rmColumns common) relTarget True io, deps)

defaultBuildArrayRelationshipInfo ::
  forall b m.
  (QErrM m, Backend b) =>
  SourceName ->
  HashMap (TableName b) (HashSet (ForeignKey b)) ->
  TableName b ->
  ArrRelDef b ->
  m (RelInfo b, Seq SchemaDependency)
defaultBuildArrayRelationshipInfo source foreignKeys qt (RelDef rn ru _) = case ru of
  RUManual (RelManualNativeQueryConfig (RelManualNativeQueryConfigC {rmnNativeQueryName = refqt, rmnCommon = common})) -> do
    let (lCols, rCols) = unzip $ HashMap.toList $ unRelMapping $ rmColumns common
        deps =
          ( fmap
              ( \c ->
                  SchemaDependency
                    ( SOSourceObj source
                        $ AB.mkAnyBackend
                        $ SOITableObj @b qt
                        $ TOCol @b
                        $ getColumnPathColumn @b c
                    )
                    DRLeftColumn
              )
              (Seq.fromList lCols)
          )
            <> fmap
              ( \c ->
                  SchemaDependency
                    ( SOSourceObj source
                        $ AB.mkAnyBackend
                        $ SOINativeQueryObj @b refqt
                        $ NQOCol @b
                        $ getColumnPathColumn @b c
                    )
                    DRRightColumn
              )
              (Seq.fromList rCols)
    pure (RelInfo rn ArrRel (rmColumns common) (RelTargetNativeQuery refqt) True AfterParent, deps)
  RUManual (RelManualTableConfig (RelManualTableConfigC {rmtTable = refqt, rmtCommon = common})) -> do
    let (lCols, rCols) = unzip $ HashMap.toList $ unRelMapping $ rmColumns common
        deps =
          ( fmap
              ( \c ->
                  SchemaDependency
                    ( SOSourceObj source
                        $ AB.mkAnyBackend
                        $ SOITableObj @b qt
                        $ TOCol @b
                        $ getColumnPathColumn @b c
                    )
                    DRLeftColumn
              )
              (Seq.fromList lCols)
          )
            <> fmap
              ( \c ->
                  SchemaDependency
                    ( SOSourceObj source
                        $ AB.mkAnyBackend
                        $ SOITableObj @b refqt
                        $ TOCol @b
                        $ getColumnPathColumn @b c
                    )
                    DRRightColumn
              )
              (Seq.fromList rCols)
    pure (RelInfo rn ArrRel (rmColumns common) (RelTargetTable refqt) True AfterParent, deps)
  RUFKeyOn (ArrRelUsingFKeyOn refqt refCols) ->
    mkFkeyRel ArrRel AfterParent source rn qt refqt refCols foreignKeys

mkFkeyRel ::
  forall b m.
  (QErrM m) =>
  (Backend b) =>
  RelType ->
  InsertOrder ->
  SourceName ->
  RelName ->
  TableName b ->
  TableName b ->
  NonEmpty (ColumnPath b) ->
  HashMap (TableName b) (HashSet (ForeignKey b)) ->
  m (RelInfo b, Seq SchemaDependency)
mkFkeyRel relType io source rn sourceTable remoteTable remoteColumns foreignKeys = do
  foreignTableForeignKeys <-
    HashMap.lookup remoteTable foreignKeys
      `onNothing` throw400 NotFound ("table " <> remoteTable <<> " does not exist in source: " <> sourceNameToText source)
  let keysThatReferenceUs = filter ((== sourceTable) . _fkForeignTable) (Set.toList foreignTableForeignKeys)
  ForeignKey constraint _foreignTable colMap <- getRequiredFkey remoteColumns keysThatReferenceUs
  let dependencies =
        Seq.fromList
          [ SchemaDependency
              ( SOSourceObj source
                  $ AB.mkAnyBackend
                  $ SOITableObj @b remoteTable
                  $ TOForeignKey @b (_cName constraint)
              )
              DRRemoteFkey,
            SchemaDependency
              ( SOSourceObj source
                  $ AB.mkAnyBackend
                  $ SOITable @b remoteTable
              )
              DRRemoteTable
          ]
          <> ( drUsingColumnDep @b source remoteTable
                 <$> Seq.fromList (toList $ getColumnPathColumn @b <$> remoteColumns)
             )
  pure (RelInfo rn relType (RelMapping $ reverseMap $ NEHashMap.toHashMap colMap) (RelTargetTable remoteTable) False io, dependencies)
  where
    reverseMap :: (Hashable y) => HashMap x y -> HashMap y x
    reverseMap = HashMap.fromList . fmap swap . HashMap.toList

-- | Try to find a foreign key constraint, identifying a constraint by its set of columns
getRequiredFkey ::
  forall m b.
  (QErrM m, Backend b) =>
  NonEmpty (ColumnPath b) ->
  [ForeignKey b] ->
  m (ForeignKey b)
getRequiredFkey cols fkeys =
  case filteredFkeys of
    [k] -> return k
    [] -> throw400 ConstraintError "no foreign constraint exists on the given column(s)"
    _ -> throw400 ConstraintError "more than one foreign key constraint exists on the given column(s)"
  where
    filteredFkeys = filter ((== Set.fromList (toList cols)) . HashMap.keysSet . NEHashMap.toHashMap . _fkColumnMapping) fkeys

drUsingColumnDep ::
  forall b.
  (Backend b) =>
  SourceName ->
  TableName b ->
  Column b ->
  SchemaDependency
drUsingColumnDep source qt col =
  SchemaDependency
    ( SOSourceObj source
        $ AB.mkAnyBackend
        $ SOITableObj @b qt
        $ TOCol @b col
    )
    DRUsingColumn

--------------------------------------------------------------------------------
-- Drop local relationship

data DropRel b = DropRel
  { _drSource :: SourceName,
    _drTable :: TableName b,
    _drRelationship :: RelName,
    _drCascade :: Bool
  }

instance (Backend b) => FromJSON (DropRel b) where
  parseJSON = withObject "DropRel" $ \o ->
    DropRel
      <$> o
      .:? "source"
      .!= defaultSource
      <*> o
      .: "table"
      <*> o
      .: "relationship"
      <*> o
      .:? "cascade"
      .!= False

execDropRel ::
  forall b m.
  (MonadError QErr m, CacheRWM m, BackendMetadata b) =>
  DropRel b ->
  m MetadataModifier
execDropRel (DropRel source qt rn cascade) = do
  tableInfo <- askTableCoreInfo @b source qt

  _ <- askRelType (_tciFieldInfoMap tableInfo) rn ""
  schemaCache <- askSchemaCache

  let sourceObj :: SchemaObjId
      sourceObj =
        SOSourceObj source
          $ AB.mkAnyBackend
          $ SOITableObj @b qt
          $ TORel rn

      depObjs :: [SchemaObjId]
      depObjs = getDependentObjs schemaCache sourceObj

  unless (null depObjs || cascade) do
    reportDependentObjectsExist depObjs

  metadataModifiers <- traverse purgeRelDep depObjs

  let modifier :: TableMetadata b -> TableMetadata b
      modifier = dropRelationshipInMetadata rn . foldr (.) id metadataModifiers

  pure (MetadataModifier (tableMetadataSetter @b source qt %~ modifier))

runDropRel ::
  forall m b.
  (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b) =>
  DropRel b ->
  m EncJSON
runDropRel dropRel = do
  withNewInconsistentObjsCheck do
    metadataModifier <- execDropRel dropRel
    buildSchemaCache metadataModifier

  pure successMsg

purgeRelDep ::
  forall b m.
  (QErrM m) =>
  (Backend b) =>
  SchemaObjId ->
  m (TableMetadata b -> TableMetadata b)
purgeRelDep (SOSourceObj _ exists)
  | Just (SOITableObj _ (TOPerm rn pt)) <- AB.unpackAnyBackend @b exists =
      pure $ dropPermissionInMetadata rn pt
purgeRelDep d =
  throw500
    $ "unexpected dependency of relationship: "
    <> reportSchemaObj d

--------------------------------------------------------------------------------
-- Set local relationship comment

data SetRelComment b = SetRelComment
  { arSource :: SourceName,
    arTable :: TableName b,
    arRelationship :: RelName,
    arComment :: Maybe Text
  }
  deriving (Generic)

deriving instance (Backend b) => Show (SetRelComment b)

deriving instance (Backend b) => Eq (SetRelComment b)

instance (Backend b) => FromJSON (SetRelComment b) where
  parseJSON = withObject "SetRelComment" $ \o ->
    SetRelComment
      <$> o
      .:? "source"
      .!= defaultSource
      <*> o
      .: "table"
      <*> o
      .: "relationship"
      <*> o
      .:? "comment"

runSetRelComment ::
  forall m b.
  (CacheRWM m, MonadError QErr m, MetadataM m, BackendMetadata b) =>
  SetRelComment b ->
  m EncJSON
runSetRelComment defn = do
  tabInfo <- askTableCoreInfo @b source qt
  relType <- riType <$> askRelType (_tciFieldInfoMap tabInfo) rn ""
  let metadataObj =
        MOSourceObjId source
          $ AB.mkAnyBackend
          $ SMOTableObj @b qt
          $ MTORel rn relType
  buildSchemaCacheFor metadataObj
    $ MetadataModifier
    $ tableMetadataSetter @b source qt
    %~ case relType of
      ObjRel -> tmObjectRelationships . ix rn . rdComment .~ comment
      ArrRel -> tmArrayRelationships . ix rn . rdComment .~ comment
  pure successMsg
  where
    SetRelComment source qt rn comment = defn
