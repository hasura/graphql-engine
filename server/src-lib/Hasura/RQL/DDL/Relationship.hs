module Hasura.RQL.DDL.Relationship
  ( runCreateRelationship
  , objRelP2Setup
  , arrRelP2Setup

  , runDropRel
  , dropRelationshipInMetadata

  , runSetRelComment
  )
where

import           Hasura.Prelude

import qualified Data.HashMap.Strict        as HM
import qualified Data.HashMap.Strict.InsOrd as OMap
import qualified Data.HashSet               as HS

import           Control.Lens               ((.~))
import           Data.Aeson.Types
import           Data.Text.Extended
import           Data.Tuple                 (swap)

import qualified Hasura.SQL.AnyBackend      as AB

import           Hasura.EncJSON
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.Types

runCreateRelationship
  :: forall m b a
   . (MonadError QErr m, CacheRWM m, ToJSON a, MetadataM m, Backend b, BackendMetadata b)
  => RelType -> WithTable b (RelDef a) -> m EncJSON
runCreateRelationship relType (WithTable source tableName relDef) = do
  let relName = _rdName relDef
  -- Check if any field with relationship name already exists in the table
  tableFields <- _tciFieldInfoMap <$> askTableCoreInfo source tableName
  onJust (HM.lookup (fromRel relName) tableFields) $ const $
    throw400 AlreadyExists $
    "field with name " <> relName <<> " already exists in table " <>> tableName
  let comment = _rdComment relDef
      metadataObj = MOSourceObjId source
                      $ AB.mkAnyBackend
                      $ SMOTableObj tableName
                      $ MTORel relName relType
  addRelationshipToMetadata <- case relType of
    ObjRel -> do
      value <- decodeValue $ toJSON $ _rdUsing relDef
      pure $ tmObjectRelationships %~ OMap.insert relName (RelDef relName value comment)
    ArrRel -> do
      value <- decodeValue $ toJSON $ _rdUsing relDef
      pure $ tmArrayRelationships %~ OMap.insert relName (RelDef relName value comment)

  buildSchemaCacheFor metadataObj
    $ MetadataModifier
    $ tableMetadataSetter source tableName %~ addRelationshipToMetadata
  pure successMsg

runDropRel
  :: forall b m
   . (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b)
  => DropRel b -> m EncJSON
runDropRel (DropRel source qt rn cascade) = do
  depObjs <- collectDependencies
  withNewInconsistentObjsCheck do
    metadataModifiers <- traverse purgeRelDep depObjs
    buildSchemaCache $ MetadataModifier $
      tableMetadataSetter source qt %~
      dropRelationshipInMetadata rn . foldr (.) id metadataModifiers
  pure successMsg
  where
    collectDependencies = do
      tabInfo <- askTableCoreInfo source qt
      void $ askRelType (_tciFieldInfoMap tabInfo) rn ""
      sc      <- askSchemaCache
      let depObjs = getDependentObjs
                      sc
                      (SOSourceObj source
                        $ AB.mkAnyBackend
                        $ SOITableObj qt
                        $ TORel rn)
      when (depObjs /= [] && not cascade) $ reportDeps depObjs
      pure depObjs

dropRelationshipInMetadata
  :: RelName -> TableMetadata b -> TableMetadata b
dropRelationshipInMetadata relName =
  -- Since the name of a relationship is unique in a table, the relationship
  -- with given name may present in either array or object relationships but
  -- not in both.
  (tmObjectRelationships %~ OMap.delete relName)
  . (tmArrayRelationships %~ OMap.delete relName)

objRelP2Setup
  :: forall b m
   . (QErrM m, Backend b)
  => SourceName
  -> TableName b
  -> HashMap (TableName b) (HashSet (ForeignKey b))
  -> RelDef (ObjRelUsing b)
  -> FieldInfoMap (ColumnInfo b)
  -> m (RelInfo b, [SchemaDependency])
objRelP2Setup source qt foreignKeys (RelDef rn ru _) fieldInfoMap = case ru of
  RUManual rm -> do
    let refqt = rmTable rm
        (lCols, rCols) = unzip $ HM.toList $ rmColumns rm
        io = fromMaybe BeforeParent $ rmInsertOrder rm
        mkDependency tableName reason col = SchemaDependency
                                              (SOSourceObj source
                                                $ AB.mkAnyBackend
                                                $ SOITableObj tableName
                                                $ TOCol col)
                                              reason
        dependencies = map (mkDependency qt DRLeftColumn) lCols
                    <> map (mkDependency refqt DRRightColumn) rCols
    pure (RelInfo rn ObjRel (rmColumns rm) refqt True True io, dependencies)
  RUFKeyOn (SameTable columnName) -> do
    foreignTableForeignKeys <- findTable qt foreignKeys
    ForeignKey constraint foreignTable colMap <- getRequiredFkey columnName (HS.toList foreignTableForeignKeys)
    let dependencies =
          [ SchemaDependency
              (SOSourceObj source
                $ AB.mkAnyBackend
                $ SOITableObj qt
                $ TOForeignKey (_cName constraint))
              DRFkey
          , SchemaDependency
              (SOSourceObj source
                $ AB.mkAnyBackend
                $ SOITableObj qt
                $ TOCol columnName)
              DRUsingColumn
          -- this needs to be added explicitly to handle the remote table being untracked. In this case,
          -- neither the using_col nor the constraint name will help.
          , SchemaDependency
              (SOSourceObj source
                $ AB.mkAnyBackend
                $ SOITable foreignTable)
              DRRemoteTable
          ]
    colInfo <- HM.lookup (fromCol columnName) fieldInfoMap
               `onNothing` throw500 "could not find column info in schema cache"
    let nullable = pgiIsNullable colInfo
    pure (RelInfo rn ObjRel colMap foreignTable False nullable BeforeParent, dependencies)
  RUFKeyOn (RemoteTable remoteTable remoteCol) -> do
    foreignTableForeignKeys <- findTable remoteTable foreignKeys
    ForeignKey constraint _foreignTable colMap <- getRequiredRemoteFkey remoteCol (HS.toList foreignTableForeignKeys)
    let dependencies =
          [ SchemaDependency
              (SOSourceObj source
                $ AB.mkAnyBackend
                $ SOITableObj remoteTable
                $ TOForeignKey (_cName constraint))
              DRRemoteFkey
          , SchemaDependency
              (SOSourceObj source
                $ AB.mkAnyBackend
                $ SOITableObj qt
                $ TOCol remoteCol)
              DRUsingColumn
          , SchemaDependency
              (SOSourceObj source
                $ AB.mkAnyBackend
                $ SOITable remoteTable)
              DRRemoteTable
          ]
    pure (RelInfo rn ObjRel colMap remoteTable False False AfterParent, dependencies)

arrRelP2Setup
  :: forall b m
   . (QErrM m, Backend b)
  => HashMap (TableName b) (HashSet (ForeignKey b))
  -> SourceName
  -> TableName b
  -> ArrRelDef b
  -> m (RelInfo b, [SchemaDependency])
arrRelP2Setup foreignKeys source qt (RelDef rn ru _) = case ru of
  RUManual rm -> do
    let refqt = rmTable rm
        (lCols, rCols) = unzip $ HM.toList $ rmColumns rm
        deps  = map (\c -> SchemaDependency
                             (SOSourceObj source
                               $ AB.mkAnyBackend
                               $ SOITableObj qt
                               $ TOCol c) DRLeftColumn)
                  lCols
                  <> map (\c -> SchemaDependency
                                  (SOSourceObj source
                                    $ AB.mkAnyBackend
                                    $ SOITableObj refqt
                                    $ TOCol c)
                                  DRRightColumn)
                  rCols
    pure (RelInfo rn ArrRel (rmColumns rm) refqt True True BeforeParent, deps)
  RUFKeyOn (ArrRelUsingFKeyOn refqt refCol) -> do
    foreignTableForeignKeys <- findTable refqt foreignKeys
    let keysThatReferenceUs = filter ((== qt) . _fkForeignTable) (HS.toList foreignTableForeignKeys)
    ForeignKey constraint _ colMap <- getRequiredFkey refCol keysThatReferenceUs
    let deps = [ SchemaDependency
                   (SOSourceObj source
                     $ AB.mkAnyBackend
                     $ SOITableObj refqt
                     $ TOForeignKey (_cName constraint))
                   DRRemoteFkey
               , SchemaDependency
                   (SOSourceObj source
                     $ AB.mkAnyBackend
                     $ SOITableObj refqt
                     $ TOCol refCol)
                   DRUsingColumn
               -- we don't need to necessarily track the remote table like we did in
               -- case of obj relationships as the remote table is indirectly
               -- tracked by tracking the constraint name and 'using_col'
               , SchemaDependency
                   (SOSourceObj source
                     $ AB.mkAnyBackend
                     $ SOITable refqt)
                   DRRemoteTable
               ]
        mapping = HM.fromList $ map swap $ HM.toList colMap
    pure (RelInfo rn ArrRel mapping refqt False False BeforeParent, deps)

purgeRelDep
  :: forall b m
   . QErrM m
  => Backend b
  => SchemaObjId -> m (TableMetadata b -> TableMetadata b)
purgeRelDep (SOSourceObj _ exists)
  | Just (SOITableObj _ (TOPerm rn pt)) <- AB.unpackAnyBackend @b exists =
      pure $ dropPermissionInMetadata rn pt
purgeRelDep d = throw500 $ "unexpected dependency of relationship : "
                <> reportSchemaObj d

runSetRelComment
  :: forall m b
   . (CacheRWM m, MonadError QErr m, MetadataM m, BackendMetadata b)
  => SetRelComment b -> m EncJSON
runSetRelComment defn = do
  tabInfo <- askTableCoreInfo source qt
  relType <- riType <$> askRelType (_tciFieldInfoMap tabInfo) rn ""
  let metadataObj = MOSourceObjId source
                      $ AB.mkAnyBackend
                      $ SMOTableObj qt
                      $ MTORel rn relType
  buildSchemaCacheFor metadataObj
    $ MetadataModifier
    $ tableMetadataSetter source qt %~ case relType of
      ObjRel -> tmObjectRelationships.ix rn.rdComment .~ comment
      ArrRel -> tmArrayRelationships.ix rn.rdComment .~ comment
  pure successMsg
  where
    SetRelComment source qt rn comment = defn

getRequiredFkey
  :: (QErrM m, Backend b)
  => Column b
  -> [ForeignKey b]
  -> m (ForeignKey b)
getRequiredFkey col fkeys =
  case filteredFkeys of
    []  -> throw400 ConstraintError
          "no foreign constraint exists on the given column"
    [k] -> return k
    _   -> throw400 ConstraintError
           "more than one foreign key constraint exists on the given column"
  where
    filteredFkeys = filter ((== [col]) . HM.keys . _fkColumnMapping) fkeys

getRequiredRemoteFkey
  :: QErrM m
  => Backend b
  => Column b
  -> [ForeignKey b]
  -> m (ForeignKey b)
getRequiredRemoteFkey col fkeys =
  case filteredFkeys of
    []  -> throw400 ConstraintError
          "no foreign constraint exists on the given column"
    [k] -> return k
    _   -> throw400 ConstraintError
           "more than one foreign key constraint exists on the given column"
  where
    filteredFkeys = filter ((== [col]) . HM.elems . _fkColumnMapping) fkeys
