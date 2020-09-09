module Hasura.RQL.DDL.Relationship
  ( runCreateRelationship
  , insertRelationshipToCatalog
  , objRelP2Setup
  , arrRelP2Setup

  , runDropRel
  -- , delRelFromCatalog
  , dropRelationshipInMetadata

  , runSetRelComment
  )
where

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.DDL.Permission (dropPermissionInMetadata)
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import           Control.Lens              (ix)
import           Data.Aeson.Types
import           Data.Tuple                (swap)
import           Instances.TH.Lift         ()

import qualified Data.HashMap.Strict       as HM
import qualified Data.HashSet              as HS
import qualified Database.PG.Query         as Q

runCreateRelationship
  :: (MonadTx m, CacheRWM m, HasSystemDefined m, ToJSON a)
  => RelType -> WithTable (RelDef a) -> m EncJSON
runCreateRelationship relType (WithTable source tableName relDef) = do
  -- FIXME: Add relationship validation
  -- insertRelationshipToCatalog tableName relType relDef
  let relName = _rdName relDef
      comment = _rdComment relDef
      metadataObj = MOSourceObjId source $ SMOTableObj tableName $
                    MTORel relName relType

  addRelationshipToMetadata <- case relType of
    ObjRel -> do
      value <- decodeValue $ toJSON $ _rdUsing relDef
      pure $ tmObjectRelationships %~ HM.insert relName (RelDef relName value comment)
    ArrRel -> do
      value <- decodeValue $ toJSON $ _rdUsing relDef
      pure $ tmArrayRelationships %~ HM.insert relName (RelDef relName value comment)

  buildSchemaCacheFor metadataObj
    $ MetadataModifier
    $ tableMetadataSetter source tableName %~ addRelationshipToMetadata
  pure successMsg

insertRelationshipToCatalog
  :: (MonadTx m, HasSystemDefined m, ToJSON a)
  => QualifiedTable
  -> RelType
  -> RelDef a
  -> m ()
insertRelationshipToCatalog (QualifiedObject schema table) relType (RelDef name using comment) = do
  systemDefined <- askSystemDefined
  let args = (schema, table, name, relTypeToTxt relType, Q.AltJ using, comment, systemDefined)
  liftTx $ Q.unitQE defaultTxErrorHandler query args True
  where
    query = [Q.sql|
      INSERT INTO
        hdb_catalog.hdb_relationship
        (table_schema, table_name, rel_name, rel_type, rel_def, comment, is_system_defined)
      VALUES ($1, $2, $3, $4, $5 :: jsonb, $6, $7) |]

runDropRel :: (MonadTx m, CacheRWM m) => DropRel -> m EncJSON
runDropRel (DropRel source qt rn cascade) = do
  (relType, depObjs) <- collectDependencies
  withNewInconsistentObjsCheck do
    metadataModifiers <- traverse purgeRelDep depObjs
    buildSchemaCache $ MetadataModifier $
      tableMetadataSetter source qt %~
      dropRelationshipInMetadata rn relType . foldr (.) id metadataModifiers
  pure successMsg
  where
    collectDependencies = do
      tabInfo <- askTableCoreInfo source qt
      relType <- riType <$> askRelType (_tciFieldInfoMap tabInfo) rn ""
      sc      <- askSchemaCache
      let depObjs = getDependentObjs sc (SOSourceObj source $ SOITableObj qt $ TORel rn relType)
      when (depObjs /= [] && not (or cascade)) $ reportDeps depObjs
      pure (relType, depObjs)

dropRelationshipInMetadata
  :: RelName -> RelType -> TableMetadata -> TableMetadata
dropRelationshipInMetadata rn = \case
  ObjRel -> tmObjectRelationships %~ HM.delete rn
  ArrRel -> tmArrayRelationships %~ HM.delete rn

-- delRelFromCatalog
--   :: QualifiedTable
--   -> RelName
--   -> Q.TxE QErr ()
-- delRelFromCatalog (QualifiedObject sn tn) rn =
--   Q.unitQE defaultTxErrorHandler [Q.sql|
--            DELETE FROM
--                   hdb_catalog.hdb_relationship
--            WHERE table_schema =  $1
--              AND table_name = $2
--              AND rel_name = $3
--                 |] (sn, tn, rn) True

objRelP2Setup
  :: (QErrM m)
  => SourceName
  -> QualifiedTable
  -> HashSet ForeignKey
  -> RelDef ObjRelUsing
  -> m (RelInfo, [SchemaDependency])
objRelP2Setup source qt foreignKeys (RelDef rn ru _) = case ru of
  RUManual rm -> do
    let refqt = rmTable rm
        (lCols, rCols) = unzip $ HM.toList $ rmColumns rm
        mkDependency tableName reason col =
          SchemaDependency (SOSourceObj source $ SOITableObj tableName $ TOCol col) reason
        dependencies = map (mkDependency qt DRLeftColumn) lCols
                    <> map (mkDependency refqt DRRightColumn) rCols
    pure (RelInfo rn ObjRel (rmColumns rm) refqt True True, dependencies)
  RUFKeyOn columnName -> do
    ForeignKey constraint foreignTable colMap <- getRequiredFkey columnName (HS.toList foreignKeys)
    let withSource = SOSourceObj source
        dependencies =
          [ SchemaDependency (withSource $ SOITableObj qt $ TOForeignKey (_cName constraint)) DRFkey
          , SchemaDependency (withSource $ SOITableObj qt $ TOCol columnName) DRUsingColumn
          -- this needs to be added explicitly to handle the remote table being untracked. In this case,
          -- neither the using_col nor the constraint name will help.
          , SchemaDependency (withSource $ SOITable foreignTable) DRRemoteTable
          ]
    -- TODO(PDV?): this is too optimistic. Some object relationships are nullable, but
    -- we are marking some as non-nullable here.  This should really be done by
    -- checking nullability in the SQL schema.
    pure (RelInfo rn ObjRel colMap foreignTable False False, dependencies)

arrRelP2Setup
  :: (QErrM m)
  => SourceName
  -> HashMap QualifiedTable (HashSet ForeignKey)
  -> QualifiedTable
  -> ArrRelDef
  -> m (RelInfo, [SchemaDependency])
arrRelP2Setup source foreignKeys qt (RelDef rn ru _) = case ru of
  RUManual rm -> do
    let refqt = rmTable rm
        (lCols, rCols) = unzip $ HM.toList $ rmColumns rm
        deps  = map (\c -> SchemaDependency (withSource $ SOITableObj qt $ TOCol c) DRLeftColumn) lCols
                <> map (\c -> SchemaDependency (withSource $ SOITableObj refqt $ TOCol c) DRRightColumn) rCols
    pure (RelInfo rn ArrRel (rmColumns rm) refqt True True, deps)
  RUFKeyOn (ArrRelUsingFKeyOn refqt refCol) -> do
    foreignTableForeignKeys <- getTableInfo refqt foreignKeys
    let keysThatReferenceUs = filter ((== qt) . _fkForeignTable) (HS.toList foreignTableForeignKeys)
    ForeignKey constraint _ colMap <- getRequiredFkey refCol keysThatReferenceUs
    let deps = [ SchemaDependency (withSource $ SOITableObj refqt $ TOForeignKey (_cName constraint)) DRRemoteFkey
               , SchemaDependency (withSource $ SOITableObj refqt $ TOCol refCol) DRUsingColumn
               -- we don't need to necessarily track the remote table like we did in
               -- case of obj relationships as the remote table is indirectly
               -- tracked by tracking the constraint name and 'using_col'
               , SchemaDependency (withSource $ SOITable refqt) DRRemoteTable
               ]
        mapping = HM.fromList $ map swap $ HM.toList colMap
    pure (RelInfo rn ArrRel mapping refqt False False, deps)
  where
    withSource = SOSourceObj source

purgeRelDep
  :: (QErrM m)
  => SchemaObjId -> m (TableMetadata -> TableMetadata)
purgeRelDep = \case
  SOSourceObj _ (SOITableObj _ (TOPerm rn pt)) -> pure $ dropPermissionInMetadata rn pt
  d                           ->
    throw500 $ "unexpected dependency of relationship : "
    <> reportSchemaObj d

setRelCommentP2
  :: (QErrM m, MonadTx m)
  => SetRelComment -> m EncJSON
setRelCommentP2 arc = do
  liftTx $ setRelComment arc
  return successMsg

runSetRelComment
  :: (QErrM m, CacheRM m, MonadTx m)
  => SetRelComment -> m EncJSON
runSetRelComment defn = do
  tabInfo <- askTableCoreInfo source qt
  void $ askRelType (_tciFieldInfoMap tabInfo) rn ""
  setRelCommentP2 defn
  where
    SetRelComment source qt rn _ = defn

setRelComment :: SetRelComment
              -> Q.TxE QErr ()
setRelComment (SetRelComment source (QualifiedObject sn tn) rn comment) =
  Q.unitQE defaultTxErrorHandler [Q.sql|
           UPDATE hdb_catalog.hdb_relationship
           SET comment = $1
           WHERE table_schema =  $2
             AND table_name = $3
             AND rel_name = $4
                |] (comment, sn, tn, rn) True

getRequiredFkey
  :: (QErrM m)
  => PGCol
  -> [ForeignKey]
  -> m ForeignKey
getRequiredFkey col fkeys =
  case filteredFkeys of
    []  -> throw400 ConstraintError
          "no foreign constraint exists on the given column"
    [k] -> return k
    _   -> throw400 ConstraintError
           "more than one foreign key constraint exists on the given column"
  where
    filteredFkeys = filter ((== [col]) . HM.keys . _fkColumnMapping) fkeys
