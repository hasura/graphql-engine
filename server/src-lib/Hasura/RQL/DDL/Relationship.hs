module Hasura.RQL.DDL.Relationship
  ( runCreateRelationship
  , insertRelationshipToCatalog
  , objRelP2Setup
  , arrRelP2Setup

  , runDropRel
  , delRelFromCatalog

  , runSetRelComment
  , module Hasura.RQL.DDL.Relationship.Types
  )
where

import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.SchemaCacheTypes
import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.DDL.Permission                  (purgePerm)
import           Hasura.RQL.DDL.Relationship.Types
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import           Data.Aeson.Types
import           Data.Tuple                                 (swap)
import           Instances.TH.Lift                          ()

import qualified Data.HashMap.Strict                        as HM
import qualified Data.HashSet                               as HS
import qualified Database.PG.Query                          as Q

runCreateRelationship
  :: (MonadTx m, CacheRWM m, HasSystemDefined m, ToJSON a)
  => RelType -> WithTable (RelDef a) -> m EncJSON
runCreateRelationship relType (WithTable tableName relDef) = do
  insertRelationshipToCatalog tableName relType relDef
  buildSchemaCacheFor $ MOTableObj tableName (MTORel (rdName relDef) relType)
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
runDropRel (DropRel qt rn cascade) = do
  depObjs <- collectDependencies
  withNewInconsistentObjsCheck do
    traverse_ purgeRelDep depObjs
    liftTx $ delRelFromCatalog qt rn
    buildSchemaCache
  pure successMsg
  where
    collectDependencies = do
      tabInfo <- askTableCoreInfo qt
      _       <- askRelType (_tciFieldInfoMap tabInfo) rn ""
      sc      <- askSchemaCache
      let depObjs = getDependentObjs sc (SOTableObj qt $ TORel rn)
      when (depObjs /= [] && not (or cascade)) $ reportDeps depObjs
      pure depObjs

delRelFromCatalog
  :: QualifiedTable
  -> RelName
  -> Q.TxE QErr ()
delRelFromCatalog (QualifiedObject sn tn) rn =
  Q.unitQE defaultTxErrorHandler [Q.sql|
           DELETE FROM
                  hdb_catalog.hdb_relationship
           WHERE table_schema =  $1
             AND table_name = $2
             AND rel_name = $3
                |] (sn, tn, rn) True

objRelP2Setup
  :: (QErrM m)
  => QualifiedTable
  -> HashSet ForeignKey
  -> RelDef ObjRelUsing
  -> m (RelInfo, [SchemaDependency])
objRelP2Setup qt foreignKeys (RelDef rn ru _) = case ru of
  RUManual rm -> do
    let refqt = rmTable rm
        (lCols, rCols) = unzip $ HM.toList $ rmColumns rm
        mkDependency tableName reason col = SchemaDependency (SOTableObj tableName $ TOCol col) reason
        dependencies = map (mkDependency qt DRLeftColumn) lCols
                    <> map (mkDependency refqt DRRightColumn) rCols
    pure (RelInfo rn ObjRel (rmColumns rm) refqt True, dependencies)
  RUFKeyOn columnName -> do
    ForeignKey constraint foreignTable colMap <- getRequiredFkey columnName (HS.toList foreignKeys)
    let dependencies =
          [ SchemaDependency (SOTableObj qt $ TOForeignKey (_cName constraint)) DRFkey
          , SchemaDependency (SOTableObj qt $ TOCol columnName) DRUsingColumn
          -- this needs to be added explicitly to handle the remote table being untracked. In this case,
          -- neither the using_col nor the constraint name will help.
          , SchemaDependency (SOTable foreignTable) DRRemoteTable
          ]
    pure (RelInfo rn ObjRel colMap foreignTable False, dependencies)

arrRelP2Setup
  :: (QErrM m)
  => HashMap QualifiedTable (HashSet ForeignKey)
  -> QualifiedTable
  -> ArrRelDef
  -> m (RelInfo, [SchemaDependency])
arrRelP2Setup foreignKeys qt (RelDef rn ru _) = case ru of
  RUManual rm -> do
    let refqt = rmTable rm
        (lCols, rCols) = unzip $ HM.toList $ rmColumns rm
        deps  = map (\c -> SchemaDependency (SOTableObj qt $ TOCol c) DRLeftColumn) lCols
                <> map (\c -> SchemaDependency (SOTableObj refqt $ TOCol c) DRRightColumn) rCols
    pure (RelInfo rn ArrRel (rmColumns rm) refqt True, deps)
  RUFKeyOn (ArrRelUsingFKeyOn refqt refCol) -> do
    foreignTableForeignKeys <- getTableInfo refqt foreignKeys
    let keysThatReferenceUs = filter ((== qt) . _fkForeignTable) (HS.toList foreignTableForeignKeys)
    ForeignKey constraint _ colMap <- getRequiredFkey refCol keysThatReferenceUs
    let deps = [ SchemaDependency (SOTableObj refqt $ TOForeignKey (_cName constraint)) DRRemoteFkey
               , SchemaDependency (SOTableObj refqt $ TOCol refCol) DRUsingColumn
               -- we don't need to necessarily track the remote table like we did in
               -- case of obj relationships as the remote table is indirectly
               -- tracked by tracking the constraint name and 'using_col'
               , SchemaDependency (SOTable refqt) DRRemoteTable
               ]
        mapping = HM.fromList $ map swap $ HM.toList colMap
    pure (RelInfo rn ArrRel mapping refqt False, deps)

purgeRelDep :: (MonadTx m) => SchemaObjId -> m ()
purgeRelDep (SOTableObj tn (TOPerm rn pt)) = purgePerm tn rn pt
purgeRelDep d = throw500 $ "unexpected dependency of relationship : "
                <> reportSchemaObj d

validateRelP1
  :: (UserInfoM m, QErrM m, TableCoreInfoRM m)
  => QualifiedTable -> RelName -> m RelInfo
validateRelP1 qt rn = do
  tabInfo <- askTableCoreInfo qt
  askRelType (_tciFieldInfoMap tabInfo) rn ""

setRelCommentP2
  :: (QErrM m, MonadTx m)
  => SetRelComment -> m EncJSON
setRelCommentP2 arc = do
  liftTx $ setRelComment arc
  return successMsg

runSetRelComment
  :: (QErrM m, CacheRM m, MonadTx m, UserInfoM m)
  => SetRelComment -> m EncJSON
runSetRelComment defn = do
  void $ validateRelP1 qt rn
  setRelCommentP2 defn
  where
    SetRelComment qt rn _ = defn

setRelComment :: SetRelComment
              -> Q.TxE QErr ()
setRelComment (SetRelComment (QualifiedObject sn tn) rn comment) =
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
