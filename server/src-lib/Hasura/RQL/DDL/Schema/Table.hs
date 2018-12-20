module Hasura.RQL.DDL.Schema.Table where

import           Hasura.GraphQL.RemoteServer
import           Hasura.Prelude
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.DDL.Permission.Internal
import           Hasura.RQL.DDL.QueryTemplate
import           Hasura.RQL.DDL.Relationship
import           Hasura.RQL.DDL.RemoteSchema
import           Hasura.RQL.DDL.Schema.Diff
import           Hasura.RQL.DDL.Subscribe
import           Hasura.RQL.DDL.Utils
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Database.PG.Query                  as Q
import qualified Hasura.GraphQL.Schema              as GS

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Instances.TH.Lift                  ()
import           Language.Haskell.TH.Syntax         (Lift)
import           Network.URI.Extended               ()

import qualified Data.HashMap.Strict                as M
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as TE
import qualified Database.PostgreSQL.LibPQ          as PQ
import qualified Language.GraphQL.Draft.Syntax      as G

delTableFromCatalog :: QualifiedTable -> Q.Tx ()
delTableFromCatalog (QualifiedTable sn tn) =
  Q.unitQ [Q.sql|
            DELETE FROM "hdb_catalog"."hdb_table"
            WHERE table_schema = $1 AND table_name = $2
                |] (sn, tn) False

saveTableToCatalog :: QualifiedTable -> Q.Tx ()
saveTableToCatalog (QualifiedTable sn tn) =
  Q.unitQ [Q.sql|
            INSERT INTO "hdb_catalog"."hdb_table" VALUES ($1, $2)
                |] (sn, tn) False

-- Build the TableInfo with all its columns
getTableInfo :: QualifiedTable -> Bool -> Q.TxE QErr TableInfo
getTableInfo qt@(QualifiedTable sn tn) isSystemDefined = do
  tableData <- Q.catchE defaultTxErrorHandler $
               Q.listQ $(Q.sqlFromFile "src-rsr/table_info.sql")(sn, tn) True
  case tableData of
    [] -> throw400 NotExists $ "no such table/view exists in postgres : " <>> qt
    [(Q.AltJ cols, Q.AltJ pkeyCols, Q.AltJ cons, Q.AltJ viewInfoM)] ->
      return $ mkTableInfo qt isSystemDefined cons cols pkeyCols viewInfoM
    _ -> throw500 $ "more than one row found for: " <>> qt

newtype TrackTable
  = TrackTable
  { tName :: QualifiedTable }
  deriving (Show, Eq, FromJSON, ToJSON, Lift)

trackExistingTableOrViewP1
  :: (CacheRM m, UserInfoM m, QErrM m) => TrackTable -> m ()
trackExistingTableOrViewP1 (TrackTable vn) = do
  adminOnly
  rawSchemaCache <- askSchemaCache
  when (M.member vn $ scTables rawSchemaCache) $
    throw400 AlreadyTracked $ "view/table already tracked : " <>> vn

trackExistingTableOrViewP2Setup
  :: (QErrM m, CacheRWM m, MonadTx m)
  => QualifiedTable -> Bool -> m ()
trackExistingTableOrViewP2Setup tn isSystemDefined = do
  ti <- liftTx $ getTableInfo tn isSystemDefined
  addTableToCache ti

trackExistingTableOrViewP2
  :: (QErrM m, CacheRWM m, MonadTx m, MonadIO m, HasHttpManager m)
  => QualifiedTable -> Bool -> m RespBody
trackExistingTableOrViewP2 vn isSystemDefined = do
  sc <- askSchemaCache
  let defGCtx = scDefaultRemoteGCtx sc
  GS.checkConflictingNode defGCtx (G.Name tn)

  trackExistingTableOrViewP2Setup vn isSystemDefined
  liftTx $ Q.catchE defaultTxErrorHandler $
    saveTableToCatalog vn

  -- refresh the gCtx in schema cache
  refreshGCtxMapInSchema

  return successMsg
  where
    getSchemaN = getSchemaTxt . qtSchema
    getTableN = getTableTxt . qtTable
    tn = case getSchemaN vn of
      "public" -> getTableN vn
      _        -> getSchemaN vn <> "_" <> getTableN vn

runTrackTableQ
  :: ( QErrM m, CacheRWM m, MonadTx m
     , MonadIO m, HasHttpManager m, UserInfoM m
     )
  => TrackTable -> m RespBody
runTrackTableQ q = do
  trackExistingTableOrViewP1 q
  trackExistingTableOrViewP2 (tName q) False

purgeDep :: (CacheRWM m, MonadTx m)
         => SchemaObjId -> m ()
purgeDep schemaObjId = case schemaObjId of
  (SOTableObj tn (TOPerm rn pt)) -> do
    liftTx $ dropPermFromCatalog tn rn pt
    withPermType pt delPermFromCache rn tn

  (SOTableObj qt (TORel rn))     -> do
    liftTx $ delRelFromCatalog qt rn
    delRelFromCache rn qt

  (SOQTemplate qtn)              -> do
    liftTx $ delQTemplateFromCatalog qtn
    delQTemplateFromCache qtn

  (SOTableObj qt (TOTrigger trn)) -> do
    liftTx $ delEventTriggerFromCatalog trn
    delEventTriggerFromCache qt trn

  _                              -> throw500 $
    "unexpected dependent object : " <> reportSchemaObj schemaObjId

processTableChanges
  :: (QErrM m, CacheRWM m) => TableInfo -> TableDiff -> m ()
processTableChanges ti tableDiff = do

  when (isJust mNewName) $
    throw400 NotSupported $ "table renames are not yet supported : " <>> tn

  -- for all the dropped columns
  forM_ droppedCols $ \droppedCol ->
    -- Drop the column from the cache
    delColFromCache droppedCol tn

  -- In the newly added columns check that there is no conflict with relationships
  forM_ addedCols $ \colInfo@(PGColInfo colName _ _) ->
    case M.lookup (fromPGCol colName) $ tiFieldInfoMap ti of
      Just (FIRelationship _) ->
        throw400 AlreadyExists $ "cannot add column " <> colName
        <<> " in table " <> tn <<>
        " as a relationship with the name already exists"
      _ -> addColToCache colName colInfo tn

  sc <- askSchemaCache
  -- for rest of the columns
  forM_ alteredCols $ \(PGColInfo oColName oColTy _, nci@(PGColInfo nColName nColTy _)) ->
    if | oColName /= nColName ->
           throw400 NotSupported $ "column renames are not yet supported : " <>
             tn <<> "." <>> oColName
       | oColTy /= nColTy -> do
           let colId   = SOTableObj tn $ TOCol oColName
               depObjs = getDependentObjsWith (== "on_type") sc colId
           if null depObjs
             then updateFldInCache oColName nci
             else throw400 DependencyError $ "cannot change type of column " <> oColName <<> " in table "
                  <> tn <<> " because of the following dependencies : " <>
                  reportSchemaObjs depObjs
       | otherwise -> return ()
  where
    updateFldInCache cn ci = do
      delColFromCache cn tn
      addColToCache cn ci tn
    tn = tiName ti
    TableDiff mNewName droppedCols addedCols alteredCols _ = tableDiff

delTableAndDirectDeps
  :: (QErrM m, CacheRWM m, MonadTx m) => QualifiedTable -> m ()
delTableAndDirectDeps qtn@(QualifiedTable sn tn) = do
  liftTx $ Q.catchE defaultTxErrorHandler $ do
    Q.unitQ [Q.sql|
             DELETE FROM "hdb_catalog"."hdb_relationship"
             WHERE table_schema = $1 AND table_name = $2
              |] (sn, tn) False
    Q.unitQ [Q.sql|
             DELETE FROM "hdb_catalog"."hdb_permission"
             WHERE table_schema = $1 AND table_name = $2
              |] (sn, tn) False
    Q.unitQ [Q.sql|
             DELETE FROM "hdb_catalog"."event_triggers"
             WHERE schema_name = $1 AND table_name = $2
              |] (sn, tn) False
    delTableFromCatalog qtn
  delTableFromCache qtn

processSchemaChanges
  :: (QErrM m, CacheRWM m, MonadTx m) => SchemaDiff -> m ()
processSchemaChanges schemaDiff = do
  -- Purge the dropped tables
  mapM_ delTableAndDirectDeps droppedTables
  -- Get schema cache
  sc <- askSchemaCache
  forM_ alteredTables $ \(oldQtn, tableDiff) -> do
    ti <- case M.lookup oldQtn $ scTables sc of
      Just ti -> return ti
      Nothing -> throw500 $ "old table metadata not found in cache : " <>> oldQtn
    processTableChanges ti tableDiff
  where
    SchemaDiff droppedTables alteredTables = schemaDiff

data UntrackTable =
  UntrackTable
  { utTable   :: !QualifiedTable
  , utCascade :: !(Maybe Bool)
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''UntrackTable)

unTrackExistingTableOrViewP1
  :: (CacheRM m, UserInfoM m, QErrM m) => UntrackTable -> m ()
unTrackExistingTableOrViewP1 (UntrackTable vn _) = do
  adminOnly
  rawSchemaCache <- askSchemaCache
  case M.lookup vn (scTables rawSchemaCache) of
    Just ti ->
      -- Check if table/view is system defined
      when (tiSystemDefined ti) $ throw400 NotSupported $
        vn <<> " is system defined, cannot untrack"
    Nothing -> throw400 AlreadyUntracked $
      "view/table already untracked : " <>> vn

unTrackExistingTableOrViewP2
  :: (QErrM m, CacheRWM m, MonadTx m, MonadIO m, HasHttpManager m)
  => UntrackTable -> m RespBody
unTrackExistingTableOrViewP2 (UntrackTable qtn cascade) = do
  sc <- askSchemaCache

  -- Get relational and query template dependants
  let allDeps = getDependentObjs sc (SOTable qtn)
      indirectDeps = filter (not . isDirectDep) allDeps

  -- Report bach with an error if cascade is not set
  when (indirectDeps /= [] && not (or cascade)) $ reportDepsExt indirectDeps []

  -- Purge all the dependants from state
  mapM_ purgeDep indirectDeps

  -- delete the table and its direct dependencies
  delTableAndDirectDeps qtn

  -- refresh the gctxmap in schema cache
  refreshGCtxMapInSchema

  return successMsg
  where
    isDirectDep = \case
      (SOTableObj dtn _) -> qtn == dtn
      _                  -> False

runUntrackTableQ
  :: ( QErrM m, CacheRWM m, MonadTx m
     , MonadIO m, HasHttpManager m, UserInfoM m
     )
  => UntrackTable -> m RespBody
runUntrackTableQ q = do
  unTrackExistingTableOrViewP1 q
  unTrackExistingTableOrViewP2 q

buildSchemaCache
  :: (MonadTx m, CacheRWM m, MonadIO m, HasHttpManager m)
  => m ()
buildSchemaCache = do
  -- reset the current schemacache
  writeSchemaCache emptySchemaCache
  hMgr <- askHttpManager
  tables <- liftTx $ Q.catchE defaultTxErrorHandler fetchTables
  forM_ tables $ \(sn, tn, isSystemDefined) ->
    modifyErr (\e -> "table " <> tn <<> "; " <> e) $
    trackExistingTableOrViewP2Setup (QualifiedTable sn tn) isSystemDefined

  -- Fetch all the relationships
  relationships <- liftTx $ Q.catchE defaultTxErrorHandler fetchRelationships

  forM_ relationships $ \(sn, tn, rn, rt, Q.AltJ rDef) ->
    modifyErr (\e -> "table " <> tn <<> "; rel " <> rn <<> "; " <> e) $ case rt of
    ObjRel -> do
      using <- decodeValue rDef
      objRelP2Setup (QualifiedTable sn tn) $ RelDef rn using Nothing
    ArrRel -> do
      using <- decodeValue rDef
      arrRelP2Setup (QualifiedTable sn tn) $ RelDef rn using Nothing

  -- Fetch all the permissions
  permissions <- liftTx $ Q.catchE defaultTxErrorHandler fetchPermissions

  forM_ permissions $ \(sn, tn, rn, pt, Q.AltJ pDef) ->
    modifyErr (\e -> "table " <> tn <<> "; role " <> rn <<> "; " <> e) $ case pt of
    PTInsert -> permHelper sn tn rn pDef PAInsert
    PTSelect -> permHelper sn tn rn pDef PASelect
    PTUpdate -> permHelper sn tn rn pDef PAUpdate
    PTDelete -> permHelper sn tn rn pDef PADelete

  -- Fetch all the query templates
  qtemplates <- liftTx $ Q.catchE defaultTxErrorHandler fetchQTemplates
  forM_ qtemplates $ \(qtn, Q.AltJ qtDefVal) -> do
    qtDef <- decodeValue qtDefVal
    qCtx <- mkAdminQCtx <$> askSchemaCache
    (qti, deps) <- liftP1WithQCtx qCtx $ createQueryTemplateP1 $
           CreateQueryTemplate qtn qtDef Nothing
    addQTemplateToCache qti deps

  eventTriggers <- liftTx $ Q.catchE defaultTxErrorHandler fetchEventTriggers
  forM_ eventTriggers $ \(sn, tn, trid, trn, Q.AltJ configuration) -> do
    etc <- decodeValue configuration

    let qt = QualifiedTable sn tn
    subTableP2Setup qt trid etc
    allCols <- getCols . tiFieldInfoMap <$> askTabInfo qt
    liftTx $ mkTriggerQ trid trn qt allCols (etcDefinition etc)

  -- remote schemas
  res <- liftTx fetchRemoteSchemas
  sc <- askSchemaCache
  gCtxMap <- GS.mkGCtxMap (scTables sc)

  remoteScConf <- forM res $ \(AddRemoteSchemaQuery n def _) ->
    (,) n <$> validateRemoteSchemaDef def
  let rmScMap = M.fromList remoteScConf
  (mergedGCtxMap, defGCtx) <- mergeSchemas rmScMap gCtxMap hMgr
  writeRemoteSchemasToCache mergedGCtxMap rmScMap
  postMergeSc <- askSchemaCache
  writeSchemaCache postMergeSc { scDefaultRemoteGCtx = defGCtx }

  where
    permHelper sn tn rn pDef pa = do
      qCtx <- mkAdminQCtx <$> askSchemaCache
      perm <- decodeValue pDef
      let qt = QualifiedTable sn tn
          permDef = PermDef rn perm Nothing
          createPerm = WithTable qt permDef
      (permInfo, deps) <- liftP1WithQCtx qCtx $ createPermP1 createPerm
      addPermP2Setup qt permDef permInfo
      addPermToCache qt rn pa permInfo deps
      -- p2F qt rn p1Res

    fetchTables =
      Q.listQ [Q.sql|
                SELECT table_schema, table_name, is_system_defined
                  FROM hdb_catalog.hdb_table
                    |] () False

    fetchRelationships =
      Q.listQ [Q.sql|
                SELECT table_schema, table_name, rel_name, rel_type, rel_def::json
                  FROM hdb_catalog.hdb_relationship
                    |] () False

    fetchPermissions =
      Q.listQ [Q.sql|
                SELECT table_schema, table_name, role_name, perm_type, perm_def::json
                  FROM hdb_catalog.hdb_permission
                    |] () False

    fetchQTemplates =
      Q.listQ [Q.sql|
                SELECT template_name, template_defn :: json FROM hdb_catalog.hdb_query_template
                  |] () False

    fetchEventTriggers =
      Q.listQ [Q.sql|
               SELECT e.schema_name, e.table_name, e.id, e.name, e.configuration::json
                 FROM hdb_catalog.event_triggers e
               |] () False

data RunSQL
  = RunSQL
  { rSql     :: T.Text
  , rCascade :: !(Maybe Bool)
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 1 snakeCase){omitNothingFields=True} ''RunSQL)

data RunSQLRes
  = RunSQLRes
  { rrResultType :: !T.Text
  , rrResult     :: !Value
  } deriving (Show, Eq)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''RunSQLRes)

runSqlP2
  :: (QErrM m, CacheRWM m, MonadTx m, MonadIO m, HasHttpManager m)
  => RunSQL -> m RespBody
runSqlP2 (RunSQL t cascade) = do

  -- Drop hdb_views so no interference is caused to the sql query
  liftTx $ Q.catchE defaultTxErrorHandler clearHdbViews

  -- Get the metadata before the sql query, everything, need to filter this
  oldMetaU <- liftTx $ Q.catchE defaultTxErrorHandler fetchTableMeta

  -- Run the SQL
  res <- liftTx $ Q.multiQE rawSqlErrHandler $ Q.fromText t

  -- Get the metadata after the sql query
  newMeta <- liftTx $ Q.catchE defaultTxErrorHandler fetchTableMeta
  sc <- askSchemaCache
  let existingTables = M.keys $ scTables sc
      oldMeta = flip filter oldMetaU $ \tm -> tmTable tm `elem` existingTables
      schemaDiff = getSchemaDiff oldMeta newMeta

  indirectDeps <- getSchemaChangeDeps schemaDiff

  -- Report back with an error if cascade is not set
  when (indirectDeps /= [] && not (or cascade)) $ reportDepsExt indirectDeps []

  -- Purge all the indirect dependents from state
  mapM_ purgeDep indirectDeps

  -- update the schema cache with the changes
  processSchemaChanges schemaDiff

  postSc <- askSchemaCache
  -- recreate the insert permission infra
  forM_ (M.elems $ scTables postSc) $ \ti -> do
    let tn = tiName ti
    forM_ (M.elems $ tiRolePermInfoMap ti) $ \rpi ->
      maybe (return ()) (liftTx . buildInsInfra tn) $ _permIns rpi

  --recreate triggers
  forM_ (M.elems $ scTables postSc) $ \ti -> do
    let tn = tiName ti
        cols = getCols $ tiFieldInfoMap ti
    forM_ (M.toList $ tiEventTriggerInfoMap ti) $ \(trn, eti) -> do
      let opsDef = etiOpsDef eti
          trid = etiId eti
      liftTx $ mkTriggerQ trid trn tn cols opsDef

  -- refresh the gCtxMap in schema cache
  refreshGCtxMapInSchema

  return $ encode (res :: RunSQLRes)

  where
    rawSqlErrHandler :: Q.PGTxErr -> QErr
    rawSqlErrHandler txe =
      let e = err400 PostgresError "query execution failed"
      in e {qeInternal = Just $ toJSON txe}

runRunSQL
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m, MonadIO m, HasHttpManager m)
  => RunSQL -> m RespBody
runRunSQL q =
  adminOnly >> runSqlP2 q

-- Should be used only after checking the status
resToCSV :: PQ.Result -> ExceptT T.Text IO [[T.Text]]
resToCSV r =  do
  nr  <- liftIO $ PQ.ntuples r
  nc  <- liftIO $ PQ.nfields r

  hdr <- forM [0..pred nc] $ \ic -> do
    colNameBS <- liftIO $ PQ.fname r ic
    maybe (return "unknown") decodeBS colNameBS

  rows <- forM [0..pred nr] $ \ir ->
    forM [0..pred nc] $ \ic -> do
      cellValBS <- liftIO $ PQ.getvalue r ir ic
      maybe (return "NULL") decodeBS cellValBS

  return $ hdr:rows

  where
    decodeBS = either (throwError . T.pack . show) return . TE.decodeUtf8'

instance Q.FromRes RunSQLRes where
  fromRes (Q.ResultOkEmpty _) =
    return $ RunSQLRes "CommandOk" Null
  fromRes (Q.ResultOkData res) = do
    csvRows <- resToCSV res
    return $ RunSQLRes "TuplesOk" $ toJSON csvRows
