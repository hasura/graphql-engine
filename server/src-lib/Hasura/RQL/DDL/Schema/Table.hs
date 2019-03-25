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
import           Hasura.RQL.DDL.Schema.Function
import           Hasura.RQL.DDL.Schema.Rename
import           Hasura.RQL.DDL.Subscribe
import           Hasura.RQL.DDL.Utils
import           Hasura.RQL.Types
import           Hasura.Server.Utils                (matchRegex)
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

delTableFromCatalog :: QualifiedTable -> Q.Tx ()
delTableFromCatalog (QualifiedObject sn tn) =
  Q.unitQ [Q.sql|
            DELETE FROM "hdb_catalog"."hdb_table"
            WHERE table_schema = $1 AND table_name = $2
                |] (sn, tn) False

saveTableToCatalog :: QualifiedTable -> Q.Tx ()
saveTableToCatalog (QualifiedObject sn tn) =
  Q.unitQ [Q.sql|
            INSERT INTO "hdb_catalog"."hdb_table" VALUES ($1, $2)
                |] (sn, tn) False

-- Build the TableInfo with all its columns
getTableInfo :: QualifiedTable -> Bool -> Q.TxE QErr TableInfo
getTableInfo qt@(QualifiedObject sn tn) isSystemDefined = do
  tableData <- Q.catchE defaultTxErrorHandler $
               Q.listQ $(Q.sqlFromFile "src-rsr/table_info.sql")(sn, tn) True
  case tableData of
    [] -> throw400 NotExists $ "no such table/view exists in postgres : " <>> qt
    [(Q.AltJ cols, Q.AltJ cons, Q.AltJ viewInfoM)] ->
      return $ mkTableInfo qt isSystemDefined cons cols viewInfoM
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
      tn = GS.qualObjectToName vn
  GS.checkConflictingNode defGCtx tn

  trackExistingTableOrViewP2Setup vn isSystemDefined
  liftTx $ Q.catchE defaultTxErrorHandler $
    saveTableToCatalog vn

  -- refresh the gCtx in schema cache
  refreshGCtxMapInSchema

  return successMsg

runTrackTableQ
  :: ( QErrM m, CacheRWM m, MonadTx m
     , MonadIO m, HasHttpManager m, UserInfoM m
     )
  => TrackTable -> m RespBody
runTrackTableQ q = do
  trackExistingTableOrViewP1 q
  trackExistingTableOrViewP2 (tName q) False

purgeDep :: (CacheRWM m, MonadTx m)
         => Bool -> SchemaObjId -> m ()
purgeDep purgeInCache schemaObjId = case schemaObjId of
  (SOTable tn) -> do
    liftTx $ Q.catchE defaultTxErrorHandler $ delTableFromCatalog tn
    withCache $ delTableFromCache tn

  (SOTableObj tn (TOPerm rn pt)) -> do
    liftTx $ dropPermFromCatalog tn rn pt
    withCache $ withPermType pt delPermFromCache rn tn

  (SOTableObj qt (TORel rn))     -> do
    liftTx $ delRelFromCatalog qt rn
    withCache $ delRelFromCache rn qt

  (SOQTemplate qtn)              -> do
    liftTx $ delQTemplateFromCatalog qtn
    withCache $ delQTemplateFromCache qtn

  (SOFunction qf) -> do
    liftTx $ delFunctionFromCatalog qf
    withCache $ delFunctionFromCache qf

  (SOTableObj qt (TOTrigger trn)) -> do
    liftTx $ delEventTriggerFromCatalog trn
    withCache $ delEventTriggerFromCache qt trn

  _                              -> throw500 $
    "unexpected dependent object : " <> reportSchemaObj schemaObjId
  where
    withCache = when purgeInCache

processTableChanges :: (MonadTx m, CacheRWM m)
                    => TableInfo -> TableDiff -> m Bool
processTableChanges ti tableDiff = do
  -- If table rename occurs then don't replace constraints and
  -- process dropped/added columns, because schema reload happens eventually
  sc <- askSchemaCache
  let tn = tiName ti
      withOldTabName = do
        -- replace constraints
        replaceConstraints tn
        -- for all the dropped columns
        procDroppedCols tn
        -- for all added columns
        procAddedCols tn
        -- for all altered columns
        procAlteredCols sc tn

      withNewTabName newTN = do
        let tnGQL = GS.qualObjectToName newTN
            defGCtx = scDefaultRemoteGCtx sc
        -- check for GraphQL schema conflicts on new name
        GS.checkConflictingNode defGCtx tnGQL
        void $ procAlteredCols sc tn
        -- update new table in catalog
        renameTableInCatalog newTN tn
        return True

  maybe withOldTabName withNewTabName mNewName

  where
    TableDiff mNewName droppedCols addedCols alteredCols _ constraints = tableDiff
    replaceConstraints tn = flip modTableInCache tn $ \tInfo ->
      return $ tInfo {tiUniqOrPrimConstraints = constraints}

    procDroppedCols tn =
      forM_ droppedCols $ \droppedCol ->
        -- Drop the column from the cache
        delColFromCache droppedCol tn

    procAddedCols tn =
      -- In the newly added columns check that there is no conflict with relationships
      forM_ addedCols $ \pci@(PGColInfo colName _ _) ->
        case M.lookup (fromPGCol colName) $ tiFieldInfoMap ti of
          Just (FIRelationship _) ->
            throw400 AlreadyExists $ "cannot add column " <> colName
            <<> " in table " <> tn <<>
            " as a relationship with the name already exists"
          _ -> addColToCache colName pci tn

    procAlteredCols sc tn = fmap or $
      forM alteredCols $ \(PGColInfo oColName oColTy _, PGColInfo nColName nColTy _) ->
        if | oColName /= nColName -> do
               renameColInCatalog oColName nColName tn ti
               return True
           | oColTy /= nColTy -> do
               let colId   = SOTableObj tn $ TOCol oColName
                   depObjs = getDependentObjsWith (== "on_type") sc colId
               unless (null depObjs) $ throw400 DependencyError $
                 "cannot change type of column " <> oColName <<> " in table "
                 <> tn <<> " because of the following dependencies : " <>
                 reportSchemaObjs depObjs
               return False
           | otherwise -> return False

delTableAndDirectDeps
  :: (QErrM m, CacheRWM m, MonadTx m) => QualifiedTable -> m ()
delTableAndDirectDeps qtn@(QualifiedObject sn tn) = do
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

processSchemaChanges :: (MonadTx m, CacheRWM m) => SchemaDiff -> m Bool
processSchemaChanges schemaDiff = do
  -- Purge the dropped tables
  mapM_ delTableAndDirectDeps droppedTables

  sc <- askSchemaCache
  fmap or $ forM alteredTables $ \(oldQtn, tableDiff) -> do
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

  -- Get relational, query template and function dependants
  let allDeps = getDependentObjs sc (SOTable qtn)
      indirectDeps = filter (not . isDirectDep) allDeps

  -- Report bach with an error if cascade is not set
  when (indirectDeps /= [] && not (or cascade)) $ reportDepsExt indirectDeps []

  -- Purge all the dependants from state
  mapM_ (purgeDep True) indirectDeps

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

handleInconsistentObj
  :: (QErrM m, CacheRWM m)
  => Bool -> SchemaObjId -> m () -> m ()
handleInconsistentObj False _ act = act
handleInconsistentObj True obj action =
  action `catchError` \err -> do
    sc <- askSchemaCache
    let inconsObj = InconsistentObj obj $ qeError err
        allInconsObjs = inconsObj:scInconsistentObjs sc
    writeSchemaCache $ sc{scInconsistentObjs = allInconsObjs}

buildSchemaCache
  :: (MonadTx m, CacheRWM m, MonadIO m, HasHttpManager m, HasServeOptsCtx m)
  => m ()
buildSchemaCache = do
  -- clean hdb_views
  liftTx $ Q.catchE defaultTxErrorHandler clearHdbViews
  -- reset the current schemacache
  writeSchemaCache emptySchemaCache
  hMgr <- askHttpManager
  serveOptsCtx@(ServeOptsCtx strfyNum softStart) <- askServeOptsCtx
  tables <- liftTx $ Q.catchE defaultTxErrorHandler fetchTables
  forM_ tables $ \(sn, tn, isSystemDefined) -> do
    let qt = QualifiedObject sn tn
    handleInconsistentObj softStart (SOTable qt) $
      modifyErr (\e -> "table " <> tn <<> "; " <> e) $
      trackExistingTableOrViewP2Setup qt isSystemDefined

  -- Fetch all the relationships
  relationships <- liftTx $ Q.catchE defaultTxErrorHandler fetchRelationships

  forM_ relationships $ \(sn, tn, rn, rt, Q.AltJ rDef) -> do
    let qt = QualifiedObject sn tn
        schObj = SOTableObj qt $ TORel rn
    handleInconsistentObj softStart schObj $
      modifyErr (\e -> "table " <> tn <<> "; rel " <> rn <<> "; " <> e) $ case rt of
      ObjRel -> do
        using <- decodeValue rDef
        objRelP2Setup qt $ RelDef rn using Nothing
      ArrRel -> do
        using <- decodeValue rDef
        arrRelP2Setup qt $ RelDef rn using Nothing

  -- Fetch all the permissions
  permissions <- liftTx $ Q.catchE defaultTxErrorHandler fetchPermissions

  forM_ permissions $ \(sn, tn, rn, pt, Q.AltJ pDef) -> do
    let schObj = SOTableObj (QualifiedObject sn tn) $ TOPerm rn pt
    handleInconsistentObj softStart schObj $
      modifyErr (\e -> "table " <> tn <<> "; role " <> rn <<> "; " <> e) $ case pt of
          PTInsert -> permHelper serveOptsCtx sn tn rn pDef PAInsert
          PTSelect -> permHelper serveOptsCtx sn tn rn pDef PASelect
          PTUpdate -> permHelper serveOptsCtx sn tn rn pDef PAUpdate
          PTDelete -> permHelper serveOptsCtx sn tn rn pDef PADelete

  -- Fetch all the query templates
  qtemplates <- liftTx $ Q.catchE defaultTxErrorHandler fetchQTemplates
  forM_ qtemplates $ \(qtn, Q.AltJ qtDefVal) -> do
    let schObj = SOQTemplate qtn
    handleInconsistentObj softStart schObj $ do
      qtDef <- decodeValue qtDefVal
      qCtx <- mkAdminQCtx serveOptsCtx <$> askSchemaCache
      (qti, deps) <- liftP1WithQCtx qCtx $ createQueryTemplateP1 $
             CreateQueryTemplate qtn qtDef Nothing
      addQTemplateToCache qti deps

  eventTriggers <- liftTx $ Q.catchE defaultTxErrorHandler fetchEventTriggers
  forM_ eventTriggers $ \(sn, tn, trid, trn, Q.AltJ configuration) -> do
    let qt = QualifiedObject sn tn
        schObj = SOTableObj qt $ TOTrigger trn
    handleInconsistentObj softStart schObj $ do
      etc <- decodeValue configuration

      subTableP2Setup qt trid etc
      allCols <- getCols . tiFieldInfoMap <$> askTabInfo qt
      liftTx $ mkTriggerQ trid trn qt allCols strfyNum (etcDefinition etc)

  functions <- liftTx $ Q.catchE defaultTxErrorHandler fetchFunctions
  forM_ functions $ \(sn, fn) -> do
    let schObj = SOFunction $ QualifiedObject sn fn
    handleInconsistentObj softStart schObj $
      modifyErr (\e -> "function " <> fn <<> "; " <> e) $
      trackFunctionP2Setup (QualifiedObject sn fn)

  -- remote schemas
  res <- liftTx fetchRemoteSchemas
  sc <- askSchemaCache
  gCtxMap <- GS.mkGCtxMap (scTables sc) (scFunctions sc)

  remoteScConf <- forM res $ \(AddRemoteSchemaQuery n def _) ->
    (,) n <$> validateRemoteSchemaDef def
  let rmScMap = M.fromList remoteScConf
  (mergedGCtxMap, defGCtx) <- mergeSchemas rmScMap gCtxMap hMgr
  writeRemoteSchemasToCache mergedGCtxMap rmScMap
  postMergeSc <- askSchemaCache
  writeSchemaCache postMergeSc { scDefaultRemoteGCtx = defGCtx }

  where
    permHelper serveOptsCtx sn tn rn pDef pa = do
      qCtx <- mkAdminQCtx serveOptsCtx <$> askSchemaCache
      perm <- decodeValue pDef
      let qt = QualifiedObject sn tn
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
    fetchFunctions =
      Q.listQ [Q.sql|
               SELECT function_schema, function_name
                 FROM hdb_catalog.hdb_function
                    |] () False

data RunSQL
  = RunSQL
  { rSql                      :: T.Text
  , rCascade                  :: !(Maybe Bool)
  , rCheckMetadataConsistency :: !(Maybe Bool)
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 1 snakeCase){omitNothingFields=True} ''RunSQL)

data RunSQLRes
  = RunSQLRes
  { rrResultType :: !T.Text
  , rrResult     :: !Value
  } deriving (Show, Eq)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''RunSQLRes)

execRawSQL :: (MonadTx m) => T.Text -> m RunSQLRes
execRawSQL =
  liftTx . Q.multiQE rawSqlErrHandler . Q.fromText
  where
    rawSqlErrHandler txe =
      let e = err400 PostgresError "query execution failed"
      in e {qeInternal = Just $ toJSON txe}

execWithMDCheck
  :: (QErrM m, CacheRWM m, MonadTx m, MonadIO m, HasHttpManager m, HasServeOptsCtx m)
  => RunSQL -> m RunSQLRes
execWithMDCheck (RunSQL t cascade _) = do

  -- Drop hdb_views so no interference is caused to the sql query
  liftTx $ Q.catchE defaultTxErrorHandler clearHdbViews

  -- Get the metadata before the sql query, everything, need to filter this
  oldMetaU <- liftTx $ Q.catchE defaultTxErrorHandler fetchTableMeta
  oldFuncMetaU <-
    liftTx $ Q.catchE defaultTxErrorHandler fetchFunctionMeta

  -- Run the SQL
  res <- execRawSQL t

  -- Get the metadata after the sql query
  newMeta <- liftTx $ Q.catchE defaultTxErrorHandler fetchTableMeta
  newFuncMeta <- liftTx $ Q.catchE defaultTxErrorHandler fetchFunctionMeta
  sc <- askSchemaCache
  let existingTables = M.keys $ scTables sc
      oldMeta = flip filter oldMetaU $ \tm -> tmTable tm `elem` existingTables
      schemaDiff = getSchemaDiff oldMeta newMeta
      existingFuncs = M.keys $ scFunctions sc
      oldFuncMeta = flip filter oldFuncMetaU $ \fm -> funcFromMeta fm `elem` existingFuncs
      FunctionDiff droppedFuncs alteredFuncs = getFuncDiff oldFuncMeta newFuncMeta
      overloadedFuncs = getOverloadedFuncs existingFuncs newFuncMeta

  -- Do not allow overloading functions
  unless (null overloadedFuncs) $
    throw400 NotSupported $ "the following tracked function(s) cannot be overloaded: "
    <> reportFuncs overloadedFuncs

  indirectDeps <- getSchemaChangeDeps schemaDiff

  -- Report back with an error if cascade is not set
  when (indirectDeps /= [] && not (or cascade)) $ reportDepsExt indirectDeps []

  -- Purge all the indirect dependents from state
  mapM_ (purgeDep True) indirectDeps

  -- Purge all dropped functions
  let purgedFuncs = flip mapMaybe indirectDeps $ \dep ->
        case dep of
          SOFunction qf -> Just qf
          _             -> Nothing

  forM_ (droppedFuncs \\ purgedFuncs) $ \qf -> do
    liftTx $ delFunctionFromCatalog qf
    delFunctionFromCache qf

  -- Process altered functions
  forM_ alteredFuncs $ \(qf, newTy) ->
    when (newTy == FTVOLATILE) $
      throw400 NotSupported $
      "type of function " <> qf <<> " is altered to \"VOLATILE\" which is not supported now"

  -- update the schema cache and hdb_catalog with the changes
  reloadRequired <- processSchemaChanges schemaDiff

  let withReload = buildSchemaCache
      withoutReload = do
        postSc <- askSchemaCache
        -- recreate the insert permission infra
        forM_ (M.elems $ scTables postSc) $ \ti -> do
          let tn = tiName ti
          forM_ (M.elems $ tiRolePermInfoMap ti) $ \rpi ->
            maybe (return ()) (liftTx . buildInsInfra tn) $ _permIns rpi

        strfyNum <- socStringifyNum <$> askServeOptsCtx
        --recreate triggers
        forM_ (M.elems $ scTables postSc) $ \ti -> do
          let tn = tiName ti
              cols = getCols $ tiFieldInfoMap ti
          forM_ (M.toList $ tiEventTriggerInfoMap ti) $ \(trn, eti) -> do
            let opsDef = etiOpsDef eti
                trid = etiId eti
            liftTx $ mkTriggerQ trid trn tn cols strfyNum opsDef

  bool withoutReload withReload reloadRequired

  -- refresh the gCtxMap in schema cache
  refreshGCtxMapInSchema

  return res
  where
    reportFuncs = T.intercalate ", " . map dquoteTxt

isAltrDropReplace :: QErrM m => T.Text -> m Bool
isAltrDropReplace = either throwErr return . matchRegex regex False
  where
    throwErr s = throw500 $ "compiling regex failed: " <> T.pack s
    regex = "alter|drop|replace|create function"

runRunSQL
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m, MonadIO m, HasHttpManager m, HasServeOptsCtx m)
  => RunSQL -> m RespBody
runRunSQL q@(RunSQL t _ mChkMDCnstcy) = do
  adminOnly
  isMDChkNeeded <- maybe (isAltrDropReplace t) return mChkMDCnstcy
  encode <$> bool (execRawSQL t) (execWithMDCheck q) isMDChkNeeded

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
