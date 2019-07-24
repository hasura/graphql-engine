{- |
Description: Create/delete SQL tables to/from Hasura metadata.
-}

{-# LANGUAGE TypeApplications #-}

module Hasura.RQL.DDL.Schema.Table where

import           Hasura.EncJSON
import           Hasura.GraphQL.RemoteServer
import           Hasura.Prelude
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.DDL.EventTrigger
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.DDL.Permission.Internal
import           Hasura.RQL.DDL.Relationship
import           Hasura.RQL.DDL.RemoteSchema
import           Hasura.RQL.DDL.Schema.Diff
import           Hasura.RQL.DDL.Schema.Function
import           Hasura.RQL.DDL.Schema.Rename
import           Hasura.RQL.DDL.Utils
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Catalog
import           Hasura.RQL.Types.QueryCollection
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
import qualified Data.HashSet                       as HS
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

newtype TrackTable
  = TrackTable
  { tName :: QualifiedTable }
  deriving (Show, Eq, FromJSON, ToJSON, Lift)

-- | Track table/view, Phase 1:
-- Validate table tracking operation. Fails if table is already being tracked,
-- or if a function with the same name is being tracked.
trackExistingTableOrViewP1
  :: (CacheRM m, UserInfoM m, QErrM m) => TrackTable -> m ()
trackExistingTableOrViewP1 (TrackTable vn) = do
  adminOnly
  rawSchemaCache <- askSchemaCache
  when (M.member vn $ scTables rawSchemaCache) $
    throw400 AlreadyTracked $ "view/table already tracked : " <>> vn
  let qf = fmap (FunctionName . getTableTxt) vn
  when (M.member qf $ scFunctions rawSchemaCache) $
    throw400 NotSupported $ "function with name " <> vn <<> " already exists"

trackExistingTableOrViewP2
  :: (QErrM m, CacheRWM m, MonadTx m)
  => QualifiedTable -> Bool -> m EncJSON
trackExistingTableOrViewP2 vn isSystemDefined = do
  sc <- askSchemaCache
  let defGCtx = scDefaultRemoteGCtx sc
  GS.checkConflictingNode defGCtx $ GS.qualObjectToName vn

  tables <- liftTx fetchTableCatalog
  case tables of
    []   -> throw400 NotExists $ "no such table/view exists in postgres : " <>> vn
    [ti] -> addTableToCache ti
    _    -> throw500 $ "more than one row found for: " <>> vn
  liftTx $ Q.catchE defaultTxErrorHandler $ saveTableToCatalog vn

  return successMsg
  where
    QualifiedObject sn tn = vn
    mkTableInfo (cols, pCols, constraints, viewInfoM) =
      let colMap = M.fromList $ flip map (Q.getAltJ cols) $
            \c -> (fromPGCol $ pgiName c, FIColumn c)
      in TableInfo vn isSystemDefined colMap mempty (Q.getAltJ constraints)
                  (Q.getAltJ pCols) (Q.getAltJ viewInfoM) mempty
    fetchTableCatalog = map mkTableInfo <$>
      Q.listQE defaultTxErrorHandler [Q.sql|
           SELECT columns, primary_key_columns,
                  constraints, view_info
           FROM hdb_catalog.hdb_table_info_agg
           WHERE table_schema = $1 AND table_name = $2
           |] (sn, tn) True

runTrackTableQ
  :: (QErrM m, CacheRWM m, MonadTx m, UserInfoM m)
  => TrackTable -> m EncJSON
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

  (SOTableObj qt (TORemoteRel rn))     -> do
    liftTx $ delRemoteRelFromCatalog qt rn
    delRemoteRelFromCache qt rn

  (SOFunction qf) -> do
    liftTx $ delFunctionFromCatalog qf
    delFunctionFromCache qf

  (SOTableObj qt (TOTrigger trn)) -> do
    liftTx $ delEventTriggerFromCatalog trn
    delEventTriggerFromCache qt trn

  _                              -> throw500 $
    "unexpected dependent object : " <> reportSchemaObj schemaObjId

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

    procAlteredCols sc tn = fmap or $ forM alteredCols $
      \( PGColInfo oColName oColTy oNullable
       , npci@(PGColInfo nColName nColTy nNullable)
       ) ->
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
               updColInCache nColName npci tn
               return False
           | oNullable /= nNullable -> do
               updColInCache nColName npci tn
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
    Q.unitQ [Q.sql|
             DELETE FROM "hdb_catalog"."hdb_remote_relationship"
             WHERE table_schema = $1 AND table_name = $2
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
  :: (QErrM m, CacheRWM m, MonadTx m)
  => UntrackTable -> m EncJSON
unTrackExistingTableOrViewP2 (UntrackTable qtn cascade) = do
  sc <- askSchemaCache

  -- Get relational, query template and function dependants
  let allDeps = getDependentObjs sc (SOTable qtn)
      indirectDeps = filter (not . isDirectDep) allDeps

  -- Report bach with an error if cascade is not set
  when (indirectDeps /= [] && not (or cascade)) $ reportDepsExt indirectDeps []

  -- Purge all the dependants from state
  mapM_ purgeDep indirectDeps

  -- delete the table and its direct dependencies
  delTableAndDirectDeps qtn

  return successMsg
  where
    isDirectDep = \case
      (SOTableObj dtn _) -> qtn == dtn
      _                  -> False

runUntrackTableQ
  :: (QErrM m, CacheRWM m, MonadTx m, UserInfoM m)
  => UntrackTable -> m EncJSON
runUntrackTableQ q = do
  unTrackExistingTableOrViewP1 q
  unTrackExistingTableOrViewP2 q

handleInconsistentObj
  :: (QErrM m, CacheRWM m)
  => (T.Text -> InconsistentMetadataObj)
  -> m ()
  -> m ()
handleInconsistentObj f action =
  action `catchError` \err -> do
    sc <- askSchemaCache
    let inconsObj = f $ qeError err
        allInconsObjs = inconsObj:scInconsistentObjs sc
    writeSchemaCache $ sc{scInconsistentObjs = allInconsObjs}

checkNewInconsistentMeta
  :: (QErrM m)
  => SchemaCache -- old schema cache
  -> SchemaCache -- new schema cache
  -> m ()
checkNewInconsistentMeta oldSC newSC =
  unless (null newInconsMetaObjects) $ do
    let err = err500 Unexpected
          "cannot continue due to newly found inconsistent metadata"
    throwError err{qeInternal = Just $ toJSON newInconsMetaObjects}
  where
    oldInconsMeta = scInconsistentObjs oldSC
    newInconsMeta = scInconsistentObjs newSC
    newInconsMetaObjects = getDifference _moId newInconsMeta oldInconsMeta

buildSchemaCacheStrict
  :: (MonadTx m, CacheRWM m, MonadIO m, HasHttpManager m, HasSQLGenCtx m)
  => m ()
buildSchemaCacheStrict = do
  buildSchemaCache
  sc <- askSchemaCache
  let inconsObjs = scInconsistentObjs sc
  unless (null inconsObjs) $ do
    let err = err400 Unexpected "cannot continue due to inconsistent metadata"
    throwError err{qeInternal = Just $ toJSON inconsObjs}

buildSchemaCache
  :: (MonadTx m, CacheRWM m, MonadIO m, HasHttpManager m, HasSQLGenCtx m)
  => m ()
buildSchemaCache = buildSchemaCacheG True

buildSCWithoutSetup
  :: (MonadTx m, CacheRWM m, MonadIO m, HasHttpManager m, HasSQLGenCtx m)
  => m ()
buildSCWithoutSetup = buildSchemaCacheG False

buildSchemaCacheG
  :: (MonadTx m, CacheRWM m, MonadIO m, HasHttpManager m, HasSQLGenCtx m)
  => Bool -> m ()
buildSchemaCacheG withSetup = do
  -- clean hdb_views
  when withSetup $ liftTx $ Q.catchE defaultTxErrorHandler clearHdbViews
  -- reset the current schemacache
  writeSchemaCache emptySchemaCache
  sqlGenCtx <- askSQLGenCtx

  -- fetch all catalog metadata
  CatalogMetadata tables relationships permissions
    eventTriggers remoteSchemas functions fkeys' allowlistDefs
    remoteRelationships
    <- liftTx fetchCatalogData

  let fkeys = HS.fromList fkeys'

  -- tables
  forM_ tables $ \ct -> do
    let qt = _ctTable ct
        isSysDef = _ctSystemDefined ct
        tableInfoM = _ctInfo ct
        mkInconsObj = InconsistentMetadataObj (MOTable qt)
                      MOTTable $ toJSON $ TrackTable qt
    modifyErr (\e -> "table " <> qt <<> "; " <> e) $
      handleInconsistentObj mkInconsObj $ do
      ti <- onNothing tableInfoM $ throw400 NotExists $
            "no such table/view exists in postgres : " <>> qt
      addTableToCache $ ti{tiSystemDefined = isSysDef}

  -- relationships
  forM_ relationships $ \(CatalogRelation qt rn rt rDef cmnt) -> do
    let objId = MOTableObj qt $ MTORel rn rt
        def = toJSON $ WithTable qt $ RelDef rn rDef cmnt
        mkInconsObj = InconsistentMetadataObj objId (MOTRel rt) def
    modifyErr (\e -> "table " <> qt <<> "; rel " <> rn <<> "; " <> e) $
      handleInconsistentObj mkInconsObj $
      case rt of
        ObjRel -> do
          using <- decodeValue rDef
          let relDef = RelDef rn using Nothing
          validateObjRel qt relDef
          objRelP2Setup qt fkeys relDef
        ArrRel -> do
          using <- decodeValue rDef
          let relDef = RelDef rn using Nothing
          validateArrRel qt relDef
          arrRelP2Setup qt fkeys relDef

  -- permissions
  forM_ permissions $ \(CatalogPermission qt rn pt pDef cmnt) -> do
    let objId = MOTableObj qt $ MTOPerm rn pt
        def = toJSON $ WithTable qt $ PermDef rn pDef cmnt
        mkInconsObj = InconsistentMetadataObj objId (MOTPerm pt) def
    modifyErr (\e -> "table " <> qt <<> "; role " <> rn <<> "; " <> e) $
      handleInconsistentObj mkInconsObj $
      case pt of
          PTInsert -> permHelper withSetup sqlGenCtx qt rn pDef PAInsert
          PTSelect -> permHelper withSetup sqlGenCtx qt rn pDef PASelect
          PTUpdate -> permHelper withSetup sqlGenCtx qt rn pDef PAUpdate
          PTDelete -> permHelper withSetup sqlGenCtx qt rn pDef PADelete

  -- event triggers
  forM_ eventTriggers $ \(CatalogEventTrigger qt trn configuration) -> do
    let objId = MOTableObj qt $ MTOEventTrigger trn
        def = object ["table" .= qt, "configuration" .= configuration]
        mkInconsObj = InconsistentMetadataObj objId MOTEventTrigger def
    handleInconsistentObj mkInconsObj $ do
      etc <- decodeValue configuration
      subTableP2Setup qt etc
      allCols <- getCols . tiFieldInfoMap <$> askTabInfo qt
      when withSetup $ liftTx $
        mkAllTriggersQ trn qt allCols (stringifyNum sqlGenCtx) (etcDefinition etc)

  -- sql functions
  forM_ functions $ \(CatalogFunction qf rawfiM) -> do
    let def = toJSON $ TrackFunction qf
        mkInconsObj =
          InconsistentMetadataObj (MOFunction qf) MOTFunction def
    modifyErr (\e -> "function " <> qf <<> "; " <> e) $
      handleInconsistentObj mkInconsObj $ do
      rawfi <- onNothing rawfiM $
        throw400 NotExists $ "no such function exists in postgres : " <>> qf
      trackFunctionP2Setup qf rawfi

  -- allow list
  replaceAllowlist $ concatMap _cdQueries allowlistDefs

  -- build GraphQL context with tables and functions
  GS.buildGCtxMapPG

  -- remote schemas
  forM_ remoteSchemas resolveSingleRemoteSchema

  forM_ remoteRelationships setupRemoteRelFromCatalog

  where
    permHelper setup sqlGenCtx qt rn pDef pa = do
      qCtx <- mkAdminQCtx sqlGenCtx <$> askSchemaCache
      perm <- decodeValue pDef
      let permDef = PermDef rn perm Nothing
          createPerm = WithTable qt permDef
      (permInfo, deps) <- liftP1WithQCtx qCtx $ createPermP1 createPerm
      when setup $ addPermP2Setup qt permDef permInfo
      addPermToCache qt rn pa permInfo deps
      -- p2F qt rn p1Res

    resolveSingleRemoteSchema rs = do
      let AddRemoteSchemaQuery name _ _ = rs
          mkInconsObj = InconsistentMetadataObj (MORemoteSchema name)
                        MOTRemoteSchema (toJSON rs)
      handleInconsistentObj mkInconsObj $ do
        rsCtx <- addRemoteSchemaP2Setup rs
        sc <- askSchemaCache
        let gCtxMap = scGCtxMap sc
            defGCtx = scDefaultRemoteGCtx sc
            rGCtx = convRemoteGCtx $ rscGCtx rsCtx
        mergedGCtxMap <- mergeRemoteSchema gCtxMap rGCtx
        mergedDefGCtx <- mergeGCtx defGCtx rGCtx
        writeSchemaCache sc { scGCtxMap = mergedGCtxMap
                            , scDefaultRemoteGCtx = mergedDefGCtx
                            }

    setupRemoteRelFromCatalog remoteRelationship = do
      let objId = MOTableObj qt $ MTORemoteRelationship relName
          def = object ["table" .= qt, "configuration" .= remoteRelationship]
          mkInconsObj = InconsistentMetadataObj
                        objId
                        MOTRemoteRelationship
                        def
      handleInconsistentObj mkInconsObj $ do
        (remoteField, additionalTypesMap) <-
          runCreateRemoteRelationshipP1 remoteRelationship
        runCreateRemoteRelationshipP2Setup remoteField additionalTypesMap
      where
        qt = rtrTable remoteRelationship
        relName = rtrName remoteRelationship

fetchCatalogData :: Q.TxE QErr CatalogMetadata
fetchCatalogData =
  (Q.getAltJ . runIdentity . Q.getRow) <$> Q.withQE defaultTxErrorHandler
    $(Q.sqlFromFile "src-rsr/catalog_metadata.sql") () True

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

instance Q.FromRes RunSQLRes where
  fromRes (Q.ResultOkEmpty _) =
    return $ RunSQLRes "CommandOk" Null
  fromRes (Q.ResultOkData res) = do
    csvRows <- resToCSV res
    return $ RunSQLRes "TuplesOk" $ toJSON csvRows

execRawSQL :: (MonadTx m) => T.Text -> m EncJSON
execRawSQL =
  fmap (encJFromJValue @RunSQLRes) .
  liftTx . Q.multiQE rawSqlErrHandler . Q.fromText
  where
    rawSqlErrHandler txe =
      let e = err400 PostgresError "query execution failed"
      in e {qeInternal = Just $ toJSON txe}

execWithMDCheck
  :: (QErrM m, CacheRWM m, MonadTx m, MonadIO m, HasHttpManager m, HasSQLGenCtx m)
  => RunSQL -> m EncJSON
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
  mapM_ purgeDep indirectDeps

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

  let withReload = do -- in case of any rename
        buildSchemaCache
        newSC <- askSchemaCache
        checkNewInconsistentMeta sc newSC

      withoutReload = do
        postSc <- askSchemaCache
        -- recreate the insert permission infra
        forM_ (M.elems $ scTables postSc) $ \ti -> do
          let tn = tiName ti
          forM_ (M.elems $ tiRolePermInfoMap ti) $ \rpi ->
            maybe (return ()) (liftTx . buildInsInfra tn) $ _permIns rpi

        strfyNum <- stringifyNum <$> askSQLGenCtx
        --recreate triggers
        forM_ (M.elems $ scTables postSc) $ \ti -> do
          let tn = tiName ti
              cols = getCols $ tiFieldInfoMap ti
          forM_ (M.toList $ tiEventTriggerInfoMap ti) $ \(trn, eti) -> do
            let fullspec = etiOpsDef eti
            liftTx $ mkAllTriggersQ trn tn cols strfyNum fullspec

  bool withoutReload withReload reloadRequired

  return res
  where
    reportFuncs = T.intercalate ", " . map dquoteTxt

isAltrDropReplace :: QErrM m => T.Text -> m Bool
isAltrDropReplace = either throwErr return . matchRegex regex False
  where
    throwErr s = throw500 $ "compiling regex failed: " <> T.pack s
    regex = "alter|drop|replace|create function"

runRunSQL
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m, MonadIO m, HasHttpManager m, HasSQLGenCtx m)
  => RunSQL -> m EncJSON
runRunSQL q@(RunSQL t _ mChkMDCnstcy) = do
  adminOnly
  isMDChkNeeded <- maybe (isAltrDropReplace t) return mChkMDCnstcy
  bool (execRawSQL t) (execWithMDCheck q) isMDChkNeeded

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
