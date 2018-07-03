{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Hasura.RQL.DDL.Schema.Table where

import           Hasura.Prelude
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.DDL.Permission.Internal
import           Hasura.RQL.DDL.QueryTemplate
import           Hasura.RQL.DDL.Relationship
import           Hasura.RQL.DDL.Schema.Diff
import           Hasura.RQL.DDL.Utils
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Database.PG.Query                  as Q

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Instances.TH.Lift                  ()
import           Language.Haskell.TH.Syntax         (Lift)

import qualified Data.HashMap.Strict                as M
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as TE
import qualified Database.PostgreSQL.LibPQ          as PQ

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

updateTableInCatalog :: QualifiedTable -> QualifiedTable -> Q.Tx ()
updateTableInCatalog oldTable newTable =
  Q.unitQ [Q.sql|
           UPDATE "hdb_catalog"."hdb_table"
              SET table_schema = $1, table_name = $2
            WHERE table_schema = $3 AND table_name = $4
                |] (nsn, ntn, osn, otn) False
  where
    QualifiedTable osn otn = oldTable
    QualifiedTable nsn ntn = newTable

-- Build the TableInfo with all its columns
getTableInfo :: QualifiedTable -> Bool -> Q.TxE QErr TableInfo
getTableInfo qt@(QualifiedTable sn tn) isSystemDefined = do
  tableExists <- Q.catchE defaultTxErrorHandler $ Q.listQ [Q.sql|
            SELECT true from information_schema.tables
             WHERE table_schema = $1
               AND table_name = $2;
                           |] (sn, tn) False

  -- if no columns are found, there exists no such view/table
  unless (tableExists == [Identity True]) $
    throw400 NotExists $ "no such table/view exists in postgres : " <>> qt

  -- Fetch the column details
  colData <- Q.catchE defaultTxErrorHandler $ Q.listQ [Q.sql|
            SELECT column_name, to_json(udt_name)
              FROM information_schema.columns
             WHERE table_schema = $1
               AND table_name = $2
                           |] (sn, tn) False
  return $ mkTableInfo qt isSystemDefined $ map (fmap Q.getAltJ) colData

newtype TrackTable
  = TrackTable
  { tName :: QualifiedTable }
  deriving (Show, Eq, FromJSON, ToJSON, Lift)

trackExistingTableOrViewP1 :: TrackTable -> P1 ()
trackExistingTableOrViewP1 (TrackTable vn) = do
  adminOnly
  rawSchemaCache <- getSchemaCache <$> lift ask
  when (M.member vn $ scTables rawSchemaCache) $
    throw400 AlreadyTracked $ "view/table already tracked : " <>> vn

trackExistingTableOrViewP2Setup :: (P2C m) => QualifiedTable -> Bool -> m ()
trackExistingTableOrViewP2Setup tn isSystemDefined = do
  ti <- liftTx $ getTableInfo tn isSystemDefined
  addTableToCache ti

trackExistingTableOrViewP2
  :: (P2C m) => QualifiedTable -> Bool -> m RespBody
trackExistingTableOrViewP2 vn isSystemDefined = do
  trackExistingTableOrViewP2Setup vn isSystemDefined
  liftTx $ Q.catchE defaultTxErrorHandler $
    saveTableToCatalog vn
  return successMsg

instance HDBQuery TrackTable where

  type Phase1Res TrackTable = ()
  phaseOne = trackExistingTableOrViewP1

  phaseTwo (TrackTable tn) _ = trackExistingTableOrViewP2 tn False

  schemaCachePolicy = SCPReload

purgeDep :: (CacheRWM m, MonadError QErr m, MonadTx m)
         => SchemaObjId -> m ()
purgeDep schemaObjId = case schemaObjId of
  (SOTableObj tn (TOPerm rn pt)) -> do
    liftTx $ dropPermFromCatalog tn rn pt
    withPermType pt delPermFromCache rn tn

  (SOTableObj qt (TORel rn))     -> do
    liftTx $ delRelFromCatalog qt rn
    delFldFromCache (fromRel rn) qt

  (SOQTemplate qtn)              -> do
    liftTx $ delQTemplateFromCatalog qtn
    delQTemplateFromCache qtn

  _                              -> throw500 $
    "unexpected dependent object : " <> reportSchemaObj schemaObjId
updateObjRelDef :: (P2C m) => QualifiedTable
                -> QualifiedTable -> RelName -> m ()
updateObjRelDef newQT qt rn = do
  oldDefV <- liftTx $ getRelDef qt rn
  oldDef :: ObjRelUsing <- decodeValue oldDefV
  case oldDef of
    RUFKeyOn _ -> return ()
    RUManual (ObjRelManualConfig (RelManualConfig _ rmCols)) -> do
      let newDef = mkObjRelUsing rmCols
      liftTx $ updateRel qt rn $ toJSON (newDef :: ObjRelUsing)
  where
    mkObjRelUsing colMap = RUManual $ ObjRelManualConfig $
      RelManualConfig newQT colMap

updateArrRelDef :: (P2C m) => QualifiedTable
                -> QualifiedTable -> RelName -> m ()
updateArrRelDef newQT qt rn = do
  oldDefV <- liftTx $ getRelDef qt rn
  oldDef  <- decodeValue oldDefV
  liftTx $ updateRel qt rn $ toJSON $ mkNewArrRelUsing oldDef
  where
    mkNewArrRelUsing arrRelUsing = case arrRelUsing of
      RUFKeyOn (ArrRelUsingFKeyOn _ c) ->
        RUFKeyOn $ ArrRelUsingFKeyOn newQT c
      RUManual (ArrRelManualConfig (RelManualConfig _ rmCols)) ->
        RUManual $ ArrRelManualConfig $ RelManualConfig newQT rmCols

getRelDef :: QualifiedTable -> RelName -> Q.TxE QErr Value
getRelDef (QualifiedTable sn tn) rn =
  Q.getAltJ . runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler
    [Q.sql|
     SELECT rel_def::json FROM hdb_catalog.hdb_relationship
      WHERE table_schema = $1 AND table_name = $2
        AND rel_name = $3
    |] (sn, tn, rn) True

updateRelDefs
  :: (P2C m)
  => QualifiedTable
  -> QualifiedTable
  -> (QualifiedTable, [RelInfo])
  -> m ()
updateRelDefs newQT oldQT (qt, rels) =
  forM_ rels $ \rel -> when (oldQT == riRTable rel) $
    case riType rel of
      ObjRel -> updateObjRelDef newQT qt $ riName rel
      ArrRel -> updateArrRelDef newQT qt $ riName rel

updateCols :: PGCol -> PGCol -> PermColSpec -> (PermColSpec, Bool)
updateCols oCol nCol cols = case cols of
  PCStar -> (cols, False)
  PCCols c -> ( PCCols $ flip map c $ \col -> if col == oCol then nCol else col
              , any (== oCol) c
              )

updateBoolExp :: PGCol -> PGCol -> BoolExp -> (BoolExp, Bool)
updateBoolExp oCol nCol boolExp = case boolExp of
  BoolAnd exps -> ( BoolAnd $ fst $ updateExps exps
                  , or $ snd $ updateExps exps
                  )
  BoolOr exps -> ( BoolOr $ fst $ updateExps exps
                 , or $ snd $ updateExps exps
                 )
  be@(BoolCol (ColExp c v)) -> if oCol == PGCol (getFieldNameTxt c)
                               then ( BoolCol $ ColExp (fromPGCol nCol) v
                                    , True
                                    )
                               else (be, False)
  BoolNot be -> let updatedExp = updateBoolExp oCol nCol be
                in ( BoolNot $ fst updatedExp
                   , snd updatedExp
                   )
  where
    updateExps exps = unzip $ flip map exps $ updateBoolExp oCol nCol

updateInsPermCols :: (P2C m) => PGCol -> PGCol -> CreateInsPerm -> m ()
updateInsPermCols oCol nCol (WithTable qt (PermDef rn (InsPerm chk _) _)) = do
  let updatedBoolExp = updateBoolExp oCol nCol chk
  if snd updatedBoolExp
  then liftTx $ updatePermInCatalog PTInsert qt $
    PermDef rn (InsPerm (fst updatedBoolExp) Nothing) Nothing
  else return ()

updateSelPermCols :: (P2C m) => PGCol -> PGCol -> CreateSelPerm -> m ()
updateSelPermCols oCol nCol (WithTable qt (PermDef rn (SelPerm cols fltr) _)) = do
  if or [updNeededFromCols, updNeededFromBoolExp] then
    liftTx $ updatePermInCatalog PTSelect qt $
      PermDef rn (SelPerm updCols updBoolExp) Nothing
  else return ()
  where
    (updCols, updNeededFromCols) = updateCols oCol nCol cols
    (updBoolExp, updNeededFromBoolExp) = updateBoolExp oCol nCol fltr

updateUpdPermCols :: (P2C m) => PGCol -> PGCol -> CreateUpdPerm -> m ()
updateUpdPermCols oCol nCol (WithTable qt (PermDef rn (UpdPerm cols fltr) _)) = do
  if or [updNeededFromCols, updNeededFromBoolExp] then
    liftTx $ updatePermInCatalog PTUpdate qt $
      PermDef rn (UpdPerm updCols updBoolExp) Nothing
  else return ()
  where
    (updCols, updNeededFromCols) = updateCols oCol nCol cols
    (updBoolExp, updNeededFromBoolExp) = updateBoolExp oCol nCol fltr

updateDelPermCols :: (P2C m) => PGCol -> PGCol -> CreateDelPerm -> m ()
updateDelPermCols oCol nCol (WithTable qt (PermDef rn (DelPerm fltr)_)) = do
  let updatedFltrExp = updateBoolExp oCol nCol fltr
  if snd updatedFltrExp then
    liftTx $ updatePermInCatalog PTDelete qt $
      PermDef rn (DelPerm $ fst updatedFltrExp) Nothing
  else return ()

updatePermCols :: (P2C m) => PGCol -> PGCol -> QualifiedTable -> m ()
updatePermCols oCol nCol qt@(QualifiedTable sn tn) = do
  perms <- liftTx fetchPerms
  forM_ perms $ \(rn, ty, Q.AltJ (pDef :: Value)) -> do
    case ty of
      PTInsert -> do
        perm <- decodeValue pDef
        updateInsPermCols oCol nCol $
          WithTable qt $ PermDef rn perm Nothing
      PTSelect -> do
        perm <- decodeValue pDef
        updateSelPermCols oCol nCol $
          WithTable qt $ PermDef rn perm Nothing
      PTUpdate -> do
        perm <- decodeValue pDef
        updateUpdPermCols oCol nCol $
          WithTable qt $ PermDef rn perm Nothing
      PTDelete -> do
        perm <- decodeValue pDef
        updateDelPermCols oCol nCol $
          WithTable qt $ PermDef rn perm Nothing
  where
    fetchPerms = Q.listQE defaultTxErrorHandler [Q.sql|
                  SELECT role_name, perm_type, perm_def::json
                    FROM hdb_catalog.hdb_permission
                   WHERE table_schema = $1
                     AND table_name = $2
                 |] (sn, tn) True

processTableChanges :: (P2C m) => TableInfo -> TableDiff -> m ()
processTableChanges ti tableDiff = do
  sc <- askSchemaCache

  case mNewName of
    Just newqt -> do
      let allRels = getAllRelations $ scTables sc
      -- Update depended relations on this table with new name
      forM_ allRels $ \rel -> updateRelDefs newqt tn rel
      -- Update table name in hdb_catalog
      liftTx $ Q.catchE defaultTxErrorHandler $
        updateTableInCatalog tn newqt
    Nothing -> return ()  -- when (isJust mNewName) $
  --   throw400 NotSupported $ "table renames are not yet supported : " <>> tn

  -- for all the dropped columns
  forM_ droppedCols $ \droppedCol ->
    -- Drop the column from the cache
    delFldFromCache (fromPGCol droppedCol) tn

  -- In the newly added columns check that there is no conflict with relationships
  forM_ addedCols $ \colInfo@(PGColInfo colName _) ->
    case M.lookup (fromPGCol colName) $ tiFieldInfoMap ti of
      Just (FIRelationship _) ->
        throw400 AlreadyExists $ "cannot add column " <> colName
        <<> " in table " <> tn <<>
        " as a relationship with the name already exists"
      _ -> addFldToCache (fromPGCol colName) (FIColumn colInfo) tn

  -- for rest of the columns
  forM_ alteredCols $ \(PGColInfo oColName oColTy, (PGColInfo nColName nColTy)) ->
    if | oColName /= nColName -> do
           -- Update cols in permissions
           updatePermCols oColName nColName tn
           -- Update cols in relations
           -- throw400 NotSupported $ "column renames are not yet supported : "
           --   <> tn <<> "." <>> oColName
       | oColTy /= nColTy -> do
           let colId   = SOTableObj tn $ TOCol oColName
               depObjs = getDependentObjsWith (== "on_type") sc colId
           unless (null depObjs) $ throw400 DependencyError $ "cannot change type of column " <> oColName <<> " in table "
                  <> tn <<> " because of the following dependencies : " <>
                  reportSchemaObjs depObjs
       | otherwise -> return ()
  where
    tn = tiName ti
    TableDiff mNewName droppedCols addedCols alteredCols _ = tableDiff

processSchemaChanges :: (P2C m) => SchemaDiff -> m ()
processSchemaChanges schemaDiff = do
  -- Purge the dropped tables
  forM_ droppedTables $ \qtn@(QualifiedTable sn tn) -> do
    liftTx $ Q.catchE defaultTxErrorHandler $ do
      Q.unitQ [Q.sql|
               DELETE FROM "hdb_catalog"."hdb_relationship"
               WHERE table_schema = $1 AND table_name = $2
                |] (sn, tn) False
      Q.unitQ [Q.sql|
               DELETE FROM "hdb_catalog"."hdb_permission"
               WHERE table_schema = $1 AND table_name = $2
                |] (sn, tn) False
      delTableFromCatalog qtn
    delTableFromCache qtn
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

unTrackExistingTableOrViewP1 :: UntrackTable -> P1 (UntrackTable, TableInfo)
unTrackExistingTableOrViewP1 ut@(UntrackTable vn _) = do
  adminOnly
  rawSchemaCache <- getSchemaCache <$> lift ask
  case M.lookup vn (scTables rawSchemaCache) of
    Just ti -> do
      -- Check if table/view is system defined
      when (tiSystemDefined ti) $ throw400 NotSupported $
        vn <<> " is system defined, cannot untrack"
      return (ut, ti)
    Nothing -> throw400 AlreadyUntracked $
      "view/table already untracked : " <>> vn

unTrackExistingTableOrViewP2 :: (P2C m)
                             => UntrackTable -> TableInfo -> m RespBody
unTrackExistingTableOrViewP2 (UntrackTable vn cascade) tableInfo = do
  sc <- askSchemaCache

  -- Get Foreign key constraints to this table
  fKeyTables <- liftTx getFKeyTables
  let fKeyDepIds = mkFKeyObjIds $ filterTables fKeyTables $ scTables sc

  -- Report back with an error if any fkey object ids are present
  when (fKeyDepIds /= []) $ reportDepsExt fKeyDepIds []

  -- Get relational and query template dependants
  let allRels = getAllRelations $ scTables sc
      directRelDep = (vn, getRels $ tiFieldInfoMap tableInfo)
      relDeps = directRelDep : foldl go [] allRels
      relDepIds = concatMap mkObjIdFromRel relDeps
      queryTDepIds = getDependentObjsOfQTemplateCache (SOTable vn)
                     (scQTemplates sc)
      allDepIds = relDepIds <> queryTDepIds

  -- Report bach with an error if cascade is not set
  when (allDepIds /= [] && not (or cascade)) $ reportDepsExt allDepIds []

  -- Purge all the dependants from state
  mapM_ purgeDep allDepIds

  -- update the schema cache with the changes
  processSchemaChanges $ SchemaDiff [vn] []

  return successMsg
  where
    QualifiedTable sn tn = vn
    getFKeyTables = Q.catchE defaultTxErrorHandler $ Q.listQ [Q.sql|
                    SELECT constraint_name,
                           table_schema,
                           table_name
                     FROM  hdb_catalog.hdb_foreign_key_constraint
                     WHERE ref_table_table_schema = $1
                       AND ref_table = $2
                   |] (sn, tn) False
    filterTables tables tc = flip filter tables $ \(_, s, t) ->
      isJust $ M.lookup (QualifiedTable s t) tc

    mkFKeyObjIds tables = flip map tables $ \(cn, s, t) ->
                     SOTableObj (QualifiedTable s t) (TOCons cn)

    go l (qt, ris) = if any isDep ris
                     then (qt, filter isDep ris):l
                     else l
    isDep relInfo = vn == riRTable relInfo
    mkObjIdFromRel (qt, ris) = flip map ris $ \ri ->
      SOTableObj qt (TORel $ riName ri)

instance HDBQuery UntrackTable where
  type Phase1Res UntrackTable = (UntrackTable, TableInfo)
  phaseOne = unTrackExistingTableOrViewP1

  phaseTwo _ = uncurry unTrackExistingTableOrViewP2

  schemaCachePolicy = SCPReload

buildSchemaCache :: Q.TxE QErr SchemaCache
buildSchemaCache = flip execStateT emptySchemaCache $ do
  tables <- lift $ Q.catchE defaultTxErrorHandler fetchTables
  forM_ tables $ \(sn, tn, isSystemDefined) ->
    modifyErr (\e -> "table " <> tn <<> "; " <> e) $
    trackExistingTableOrViewP2Setup (QualifiedTable sn tn) isSystemDefined

  -- Fetch all the relationships
  relationships <- lift $ Q.catchE defaultTxErrorHandler fetchRelationships

  forM_ relationships $ \(sn, tn, rn, rt, Q.AltJ rDef) ->
    modifyErr (\e -> "table " <> tn <<> "; rel " <> rn <<> "; " <> e) $ case rt of
    ObjRel -> do
      using <- decodeValue rDef
      objRelP2Setup (QualifiedTable sn tn) $ RelDef rn using Nothing
    ArrRel -> do
      using <- decodeValue rDef
      arrRelP2Setup (QualifiedTable sn tn) $ RelDef rn using Nothing

  -- Fetch all the permissions
  permissions <- lift $ Q.catchE defaultTxErrorHandler fetchPermissions

  forM_ permissions $ \(sn, tn, rn, pt, Q.AltJ pDef) ->
    modifyErr (\e -> "table " <> tn <<> "; role " <> rn <<> "; " <> e) $ case pt of
    PTInsert -> permHelper sn tn rn pDef PAInsert
    PTSelect -> permHelper sn tn rn pDef PASelect
    PTUpdate -> permHelper sn tn rn pDef PAUpdate
    PTDelete -> permHelper sn tn rn pDef PADelete

  -- Fetch all the query templates
  qtemplates <- lift $ Q.catchE defaultTxErrorHandler fetchQTemplates
  forM_ qtemplates $ \(qtn, Q.AltJ qtDefVal) -> do
    qtDef <- decodeValue qtDefVal
    qCtx <- mkAdminQCtx <$> get
    qti <- liftP1 qCtx $ createQueryTemplateP1 $
           CreateQueryTemplate qtn qtDef Nothing
    addQTemplateToCache qti
  where
    permHelper sn tn rn pDef pa = do
      qCtx <- mkAdminQCtx <$> get
      perm <- decodeValue pDef
      let qt = QualifiedTable sn tn
          permDef = PermDef rn perm Nothing
          createPerm = WithTable qt permDef
      p1Res <- liftP1 qCtx $ phaseOne createPerm
      addPermP2Setup qt permDef p1Res
      addPermToCache qt rn pa p1Res
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

reloadSchemaCache :: (CacheRWM m, MonadTx m) => m ()
reloadSchemaCache = do
  sc <- liftTx buildSchemaCache
  writeSchemaCache sc

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

runSqlP2 :: (P2C m) => RunSQL -> m RespBody
runSqlP2 (RunSQL t cascade) = do

  -- Drop hdb_views so no interference is caused to the sql query
  liftTx $ Q.catchE defaultTxErrorHandler $
    Q.unitQ clearHdbViews () False

  -- Get the metadata before the sql query, everything, need to filter this
  oldMetaU <- liftTx $ Q.catchE defaultTxErrorHandler fetchTableMeta

  -- Run the SQL
  res <- liftTx $ Q.multiQE rawSqlErrHandler $ Q.fromBuilder $ TE.encodeUtf8Builder t

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

  -- Reload SchemaCache
  reloadSchemaCache

  -- -- recreate the insert permission infra
  -- forM_ (M.elems $ scTables postSc) $ \ti -> do
  --   let tn = tiName ti
  --   forM_ (M.elems $ tiRolePermInfoMap ti) $ \rpi ->
  --     maybe (return ()) (liftTx . buildInsInfra tn) $ _permIns rpi

  return $ encode (res :: RunSQLRes)

  where
    rawSqlErrHandler :: Q.PGTxErr -> QErr
    rawSqlErrHandler txe =
      let e = err400 PostgresError "query execution failed"
      in e {qeInternal = Just $ toJSON txe}

instance HDBQuery RunSQL where

  type Phase1Res RunSQL = ()
  phaseOne _ = adminOnly

  phaseTwo q _ = runSqlP2 q

  schemaCachePolicy = SCPReload

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
