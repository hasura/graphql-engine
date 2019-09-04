{-| Top-level functions concerned specifically with operations on the schema cache, such as
rebuilding it from the catalog and incorporating schema changes. See the module documentation for
"Hasura.RQL.DDL.Schema" for more details.

__Note__: this module is __mutually recursive__ with other @Hasura.RQL.DDL.Schema.*@ modules, which
both define pieces of the implementation of building the schema cache and define handlers that
trigger schema cache rebuilds. -}
module Hasura.RQL.DDL.Schema.Cache
  ( CacheBuildM
  , buildSchemaCache
  , buildSchemaCacheFor
  , buildSchemaCacheStrict
  , buildSchemaCacheWithoutSetup

  , withNewInconsistentObjsCheck
  , withMetadataCheck
  , purgeDependentObject

  , withSchemaObject
  , withSchemaObject_
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                as M
import qualified Data.HashSet                       as HS
import qualified Data.Text                          as T
import qualified Database.PG.Query                  as Q

import           Data.Aeson

import qualified Hasura.GraphQL.Schema              as GS

import           Hasura.Db
import           Hasura.GraphQL.RemoteServer
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.DDL.EventTrigger
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.DDL.Permission.Internal
import           Hasura.RQL.DDL.Relationship
import           Hasura.RQL.DDL.RemoteSchema
import           Hasura.RQL.DDL.Schema.Catalog
import           Hasura.RQL.DDL.Schema.Diff
import           Hasura.RQL.DDL.Schema.Function
import           Hasura.RQL.DDL.Schema.Table
import           Hasura.RQL.DDL.Utils
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Catalog
import           Hasura.RQL.Types.QueryCollection
import           Hasura.SQL.Types

type CacheBuildM m = (CacheRWM m, MonadTx m, MonadIO m, HasHttpManager m, HasSQLGenCtx m)

buildSchemaCache :: (CacheBuildM m) => m ()
buildSchemaCache = buildSchemaCacheWithOptions True

buildSchemaCacheWithoutSetup :: (CacheBuildM m) => m ()
buildSchemaCacheWithoutSetup = buildSchemaCacheWithOptions False

buildSchemaCacheWithOptions :: (CacheBuildM m) => Bool -> m ()
buildSchemaCacheWithOptions withSetup = do
  -- clean hdb_views
  when withSetup $ liftTx $ Q.catchE defaultTxErrorHandler clearHdbViews
  -- reset the current schemacache
  writeSchemaCache emptySchemaCache
  sqlGenCtx <- askSQLGenCtx

  -- fetch all catalog metadata
  CatalogMetadata tables relationships permissions
    eventTriggers remoteSchemas functions fkeys' allowlistDefs
    <- liftTx fetchCatalogData

  let fkeys = HS.fromList fkeys'

  -- tables
  modTableCache =<< buildTableCache tables

  -- relationships
  forM_ relationships $ \(CatalogRelation qt rn rt rDef cmnt) -> do
    let objId = MOTableObj qt $ MTORel rn rt
        def = toJSON $ WithTable qt $ RelDef rn rDef cmnt
        mkInconsObj = InconsistentMetadataObj objId (MOTRel rt) def
    modifyErr (\e -> "table " <> qt <<> "; rel " <> rn <<> "; " <> e) $
      withSchemaObject_ mkInconsObj $
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
      withSchemaObject_ mkInconsObj $
      case pt of
          PTInsert -> permHelper withSetup sqlGenCtx qt rn pDef PAInsert
          PTSelect -> permHelper withSetup sqlGenCtx qt rn pDef PASelect
          PTUpdate -> permHelper withSetup sqlGenCtx qt rn pDef PAUpdate
          PTDelete -> permHelper withSetup sqlGenCtx qt rn pDef PADelete

  -- event triggers
  forM_ eventTriggers $ \(CatalogEventTrigger qt trn configuration) -> do
    let objId = MOTableObj qt $ MTOTrigger trn
        def = object ["table" .= qt, "configuration" .= configuration]
        mkInconsObj = InconsistentMetadataObj objId MOTEventTrigger def
    withSchemaObject_ mkInconsObj $ do
      etc <- decodeValue configuration
      subTableP2Setup qt etc
      allCols <- getCols . _tiFieldInfoMap <$> askTabInfo qt
      when withSetup $ liftTx $
        mkAllTriggersQ trn qt allCols (stringifyNum sqlGenCtx) (etcDefinition etc)

  -- sql functions
  forM_ functions $ \(CatalogFunction qf rawfiM) -> do
    let def = toJSON $ TrackFunction qf
        mkInconsObj =
          InconsistentMetadataObj (MOFunction qf) MOTFunction def
    modifyErr (\e -> "function " <> qf <<> "; " <> e) $
      withSchemaObject_ mkInconsObj $ do
      rawfi <- onNothing rawfiM $
        throw400 NotExists $ "no such function exists in postgres : " <>> qf
      trackFunctionP2Setup qf rawfi

  -- allow list
  replaceAllowlist $ concatMap _cdQueries allowlistDefs

  -- build GraphQL context with tables and functions
  GS.buildGCtxMapPG

  -- remote schemas
  forM_ remoteSchemas resolveSingleRemoteSchema

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
      withSchemaObject_ mkInconsObj $ do
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

-- | Rebuilds the schema cache. If an object with the given object id became newly inconsistent,
-- raises an error about it specifically. Otherwise, raises a generic metadata inconsistency error.
buildSchemaCacheFor :: (CacheBuildM m) => MetadataObjId -> m ()
buildSchemaCacheFor objectId = do
  oldSchemaCache <- askSchemaCache
  buildSchemaCache
  newSchemaCache <- askSchemaCache

  let diffInconsistentObjects = getDifference _moId `on` scInconsistentObjs
      newInconsistentObjects = newSchemaCache `diffInconsistentObjects` oldSchemaCache

  for_ (find ((== objectId) . _moId) newInconsistentObjects) $ \matchingObject ->
    throw400 ConstraintViolation (_moReason matchingObject)

  unless (null newInconsistentObjects) $
    throwError (err400 Unexpected "cannot continue due to new inconsistent metadata")
      { qeInternal = Just $ toJSON newInconsistentObjects }

-- | Like 'buildSchemaCache', but fails if there is any inconsistent metadata.
buildSchemaCacheStrict :: (CacheBuildM m) => m ()
buildSchemaCacheStrict = do
  buildSchemaCache
  sc <- askSchemaCache
  let inconsObjs = scInconsistentObjs sc
  unless (null inconsObjs) $ do
    let err = err400 Unexpected "cannot continue due to inconsistent metadata"
    throwError err{qeInternal = Just $ toJSON inconsObjs}

-- | Executes the given action, and if any new 'InconsistentMetadataObj's are added to the schema
-- cache as a result of its execution, raises an error.
withNewInconsistentObjsCheck :: (QErrM m, CacheRM m) => m a -> m a
withNewInconsistentObjsCheck action = do
  originalObjects <- scInconsistentObjs <$> askSchemaCache
  result <- action
  currentObjects <- scInconsistentObjs <$> askSchemaCache
  checkNewInconsistentMeta originalObjects currentObjects
  pure result

-- | @'withMetadataCheck' cascade action@ runs @action@ and checks if the schema changed as a
-- result. If it did, it checks to ensure the changes do not violate any integrity constraints, and
-- if not, incorporates them into the schema cache.
withMetadataCheck :: (CacheBuildM m) => Bool -> m a -> m a
withMetadataCheck cascade action = do
  -- Drop hdb_views so no interference is caused to the sql query
  liftTx $ Q.catchE defaultTxErrorHandler clearHdbViews

  -- Get the metadata before the sql query, everything, need to filter this
  oldMetaU <- liftTx $ Q.catchE defaultTxErrorHandler fetchTableMeta
  oldFuncMetaU <- liftTx $ Q.catchE defaultTxErrorHandler fetchFunctionMeta

  -- Run the action
  res <- action

  -- Get the metadata after the sql query
  newMeta <- liftTx $ Q.catchE defaultTxErrorHandler fetchTableMeta
  newFuncMeta <- liftTx $ Q.catchE defaultTxErrorHandler fetchFunctionMeta
  sc <- askSchemaCache
  let existingInconsistentObjs = scInconsistentObjs sc
      existingTables = M.keys $ scTables sc
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
  when (indirectDeps /= [] && not cascade) $ reportDepsExt indirectDeps []

  -- Purge all the indirect dependents from state
  mapM_ purgeDependentObject indirectDeps

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
        currentInconsistentObjs <- scInconsistentObjs <$> askSchemaCache
        checkNewInconsistentMeta existingInconsistentObjs currentInconsistentObjs

      withoutReload = do
        postSc <- askSchemaCache
        -- recreate the insert permission infra
        forM_ (M.elems $ scTables postSc) $ \ti -> do
          let tn = _tiName ti
          forM_ (M.elems $ _tiRolePermInfoMap ti) $ \rpi ->
            maybe (return ()) (liftTx . buildInsInfra tn) $ _permIns rpi

        strfyNum <- stringifyNum <$> askSQLGenCtx
        --recreate triggers
        forM_ (M.elems $ scTables postSc) $ \ti -> do
          let tn = _tiName ti
              cols = getCols $ _tiFieldInfoMap ti
          forM_ (M.toList $ _tiEventTriggerInfoMap ti) $ \(trn, eti) -> do
            let fullspec = etiOpsDef eti
            liftTx $ mkAllTriggersQ trn tn cols strfyNum fullspec

  bool withoutReload withReload reloadRequired

  return res
  where
    reportFuncs = T.intercalate ", " . map dquoteTxt

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

checkNewInconsistentMeta
  :: (QErrM m)
  => [InconsistentMetadataObj] -> [InconsistentMetadataObj] -> m ()
checkNewInconsistentMeta originalInconsMeta currentInconsMeta =
  unless (null newInconsMetaObjects) $
    throwError (err500 Unexpected "cannot continue due to newly found inconsistent metadata")
      { qeInternal = Just $ toJSON newInconsMetaObjects }
  where
    newInconsMetaObjects = getDifference _moId currentInconsMeta originalInconsMeta

purgeDependentObject :: (CacheRWM m, MonadTx m) => SchemaObjId -> m ()
purgeDependentObject schemaObjId = case schemaObjId of
  (SOTableObj tn (TOPerm rn pt)) -> do
    liftTx $ dropPermFromCatalog tn rn pt
    withPermType pt delPermFromCache rn tn

  (SOTableObj qt (TORel rn)) -> do
    liftTx $ delRelFromCatalog qt rn
    delRelFromCache rn qt

  (SOFunction qf) -> do
    liftTx $ delFunctionFromCatalog qf
    delFunctionFromCache qf

  (SOTableObj qt (TOTrigger trn)) -> do
    liftTx $ delEventTriggerFromCatalog trn
    delEventTriggerFromCache qt trn

  _ -> throw500 $
    "unexpected dependent object : " <> reportSchemaObj schemaObjId

-- | @'withSchemaObject' f action@ runs @action@, and if it raises any errors, applies @f@ to the
-- error message to produce an 'InconsistentMetadataObj', then adds the object to the schema cache
-- and returns 'Nothing' instead of aborting.
withSchemaObject :: (QErrM m, CacheRWM m) => (Text -> InconsistentMetadataObj) -> m a -> m (Maybe a)
withSchemaObject f action =
  (Just <$> action) `catchError` \err -> do
    sc <- askSchemaCache
    let inconsObj = f $ qeError err
        allInconsObjs = inconsObj:scInconsistentObjs sc
    writeSchemaCache sc { scInconsistentObjs = allInconsObjs }
    pure Nothing

withSchemaObject_ :: (QErrM m, CacheRWM m) => (Text -> InconsistentMetadataObj) -> m () -> m ()
withSchemaObject_ f = void . withSchemaObject f
