{-# LANGUAGE RecordWildCards #-}

module Hasura.RQL.DDL.RemoteSchema
  ( runAddRemoteSchema
  , runRemoveRemoteSchema
  , removeRemoteSchemaFromCatalog
  , runReloadRemoteSchema
  , buildGCtxMap
  , mkCacheForRemoteSchema
  , fetchRemoteSchemas
  , addRemoteSchemaP1
  , addRemoteSchemaP2Setup
  , addRemoteSchemaP2
  , runAddRemoteSchemaPermissions
  , runAddRemoteSchemaPermissionsP1
  , runAddRemoteSchemaPermissionsP2Setup
  , addRemoteSchemaPermissionsToCatalog
  , runDropRemoteSchemaPermissions
  , fetchRemoteSchemaPerms
  , dropRemoteSchemaPermissionsFromCatalog
  ) where

import           Hasura.EncJSON
import           Hasura.Prelude

import qualified Data.Aeson                    as J
import qualified Data.HashMap.Strict           as Map
import qualified Data.List                     as List
import qualified Data.Text                     as T
import qualified Database.PG.Query             as Q
import qualified Hasura.GraphQL.Validate.Types as VT

import           Control.Monad.Validate
import           Hasura.GraphQL.RemoteServer
import           Hasura.GraphQL.Utils
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Hasura.GraphQL.Context        as GC
import qualified Hasura.GraphQL.Schema         as GS

runAddRemoteSchema
  :: ( QErrM m, UserInfoM m
     , CacheRWM m, MonadTx m
     , MonadIO m, HasHttpManager m
     , HasFeatureFlags m
     )
  => AddRemoteSchemaQuery -> m EncJSON
runAddRemoteSchema q = do
  addRemoteSchemaP1 name >> addRemoteSchemaP2 q
  where
    name = _arsqName q

addRemoteSchemaP1
  :: (QErrM m, UserInfoM m, CacheRM m)
  => RemoteSchemaName -> m ()
addRemoteSchemaP1 name = do
  adminOnly
  remoteSchemaMap <- scRemoteSchemas <$> askSchemaCache
  onJust (Map.lookup name remoteSchemaMap) $ const $
    throw400 AlreadyExists $ "remote schema with name "
    <> name <<> " already exists"

addRemoteSchemaP2Setup ::
     (QErrM m, CacheRWM m, MonadIO m, HasHttpManager m, HasFeatureFlags m)
  => AddRemoteSchemaQuery
  -> m RemoteSchemaCtx
addRemoteSchemaP2Setup q = do
  httpMgr <- askHttpManager
  remotePermsEnabled <- ffRemoteSchemaPermissions <$> askFeatureFlags
  let remotePerms = if remotePermsEnabled then Just Map.empty else Nothing
  rsi <- validateRemoteSchemaDef def
  gCtx <- fetchRemoteSchema httpMgr name rsi
  let rsCtx = RemoteSchemaCtx name gCtx rsi remotePerms
  addRemoteSchemaToCache rsCtx
  return rsCtx
  where
    AddRemoteSchemaQuery name def _ = q

addRemoteSchemaP2
  :: ( QErrM m
     , CacheRWM m
     , MonadTx m
     , MonadIO m
     , HasHttpManager m
     , HasFeatureFlags m
     )
  => AddRemoteSchemaQuery
  -> m EncJSON
addRemoteSchemaP2 q = do
  void $ addRemoteSchemaP2Setup q
  liftTx $ addRemoteSchemaToCatalog q
  return successMsg

runRemoveRemoteSchema
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m)
  => RemoteSchemaNameQuery -> m EncJSON
runRemoveRemoteSchema (RemoteSchemaNameQuery rsn)= do
  removeRemoteSchemaP1 rsn
  removeRemoteSchemaP2 rsn

removeRemoteSchemaP1
  :: (UserInfoM m, QErrM m, CacheRM m)
  => RemoteSchemaName -> m ()
removeRemoteSchemaP1 rsn = do
  adminOnly
  sc <- askSchemaCache
  let rmSchemas = scRemoteSchemas sc
  void $ onNothing (Map.lookup rsn rmSchemas) $
    throw400 NotExists "no such remote schema"

removeRemoteSchemaP2
  :: ( CacheRWM m
     , MonadTx m
     )
  => RemoteSchemaName
  -> m EncJSON
removeRemoteSchemaP2 rsn = do
  dropRemoteSchemaDirectDeps rsn
  delRemoteSchemaFromCache rsn
  liftTx $ removeRemoteSchemaFromCatalog rsn
  return successMsg

dropRemoteSchemaDirectDeps
  :: ( CacheRWM m
     , MonadTx m
     )
  => RemoteSchemaName
  -> m ()
dropRemoteSchemaDirectDeps rsn = do
  sc <- askSchemaCache
  let depObjs = getDependentObjs sc objId
  mapM_ dropRemoteSchemaPermDep depObjs
  where
    objId = SORemoteSchema rsn

dropRemoteSchemaPermDep
  :: ( MonadTx m )
  => SchemaObjId
  -> m ()
dropRemoteSchemaPermDep = \case
  SORemoteSchemaObj rsn (RSOPerm role) -> dropRemoteSchemaPermissionsFromCatalog rsn role
  _ -> throw500 "unexpected dependency found while dropping remote schema"

runReloadRemoteSchema
  :: ( QErrM m, UserInfoM m , CacheRWM m
     , MonadIO m, HasHttpManager m, HasFeatureFlags m
     )
  => RemoteSchemaNameQuery -> m EncJSON
runReloadRemoteSchema (RemoteSchemaNameQuery name) = do
  adminOnly
  rmSchemas <- scRemoteSchemas <$> askSchemaCache
  remotePermsEnabled <- ffRemoteSchemaPermissions <$> askFeatureFlags
  let remotePerms = if remotePermsEnabled then Just Map.empty else Nothing
  rsi <- fmap rscInfo $ onNothing (Map.lookup name rmSchemas) $
         throw400 NotExists $ "remote schema with name "
         <> name <<> " does not exist"
  httpMgr <- askHttpManager
  gCtx <- fetchRemoteSchema httpMgr name rsi
  delRemoteSchemaFromCache name
  addRemoteSchemaToCache $ RemoteSchemaCtx name gCtx rsi remotePerms
  return successMsg

-- | build GraphQL schema
buildGCtxMap
  :: (QErrM m, CacheRWM m) => m ()
buildGCtxMap = do
  -- build GraphQL Context with Hasura schema
  GS.buildGCtxMapPG
  sc <- askSchemaCache
  let gCtxMap = scGCtxMap sc
  -- add remote schemas
  mergedGCtxMap <- mergeRemoteSchemas (scRemoteSchemas sc) gCtxMap
  allRemotesGCtx <- mkAllRemoteGCtx (scRemoteSchemas sc)
  writeSchemaCache sc { scGCtxMap = mergedGCtxMap
                      , scAllRemoteGCtx = allRemotesGCtx
                      }

mkCacheForRemoteSchema
  :: (QErrM m) => RemoteSchemaMap -> RemoteSchemaName -> m SchemaCache
mkCacheForRemoteSchema rsMap rsName = do
  let newRSMap =
        maybe Map.empty (Map.singleton rsName) $
        Map.lookup rsName rsMap
  mergedGCtxMap <-
    mergeRemoteSchemas newRSMap initGCtxMap
  pure emptySchemaCache
    { scRemoteSchemas = newRSMap
    , scGCtxMap = mergedGCtxMap
    }
  where
    initGCtxMap = Map.singleton adminRole GC.emptyGCtx

addRemoteSchemaToCatalog
  :: AddRemoteSchemaQuery
  -> Q.TxE QErr ()
addRemoteSchemaToCatalog (AddRemoteSchemaQuery name def comment) =
  Q.unitQE defaultTxErrorHandler [Q.sql|
    INSERT into hdb_catalog.remote_schemas
      (name, definition, comment)
      VALUES ($1, $2, $3)
  |] (name, Q.AltJ $ J.toJSON def, comment) True

removeRemoteSchemaFromCatalog :: RemoteSchemaName -> Q.TxE QErr ()
removeRemoteSchemaFromCatalog name =
  Q.unitQE defaultTxErrorHandler [Q.sql|
    DELETE FROM hdb_catalog.remote_schemas
      WHERE name = $1
  |] (Identity name) True


fetchRemoteSchemas :: Q.TxE QErr [AddRemoteSchemaQuery]
fetchRemoteSchemas =
  map fromRow <$> Q.listQE defaultTxErrorHandler
    [Q.sql|
     SELECT name, definition, comment
       FROM hdb_catalog.remote_schemas
     |] () True
  where
    fromRow (n, Q.AltJ def, comm) = AddRemoteSchemaQuery n def comm

withRSPermFlagCheck :: (QErrM m, HasFeatureFlags m) => Bool -> m () -> m ()
withRSPermFlagCheck strict action = do
   featureFlags <- askFeatureFlags
   if (ffRemoteSchemaPermissions featureFlags)
     then
     action
     else
     when strict $ throw404 "remote schema permissions are not enabled"

runAddRemoteSchemaPermissions ::
     (QErrM m, UserInfoM m, CacheRWM m, MonadTx m, HasFeatureFlags m)
  => RemoteSchemaPermissions
  -> m EncJSON
runAddRemoteSchemaPermissions q = withRSPermFlagCheck True (do
  adminOnly
  newRGCtx <- runAddRemoteSchemaPermissionsP1 q
  runAddRemoteSchemaPermissionsP2 q newRGCtx) >>
  pure successMsg

runAddRemoteSchemaPermissionsP1
  :: (QErrM m, CacheRM m) => RemoteSchemaPermissions -> m GC.RemoteGCtx
runAddRemoteSchemaPermissionsP1 remoteSchemaPermission = do
  validateRemoteSchemaPermissions remoteSchemaPermission

runAddRemoteSchemaPermissionsP2 ::
     (QErrM m, CacheRWM m, MonadTx m, HasFeatureFlags m)
  => RemoteSchemaPermissions
  -> GC.RemoteGCtx
  -> m ()
runAddRemoteSchemaPermissionsP2 rsPerm rGCtx = withRSPermFlagCheck False $ do
  runAddRemoteSchemaPermissionsP2Setup rsPerm rGCtx
  liftTx $ addRemoteSchemaPermissionsToCatalog rsPerm

runAddRemoteSchemaPermissionsP2Setup ::
     (QErrM m, CacheRWM m, HasFeatureFlags m)
  => RemoteSchemaPermissions
  -> GC.RemoteGCtx
  -> m ()
runAddRemoteSchemaPermissionsP2Setup (RemoteSchemaPermissions rsName role _def) rGCtx =
  withRSPermFlagCheck False $ do
    sc <- askSchemaCache
    let remoteSchemas = scRemoteSchemas sc
    rsCtx <-
      onNothing
        (Map.lookup rsName remoteSchemas)
        (throw400
           NotFound
           ("remote schema " <> remoteSchemaNameToText rsName <> " not found"))
    newPerms <-
      case rscPerms rsCtx of
        Nothing      -> throw500 ("remote schema permissions are disabled")
        Just roleMap -> pure $ Map.insert role rGCtx roleMap
    let newRSCtx = rsCtx {rscPerms = pure newPerms}
    writeSchemaCache
      sc {scRemoteSchemas = Map.insert rsName newRSCtx remoteSchemas}
    modDepMapInCache (addToDepMap schObjId deps)
  where
    schObjId = SORemoteSchemaObj rsName (RSOPerm role)
    deps = [SchemaDependency (SORemoteSchema rsName) DRParent]

validateRemoteSchemaPermissions :: (QErrM m, CacheRM m) => RemoteSchemaPermissions -> m GC.RemoteGCtx
validateRemoteSchemaPermissions remoteSchemaPerm = do
  sc <- askSchemaCache
  case Map.lookup (rsPermRemoteSchema remoteSchemaPerm) (scRemoteSchemas sc) of
    Nothing -> throw400 RemoteSchemaError "No such remote schema"
    Just remoteSchemaCtx -> do
      newTypes <-
        do assertUniqueTypes
             (rsPermDefinition remoteSchemaPerm)
           validateTypes
             (rsPermDefinition remoteSchemaPerm)
             (GC._rgTypes $ rscGCtx remoteSchemaCtx)
      updateRemoteGCtxFromTypes newTypes (rscGCtx remoteSchemaCtx)
  where
    assertUniqueTypes RemoteSchemaPermDef {..} =
      checkDuplicateTypes rspdAllowedObjects >>
      checkDuplicateTypes rspdAllowedInputObjects

checkDuplicateTypes :: (QErrM m) => [RemoteAllowedFields] -> m ()
checkDuplicateTypes typePerms = do
  let types = map rafType typePerms
      nubbedTypes = List.nub types
  if types /= nubbedTypes
    then throw400 InvalidParams $
         "duplicate types found in permission specification: " <>
         (T.pack . show) (types \\ nubbedTypes)
    else pure ()

-- only TIObj and TIInpObj can be permissioned, other types pass-thru
validateTypes :: (QErrM m) => RemoteSchemaPermDef -> VT.TypeMap -> m VT.TypeMap
validateTypes permDef allTypes = do
  eitherTypeMap <-
    runValidateT $ do
      modTypeMap <- validateObjectTypes allTypes (rspdAllowedObjects permDef)
      validateInputObjectTypes modTypeMap (rspdAllowedInputObjects permDef)
  case eitherTypeMap of
    Left errs     -> throw400 Unexpected (mconcat errs)
    Right typeMap -> pure typeMap
  where
    toFieldMap =
      Map.fromList . map (\(RemoteAllowedFields ty flds) -> (ty, flds))
    validateObjectTypes ::
         (MonadValidate [Text] m)
      => VT.TypeMap
      -> [RemoteAllowedFields]
      -> m VT.TypeMap
    validateObjectTypes initTypes allowedFields = do
      Map.traverseWithKey
        (\namedType typeInfo ->
           case typeInfo of
             VT.TIObj objTy ->
               case Map.lookup namedType (toFieldMap allowedFields) of
                 Nothing -> pure $ VT.TIObj objTy {VT._otiFields = Map.empty}
                 Just permFields -> do
                   newFields <-
                     applyPerms namedType permFields (VT._otiFields objTy)
                   pure $
                     VT.TIObj objTy {VT._otiFields = Map.fromList newFields}
             otherType -> pure otherType)
        initTypes
    validateInputObjectTypes ::
         (MonadValidate [Text] m)
      => VT.TypeMap
      -> [RemoteAllowedFields]
      -> m VT.TypeMap
    validateInputObjectTypes initTypes allowedFields = do
      Map.traverseWithKey
        (\namedType typeInfo ->
           case typeInfo of
             VT.TIInpObj inpObjTy ->
               case Map.lookup namedType (toFieldMap allowedFields) of
                 Nothing ->
                   pure $ VT.TIInpObj inpObjTy {VT._iotiFields = Map.empty}
                 Just permArgs -> do
                   newFields <-
                     applyPerms namedType permArgs (VT._iotiFields inpObjTy)
                   pure $
                     VT.TIInpObj
                       inpObjTy {VT._iotiFields = Map.fromList newFields}
             otherType -> pure otherType)
        initTypes
    applyPerms namedType permFields allFields =
      traverse
        (\fieldName ->
           case Map.lookup fieldName allFields of
             Nothing ->
               refute
                 [ "field: " <> showName fieldName <> " not found in type: " <>
                   showNamedTy namedType
                 ]
             Just fieldInfo -> pure (fieldName, fieldInfo))
        permFields

updateRemoteGCtxFromTypes :: (QErrM m) => VT.TypeMap -> GC.RemoteGCtx -> m GC.RemoteGCtx
updateRemoteGCtxFromTypes newTypeMap oldRemoteGCtx = do
  let mQrTyp = Map.lookup qRootN newTypeMap
      mMrTyp = maybe Nothing (`Map.lookup` newTypeMap) mRootN
      mSrTyp = maybe Nothing (`Map.lookup` newTypeMap) sRootN
  qrTyp <- liftMaybe noQueryRoot mQrTyp
  let mRmQR = VT.getObjTyM qrTyp
      mRmMR = join $ VT.getObjTyM <$> mMrTyp
      mRmSR = join $ VT.getObjTyM <$> mSrTyp
  rmQR <- liftMaybe (err400 Unexpected "query root has to be an object type") mRmQR
  return $ GC.RemoteGCtx newTypeMap rmQR mRmMR mRmSR
  where
    qRootN = VT._otiName $ GC._rgQueryRoot oldRemoteGCtx
    mRootN = VT._otiName <$> GC._rgMutationRoot oldRemoteGCtx
    sRootN = VT._otiName <$> GC._rgSubscriptionRoot oldRemoteGCtx
    noQueryRoot = err400 Unexpected "query root is missing"

addRemoteSchemaPermissionsToCatalog
  :: RemoteSchemaPermissions
  -> Q.TxE QErr ()
addRemoteSchemaPermissionsToCatalog (RemoteSchemaPermissions rsName rsRole rsDef) =
  Q.unitQE defaultTxErrorHandler [Q.sql|
    INSERT into hdb_catalog.hdb_remote_schema_permission
      (remote_schema, role_name, definition)
      VALUES ($1, $2, $3)
  |] (rsName, rsRole, Q.AltJ $ J.toJSON rsDef) True

runDropRemoteSchemaPermissions
  :: ( QErrM m, UserInfoM m
     , CacheRWM m, MonadTx m, HasFeatureFlags m
     )
  => DropRemoteSchemaPermissions -> m EncJSON
runDropRemoteSchemaPermissions q = withRSPermFlagCheck True (do
  adminOnly
  featureFlags <- askFeatureFlags
  unless (ffRemoteSchemaPermissions featureFlags) $
    throw404 "remote schema permissions are not enabled"
  dropRemoteSchemaPermissionsP1 q
  dropRemoteSchemaPermissionsP2 q) >>
  pure successMsg

dropRemoteSchemaPermissionsP1 ::
     (QErrM m, CacheRM m, MonadTx m) => DropRemoteSchemaPermissions -> m ()
dropRemoteSchemaPermissionsP1 (DropRemoteSchemaPermissions rsName role) = do
  sc <- askSchemaCache
  let remoteSchemas = scRemoteSchemas sc
  rsCtx <-
    onNothing
      (Map.lookup rsName remoteSchemas)
      (throw400
         NotFound
         ("remote schema " <> remoteSchemaNameToText rsName <> " not found"))
  case rscPerms rsCtx of
    Nothing -> throw400 Disabled ("remote schema permissions are disabled")
    Just roleMap ->
      void $
      onNothing
        (Map.lookup role roleMap)
        (throw400
           NotFound
           ("role: " <> roleNameToTxt role <> " not found for remote schema " <>
            remoteSchemaNameToText rsName))

dropRemoteSchemaPermissionsP2 ::
     (QErrM m, CacheRWM m, MonadTx m, HasFeatureFlags m) => DropRemoteSchemaPermissions -> m ()
dropRemoteSchemaPermissionsP2 q@(DropRemoteSchemaPermissions rsName role) = withRSPermFlagCheck False $ do
  dropRemoteSchemaPermissionsP2Setup q
  dropRemoteSchemaPermissionsFromCatalog rsName role

dropRemoteSchemaPermissionsP2Setup ::
     (QErrM m, CacheRWM m, HasFeatureFlags m) => DropRemoteSchemaPermissions -> m ()
dropRemoteSchemaPermissionsP2Setup (DropRemoteSchemaPermissions rsName role) = withRSPermFlagCheck False $ do
  sc <- askSchemaCache
  let remoteSchemas = scRemoteSchemas sc
  rsCtx <-
    onNothing
      (Map.lookup rsName remoteSchemas)
      (throw400
         NotFound
         ("remote schema " <> remoteSchemaNameToText rsName <> " not found"))
  newPerms <- case rscPerms rsCtx of
    Nothing      -> throw500 ("remote schema permissions are disabled")
    Just roleMap -> pure $ Map.delete role roleMap
  let newRSCtx = rsCtx { rscPerms = pure newPerms }
  writeSchemaCache sc { scRemoteSchemas = Map.insert rsName newRSCtx remoteSchemas}

dropRemoteSchemaPermissionsFromCatalog ::
     (MonadTx m) => RemoteSchemaName -> RoleName -> m ()
dropRemoteSchemaPermissionsFromCatalog rsName role = do
  liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
    DELETE FROM hdb_catalog.hdb_remote_schema_permission
      WHERE remote_schema = $1 AND role_name = $2
  |] (rsName, role) True

fetchRemoteSchemaPerms :: Q.TxE QErr [RemoteSchemaPermissions]
fetchRemoteSchemaPerms =
  map uncurryRow <$> Q.listQE defaultTxErrorHandler
    [Q.sql|
     SELECT remote_schema, role_name, definition::json
       FROM hdb_catalog.hdb_remote_schema_permission
     |] () True
  where
    uncurryRow (name, role, Q.AltJ def) = RemoteSchemaPermissions name role def
