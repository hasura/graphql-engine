{-# LANGUAGE RecordWildCards #-}

module Hasura.RQL.DDL.RemoteSchema
  ( runAddRemoteSchema
  , runRemoveRemoteSchema
  , removeRemoteSchemaFromCatalog
  , runReloadRemoteSchema
  , buildGCtxMap
  , fetchRemoteSchemas
  , addRemoteSchemaP1
  , addRemoteSchemaP2Setup
  , addRemoteSchemaP2
  , runAddRemoteSchemaPermissions
  , runAddRemoteSchemaPermissionsP1
  , runAddRemoteSchemaPermissionsP2Setup
  , runDropRemoteSchemaPermissions
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

addRemoteSchemaP2Setup
  :: (QErrM m, CacheRWM m, MonadIO m, HasHttpManager m)
  => AddRemoteSchemaQuery -> m RemoteSchemaCtx
addRemoteSchemaP2Setup q = do
  httpMgr <- askHttpManager
  rsi <- validateRemoteSchemaDef def
  gCtx <- fetchRemoteSchema httpMgr name rsi
  let rsCtx = RemoteSchemaCtx name gCtx rsi
  addRemoteSchemaToCache rsCtx
  return rsCtx
  where
    AddRemoteSchemaQuery name def _ = q

addRemoteSchemaP2
  :: ( QErrM m
     , CacheRWM m
     , MonadTx m
     , MonadIO m, HasHttpManager m
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
  delRemoteSchemaFromCache rsn
  liftTx $ removeRemoteSchemaFromCatalog rsn
  return successMsg

runReloadRemoteSchema
  :: ( QErrM m, UserInfoM m , CacheRWM m
     , MonadIO m, HasHttpManager m
     )
  => RemoteSchemaNameQuery -> m EncJSON
runReloadRemoteSchema (RemoteSchemaNameQuery name) = do
  adminOnly
  rmSchemas <- scRemoteSchemas <$> askSchemaCache
  rsi <- fmap rscInfo $ onNothing (Map.lookup name rmSchemas) $
         throw400 NotExists $ "remote schema with name "
         <> name <<> " does not exist"
  httpMgr <- askHttpManager
  gCtx <- fetchRemoteSchema httpMgr name rsi
  delRemoteSchemaFromCache name
  addRemoteSchemaToCache $ RemoteSchemaCtx name gCtx rsi
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
  mergedGCtxMap <- mergeSchemas (scRemoteSchemas sc) (scRemoteSchemasWithRole sc) gCtxMap
  writeSchemaCache sc { scGCtxMap = mergedGCtxMap }

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

runAddRemoteSchemaPermissions
  :: ( QErrM m, UserInfoM m
     , CacheRWM m, MonadTx m
     )
  => RemoteSchemaPermissions -> m EncJSON
runAddRemoteSchemaPermissions q = do
  adminOnly
  newRSCtx <- runAddRemoteSchemaPermissionsP1 q
  runAddRemoteSchemaPermissionsP2 q newRSCtx
  pure successMsg

runAddRemoteSchemaPermissionsP1
  :: (QErrM m, CacheRM m) => RemoteSchemaPermissions -> m RemoteSchemaCtx
runAddRemoteSchemaPermissionsP1 remoteSchemaPermission = do
  validateRemoteSchemaPermissions remoteSchemaPermission

runAddRemoteSchemaPermissionsP2
  :: (QErrM m, CacheRWM m, MonadTx m) => RemoteSchemaPermissions -> RemoteSchemaCtx -> m ()
runAddRemoteSchemaPermissionsP2 rsPerm rsCtx = do
  runAddRemoteSchemaPermissionsP2Setup rsPerm rsCtx
  liftTx $ addRemoteSchemaPermissionsToCatalog rsPerm

runAddRemoteSchemaPermissionsP2Setup
  :: (QErrM m, CacheRWM m) => RemoteSchemaPermissions -> RemoteSchemaCtx -> m ()
runAddRemoteSchemaPermissionsP2Setup RemoteSchemaPermissions{..} rsCtx = do
  sc <- askSchemaCache
  let rmSchemaWithRoles = scRemoteSchemasWithRole sc
      newSchemasWithRoles = Map.insert (rsPermRole, rsPermRemoteSchema) rsCtx rmSchemaWithRoles
  writeSchemaCache sc { scRemoteSchemasWithRole = newSchemasWithRoles }

validateRemoteSchemaPermissions :: (QErrM m, CacheRM m) => RemoteSchemaPermissions -> m RemoteSchemaCtx
validateRemoteSchemaPermissions remoteSchemaPerm = do
  sc <- askSchemaCache
  case Map.lookup
       (rsPermRemoteSchema remoteSchemaPerm)
       (scRemoteSchemas sc) of
    Nothing  -> throw400 RemoteSchemaError "No such remote schema"
    Just remoteSchemaCtx -> do
      newTypes <- do
        checkUniqueTypes (rsPermDefinition remoteSchemaPerm)
        validateTypes (rsPermDefinition remoteSchemaPerm) (GC._rgTypes $ rscGCtx remoteSchemaCtx)
      newGCtx <- updateRemoteGCtxFromTypes newTypes (rscGCtx remoteSchemaCtx)
      pure $ remoteSchemaCtx { rscGCtx = newGCtx }

checkUniqueTypes :: (QErrM m) => [RemoteTypePerm] -> m ()
checkUniqueTypes typePerms = do
  let types = map rtpType typePerms
      nubbedTypes = List.nub types
  if types /= nubbedTypes
    then throw400 InvalidParams $
         "duplicate types found in permission specification: " <>
         (T.pack . show) (types \\ nubbedTypes)
    else pure ()

validateTypes :: (QErrM m) => [RemoteTypePerm] -> VT.TypeMap -> m VT.TypeMap
validateTypes typePerms allTypes = do
  eitherTypeMap <- runValidateT validateTypes'
  case eitherTypeMap of
    Left errs     -> throw400 Unexpected (mconcat errs)
    Right typeMap -> pure typeMap
  where
    typePermMap = Map.fromList $ map (\(RemoteTypePerm ty flds) -> (ty, flds)) typePerms
    validateTypes' :: (MonadValidate [Text] m) => m VT.TypeMap
    validateTypes' = do
      Map.traverseWithKey
        (\namedType typeInfo ->
           -- only TIObj and TIInpObj can be permissioned, other types pass-thru
           case typeInfo of
             VT.TIObj objTy ->
               case Map.lookup namedType typePermMap of
                 Nothing -> pure $ VT.TIObj objTy {VT._otiFields = Map.empty}
                 Just permFields -> do
                   newFields <-
                     applyPerms namedType permFields (VT._otiFields objTy)
                   pure $
                     VT.TIObj objTy {VT._otiFields = Map.fromList newFields}
             VT.TIInpObj inpObjTy ->
               case Map.lookup namedType typePermMap of
                 Nothing ->
                   pure $ VT.TIInpObj inpObjTy {VT._iotiFields = Map.empty}
                 Just permArgs -> do
                   newFields <-
                     applyPerms namedType permArgs (VT._iotiFields inpObjTy)
                   pure $
                     VT.TIInpObj
                       inpObjTy {VT._iotiFields = Map.fromList newFields}
             otherType -> pure otherType)
        allTypes
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
    INSERT into hdb_catalog.remote_schema_permissions
      (remote_schema, role, definition)
      VALUES ($1, $2, $3)
  |] (rsName, rsRole, Q.AltJ $ J.toJSON rsDef) True

runDropRemoteSchemaPermissions
  :: ( QErrM m, UserInfoM m
     , CacheRWM m, MonadTx m
     )
  => DropRemoteSchemaPermissions -> m EncJSON
runDropRemoteSchemaPermissions q = do
  adminOnly
  dropRemoteSchemaPermissionsP1 q
  dropRemoteSchemaPermissionsP2 q
  pure successMsg

dropRemoteSchemaPermissionsP1 ::
     (QErrM m, CacheRWM m, MonadTx m) => DropRemoteSchemaPermissions -> m ()
dropRemoteSchemaPermissionsP1 (DropRemoteSchemaPermissions remoteSchema role) = do
  sc <- askSchemaCache
  let roleSchemas = scRemoteSchemasWithRole sc
  void $ onNothing
    (Map.lookup (role, remoteSchema) roleSchemas)
    (throw400 NotFound ("role: " <> roleNameToTxt role <> " not found for remote schema"))

dropRemoteSchemaPermissionsP2 ::
     (QErrM m, CacheRWM m, MonadTx m) => DropRemoteSchemaPermissions -> m ()
dropRemoteSchemaPermissionsP2 q = do
  dropRemoteSchemaPermissionsP2Setup q
  dropRemoteSchemaPermissionsP2FromCatalog q

dropRemoteSchemaPermissionsP2Setup ::
     (QErrM m, CacheRWM m) => DropRemoteSchemaPermissions -> m ()
dropRemoteSchemaPermissionsP2Setup (DropRemoteSchemaPermissions remoteSchema role) = do
  sc <- askSchemaCache
  let roleSchemas = scRemoteSchemasWithRole sc
      updatedRoleSchemas = Map.delete (role, remoteSchema) roleSchemas
  writeSchemaCache sc { scRemoteSchemasWithRole = updatedRoleSchemas }

dropRemoteSchemaPermissionsP2FromCatalog ::
     (MonadTx m) => DropRemoteSchemaPermissions -> m ()
dropRemoteSchemaPermissionsP2FromCatalog (DropRemoteSchemaPermissions remoteSchema role) = do
  liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
    DELETE FROM hdb_catalog.remote_schema_permissions
      WHERE remote_schema = $1 AND role = $2
  |] (remoteSchema, role) True
