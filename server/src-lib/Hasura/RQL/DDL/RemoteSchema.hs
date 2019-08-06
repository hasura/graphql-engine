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
  ) where

import           Hasura.EncJSON
import           Hasura.Prelude

import qualified Data.Aeson                    as J
import qualified Data.HashMap.Strict           as Map
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
  -- Stitch remote schemas
  (mergedGCtxMap, defGCtx) <- mergeSchemas (scRemoteSchemas sc) (scRemoteSchemasWithRole sc) gCtxMap
  writeSchemaCache sc { scGCtxMap = mergedGCtxMap
                      , scDefaultRemoteGCtx = defGCtx
                      }

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
  runAddRemoteSchemaPermissionsP2Setup q newRSCtx
  pure successMsg

runAddRemoteSchemaPermissionsP1
  :: (QErrM m, CacheRM m) => RemoteSchemaPermissions -> m RemoteSchemaCtx
runAddRemoteSchemaPermissionsP1 remoteSchemaPermission = do
  validateRemoteSchemaPermissions remoteSchemaPermission

runAddRemoteSchemaPermissionsP2Setup
  :: (QErrM m, CacheRWM m) => RemoteSchemaPermissions -> RemoteSchemaCtx -> m ()
runAddRemoteSchemaPermissionsP2Setup RemoteSchemaPermissions{..} rsCtx = do
  sc <- askSchemaCache
  let rmSchemaWithRoles = scRemoteSchemasWithRole sc
      newSchemasWithRoles = Map.insert (rsPermRole, rsPermRemoteSchemaName) rsCtx rmSchemaWithRoles
  writeSchemaCache sc { scRemoteSchemasWithRole = newSchemasWithRoles }

validateRemoteSchemaPermissions :: (QErrM m, CacheRM m) => RemoteSchemaPermissions -> m RemoteSchemaCtx
validateRemoteSchemaPermissions remoteSchemaPerm = do
  sc <- askSchemaCache
  case Map.lookup
       (rsPermRemoteSchemaName remoteSchemaPerm)
       (scRemoteSchemas sc) of
    Nothing  -> throw400 RemoteSchemaError "No such remote schema"
    Just remoteSchemaCtx -> do
      newTypes <- validateTypes (rsPermTypes remoteSchemaPerm) (GC._rgTypes $ rscGCtx remoteSchemaCtx)
      newGCtx <- updateRemoteGCtxFromTypes newTypes (rscGCtx remoteSchemaCtx)
      pure $ remoteSchemaCtx { rscGCtx = newGCtx }

validateTypes :: (QErrM m) => PermTypeMap -> VT.TypeMap -> m VT.TypeMap
validateTypes permTypes allTypes = do
  eitherTypeMap <- runValidateT validateTypes'
  case eitherTypeMap of
    Left errs     -> throw400 Unexpected (mconcat errs)
    Right typeMap -> pure typeMap
  where
    validateTypes' :: (MonadValidate [Text] m) => m VT.TypeMap
    validateTypes' = do
      Map.traverseWithKey
        (\namedType typeInfo ->
           case typeInfo of
             VT.TIObj objTy ->
               case Map.lookup namedType permTypes of
                 Nothing -> pure $ VT.TIObj objTy { VT._otiFields = Map.empty }
                 Just fields -> do
                   newFields <-
                     traverse
                       (\fieldName ->
                          case Map.lookup fieldName (VT._otiFields objTy) of
                            Nothing ->
                              refute
                                [ "field: " <> showName fieldName <>
                                  " not found in type: " <>
                                  showNamedTy namedType
                                ]
                            Just fieldInfo -> pure (fieldName, fieldInfo))
                       fields
                   pure $
                     VT.TIObj objTy {VT._otiFields = Map.fromList newFields}
             VT.TIInpObj inpObjTy ->
                case Map.lookup namedType permTypes of
                 Nothing -> pure $ VT.TIInpObj inpObjTy { VT._iotiFields = Map.empty }
                 Just fields -> do
                   newFields <-
                     traverse
                       (\fieldName ->
                          case Map.lookup fieldName (VT._iotiFields inpObjTy) of
                            Nothing ->
                              refute
                                [ "field: " <> showName fieldName <>
                                  " not found in type: " <>
                                  showNamedTy namedType
                                ]
                            Just fieldInfo -> pure (fieldName, fieldInfo))
                       fields
                   pure $
                     VT.TIInpObj inpObjTy {VT._iotiFields = Map.fromList newFields}
             _otherwise ->
               refute
                 [ ("type: " <> showNamedTy namedType <>
                    " is not an object type or input object type")
                 ])
        allTypes

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
