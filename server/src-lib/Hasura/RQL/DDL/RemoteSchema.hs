{-# LANGUAGE ViewPatterns #-}
module Hasura.RQL.DDL.RemoteSchema
  ( runAddRemoteSchema
  , runRemoveRemoteSchema
  , removeRemoteSchemaFromCatalog
  , runReloadRemoteSchema
  , fetchRemoteSchemas
  , addRemoteSchemaP1
  , addRemoteSchemaP2Setup
  , addRemoteSchemaP2
  , runIntrospectRemoteSchema
  , addRemoteSchemaToCatalog
  , addRemoteSchemaPermissionsToCatalog
  , runAddRemoteSchemaPermissions
  , dropRemoteSchemaPermFromCatalog
  , runDropRemoteSchemaPermissions
  ) where

import           Control.Monad.Unique
import           Hasura.EncJSON
-- import           Hasura.GraphQL.NormalForm
import           Hasura.GraphQL.RemoteServer
-- import           Hasura.GraphQL.Schema.Merge
import           Hasura.Prelude
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.DDL.RemoteSchema.Validate

import qualified Data.Aeson                           as J
import qualified Data.HashMap.Strict                  as Map
import qualified Data.HashSet                         as S
import qualified Database.PG.Query                    as Q

import           Hasura.RQL.Types
import           Hasura.Server.Version                (HasVersion)
import           Hasura.SQL.Types

import qualified Data.Environment                     as Env

import           Hasura.Session

runAddRemoteSchema
  :: ( HasVersion
     , QErrM m
     , CacheRWM m
     , MonadTx m
     , MonadIO m
     , MonadUnique m
     , HasHttpManager m
     )
  => Env.Environment
  -> AddRemoteSchemaQuery
  -> m EncJSON
runAddRemoteSchema env q = do
  addRemoteSchemaP1 name
  addRemoteSchemaP2 env q
  buildSchemaCacheFor $ MORemoteSchema name
  pure successMsg
  where
    name = _arsqName q

runAddRemoteSchemaPermissions
  :: ( QErrM m
     , CacheRWM m
     , MonadTx m
     )
  => AddRemoteSchemaPermissions
  -> m EncJSON
runAddRemoteSchemaPermissions q = do
  remoteSchemaMap <- scRemoteSchemas <$> askSchemaCache
  (RemoteSchemaCtxWithPermissions _ ctx perms) <-
    onNothing (Map.lookup name remoteSchemaMap) $
      throw400 NotExists $ "remote schema " <> name <<> " doesn't exist"
  onJust (Map.lookup role perms) $ \_ ->
    throw400 AlreadyExists $ "permissions for role: " <> role <<> " for remote schema:"
      <> name <<> " already exists"
  resolveRoleBasedRemoteSchema providedSchemaDoc ctx
  liftTx $ addRemoteSchemaPermissionsToCatalog q
  buildSchemaCacheFor $ MORemoteSchemaPermissions name role
  pure successMsg
  where
    AddRemoteSchemaPermissions name role defn _ = q

    providedSchemaDoc = _rspdSchema defn

runDropRemoteSchemaPermissions
  :: ( QErrM m
     , CacheRWM m
     , MonadTx m
     )
  => DropRemoteSchemaPermissions
  -> m EncJSON
runDropRemoteSchemaPermissions (DropRemoteSchemaPermissions name roleName) = do
  remoteSchemaMap <- scRemoteSchemas <$> askSchemaCache
  (RemoteSchemaCtxWithPermissions _ _ perms) <-
    onNothing (Map.lookup name remoteSchemaMap) $
      throw400 NotExists $ "remote schema " <> name <<> " doesn't exist"
  onNothing (Map.lookup roleName perms) $
    throw400 NotExists $ "permissions for role: " <> roleName <<> " for remote schema:"
     <> name <<> " doesn't exist"
  liftTx $ dropRemoteSchemaPermFromCatalog name roleName
  withNewInconsistentObjsCheck buildSchemaCache
  pure successMsg

addRemoteSchemaP1
  :: (QErrM m, CacheRM m)
  => RemoteSchemaName -> m ()
addRemoteSchemaP1 name = do
  remoteSchemaMap <- scRemoteSchemas <$> askSchemaCache
  onJust (Map.lookup name remoteSchemaMap) $ const $
    throw400 AlreadyExists $ "remote schema with name "
    <> name <<> " already exists"

addRemoteSchemaP2Setup
  :: (HasVersion, QErrM m, MonadIO m, MonadUnique m, HasHttpManager m)
  => Env.Environment
  -> AddRemoteSchemaQuery -> m RemoteSchemaCtx
addRemoteSchemaP2Setup env (AddRemoteSchemaQuery name def _) = do
  httpMgr <- askHttpManager
  rsi <- validateRemoteSchemaDef env def
  fetchRemoteSchema env httpMgr name rsi

addRemoteSchemaP2
  :: (HasVersion, MonadTx m, MonadIO m, MonadUnique m, HasHttpManager m) => Env.Environment -> AddRemoteSchemaQuery -> m ()
addRemoteSchemaP2 env q = do
  void $ addRemoteSchemaP2Setup env q
  liftTx $ addRemoteSchemaToCatalog q

runRemoveRemoteSchema
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m)
  => RemoteSchemaNameQuery -> m EncJSON
runRemoveRemoteSchema (RemoteSchemaNameQuery rsn) = do
  roles <- removeRemoteSchemaP1 rsn
  -- drop the permissions defined with this remote schema
  flip traverse_ roles $ (liftTx . dropRemoteSchemaPermFromCatalog rsn)
  liftTx $ removeRemoteSchemaFromCatalog rsn
  withNewInconsistentObjsCheck buildSchemaCache
  pure successMsg

removeRemoteSchemaP1
  :: (UserInfoM m, QErrM m, CacheRM m)
  => RemoteSchemaName -> m [RoleName]
removeRemoteSchemaP1 rsn = do
  sc <- askSchemaCache
  let rmSchemas = scRemoteSchemas sc
  void $ onNothing (Map.lookup rsn rmSchemas) $
    throw400 NotExists "no such remote schema"
  let depObjs = getDependentObjs sc remoteSchemaDepId
      roles = mapMaybe getRole depObjs
      nonPermDependentObjs = filter nonPermDependentObjPredicate depObjs
  -- report non permission dependencies (if any), this happens
  -- mostly when a remote relationship is defined with
  -- the current remote schema

  -- we only report the non permission dependencies because we
  -- drop the related permissions
  when (nonPermDependentObjs /= []) $ reportDeps depObjs
  pure roles
  where
    remoteSchemaDepId = SORemoteSchema rsn

    getRole depObj =
      case depObj of
        SORemoteSchemaPermission _ role -> Just role
        _                               -> Nothing

    nonPermDependentObjPredicate (SORemoteSchemaPermission _ _) = False
    nonPermDependentObjPredicate _                              = True

runReloadRemoteSchema
  :: (QErrM m, CacheRWM m)
  => RemoteSchemaNameQuery -> m EncJSON
runReloadRemoteSchema (RemoteSchemaNameQuery name) = do
  remoteSchemas <- getAllRemoteSchemas <$> askSchemaCache
  unless (name `elem` remoteSchemas) $ throw400 NotExists $
    "remote schema with name " <> name <<> " does not exist"

  let invalidations = mempty { ciRemoteSchemas = S.singleton name }
  withNewInconsistentObjsCheck $ buildSchemaCacheWithOptions CatalogUpdate invalidations
  pure successMsg

addRemoteSchemaToCatalog
  :: AddRemoteSchemaQuery
  -> Q.TxE QErr ()
addRemoteSchemaToCatalog (AddRemoteSchemaQuery name def comment) =
  Q.unitQE defaultTxErrorHandler [Q.sql|
    INSERT into hdb_catalog.remote_schemas
      (name, definition, comment)
      VALUES ($1, $2, $3)
  |] (name, Q.AltJ $ J.toJSON def, comment) True

addRemoteSchemaPermissionsToCatalog
  :: AddRemoteSchemaPermissions
  -> Q.TxE QErr ()
addRemoteSchemaPermissionsToCatalog (AddRemoteSchemaPermissions name role def comment) =
  Q.unitQE defaultTxErrorHandler [Q.sql|
    INSERT into hdb_catalog.hdb_remote_schema_permission
      (remote_schema_name, role_name, definition, comment)
      VALUES ($1, $2, $3, $4)
  |] (name, role, Q.AltJ $ J.toJSON def, comment) True

removeRemoteSchemaFromCatalog :: RemoteSchemaName -> Q.TxE QErr ()
removeRemoteSchemaFromCatalog name =
  Q.unitQE defaultTxErrorHandler [Q.sql|
    DELETE FROM hdb_catalog.remote_schemas
      WHERE name = $1
  |] (Identity name) True

dropRemoteSchemaPermFromCatalog :: RemoteSchemaName -> RoleName -> Q.TxE QErr ()
dropRemoteSchemaPermFromCatalog name role =
  Q.unitQE defaultTxErrorHandler [Q.sql|
    DELETE FROM hdb_catalog.hdb_remote_schema_permission
      WHERE remote_schema_name = $1 AND role_name = $2
  |] (name, role) True

fetchRemoteSchemas :: Q.TxE QErr [AddRemoteSchemaQuery]
fetchRemoteSchemas =
  map fromRow <$> Q.listQE defaultTxErrorHandler
    [Q.sql|
     SELECT name, definition, comment
       FROM hdb_catalog.remote_schemas
     ORDER BY name ASC
     |] () True
  where
    fromRow (name, Q.AltJ def, comment) =
      AddRemoteSchemaQuery name def comment

runIntrospectRemoteSchema
  :: (CacheRM m, QErrM m) => RemoteSchemaNameQuery -> m EncJSON
runIntrospectRemoteSchema (RemoteSchemaNameQuery rsName) = do
  sc <- askSchemaCache
  (RemoteSchemaCtx _ _ _ introspectionByteString _) <-
    _rscpContext <$> (onNothing (Map.lookup rsName (scRemoteSchemas sc)) $
    throw400 NotExists $
    "remote schema: " <> rsName <<> " not found")
  pure $ encJFromLBS introspectionByteString
