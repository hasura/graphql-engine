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
  ) where

import           Control.Monad.Unique
import           Hasura.EncJSON
-- import           Hasura.GraphQL.NormalForm
import           Hasura.GraphQL.RemoteServer
-- import           Hasura.GraphQL.Schema.Merge
import           Hasura.Prelude
import           Hasura.RQL.DDL.Deps

import qualified Data.Aeson                  as J
import qualified Data.HashMap.Strict         as Map
import qualified Data.HashSet                as S
import qualified Database.PG.Query           as Q

import           Hasura.GraphQL.RemoteServer
import           Hasura.RQL.Types
import           Hasura.Server.Version       (HasVersion)
import           Hasura.SQL.Types

runAddRemoteSchema
  :: ( HasVersion
     , QErrM m
     , CacheRWM m
     , MonadTx m
     , MonadIO m
     , MonadUnique m
     , HasHttpManager m
     )
  => AddRemoteSchemaQuery -> m EncJSON
runAddRemoteSchema q = do
  addRemoteSchemaP1 name
  addRemoteSchemaP2 q
  buildSchemaCacheFor $ MORemoteSchema name
  pure successMsg
  where
    name = _arsqName q

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
  => AddRemoteSchemaQuery -> m RemoteSchemaCtx
addRemoteSchemaP2Setup (AddRemoteSchemaQuery name def _) = do
  httpMgr <- askHttpManager
  rsi <- validateRemoteSchemaDef def
  -- The 'rawIntrospectionResult' contains the 'Bytestring' response of
  -- the introspection result of the remote server. We store this in the
  -- 'RemoteSchemaCtx' because we can use this when the 'introspect_remote_schema'
  -- is called by simple encoding the result to JSON.
  (introspection, rawIntrospectionResult) <- fetchRemoteSchema httpMgr name rsi
  pure $ RemoteSchemaCtx name introspection rsi rawIntrospectionResult

addRemoteSchemaP2
  :: (HasVersion, MonadTx m, MonadIO m, MonadUnique m, HasHttpManager m) => AddRemoteSchemaQuery -> m ()
addRemoteSchemaP2 q = do
  void $ addRemoteSchemaP2Setup q
  liftTx $ addRemoteSchemaToCatalog q

runRemoveRemoteSchema
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m)
  => RemoteSchemaNameQuery -> m EncJSON
runRemoveRemoteSchema (RemoteSchemaNameQuery rsn) = do
  removeRemoteSchemaP1 rsn
  liftTx $ removeRemoteSchemaFromCatalog rsn
  withNewInconsistentObjsCheck buildSchemaCache
  pure successMsg

removeRemoteSchemaP1
  :: (UserInfoM m, QErrM m, CacheRM m)
  => RemoteSchemaName -> m ()
removeRemoteSchemaP1 rsn = do
  sc <- askSchemaCache
  let rmSchemas = scRemoteSchemas sc
  void $ onNothing (Map.lookup rsn rmSchemas) $
    throw400 NotExists "no such remote schema"
  let depObjs = getDependentObjs sc remoteSchemaDepId
  when (depObjs /= []) $ reportDeps depObjs
  where
    remoteSchemaDepId = SORemoteSchema rsn

runReloadRemoteSchema
  :: (QErrM m, CacheRWM m)
  => RemoteSchemaNameQuery -> m EncJSON
runReloadRemoteSchema (RemoteSchemaNameQuery name) = do
  rmSchemas <- scRemoteSchemas <$> askSchemaCache
  void $ onNothing (Map.lookup name rmSchemas) $
    throw400 NotExists $ "remote schema with name " <> name <<> " does not exist"

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
     ORDER BY name ASC
     |] () True
  where
    fromRow (name, Q.AltJ def, comment) =
      AddRemoteSchemaQuery name def comment

runIntrospectRemoteSchema
  :: (CacheRM m, QErrM m) => RemoteSchemaNameQuery -> m EncJSON
runIntrospectRemoteSchema (RemoteSchemaNameQuery rsName) = do
  sc <- askSchemaCache
  (RemoteSchemaCtx _ _ _ introspectionByteString) <-
    case Map.lookup rsName (scRemoteSchemas sc) of
      Nothing ->
        throw400 NotExists $
        "remote schema: " <> rsName <<> " not found"
      Just rCtx -> pure rCtx
  pure $ encJFromLBS introspectionByteString
