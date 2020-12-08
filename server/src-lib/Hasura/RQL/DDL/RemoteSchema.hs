module Hasura.RQL.DDL.RemoteSchema
  ( runAddRemoteSchema
  , runRemoveRemoteSchema
  , dropRemoteSchemaInMetadata
  , runReloadRemoteSchema
  , addRemoteSchemaP1
  , addRemoteSchemaP2Setup
  , runIntrospectRemoteSchema
  ) where

import           Hasura.Prelude

import qualified Data.Environment            as Env
import qualified Data.HashMap.Strict         as Map
import qualified Data.HashMap.Strict.InsOrd  as OMap
import qualified Data.HashSet                as S

import           Control.Monad.Unique

import           Data.Text.Extended
import           Hasura.EncJSON
import           Hasura.GraphQL.RemoteServer
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.Types
import           Hasura.Server.Version       (HasVersion)


runAddRemoteSchema
  :: ( HasVersion
     , QErrM m
     , CacheRWM m
     , MonadTx m
     , MonadIO m
     , MonadUnique m
     , HasHttpManager m
     , MetadataM m
     )
  => Env.Environment
  -> AddRemoteSchemaQuery
  -> m EncJSON
runAddRemoteSchema env q = do
  addRemoteSchemaP1 name
  -- addRemoteSchemaP2 env q
  void $ addRemoteSchemaP2Setup env q
  buildSchemaCacheFor (MORemoteSchema name) $
    MetadataModifier $ metaRemoteSchemas %~ OMap.insert name q
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
  => Env.Environment
  -> AddRemoteSchemaQuery -> m RemoteSchemaCtx
addRemoteSchemaP2Setup env (AddRemoteSchemaQuery name def _) = do
  httpMgr <- askHttpManager
  rsi <- validateRemoteSchemaDef env def
  fetchRemoteSchema env httpMgr name rsi

runRemoveRemoteSchema
  :: (QErrM m, UserInfoM m, CacheRWM m, MetadataM m)
  => RemoteSchemaNameQuery -> m EncJSON
runRemoveRemoteSchema (RemoteSchemaNameQuery rsn) = do
  removeRemoteSchemaP1 rsn
  withNewInconsistentObjsCheck $ buildSchemaCache $
    dropRemoteSchemaInMetadata rsn
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
  :: (QErrM m, CacheRWM m, MetadataM m)
  => RemoteSchemaNameQuery -> m EncJSON
runReloadRemoteSchema (RemoteSchemaNameQuery name) = do
  remoteSchemas <- getAllRemoteSchemas <$> askSchemaCache
  unless (name `elem` remoteSchemas) $ throw400 NotExists $
    "remote schema with name " <> name <<> " does not exist"

  let invalidations = mempty { ciRemoteSchemas = S.singleton name }
  metadata <- getMetadata
  withNewInconsistentObjsCheck $
    buildSchemaCacheWithOptions CatalogUpdate invalidations metadata
  pure successMsg

dropRemoteSchemaInMetadata :: RemoteSchemaName -> MetadataModifier
dropRemoteSchemaInMetadata name =
  MetadataModifier $ metaRemoteSchemas %~ OMap.delete name

runIntrospectRemoteSchema
  :: (CacheRM m, QErrM m) => RemoteSchemaNameQuery -> m EncJSON
runIntrospectRemoteSchema (RemoteSchemaNameQuery rsName) = do
  sc <- askSchemaCache
  (RemoteSchemaCtx _ _ _ introspectionByteString _) <-
    onNothing (Map.lookup rsName (scRemoteSchemas sc)) $
    throw400 NotExists $
    "remote schema: " <> rsName <<> " not found"
  pure $ encJFromLBS introspectionByteString
