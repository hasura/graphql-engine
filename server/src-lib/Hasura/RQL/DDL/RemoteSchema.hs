{-# LANGUAGE ViewPatterns #-}
module Hasura.RQL.DDL.RemoteSchema
  ( runAddRemoteSchema
  , runRemoveRemoteSchema
  , removeRemoteSchemaFromCatalog
  , runReloadRemoteSchema
  , fetchRemoteSchemas
  , addRemoteSchemaP1
  , addRemoteSchemaP2Setup
  , runIntrospectRemoteSchema
  , addRemoteSchemaToCatalog
  ) where

import qualified Data.Aeson                        as J
import qualified Data.HashMap.Strict               as Map
import qualified Data.HashSet                      as S
import qualified Data.Text                         as T
import qualified Database.PG.Query                 as Q

import           Hasura.EncJSON
import           Hasura.GraphQL.NormalForm
import           Hasura.GraphQL.RemoteServer
import           Hasura.GraphQL.Schema.Merge
import           Hasura.Prelude
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.Types
import           Hasura.Server.Version             (HasVersion)
import           Hasura.SQL.Types

import qualified Hasura.GraphQL.Context            as GC
import qualified Hasura.GraphQL.Resolve.Introspect as RI
import qualified Hasura.GraphQL.Schema             as GS
import qualified Hasura.GraphQL.Validate           as VQ
import qualified Hasura.GraphQL.Validate.Types     as VT

runAddRemoteSchema
  :: ( HasVersion
     , QErrM m
     , CacheRWM m
     , MonadTx m
     , MonadIO m
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
  :: (HasVersion, QErrM m, MonadIO m, HasHttpManager m)
  => AddRemoteSchemaQuery -> m RemoteSchemaCtx
addRemoteSchemaP2Setup (AddRemoteSchemaQuery name def _) = do
  httpMgr <- askHttpManager
  rsi <- validateRemoteSchemaDef name def
  gCtx <- fetchRemoteSchema httpMgr rsi
  pure $ RemoteSchemaCtx name (convRemoteGCtx gCtx) rsi
  where
    convRemoteGCtx rmGCtx =
      GC.emptyGCtx { GS._gTypes     = GC._rgTypes rmGCtx
                   , GS._gQueryRoot = GC._rgQueryRoot rmGCtx
                   , GS._gMutRoot   = GC._rgMutationRoot rmGCtx
                   , GS._gSubRoot   = GC._rgSubscriptionRoot rmGCtx
                   }

addRemoteSchemaP2
  :: (HasVersion, MonadTx m, MonadIO m, HasHttpManager m) => AddRemoteSchemaQuery -> m ()
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
  case Map.lookup rsn rmSchemas of
    Just _  -> return ()
    Nothing -> throw400 NotExists "no such remote schema"
  let depObjs = getDependentObjs sc remoteSchemaDepId
  when (depObjs /= []) $ reportDeps depObjs
  where
    remoteSchemaDepId = SORemoteSchema rsn

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
    fromRow (n, Q.AltJ def, comm) = AddRemoteSchemaQuery n def comm

runIntrospectRemoteSchema
  :: (CacheRM m, QErrM m) => RemoteSchemaNameQuery -> m EncJSON
runIntrospectRemoteSchema (RemoteSchemaNameQuery rsName) = do
  sc <- askSchemaCache
  rGCtx <-
    case Map.lookup rsName (scRemoteSchemas sc) of
      Nothing ->
        throw400 NotExists $
        "remote schema: " <> remoteSchemaNameToTxt rsName <> " not found"
      Just rCtx -> mergeGCtx (rscGCtx rCtx) GC.emptyGCtx
      -- merge with emptyGCtx to get default query fields
  queryParts <- flip runReaderT rGCtx $ VQ.getQueryParts introspectionQuery
  (rootSelSet, _) <- flip runReaderT rGCtx $ VT.runReusabilityT $ VQ.validateGQ queryParts
  schemaField <-
    case rootSelSet of
      VQ.RQuery selSet -> getSchemaField $ toList $ unAliasedFields $
                          unObjectSelectionSet selSet
      _                -> throw500 "expected query for introspection"
  (introRes, _) <- flip runReaderT rGCtx $ VT.runReusabilityT $ RI.schemaR schemaField
  pure $ wrapInSpecKeys introRes
  where
    wrapInSpecKeys introObj =
      encJFromAssocList
        [ ( T.pack "data"
          , encJFromAssocList [(T.pack "__schema", encJFromJValue introObj)])
        ]
    getSchemaField = \case
        []  -> throw500 "found empty when looking for __schema field"
        [f] -> pure f
        _   -> throw500 "expected __schema field, found many fields"
