{-# LANGUAGE Arrows               #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Types and functions used in the process of building the schema cache from metadata information
-- stored in the @hdb_catalog@ schema in Postgres.
module Hasura.RQL.Types.SchemaCache.Build
  ( CollectedInfo(..)
  , partitionCollectedInfo

  , recordInconsistency
  , recordInconsistencies
  , recordDependencies
  , withRecordInconsistency

  , CacheRWM(..)
  , BuildReason(..)
  , CacheInvalidations(..)
  , MetadataM(..)
  , MetadataT(..)
  , runMetadataT
  , buildSchemaCacheWithInvalidations
  , buildSchemaCache
  , buildSchemaCacheFor
  , buildSchemaCacheStrict
  , withNewInconsistentObjsCheck
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended        as M
import qualified Data.Sequence                       as Seq

import           Control.Arrow.Extended
import           Control.Lens
import           Control.Monad.Morph
import           Control.Monad.Trans.Control         (MonadBaseControl)
import           Control.Monad.Unique
import           Data.Aeson                          (Value, toJSON)
import           Data.Aeson.TH
import           Data.List                           (nub)
import           Data.Text.Extended
import           Network.HTTP.Client.Extended

import qualified Hasura.Tracing                      as Tracing

import           Hasura.Backends.Postgres.Connection
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.Metadata
import           Hasura.RQL.Types.Metadata.Object
import           Hasura.RQL.Types.RemoteSchema       (RemoteSchemaName)
import           Hasura.RQL.Types.SchemaCache
import           Hasura.Session
import           Hasura.Tracing                      (TraceT)

-- ----------------------------------------------------------------------------
-- types used during schema cache construction

data CollectedInfo
  = CIInconsistency !InconsistentMetadata
  | CIDependency
    !MetadataObject -- ^ for error reporting on missing dependencies
    !SchemaObjId
    !SchemaDependency
  deriving (Eq)
$(makePrisms ''CollectedInfo)

class AsInconsistentMetadata s where
  _InconsistentMetadata :: Prism' s InconsistentMetadata
instance AsInconsistentMetadata InconsistentMetadata where
  _InconsistentMetadata = id
instance AsInconsistentMetadata CollectedInfo where
  _InconsistentMetadata = _CIInconsistency

partitionCollectedInfo
  :: Seq CollectedInfo
  -> ([InconsistentMetadata], [(MetadataObject, SchemaObjId, SchemaDependency)])
partitionCollectedInfo =
  flip foldr ([], []) \info (inconsistencies, dependencies) -> case info of
    CIInconsistency inconsistency -> (inconsistency:inconsistencies, dependencies)
    CIDependency metadataObject objectId schemaDependency ->
      let dependency = (metadataObject, objectId, schemaDependency)
      in (inconsistencies, dependency:dependencies)

recordInconsistency
  :: (ArrowWriter (Seq w) arr, AsInconsistentMetadata w) => ((Maybe Value, MetadataObject), Text) `arr` ()
recordInconsistency = first (arr (:[])) >>> recordInconsistencies'

recordInconsistencies
  :: (ArrowWriter (Seq w) arr, AsInconsistentMetadata w) => ([MetadataObject], Text) `arr` ()
recordInconsistencies = first (arr (map (Nothing,))) >>> recordInconsistencies'

recordInconsistencies'
  :: (ArrowWriter (Seq w) arr, AsInconsistentMetadata w) => ([(Maybe Value, MetadataObject)], Text) `arr` ()
recordInconsistencies' = proc (metadataObjects, reason) ->
  tellA -< Seq.fromList $ map (review _InconsistentMetadata . uncurry (InconsistentObject reason)) metadataObjects

recordDependencies
  :: (ArrowWriter (Seq CollectedInfo) arr)
  => (MetadataObject, SchemaObjId, [SchemaDependency]) `arr` ()
recordDependencies = proc (metadataObject, schemaObjectId, dependencies) ->
  tellA -< Seq.fromList $ map (CIDependency metadataObject schemaObjectId) dependencies

withRecordInconsistency
  :: (ArrowChoice arr, ArrowWriter (Seq w) arr, AsInconsistentMetadata w)
  => ErrorA QErr arr (e, s) a
  -> arr (e, (MetadataObject, s)) (Maybe a)
withRecordInconsistency f = proc (e, (metadataObject, s)) -> do
  result <- runErrorA f -< (e, s)
  case result of
    Left err -> do
      recordInconsistency -< ((qeInternal err, metadataObject), qeError err)
      returnA -< Nothing
    Right v -> returnA -< Just v
{-# INLINABLE withRecordInconsistency #-}

-- ----------------------------------------------------------------------------
-- operations for triggering a schema cache rebuild

class (CacheRM m) => CacheRWM m where
  buildSchemaCacheWithOptions
    :: BuildReason -> CacheInvalidations -> Metadata -> m ()
  setMetadataResourceVersionInSchemaCache :: MetadataResourceVersion -> m ()

data BuildReason
  -- | The build was triggered by an update this instance made to the catalog (in the
  -- currently-active transaction), so information in Postgres that needs to be kept in sync with
  -- the catalog (i.e. table event triggers in @hdb_catalog@ schema) should be updated.
  = CatalogUpdate
  -- | The build was triggered by a notification that some other currently-running Hasura instance
  -- updated the catalog. Since that instance already updated table event triggers in @hdb_catalog@,
  -- this build should be read-only.
  | CatalogSync
  deriving (Eq)

data CacheInvalidations = CacheInvalidations
  { ciMetadata      :: !Bool
  -- ^ Force reloading of all database information, including information not technically stored in
  -- metadata (currently just enum values). Set by the @reload_metadata@ API.
  , ciRemoteSchemas :: !(HashSet RemoteSchemaName)
  -- ^ Force refetching of the given remote schemas, even if their definition has not changed. Set
  -- by the @reload_remote_schema@ API.
  , ciSources       :: !(HashSet SourceName)
  -- ^ Force re-establishing connections of the given data sources, even if their configuration has not changed. Set
  -- by the @pg_reload_source@ API.
  }
$(deriveJSON hasuraJSON ''CacheInvalidations)

instance Semigroup CacheInvalidations where
  CacheInvalidations a1 b1 c1 <> CacheInvalidations a2 b2 c2 =
    CacheInvalidations (a1 || a2) (b1 <> b2) (c1 <> c2)
instance Monoid CacheInvalidations where
  mempty = CacheInvalidations False mempty mempty

instance (CacheRWM m) => CacheRWM (ReaderT r m) where
  buildSchemaCacheWithOptions a b c = lift $ buildSchemaCacheWithOptions a b c
  setMetadataResourceVersionInSchemaCache = lift . setMetadataResourceVersionInSchemaCache
instance (CacheRWM m) => CacheRWM (StateT s m) where
  buildSchemaCacheWithOptions a b c = lift $ buildSchemaCacheWithOptions a b c
  setMetadataResourceVersionInSchemaCache = lift . setMetadataResourceVersionInSchemaCache
instance (CacheRWM m) => CacheRWM (TraceT m) where
  buildSchemaCacheWithOptions a b c = lift $ buildSchemaCacheWithOptions a b c
  setMetadataResourceVersionInSchemaCache = lift . setMetadataResourceVersionInSchemaCache
instance (CacheRWM m) => CacheRWM (LazyTxT QErr m) where
  buildSchemaCacheWithOptions a b c = lift $ buildSchemaCacheWithOptions a b c
  setMetadataResourceVersionInSchemaCache = lift . setMetadataResourceVersionInSchemaCache

-- | A simple monad class which enables fetching and setting @'Metadata'
-- in the state.
class (Monad m) => MetadataM m where
  getMetadata :: m Metadata
  putMetadata :: Metadata -> m ()

instance (MetadataM m) => MetadataM (ReaderT r m) where
  getMetadata = lift getMetadata
  putMetadata = lift . putMetadata

instance (MetadataM m) => MetadataM (StateT r m) where
  getMetadata = lift getMetadata
  putMetadata = lift . putMetadata

instance (MetadataM m) => MetadataM (TraceT m) where
  getMetadata = lift getMetadata
  putMetadata = lift . putMetadata

newtype MetadataT m a
  = MetadataT {unMetadataT :: StateT Metadata m a}
  deriving
    ( Functor, Applicative, Monad, MonadTrans
    , MonadIO, MonadUnique, MonadReader r, MonadError e, MonadTx
    , SourceM, TableCoreInfoRM b, CacheRM, CacheRWM, MFunctor
    , Tracing.MonadTrace
    )

deriving instance (MonadBase IO m) => MonadBase IO (MetadataT m)
deriving instance (MonadBaseControl IO m) => MonadBaseControl IO (MetadataT m)

instance (Monad m) => MetadataM (MetadataT m) where
  getMetadata = MetadataT get
  putMetadata = MetadataT . put

instance (HasHttpManagerM m) => HasHttpManagerM (MetadataT m) where
  askHttpManager = lift askHttpManager

instance (UserInfoM m) => UserInfoM (MetadataT m) where
  askUserInfo = lift askUserInfo

runMetadataT :: Metadata -> MetadataT m a -> m (a, Metadata)
runMetadataT metadata (MetadataT m) =
  runStateT m metadata

buildSchemaCacheWithInvalidations :: (MetadataM m, CacheRWM m) => CacheInvalidations -> MetadataModifier -> m ()
buildSchemaCacheWithInvalidations cacheInvalidations metadataModifier = do
  metadata <- getMetadata
  let modifiedMetadata = unMetadataModifier metadataModifier metadata
  buildSchemaCacheWithOptions CatalogUpdate cacheInvalidations modifiedMetadata
  putMetadata modifiedMetadata

buildSchemaCache :: (MetadataM m, CacheRWM m) => MetadataModifier -> m ()
buildSchemaCache = buildSchemaCacheWithInvalidations mempty

-- | Rebuilds the schema cache after modifying metadata. If an object with the given object id became newly inconsistent,
-- raises an error about it specifically. Otherwise, raises a generic metadata inconsistency error.
buildSchemaCacheFor
  :: (QErrM m, CacheRWM m, MetadataM m)
  => MetadataObjId -> MetadataModifier -> m ()
buildSchemaCacheFor objectId metadataModifier = do
  oldSchemaCache <- askSchemaCache
  buildSchemaCache metadataModifier
  newSchemaCache <- askSchemaCache

  let diffInconsistentObjects = M.difference `on` (groupInconsistentMetadataById . scInconsistentObjs)
      newInconsistentObjects = newSchemaCache `diffInconsistentObjects` oldSchemaCache

  for_ (M.lookup objectId newInconsistentObjects) $ \matchingObjects -> do
    let reasons = commaSeparated $ imReason <$> matchingObjects
    throwError (err400 ConstraintViolation reasons) { qeInternal = Just $ toJSON matchingObjects }

  unless (null newInconsistentObjects) $
    throwError (err400 Unexpected "cannot continue due to new inconsistent metadata")
      { qeInternal = Just $ toJSON (nub . concatMap toList $ M.elems newInconsistentObjects) }

-- | Like 'buildSchemaCache', but fails if there is any inconsistent metadata.
buildSchemaCacheStrict :: (QErrM m, CacheRWM m, MetadataM m) => m ()
buildSchemaCacheStrict = do
  buildSchemaCache noMetadataModify
  sc <- askSchemaCache
  let inconsObjs = scInconsistentObjs sc
  unless (null inconsObjs) $ do
    let err = err400 Unexpected "cannot continue due to inconsistent metadata"
    throwError err{ qeInternal = Just $ toJSON inconsObjs }

-- | Executes the given action, and if any new 'InconsistentMetadata's are added to the schema
-- cache as a result of its execution, raises an error.
withNewInconsistentObjsCheck :: (QErrM m, CacheRM m) => m a -> m a
withNewInconsistentObjsCheck action = do
  originalObjects <- scInconsistentObjs <$> askSchemaCache
  result <- action
  currentObjects <- scInconsistentObjs <$> askSchemaCache

  let diffInconsistentObjects = M.difference `on` groupInconsistentMetadataById
      newInconsistentObjects =
        nub $ concatMap toList $ M.elems (currentObjects `diffInconsistentObjects` originalObjects)
  unless (null newInconsistentObjects) $
    throwError (err500 Unexpected "cannot continue due to newly found inconsistent metadata")
      { qeInternal = Just $ toJSON newInconsistentObjects }

  pure result
