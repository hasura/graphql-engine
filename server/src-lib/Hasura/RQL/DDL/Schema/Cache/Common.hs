{-# LANGUAGE Arrows               #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Types/functions shared between modules that implement "Hasura.RQL.DDL.Schema.Cache". Other
-- modules should not import this module directly.
module Hasura.RQL.DDL.Schema.Cache.Common where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended as M
import qualified Data.HashMap.Strict.InsOrd   as OMap
import qualified Data.HashSet                 as HS
import qualified Data.Sequence                as Seq
import qualified Network.HTTP.Client.Extended as HTTP

import           Control.Arrow.Extended
import           Control.Lens
import           Control.Monad.Trans.Control  (MonadBaseControl)
import           Control.Monad.Unique
import           Data.Text.Extended

import qualified Hasura.Incremental           as Inc

import           Hasura.RQL.Types

-- | 'InvalidationKeys' used to apply requested 'CacheInvalidations'.
data InvalidationKeys = InvalidationKeys
  { _ikMetadata      :: !Inc.InvalidationKey
  , _ikRemoteSchemas :: !(HashMap RemoteSchemaName Inc.InvalidationKey)
  , _ikSources       :: !(HashMap SourceName Inc.InvalidationKey)
  } deriving (Show, Eq, Generic)
instance Inc.Cacheable InvalidationKeys
instance Inc.Select InvalidationKeys
$(makeLenses ''InvalidationKeys)

initialInvalidationKeys :: InvalidationKeys
initialInvalidationKeys = InvalidationKeys Inc.initialInvalidationKey mempty mempty

invalidateKeys :: CacheInvalidations -> InvalidationKeys -> InvalidationKeys
invalidateKeys CacheInvalidations{..} InvalidationKeys{..} = InvalidationKeys
  { _ikMetadata = if ciMetadata then Inc.invalidate _ikMetadata else _ikMetadata
  , _ikRemoteSchemas = foldl' (flip invalidate) _ikRemoteSchemas ciRemoteSchemas
  , _ikSources = foldl' (flip invalidate) _ikSources ciSources
  }
  where
    invalidate
      :: (Eq a, Hashable a)
      => a -> HashMap a Inc.InvalidationKey -> HashMap a Inc.InvalidationKey
    invalidate = M.alter $ Just . maybe Inc.initialInvalidationKey Inc.invalidate

data TableBuildInput b
  = TableBuildInput
  { _tbiName          :: !(TableName b)
  , _tbiIsEnum        :: !Bool
  , _tbiConfiguration :: !(TableConfig b)
  } deriving (Show, Eq, Generic)
instance (Backend b) => NFData (TableBuildInput b)
instance (Backend b) => Inc.Cacheable (TableBuildInput b)

data NonColumnTableInputs b
  = NonColumnTableInputs
  { _nctiTable               :: !(TableName b)
  , _nctiObjectRelationships :: ![ObjRelDef b]
  , _nctiArrayRelationships  :: ![ArrRelDef b]
  , _nctiComputedFields      :: ![ComputedFieldMetadata b]
  , _nctiRemoteRelationships :: ![RemoteRelationshipMetadata]
  } deriving (Show, Eq, Generic)
-- instance NFData NonColumnTableInputs
-- instance Inc.Cacheable NonColumnTableInputs

data TablePermissionInputs b
  = TablePermissionInputs
  { _tpiTable  :: !(TableName b)
  , _tpiInsert :: ![InsPermDef b]
  , _tpiSelect :: ![SelPermDef b]
  , _tpiUpdate :: ![UpdPermDef b]
  , _tpiDelete :: ![DelPermDef b]
  } deriving (Show, Eq, Generic)
instance (Backend b) => Inc.Cacheable (TablePermissionInputs b)

mkTableInputs
  :: TableMetadata b -> (TableBuildInput b, NonColumnTableInputs b, TablePermissionInputs b)
mkTableInputs TableMetadata{..} =
  (buildInput, nonColumns, permissions)
  where
    buildInput = TableBuildInput _tmTable _tmIsEnum _tmConfiguration
    nonColumns = NonColumnTableInputs _tmTable
                 (OMap.elems _tmObjectRelationships)
                 (OMap.elems _tmArrayRelationships)
                 (OMap.elems _tmComputedFields)
                 (OMap.elems _tmRemoteRelationships)
    permissions = TablePermissionInputs _tmTable
                  (OMap.elems _tmInsertPermissions)
                  (OMap.elems _tmSelectPermissions)
                  (OMap.elems _tmUpdatePermissions)
                  (OMap.elems _tmDeletePermissions)

-- | The direct output of 'buildSchemaCacheRule'. Contains most of the things necessary to build a
-- schema cache, but dependencies and inconsistent metadata objects are collected via a separate
-- 'MonadWriter' side channel.
data BuildOutputs
  = BuildOutputs
  { _boSources        :: SourceCache
  , _boActions        :: !ActionCache
  , _boRemoteSchemas  :: !(HashMap RemoteSchemaName (RemoteSchemaCtx, MetadataObject))
  -- ^ We preserve the 'MetadataObject' from the original catalog metadata in the output so we can
  -- reuse it later if we need to mark the remote schema inconsistent during GraphQL schema
  -- generation (because of field conflicts).
  , _boAllowlist      :: !(HS.HashSet GQLQuery)
  , _boCustomTypes    :: !AnnotatedCustomTypes
  , _boCronTriggers   :: !(M.HashMap TriggerName CronTriggerInfo)
  , _boEndpoints      :: !(M.HashMap EndpointName (EndpointMetadata GQLQueryWithText))
  , _boApiLimits      :: !ApiLimit
  , _boMetricsConfig  :: !MetricsConfig
  , _boInheritedRoles :: !InheritedRolesCache
  }
$(makeLenses ''BuildOutputs)

-- | Parameters required for schema cache build
data CacheBuildParams
  = CacheBuildParams
  { _cbpManager         :: !HTTP.Manager
  , _cbpSourceResolver  :: !SourceResolver
  , _cbpServerConfigCtx :: !ServerConfigCtx
  }

-- | The monad in which @'RebuildableSchemaCache' is being run
newtype CacheBuild a
  = CacheBuild {unCacheBuild :: ReaderT CacheBuildParams (ExceptT QErr IO) a}
  deriving ( Functor, Applicative, Monad
           , MonadError QErr
           , MonadReader CacheBuildParams
           , MonadIO
           , MonadBase IO
           , MonadBaseControl IO
           , MonadUnique
           )

instance HTTP.HasHttpManagerM CacheBuild where
  askHttpManager = asks _cbpManager

instance HasServerConfigCtx CacheBuild where
  askServerConfigCtx = asks _cbpServerConfigCtx

instance MonadResolveSource CacheBuild where
  getSourceResolver = asks _cbpSourceResolver

runCacheBuild
  :: ( MonadIO m
     , MonadError QErr m
     )
  => CacheBuildParams -> CacheBuild a -> m a
runCacheBuild params (CacheBuild m) = do
  liftEitherM $ liftIO $ runExceptT (runReaderT m params)

runCacheBuildM
  :: ( MonadIO m
     , MonadError QErr m
     , HTTP.HasHttpManagerM m
     , HasServerConfigCtx m
     , MonadResolveSource m
     )
  => CacheBuild a -> m a
runCacheBuildM m = do
  params <- CacheBuildParams
            <$> HTTP.askHttpManager
            <*> getSourceResolver
            <*> askServerConfigCtx
  runCacheBuild params m

data RebuildableSchemaCache
  = RebuildableSchemaCache
  { lastBuiltSchemaCache :: !SchemaCache
  , _rscInvalidationMap :: !InvalidationKeys
  , _rscRebuild :: !(Inc.Rule (ReaderT BuildReason CacheBuild) (Metadata, InvalidationKeys) SchemaCache)
  }
$(makeLenses ''RebuildableSchemaCache)

bindErrorA
  :: (ArrowChoice arr, ArrowKleisli m arr, ArrowError e arr, MonadError e m)
  => arr (m a) a
bindErrorA = liftEitherA <<< arrM \m -> (Right <$> m) `catchError` (pure . Left)
{-# INLINE bindErrorA #-}

withRecordDependencies
  :: (ArrowWriter (Seq CollectedInfo) arr)
  => WriterA (Seq SchemaDependency) arr (e, s) a
  -> arr (e, (MetadataObject, (SchemaObjId, s))) a
withRecordDependencies f = proc (e, (metadataObject, (schemaObjectId, s))) -> do
  (result, dependencies) <- runWriterA f -< (e, s)
  recordDependencies -< (metadataObject, schemaObjectId, toList dependencies)
  returnA -< result
{-# INLINABLE withRecordDependencies #-}

noDuplicates
  :: (ArrowChoice arr, ArrowWriter (Seq CollectedInfo) arr)
  => (a -> MetadataObject)
  -> [a] `arr` Maybe a
noDuplicates mkMetadataObject = proc values -> case values of
  []      -> returnA -< Nothing
  [value] -> returnA -< Just value
  value:_ -> do
    let objectId = _moId $ mkMetadataObject value
        definitions = map (_moDefinition . mkMetadataObject) values
    tellA -< Seq.singleton $ CIInconsistency (DuplicateObjects objectId definitions)
    returnA -< Nothing
{-# INLINABLE noDuplicates #-}

-- | Processes a list of catalog metadata into a map of processed information, marking any duplicate
-- entries inconsistent.
buildInfoMap
  :: ( ArrowChoice arr, Inc.ArrowDistribute arr, ArrowWriter (Seq CollectedInfo) arr
     , Eq k, Hashable k )
  => (a -> k)
  -> (a -> MetadataObject)
  -> (e, a) `arr` Maybe b
  -> (e, [a]) `arr` HashMap k b
buildInfoMap extractKey mkMetadataObject buildInfo = proc (e, infos) ->
      (M.groupOn extractKey infos >- returnA)
  >-> (| Inc.keyed (\_ duplicateInfos ->
               (duplicateInfos >- noDuplicates mkMetadataObject)
           >-> (| traverseA (\info -> (e, info) >- buildInfo) |)
           >-> (\info -> join info >- returnA)) |)
  >-> (\infoMap -> M.catMaybes infoMap >- returnA)
{-# INLINABLE buildInfoMap #-}

-- | Like 'buildInfo', but includes each processed info’s associated 'MetadataObject' in the result.
-- This is useful if the results will be further processed, and the 'MetadataObject' is still needed
-- to mark the object inconsistent.
buildInfoMapPreservingMetadata
  :: ( ArrowChoice arr, Inc.ArrowDistribute arr, ArrowWriter (Seq CollectedInfo) arr
     , Eq k, Hashable k )
  => (a -> k)
  -> (a -> MetadataObject)
  -> (e, a) `arr` Maybe b
  -> (e, [a]) `arr` HashMap k (b, MetadataObject)
buildInfoMapPreservingMetadata extractKey mkMetadataObject buildInfo =
  buildInfoMap extractKey mkMetadataObject proc (e, info) ->
    ((e, info) >- buildInfo) >-> \result -> result <&> (, mkMetadataObject info) >- returnA
{-# INLINABLE buildInfoMapPreservingMetadata #-}

addTableContext :: (Backend b) => TableName b -> Text -> Text
addTableContext tableName e = "in table " <> tableName <<> ": " <> e
