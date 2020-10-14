{-# LANGUAGE Arrows               #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Types/functions shared between modules that implement "Hasura.RQL.DDL.Schema.Cache". Other
-- modules should not import this module directly.
module Hasura.RQL.DDL.Schema.Cache.Common where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended as M
import qualified Data.HashSet                 as HS
import qualified Data.Sequence                as Seq
import qualified Network.HTTP.Client          as HTTP

import           Control.Arrow.Extended
import           Control.Lens
import           Control.Monad.Trans.Control  (MonadBaseControl)
import           Control.Monad.Unique

import qualified Hasura.Incremental           as Inc

import           Hasura.RQL.Types
import           Hasura.SQL.Types

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

data TableBuildInput
  = TableBuildInput
  { _tbiName          :: !QualifiedTable
  , _tbiIsEnum        :: !Bool
  , _tbiConfiguration :: !TableConfig
  } deriving (Show, Eq, Generic)
instance NFData TableBuildInput
instance Inc.Cacheable TableBuildInput

data NonColumnTableInputs
  = NonColumnTableInputs
  { _nctiTable               :: !QualifiedTable
  , _nctiObjectRelationships :: ![ObjRelDef]
  , _nctiArrayRelationships  :: ![ArrRelDef]
  , _nctiComputedFields      :: ![ComputedFieldMetadata]
  , _nctiRemoteRelationships :: ![RemoteRelationshipMeta]
  } deriving (Show, Eq, Generic)
-- instance NFData NonColumnTableInputs
-- instance Inc.Cacheable NonColumnTableInputs

data TablePermissionInputs
  = TablePermissionInputs
  { _tpiTable  :: !QualifiedTable
  , _tpiInsert :: ![InsPermDef]
  , _tpiSelect :: ![SelPermDef]
  , _tpiUpdate :: ![UpdPermDef]
  , _tpiDelete :: ![DelPermDef]
  } deriving (Show, Eq, Generic)
instance Inc.Cacheable TablePermissionInputs

mkTableInputs :: TableMetadata -> (TableBuildInput, NonColumnTableInputs, TablePermissionInputs)
mkTableInputs TableMetadata{..} =
  (buildInput, nonColumns, permissions)
  where
    buildInput = TableBuildInput _tmTable _tmIsEnum _tmConfiguration
    nonColumns = NonColumnTableInputs _tmTable
                 (M.elems _tmObjectRelationships)
                 (M.elems _tmArrayRelationships)
                 (M.elems _tmComputedFields)
                 (M.elems _tmRemoteRelationships)
    permissions = TablePermissionInputs _tmTable
                  (M.elems _tmInsertPermissions)
                  (M.elems _tmSelectPermissions)
                  (M.elems _tmUpdatePermissions)
                  (M.elems _tmDeletePermissions)

data SourceOutput
  = SourceOutput
  { _soTables    :: !TableCache
  , _soFunctions :: !FunctionCache
  , _soConfig    :: !PGSourceConfig
  } deriving (Eq)
$(makeLenses ''SourceOutput)

type SourceOutputs = HashMap SourceName SourceOutput

-- | The direct output of 'buildSchemaCacheRule'. Contains most of the things necessary to build a
-- schema cache, but dependencies and inconsistent metadata objects are collected via a separate
-- 'MonadWriter' side channel.
data BuildOutputs
  = BuildOutputs
  { _boSources       :: !SourceOutputs
  , _boActions       :: !ActionCache
  , _boRemoteSchemas :: !(HashMap RemoteSchemaName (RemoteSchemaCtx, MetadataObject))
  -- ^ We preserve the 'MetadataObject' from the original catalog metadata in the output so we can
  -- reuse it later if we need to mark the remote schema inconsistent during GraphQL schema
  -- generation (because of field conflicts).
  , _boAllowlist     :: !(HS.HashSet GQLQuery)
  , _boCustomTypes   :: !AnnotatedCustomTypes
  , _boCronTriggers  :: !(M.HashMap TriggerName CronTriggerInfo)
  }
$(makeLenses ''BuildOutputs)

-- | Parameters required for schema cache build
data CacheBuildParams
  = CacheBuildParams
  { _cbpManager         :: !HTTP.Manager
  , _cbpSqlGenCtx       :: !SQLGenCtx
  , _cbpDefaultPgConfig :: !PGSourceConfig
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

instance HasHttpManager CacheBuild where
  askHttpManager = asks _cbpManager

instance HasSQLGenCtx CacheBuild where
  askSQLGenCtx = asks _cbpSqlGenCtx

instance HasDefaultSource CacheBuild where
  askDefaultSource = asks _cbpDefaultPgConfig

runCacheBuild
  :: ( MonadIO m
     , MonadError QErr m
     , HasHttpManager m
     , HasSQLGenCtx m
     , HasDefaultSource m
     )
  => CacheBuild a -> m a
runCacheBuild (CacheBuild m) = do
  httpManager <- askHttpManager
  sqlGenCtx   <- askSQLGenCtx
  defPgSource <- askDefaultSource
  let params = CacheBuildParams httpManager sqlGenCtx defPgSource
  liftEitherM $ liftIO $ runExceptT (runReaderT m params)

data RebuildableSchemaCache
  = RebuildableSchemaCache
  { lastBuiltSchemaCache :: !SchemaCache
  , _rscInvalidationMap :: !InvalidationKeys
  , _rscRebuild :: !(Inc.Rule (ReaderT BuildContext CacheBuild) (Metadata, InvalidationKeys) SchemaCache)
  }
$(makeLenses ''RebuildableSchemaCache)

data MetadataStateResult
  = MetadataStateResult
  { _msrSchemaCache        :: !RebuildableSchemaCache
  , _msrCacheInvalidations :: !CacheInvalidations
  , _msrMetadata           :: !Metadata
  }

type CacheBuildM = ReaderT BuildReason CacheBuild
type CacheBuildA = WriterA (Seq CollectedInfo) (Inc.Rule CacheBuildM)

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

addTableContext :: QualifiedTable -> Text -> Text
addTableContext tableName e = "in table " <> tableName <<> ": " <> e
