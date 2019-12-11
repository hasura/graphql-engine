{-# LANGUAGE Arrows #-}

-- | Types/functions shared between modules that implement "Hasura.RQL.DDL.Schema.Cache". Other
-- modules should not import this module directly.
module Hasura.RQL.DDL.Schema.Cache.Common where

import           Hasura.Prelude

import qualified Data.HashSet                     as HS
import qualified Data.Sequence                    as Seq

import           Control.Arrow.Extended
import           Control.Lens
import           Control.Monad.Unique

import qualified Hasura.GraphQL.Context           as GC
import qualified Hasura.Incremental               as Inc

import           Hasura.RQL.Types
import           Hasura.RQL.Types.Catalog
import           Hasura.RQL.Types.QueryCollection
import           Hasura.RQL.Types.Run
import           Hasura.SQL.Types

-- | A map used to explicitly invalidate part of the build cache, which is most useful for external
-- resources (currently only remote schemas). The 'InvalidationKey' values it contains are used as
-- inputs to build rules, so setting an entry to a fresh 'InvalidationKey' forces it to be
-- re-executed.
type InvalidationMap = HashMap RemoteSchemaName InvalidationKey
type InvalidationKey = Unique

data BuildInputs
  = BuildInputs
  { _biReason          :: !BuildReason
  , _biCatalogMetadata :: !CatalogMetadata
  , _biInvalidationMap :: !InvalidationMap
  } deriving (Eq)

-- | The direct output of 'buildSchemaCacheRule'. Contains most of the things necessary to build a
-- schema cache, but dependencies and inconsistent metadata objects are collected via a separate
-- 'MonadWriter' side channel.
data BuildOutputs
  = BuildOutputs
  { _boTables            :: !TableCache
  , _boFunctions         :: !FunctionCache
  , _boRemoteSchemas     :: !RemoteSchemaMap
  , _boAllowlist         :: !(HS.HashSet GQLQuery)
  , _boGCtxMap           :: !GC.GCtxMap
  , _boDefaultRemoteGCtx :: !GC.GCtx
  } deriving (Show, Eq)
$(makeLenses ''BuildOutputs)

data RebuildableSchemaCache m
  = RebuildableSchemaCache
  { lastBuiltSchemaCache :: !SchemaCache
  , _rscInvalidationMap :: !InvalidationMap
  , _rscRebuild :: !(Inc.Rule (ReaderT BuildReason m) (CatalogMetadata, InvalidationMap) SchemaCache)
  }
$(makeLenses ''RebuildableSchemaCache)

type CacheBuildM = ReaderT BuildReason Run
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

addTableContext :: QualifiedTable -> Text -> Text
addTableContext tableName e = "in table " <> tableName <<> ": " <> e
