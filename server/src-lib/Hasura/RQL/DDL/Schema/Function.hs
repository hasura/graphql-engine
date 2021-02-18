{- |
Description: Create/delete SQL functions to/from Hasura metadata.
-}

module Hasura.RQL.DDL.Schema.Function where

import           Hasura.Prelude

import qualified Data.HashMap.Strict        as Map
import qualified Data.HashMap.Strict.InsOrd as OMap

import           Data.Aeson
import           Data.Text.Extended

import           Hasura.EncJSON
import           Hasura.RQL.Types

import           Hasura.Session

newtype TrackFunction b
  = TrackFunction
  { tfName :: (FunctionName b)}
deriving instance (Backend b) => Show (TrackFunction b)
deriving instance (Backend b) => Eq (TrackFunction b)
deriving instance (Backend b) => FromJSON (TrackFunction b)
deriving instance (Backend b) => ToJSON (TrackFunction b)

-- | Track function, Phase 1:
-- Validate function tracking operation. Fails if function is already being
-- tracked, or if a table with the same name is being tracked.
trackFunctionP1
  :: forall m b
   . (CacheRM m, QErrM m, Backend b)
  => SourceName -> FunctionName b -> m ()
trackFunctionP1 sourceName qf = do
  rawSchemaCache <- askSchemaCache
  when (isJust $ unsafeFunctionInfo @b sourceName qf $ scPostgres rawSchemaCache) $
    throw400 AlreadyTracked $ "function already tracked : " <>> qf
  let qt = functionToTable qf
  when (isJust $ unsafeTableInfo @b sourceName qt $ scPostgres rawSchemaCache) $
    throw400 NotSupported $ "table with name " <> qf <<> " already exists"

trackFunctionP2
  :: (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b)
  => SourceName -> FunctionName b -> FunctionConfig -> m EncJSON
trackFunctionP2 sourceName qf config = do
  buildSchemaCacheFor (MOSourceObjId sourceName $ SMOFunction qf)
    $ MetadataModifier
    $ metaSources.ix sourceName.toSourceMetadata.smFunctions
      %~ OMap.insert qf (FunctionMetadata qf config mempty)
  pure successMsg

handleMultipleFunctions :: (QErrM m, Backend b) => FunctionName b -> [a] -> m a
handleMultipleFunctions qf = \case
  []      ->
    throw400 NotExists $ "no such function exists in postgres : " <>> qf
  [fi] -> return fi
  _       ->
    throw400 NotSupported $
    "function " <> qf <<> " is overloaded. Overloaded functions are not supported"

runTrackFunc
  :: (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b)
  => TrackFunction b -> m EncJSON
runTrackFunc (TrackFunction qf)= do
  -- v1 track_function lacks a means to take extra arguments
  trackFunctionP1 defaultSource qf
  trackFunctionP2 defaultSource qf emptyFunctionConfig

runTrackFunctionV2
  :: (QErrM m, CacheRWM m, MetadataM m)
  => TrackFunctionV2 -> m EncJSON
runTrackFunctionV2 (TrackFunctionV2 source qf config) = do
  trackFunctionP1 source qf
  trackFunctionP2 source qf config

-- | JSON API payload for 'untrack_function':
--
-- https://hasura.io/docs/1.0/graphql/core/api-reference/schema-metadata-api/custom-functions.html#untrack-function
data UnTrackFunction b
  = UnTrackFunction
  { _utfFunction :: !(FunctionName b)
  , _utfSource   :: !SourceName
  } deriving (Generic)
deriving instance (Backend b) => Show (UnTrackFunction b)
deriving instance (Backend b) => Eq (UnTrackFunction b)
instance (Backend b) => ToJSON (UnTrackFunction b) where
  toJSON = genericToJSON hasuraJSON

instance (Backend b) => FromJSON (UnTrackFunction b) where
  parseJSON v = withSource <|> withoutSource
    where
      withoutSource = UnTrackFunction <$> parseJSON v <*> pure defaultSource
      withSource = flip (withObject "UnTrackFunction") v \o ->
                   UnTrackFunction <$> o .: "function"
                                   <*> o .:? "source" .!= defaultSource

askFunctionInfo
  :: forall m b
   . (CacheRM m, MonadError QErr m, Backend b)
  => SourceName -> FunctionName b -> m (FunctionInfo b)
askFunctionInfo source functionName = do
  sourceCache <- scPostgres <$> askSchemaCache
  unsafeFunctionInfo @b source functionName sourceCache
    `onNothing` throw400 NotExists ("function " <> functionName <<> " not found in the cache")

runUntrackFunc
  :: (CacheRWM m, MonadError QErr m, MetadataM m, BackendMetadata b)
  => UnTrackFunction b -> m EncJSON
runUntrackFunc (UnTrackFunction functionName sourceName) = do
  void $ askFunctionInfo sourceName functionName
  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ dropFunctionInMetadata defaultSource functionName
  pure successMsg

dropFunctionInMetadata :: (BackendMetadata b) => SourceName -> FunctionName b -> MetadataModifier
dropFunctionInMetadata source function = MetadataModifier $
  metaSources.ix source.toSourceMetadata.smFunctions %~ OMap.delete function

{- Note [Function Permissions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Before we started supporting tracking volatile functions, permissions
for a function was inferred from the target table of the function.
The rationale behind this is that a stable/immutable function does not
modify the database and the data returned by the function is filtered using
the permissions that are specified precisely for that data.
Now consider mutable/volatile functions, we can't automatically infer whether or
not these functions should be exposed for the sole reason that they can modify
the database. This necessitates a permission system for functions.
So, we introduce a new API `pg_create_function_permission` which will
explicitly grant permission to a function to a role. For creating a
function permission, the role must have select permissions configured
for the target table.
Since, this is a breaking change, we enable it only when the graphql-engine
is started with
`--infer-function-permissions`/HASURA_GRAPHQL_INFER_FUNCTION_PERMISSIONS set
to false (by default, it's set to true).
-}

data CreateFunctionPermission b
  = CreateFunctionPermission
  { _afpFunction :: !(FunctionName b)
  , _afpSource   :: !SourceName
  , _afpRole     :: !RoleName
  } deriving (Generic)
deriving instance (Backend b) => Show (CreateFunctionPermission b)
deriving instance (Backend b) => Eq (CreateFunctionPermission b)
instance (Backend b) => ToJSON (CreateFunctionPermission b) where
  toJSON = genericToJSON hasuraJSON

instance (Backend b) => FromJSON (CreateFunctionPermission b) where
  parseJSON v =
    flip (withObject "CreateFunctionPermission") v $ \o ->
      CreateFunctionPermission
      <$> o .: "function"
      <*> o .:? "source" .!= defaultSource
      <*> o .: "role"

runCreateFunctionPermission
  :: forall m b
   . ( CacheRWM m
     , MonadError QErr m
     , MetadataM m
     , BackendMetadata b
     )
  => CreateFunctionPermission b
  -> m EncJSON
runCreateFunctionPermission (CreateFunctionPermission functionName source role) = do
  sourceCache <- scPostgres <$> askSchemaCache
  functionInfo <- askFunctionInfo source functionName
  when (role `elem` _fiPermissions functionInfo) $
    throw400 AlreadyExists $
    "permission of role "
    <> role <<> " already exists for function " <> functionName <<> " in source: " <>> source
  functionTableInfo <-
    unsafeTableInfo @b source (_fiReturnType functionInfo) sourceCache
    `onNothing` throw400 NotExists ("function's return table " <> (_fiReturnType functionInfo) <<> " not found in the cache")
  unless (role `Map.member` _tiRolePermInfoMap functionTableInfo) $
    throw400 NotSupported $
    "function permission can only be added when the function's return table "
    <> _fiReturnType functionInfo <<>  " has select permission configured for role: " <>> role
  buildSchemaCacheFor (MOSourceObjId source $ SMOFunctionPermission functionName role)
    $ MetadataModifier
    $ metaSources.ix source.toSourceMetadata.smFunctions.ix functionName.fmPermissions
    %~ (:) (FunctionPermissionMetadata role)
  pure successMsg

dropFunctionPermissionInMetadata
  :: (BackendMetadata b)
  => SourceName -> FunctionName b -> RoleName -> MetadataModifier
dropFunctionPermissionInMetadata source function role = MetadataModifier $
  metaSources.ix source.toSourceMetadata.smFunctions.ix function.fmPermissions %~ filter ((/=) role . _fpmRole)

type DropFunctionPermission = CreateFunctionPermission

runDropFunctionPermission
  :: ( CacheRWM m
     , MonadError QErr m
     , MetadataM m
     , BackendMetadata b
     )
  => DropFunctionPermission b
  -> m EncJSON
runDropFunctionPermission (CreateFunctionPermission functionName source role) = do
  functionInfo <- askFunctionInfo source functionName
  unless (role `elem` _fiPermissions functionInfo) $
    throw400 NotExists $
    "permission of role "
    <> role <<> " does not exist for function " <> functionName <<> " in source: " <>> source
  buildSchemaCacheFor (MOSourceObjId source $ SMOFunctionPermission functionName role)
    $ dropFunctionPermissionInMetadata source functionName role
  pure successMsg
