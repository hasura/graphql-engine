-- |
-- Description: Create/delete SQL functions to/from Hasura metadata.
module Hasura.Function.API
  ( FunctionPermissionArgument (..),
    SetFunctionCustomization (..),
    TrackFunction (..),
    TrackFunctionV2 (..),
    UnTrackFunction (..),
    doesFunctionPermissionExist,
    dropFunctionInMetadata,
    dropFunctionPermissionInMetadata,
    getSingleUniqueFunctionOverload,
    runCreateFunctionPermission,
    runDropFunctionPermission,
    runSetFunctionCustomization,
    runTrackFunc,
    runTrackFunctionV2,
    runUntrackFunc,
    trackFunctionP1,
    trackFunctionP2,
  )
where

import Control.Lens ((.~), (^.))
import Data.Aeson
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Function.Cache
import Hasura.Function.Metadata (FunctionMetadata (..), fmConfiguration, fmPermissions)
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendTag
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Metadata.Instances ()
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.Roles (RoleName)
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.SQL.AnyBackend qualified as AB

newtype TrackFunction b = TrackFunction {tfName :: FunctionName b}

deriving instance (Backend b) => FromJSON (TrackFunction b)

deriving instance (Backend b) => ToJSON (TrackFunction b)

-- | Track function, Phase 1:
-- Validate function tracking operation. Fails if function is already being
-- tracked, or if a table with the same name is being tracked.
trackFunctionP1 ::
  forall b m.
  (CacheRM m, QErrM m, Backend b) =>
  SourceName ->
  FunctionName b ->
  m ()
trackFunctionP1 sourceName qf = do
  rawSchemaCache <- askSchemaCache
  unless (isJust $ AB.unpackAnyBackend @b =<< HashMap.lookup sourceName (scSources rawSchemaCache))
    $ throw400 NotExists
    $ sourceName
    <<> " is not a known "
    <> reify (backendTag @b)
    <<> " source"
  when (isJust $ unsafeFunctionInfo @b sourceName qf $ scSources rawSchemaCache)
    $ throw400 AlreadyTracked
    $ "function already tracked: "
    <>> qf
  let qt = functionToTable @b qf
  when (isJust $ unsafeTableInfo @b sourceName qt $ scSources rawSchemaCache)
    $ throw400 NotSupported
    $ "table with name "
    <> qf
    <<> " already exists"

trackFunctionP2 ::
  forall b m.
  (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b) =>
  SourceName ->
  FunctionName b ->
  FunctionConfig b ->
  Maybe Text ->
  m EncJSON
trackFunctionP2 sourceName qf config comment = do
  buildSchemaCacheFor
    (MOSourceObjId sourceName $ AB.mkAnyBackend $ SMOFunction @b qf)
    $ MetadataModifier
    $ metaSources
    . ix sourceName
    . toSourceMetadata
    . (smFunctions @b)
    %~ InsOrdHashMap.insert qf (FunctionMetadata qf config mempty comment)
  pure successMsg

getSingleUniqueFunctionOverload ::
  forall b m.
  (QErrM m, Backend b) =>
  FunctionName b ->
  FunctionOverloads b ->
  m (RawFunctionInfo b)
getSingleUniqueFunctionOverload qf = \case
  FunctionOverloads (fi :| []) -> return fi
  _ -> throw400 NotSupported $ "function " <> qf <<> " is overloaded. Overloaded functions are not supported"

runTrackFunc ::
  forall b m.
  (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b) =>
  TrackFunction b ->
  m EncJSON
runTrackFunc (TrackFunction qf) = do
  -- v1 track_function lacks a means to take extra arguments
  trackFunctionP1 @b defaultSource qf
  trackFunctionP2 @b defaultSource qf emptyFunctionConfig Nothing

-- | JSON API payload for v2 of 'track_function':
--
-- https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-functions.html#track-function-v2
data TrackFunctionV2 (b :: BackendType) = TrackFunctionV2
  { _tfv2Source :: SourceName,
    _tfv2Function :: FunctionName b,
    _tfv2Configuration :: FunctionConfig b,
    _tfv2Comment :: Maybe Text
  }

instance (Backend b) => FromJSON (TrackFunctionV2 b) where
  parseJSON = withObject "TrackFunctionV2" $ \o ->
    TrackFunctionV2
      <$> o
      .:? "source"
      .!= defaultSource
      <*> o
      .: "function"
      <*> o
      .:? "configuration"
      .!= emptyFunctionConfig
      <*> o
      .:? "comment"

runTrackFunctionV2 ::
  forall b m.
  (BackendMetadata b, QErrM m, CacheRWM m, MetadataM m) =>
  TrackFunctionV2 b ->
  m EncJSON
runTrackFunctionV2 (TrackFunctionV2 source qf config comment) = do
  trackFunctionP1 @b source qf
  trackFunctionP2 @b source qf config comment

-- | JSON API payload for 'untrack_function':
--
-- https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-functions.html#untrack-function
data UnTrackFunction b = UnTrackFunction
  { _utfFunction :: FunctionName b,
    _utfSource :: SourceName
  }

instance (Backend b) => FromJSON (UnTrackFunction b) where
  -- Following was the previous implementation, which while seems to be correct,
  -- has an unexpected behaviour. In the case when @source@ key is present but
  -- @function@ key is absent, it would silently coerce it into a @default@
  -- source. The culprint being the _alternative_ operator, which silently fails
  -- the first parse. This note exists so that we don't try to simplify using
  -- the _alternative_ pattern here.
  -- Previous implementation :-
  -- Consider the following JSON -
  --  {
  --    "source": "custom_source",
  --    "schema": "public",
  --    "name": "my_function"
  --  }
  -- it silently fails parsing the source here because @function@ key is not
  -- present, and proceeds to parse using @withoutSource@ as default source. Now
  -- this is surprising for the user, because they mention @source@ key
  -- explicitly. A better behaviour is to explicitly look for @function@ key if
  -- a @source@ key is present.
  -- >>
  -- parseJSON v = withSource <|> withoutSource
  --   where
  --     withoutSource = UnTrackFunction <$> parseJSON v <*> pure defaultSource
  --     withSource = flip (withObject "UnTrackFunction") v \o -> do
  --                  UnTrackFunction <$> o .: "function"
  --                                  <*> o .:? "source" .!= defaultSource
  parseJSON v = flip (withObject "UnTrackFunction") v $ \o -> do
    source <- o .:? "source"
    case source of
      Just src -> flip UnTrackFunction src <$> o .: "function"
      Nothing -> UnTrackFunction <$> parseJSON v <*> pure defaultSource

runUntrackFunc ::
  forall b m.
  (CacheRWM m, MonadError QErr m, MetadataM m, BackendMetadata b) =>
  UnTrackFunction b ->
  m EncJSON
runUntrackFunc (UnTrackFunction functionName sourceName) = do
  void $ askFunctionInfo @b sourceName functionName
  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ dropFunctionInMetadata @b sourceName functionName
  pure successMsg

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

data FunctionPermissionArgument b = FunctionPermissionArgument
  { _afpFunction :: FunctionName b,
    _afpSource :: SourceName,
    _afpRole :: RoleName
  }

instance (Backend b) => FromJSON (FunctionPermissionArgument b) where
  parseJSON v =
    flip (withObject "FunctionPermissionArgument") v $ \o ->
      FunctionPermissionArgument
        <$> o
        .: "function"
        <*> o
        .:? "source"
        .!= defaultSource
        <*> o
        .: "role"

runCreateFunctionPermission ::
  forall b m.
  ( CacheRWM m,
    MonadError QErr m,
    MetadataM m,
    BackendMetadata b
  ) =>
  FunctionPermissionArgument b ->
  m EncJSON
runCreateFunctionPermission (FunctionPermissionArgument functionName source role) = do
  metadata <- getMetadata
  sourceCache <- scSources <$> askSchemaCache
  functionInfo <- askFunctionInfo @b source functionName
  when (doesFunctionPermissionExist @b metadata source functionName role)
    $ throw400 AlreadyExists
    $ "permission of role "
    <> role
    <<> " already exists for function "
    <> functionName
    <<> " in source: "
    <>> source
  (functionTableName, functionTableInfo) <- do
    let tn = _fiReturnType functionInfo
    case unsafeTableInfo @b source tn sourceCache of
      Nothing -> throw400 NotExists ("function's return table " <> tn <<> " not found in the cache")
      Just info -> pure (tn, info)
  unless (role `HashMap.member` _tiRolePermInfoMap functionTableInfo)
    $ throw400 NotSupported
    $ "function permission can only be added when the function's return table "
    <> functionTableName
    <<> " has select permission configured for role: "
    <>> role
  buildSchemaCacheFor
    ( MOSourceObjId source
        $ AB.mkAnyBackend (SMOFunctionPermission @b functionName role)
    )
    $ MetadataModifier
    $ metaSources
    . ix
      source
    . toSourceMetadata
    . (smFunctions @b)
    . ix functionName
    . fmPermissions
    %~ (:) (FunctionPermissionInfo role)
  pure successMsg

dropFunctionPermissionInMetadata ::
  forall b.
  (BackendMetadata b) =>
  SourceName ->
  FunctionName b ->
  RoleName ->
  MetadataModifier
dropFunctionPermissionInMetadata source function role =
  MetadataModifier
    $ metaSources
    . ix source
    . toSourceMetadata
    . (smFunctions @b)
    . ix function
    . fmPermissions
    %~ filter ((/=) role . _fpmRole)

doesFunctionPermissionExist :: forall b. (BackendMetadata b) => Metadata -> SourceName -> FunctionName b -> RoleName -> Bool
doesFunctionPermissionExist metadata sourceName functionName roleName =
  any ((== roleName) . _fpmRole) $ metadata ^. (metaSources . ix sourceName . toSourceMetadata . (smFunctions @b) . ix functionName . fmPermissions)

runDropFunctionPermission ::
  forall m b.
  ( CacheRWM m,
    MonadError QErr m,
    MetadataM m,
    BackendMetadata b
  ) =>
  FunctionPermissionArgument b ->
  m EncJSON
runDropFunctionPermission (FunctionPermissionArgument functionName source role) = do
  metadata <- getMetadata
  unless (doesFunctionPermissionExist @b metadata source functionName role)
    $ throw400 NotExists
    $ "permission of role "
    <> role
    <<> " does not exist for function "
    <> functionName
    <<> " in source: "
    <>> source
  buildSchemaCacheFor
    ( MOSourceObjId source
        $ AB.mkAnyBackend
        $ SMOFunctionPermission @b functionName role
    )
    $ dropFunctionPermissionInMetadata @b source functionName role
  pure successMsg

-- | Represents the payload of the API command 'pg_set_function_customization'.
--
--   See the Hasura API reference for a detailed description.
data SetFunctionCustomization b = SetFunctionCustomization
  { _sfcSource :: SourceName,
    _sfcFunction :: FunctionName b,
    _sfcConfiguration :: FunctionConfig b
  }

deriving instance (Backend b) => Show (SetFunctionCustomization b)

deriving instance (Backend b) => Eq (SetFunctionCustomization b)

instance (Backend b) => FromJSON (SetFunctionCustomization b) where
  parseJSON = withObject "set function customization" $ \o ->
    SetFunctionCustomization
      <$> o
      .:? "source"
      .!= defaultSource
      <*> o
      .: "function"
      <*> o
      .: "configuration"

-- | Changes the custom names of a function. Used in the API command 'pg_set_function_customization'.
runSetFunctionCustomization ::
  forall b m.
  (QErrM m, CacheRWM m, MetadataM m, Backend b) =>
  SetFunctionCustomization b ->
  m EncJSON
runSetFunctionCustomization (SetFunctionCustomization source function config) = do
  void $ askFunctionInfo @b source function
  buildSchemaCacheFor
    (MOSourceObjId source $ AB.mkAnyBackend $ SMOFunction @b function)
    $ MetadataModifier
    $ ((functionMetadataSetter @b source function) . fmConfiguration)
    .~ config
  return successMsg
