{-# LANGUAGE UndecidableInstances #-}

-- | Define and handle v1/metadata API operations to track, untrack, and get native queries.
module Hasura.NativeQuery.API
  ( GetNativeQuery (..),
    TrackNativeQuery (..),
    UntrackNativeQuery (..),
    runGetNativeQuery,
    runTrackNativeQuery,
    runUntrackNativeQuery,
    execTrackNativeQuery,
    execUntrackNativeQuery,
    dropNativeQueryInMetadata,
    module Hasura.NativeQuery.Types,
  )
where

import Autodocodec (HasCodec)
import Autodocodec qualified as AC
import Control.Lens (Traversal', has, preview, (^?))
import Data.Aeson
import Data.Environment qualified as Env
import Data.HashMap.Strict.InsOrd.Extended qualified as InsOrdHashMap
import Data.Text.Extended (toTxt, (<<>))
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.LogicalModel.API (getCustomTypes)
import Hasura.LogicalModel.Metadata (LogicalModelName)
import Hasura.LogicalModelResolver.Codec (nativeQueryRelationshipsCodec)
import Hasura.NativeQuery.Metadata (ArgumentName, NativeQueryMetadata (..), parseInterpolatedQuery)
import Hasura.NativeQuery.Types (NativeQueryName, NullableScalarType)
import Hasura.Prelude
import Hasura.RQL.Types.Backend (Backend, SourceConnConfiguration)
import Hasura.RQL.Types.BackendTag
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
  ( RelName,
    SourceName,
    sourceNameToText,
    successMsg,
  )
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.Relationships.Local (RelDef, RelManualNativeQueryConfig)
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.Init.FeatureFlag (HasFeatureFlagChecker (..))
import Hasura.Server.Init.FeatureFlag qualified as FF

-- | Default implementation of the 'track_native_query' request payload.
data TrackNativeQuery (b :: BackendType) = TrackNativeQuery
  { tnqSource :: SourceName,
    tnqRootFieldName :: NativeQueryName,
    tnqCode :: Text,
    tnqArguments :: HashMap ArgumentName (NullableScalarType b),
    tnqArrayRelationships :: InsOrdHashMap.InsOrdHashMap RelName (RelDef (RelManualNativeQueryConfig b)),
    tnqObjectRelationships :: InsOrdHashMap.InsOrdHashMap RelName (RelDef (RelManualNativeQueryConfig b)),
    tnqDescription :: Maybe Text,
    tnqReturns :: LogicalModelName
  }

instance (Backend b) => HasCodec (TrackNativeQuery b) where
  codec =
    AC.CommentCodec
      ("A request to track a native query")
      $ AC.object (backendPrefix @b <> "TrackNativeQuery")
      $ TrackNativeQuery
      <$> AC.requiredField "source" sourceDoc
      AC..= tnqSource
        <*> AC.requiredField "root_field_name" rootFieldDoc
      AC..= tnqRootFieldName
        <*> AC.requiredField "code" codeDoc
      AC..= tnqCode
        <*> AC.optionalFieldWithDefault "arguments" mempty argumentsDoc
      AC..= tnqArguments
        <*> AC.optionalFieldWithDefaultWith "array_relationships" nativeQueryRelationshipsCodec mempty arrayRelationshipsDoc
      AC..= tnqArrayRelationships
        <*> AC.optionalFieldWithDefaultWith "object_relationships" nativeQueryRelationshipsCodec mempty objectRelationshipsDoc
      AC..= tnqObjectRelationships
        <*> AC.optionalField "description" descriptionDoc
      AC..= tnqDescription
        <*> AC.requiredField "returns" returnsDoc
      AC..= tnqReturns
    where
      arrayRelationshipsDoc = "Any relationships between an output value and multiple values in another data source"
      objectRelationshipsDoc = "Any relationships between an output value and a single value in another data source"
      sourceDoc = "The source in which this native query should be tracked"
      rootFieldDoc = "Root field name for the native query"
      codeDoc = "Native code expression (SQL) to run"
      argumentsDoc = "Free variables in the expression and their types"
      returnsDoc = "Return type (table) of the expression"
      descriptionDoc = "A description of the query which appears in the graphql schema"

deriving via
  (AC.Autodocodec (TrackNativeQuery b))
  instance
    (Backend b) => FromJSON (TrackNativeQuery b)

deriving via
  (AC.Autodocodec (TrackNativeQuery b))
  instance
    (Backend b) => ToJSON (TrackNativeQuery b)

-- | Validate a native query and extract the native query info from the request.
nativeQueryTrackToMetadata ::
  forall b m.
  ( BackendMetadata b,
    MonadError QErr m,
    MonadIO m,
    MetadataM m
  ) =>
  Env.Environment ->
  SourceConnConfiguration b ->
  TrackNativeQuery b ->
  m (NativeQueryMetadata b)
nativeQueryTrackToMetadata env sourceConnConfig TrackNativeQuery {..} = do
  code <- parseInterpolatedQuery tnqCode `onLeft` \e -> throw400 ParseFailed e

  let nativeQueryMetadata =
        NativeQueryMetadata
          { _nqmRootFieldName = tnqRootFieldName,
            _nqmCode = code,
            _nqmReturns = tnqReturns,
            _nqmArguments = tnqArguments,
            _nqmArrayRelationships = tnqArrayRelationships,
            _nqmObjectRelationships = tnqObjectRelationships,
            _nqmDescription = tnqDescription
          }

  metadata <- getMetadata

  -- lookup logical model in existing metadata
  case metadata ^? getCustomTypes tnqSource . ix tnqReturns of
    Just logicalModel ->
      validateNativeQuery @b env sourceConnConfig logicalModel nativeQueryMetadata
    Nothing -> throw400 NotFound ("Logical model " <> tnqReturns <<> " not found.")

  pure nativeQueryMetadata

-- | API payload for the 'get_native_query' endpoint.
data GetNativeQuery (b :: BackendType) = GetNativeQuery
  { gnqSource :: SourceName
  }

deriving instance (Backend b) => Show (GetNativeQuery b)

deriving instance (Backend b) => Eq (GetNativeQuery b)

instance (Backend b) => FromJSON (GetNativeQuery b) where
  parseJSON = withObject "GetNativeQuery" $ \o -> do
    gnqSource <- o .: "source"
    pure GetNativeQuery {..}

instance (Backend b) => ToJSON (GetNativeQuery b) where
  toJSON GetNativeQuery {..} =
    object
      [ "source" .= gnqSource
      ]

-- | Handler for the 'get_native_query' endpoint.
runGetNativeQuery ::
  forall b m.
  ( BackendMetadata b,
    MetadataM m,
    HasFeatureFlagChecker m,
    MonadError QErr m
  ) =>
  GetNativeQuery b ->
  m EncJSON
runGetNativeQuery q = do
  throwIfFeatureDisabled

  metadata <- getMetadata

  let nativeQuery :: Maybe (NativeQueries b)
      nativeQuery = metadata ^? metaSources . ix (gnqSource q) . toSourceMetadata . smNativeQueries @b

  pure (encJFromJValue (InsOrdHashMap.elems <$> nativeQuery))

-- | Handler for the 'track_native_query' endpoint. The type 'TrackNativeQuery b'
-- (appearing here in wrapped as 'BackendTrackNativeQuery b' for 'AnyBackend'
-- compatibility) is defined in 'class NativeQueryMetadata'.
runTrackNativeQuery ::
  forall b m.
  ( BackendMetadata b,
    MonadError QErr m,
    MonadIO m,
    CacheRWM m,
    MetadataM m,
    HasFeatureFlagChecker m
  ) =>
  Env.Environment ->
  TrackNativeQuery b ->
  m EncJSON
runTrackNativeQuery env trackNativeQueryRequest = do
  (obj, modifier) <- execTrackNativeQuery env trackNativeQueryRequest
  buildSchemaCacheFor obj modifier
  pure successMsg

-- | Handler for the 'track_native_query' endpoint. The type 'TrackNativeQuery b'
-- (appearing here in wrapped as 'BackendTrackNativeQuery b' for 'AnyBackend'
-- compatibility) is defined in 'class NativeQueryMetadata'.
execTrackNativeQuery ::
  forall b m.
  ( BackendMetadata b,
    MonadError QErr m,
    MonadIO m,
    MetadataM m,
    HasFeatureFlagChecker m
  ) =>
  Env.Environment ->
  TrackNativeQuery b ->
  m (MetadataObjId, MetadataModifier)
execTrackNativeQuery env trackNativeQueryRequest = do
  throwIfFeatureDisabled

  sourceMetadata <-
    maybe
      ( throw400 NotFound
          $ "Source '"
          <> sourceNameToText source
          <> "' of kind "
          <> toTxt (reify (backendTag @b))
          <> " not found."
      )
      pure
      . preview (metaSources . ix source . toSourceMetadata @b)
      =<< getMetadata
  let sourceConnConfig = _smConfiguration sourceMetadata

  (metadata :: NativeQueryMetadata b) <- do
    nativeQueryTrackToMetadata @b env sourceConnConfig trackNativeQueryRequest

  let fieldName = _nqmRootFieldName metadata
      metadataObj =
        MOSourceObjId source
          $ AB.mkAnyBackend
          $ SMONativeQuery @b fieldName
      existingNativeQueries = InsOrdHashMap.keys (_smNativeQueries sourceMetadata)

  when (fieldName `elem` existingNativeQueries) do
    throw400 AlreadyTracked $ "Native query '" <> toTxt fieldName <> "' is already tracked."

  let metadataModifier =
        MetadataModifier
          $ (metaSources . ix source . toSourceMetadata @b . smNativeQueries)
          %~ InsOrdHashMap.insert fieldName metadata

  pure (metadataObj, metadataModifier)
  where
    source = tnqSource trackNativeQueryRequest

-- | API payload for the 'untrack_native_query' endpoint.
data UntrackNativeQuery (b :: BackendType) = UntrackNativeQuery
  { utnqSource :: SourceName,
    utnqRootFieldName :: NativeQueryName
  }

deriving instance Show (UntrackNativeQuery b)

deriving instance Eq (UntrackNativeQuery b)

instance FromJSON (UntrackNativeQuery b) where
  parseJSON = withObject "UntrackNativeQuery" $ \o -> do
    utnqSource <- o .: "source"
    utnqRootFieldName <- o .: "root_field_name"
    pure UntrackNativeQuery {..}

instance ToJSON (UntrackNativeQuery b) where
  toJSON UntrackNativeQuery {..} =
    object
      [ "source" .= utnqSource,
        "root_field_name" .= utnqRootFieldName
      ]

-- | Handler for the 'untrack_native_query' endpoint.
runUntrackNativeQuery ::
  forall b m.
  ( BackendMetadata b,
    MonadError QErr m,
    CacheRWM m,
    MetadataM m
  ) =>
  UntrackNativeQuery b ->
  m EncJSON
runUntrackNativeQuery untrackNativeQueryRequest = do
  (obj, modifier) <- execUntrackNativeQuery untrackNativeQueryRequest
  buildSchemaCacheFor obj modifier
  pure successMsg

execUntrackNativeQuery ::
  forall b m.
  ( BackendMetadata b,
    MonadError QErr m,
    MetadataM m
  ) =>
  UntrackNativeQuery b ->
  m (MetadataObjId, MetadataModifier)
execUntrackNativeQuery q = do
  -- we do not check for feature flag here as we always want users to be able
  -- to remove native queries if they'd like
  assertNativeQueryExists @b source fieldName

  let metadataObj =
        MOSourceObjId source
          $ AB.mkAnyBackend
          $ SMONativeQuery @b fieldName

  pure (metadataObj, dropNativeQueryInMetadata @b source fieldName)
  where
    source = utnqSource q
    fieldName = utnqRootFieldName q

dropNativeQueryInMetadata :: forall b. (BackendMetadata b) => SourceName -> NativeQueryName -> MetadataModifier
dropNativeQueryInMetadata source rootFieldName = do
  MetadataModifier
    $ metaSources
    . ix source
    . toSourceMetadata @b
    . smNativeQueries
    %~ InsOrdHashMap.delete rootFieldName

-- | check feature flag is enabled before carrying out any actions
throwIfFeatureDisabled :: (HasFeatureFlagChecker m, MonadError QErr m) => m ()
throwIfFeatureDisabled = do
  enableNativeQueries <- checkFlag FF.nativeQueryInterface
  unless enableNativeQueries (throw500 "NativeQueries is disabled!")

-- | Check whether a native query with the given root field name exists for
-- the given source.
assertNativeQueryExists :: forall b m. (Backend b, MetadataM m, MonadError QErr m) => SourceName -> NativeQueryName -> m ()
assertNativeQueryExists sourceName rootFieldName = do
  metadata <- getMetadata

  let sourceMetadataTraversal :: Traversal' Metadata (SourceMetadata b)
      sourceMetadataTraversal = metaSources . ix sourceName . toSourceMetadata @b

  sourceMetadata <-
    preview sourceMetadataTraversal metadata
      `onNothing` throw400 NotFound ("Source " <> sourceName <<> " not found.")

  let desiredNativeQuery :: Traversal' (SourceMetadata b) (NativeQueryMetadata b)
      desiredNativeQuery = smNativeQueries . ix rootFieldName

  unless (has desiredNativeQuery sourceMetadata) do
    throw400 NotFound ("Native query " <> rootFieldName <<> " not found in source " <> sourceName <<> ".")
