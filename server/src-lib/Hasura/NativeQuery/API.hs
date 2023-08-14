{-# LANGUAGE UndecidableInstances #-}

-- | Define and handle v1/metadata API operations to track, untrack, and get native queries.
module Hasura.NativeQuery.API
  ( GetNativeQuery (..),
    TrackNativeQuery (..),
    UntrackNativeQuery (..),
    runGetNativeQuery,
    execTrackNativeQuery,
    execUntrackNativeQuery,
    dropNativeQueryInMetadata,
    assertNativeQueryExists,
    module Hasura.NativeQuery.Types,
  )
where

import Autodocodec (HasCodec)
import Autodocodec qualified as AC
import Control.Lens (Traversal', has, preview, (^?))
import Data.Aeson
import Data.HashMap.Strict.InsOrd.Extended qualified as InsOrdHashMap
import Data.Text.Extended (toTxt, (<<>))
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.LogicalModelResolver.Codec (nativeQueryRelationshipsCodec)
import Hasura.LogicalModelResolver.Metadata (LogicalModelIdentifier)
import Hasura.NativeQuery.Metadata (ArgumentName, NativeQueryMetadata (..), parseInterpolatedQuery)
import Hasura.NativeQuery.Types (NativeQueryName, NullableScalarType)
import Hasura.Prelude
import Hasura.RQL.Types.Backend (Backend)
import Hasura.RQL.Types.BackendTag
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
  ( RelName,
    SourceName,
    sourceNameToText,
  )
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.Relationships.Local (RelDef, RelManualConfig)
import Hasura.SQL.AnyBackend qualified as AB

-- | Default implementation of the 'track_native_query' request payload.
data TrackNativeQuery (b :: BackendType) = TrackNativeQuery
  { tnqSource :: SourceName,
    tnqRootFieldName :: NativeQueryName,
    tnqCode :: Text,
    tnqArguments :: HashMap ArgumentName (NullableScalarType b),
    tnqArrayRelationships :: InsOrdHashMap.InsOrdHashMap RelName (RelDef (RelManualConfig b)),
    tnqObjectRelationships :: InsOrdHashMap.InsOrdHashMap RelName (RelDef (RelManualConfig b)),
    tnqDescription :: Maybe Text,
    tnqReturns :: LogicalModelIdentifier b
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
  ( MonadError QErr m
  ) =>
  TrackNativeQuery b ->
  m (NativeQueryMetadata b)
nativeQueryTrackToMetadata TrackNativeQuery {..} = do
  code <- parseInterpolatedQuery tnqCode `onLeft` \e -> throw400 ParseFailed e

  pure
    NativeQueryMetadata
      { _nqmRootFieldName = tnqRootFieldName,
        _nqmCode = code,
        _nqmReturns = tnqReturns,
        _nqmArguments = tnqArguments,
        _nqmArrayRelationships = tnqArrayRelationships,
        _nqmObjectRelationships = tnqObjectRelationships,
        _nqmDescription = tnqDescription
      }

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
    MonadError QErr m
  ) =>
  GetNativeQuery b ->
  m EncJSON
runGetNativeQuery q = do
  maybe
    ( throw400 NotFound
        $ "Source '"
        <> sourceNameToText (gnqSource q)
        <> "' of kind "
        <> toTxt (reify (backendTag @b))
        <> " not found."
    )
    (const $ pure ())
    . preview (metaSources . ix (gnqSource q) . toSourceMetadata @b)
    =<< getMetadata
  metadata <- getMetadata

  let nativeQuery :: Maybe (NativeQueries b)
      nativeQuery = metadata ^? metaSources . ix (gnqSource q) . toSourceMetadata . smNativeQueries @b

  pure (encJFromJValue (InsOrdHashMap.elems <$> nativeQuery))

-- | Handler for the 'track_native_query' endpoint. The type 'TrackNativeQuery b'
-- (appearing here in wrapped as 'BackendTrackNativeQuery b' for 'AnyBackend'
-- compatibility) is defined in 'class NativeQueryMetadata'.
execTrackNativeQuery ::
  forall b m.
  ( BackendMetadata b,
    MonadError QErr m
  ) =>
  TrackNativeQuery b ->
  Metadata ->
  m (MetadataObjId, MetadataModifier)
execTrackNativeQuery trackNativeQueryRequest metadata = do
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
      $ metadata

  (nqMetadata :: NativeQueryMetadata b) <- do
    nativeQueryTrackToMetadata @b trackNativeQueryRequest

  let fieldName = _nqmRootFieldName nqMetadata
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
          %~ InsOrdHashMap.insert fieldName nqMetadata

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
execUntrackNativeQuery ::
  forall b m.
  ( BackendMetadata b,
    MonadError QErr m
  ) =>
  UntrackNativeQuery b ->
  Metadata ->
  m (MetadataObjId, MetadataModifier)
execUntrackNativeQuery q metadata = do
  -- we do not check for feature flag here as we always want users to be able
  -- to remove native queries if they'd like
  assertNativeQueryExists @b source fieldName metadata

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

-- | Check whether a native query with the given root field name exists for
-- the given source.
assertNativeQueryExists ::
  forall b m. (Backend b, MonadError QErr m) => SourceName -> NativeQueryName -> Metadata -> m ()
assertNativeQueryExists sourceName rootFieldName metadata = do
  let sourceMetadataTraversal :: Traversal' Metadata (SourceMetadata b)
      sourceMetadataTraversal = metaSources . ix sourceName . toSourceMetadata @b

  sourceMetadata <-
    preview sourceMetadataTraversal metadata
      `onNothing` throw400 NotFound ("Source " <> sourceName <<> " not found.")

  let desiredNativeQuery :: Traversal' (SourceMetadata b) (NativeQueryMetadata b)
      desiredNativeQuery = smNativeQueries . ix rootFieldName

  unless (has desiredNativeQuery sourceMetadata) do
    throw400 NotFound ("Native query " <> rootFieldName <<> " not found in source " <> sourceName <<> ".")
