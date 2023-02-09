{-# LANGUAGE UndecidableInstances #-}

-- | This module houses the function associated with the default implementation
-- of Metadata V1 commands (and their types) for handling user-specified Native
-- Query fragments.
--
-- The definitions herein ought to suffice for any instantiation of Native
-- Queries that only deviates in the contents of the 'TrackNativeQuery' payload.
-- And as such, the metadata endpoint 'Hasura.Server.API.Metadata' is hardwired
-- directly to this module without any overloading provided.
module Hasura.NativeQuery.API
  ( GetNativeQuery (..),
    UntrackNativeQuery (..),
    runGetNativeQuery,
    runTrackNativeQuery,
    runUntrackNativeQuery,
    dropNativeQueryInMetadata,
    module Hasura.NativeQuery.Types,
  )
where

import Control.Lens (preview, (^?))
import Data.Aeson
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.NativeQuery.Types
import Hasura.Prelude
import Hasura.RQL.Types.Backend (Backend)
import Hasura.RQL.Types.Common (SourceName, sourceNameToText, successMsg)
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Backend
import Hasura.Server.Init.FeatureFlag as FF
import Hasura.Server.Types (HasServerConfigCtx (..), ServerConfigCtx (..))

-- | API payload for the 'get_native_query' endpoint.
data GetNativeQuery (b :: BackendType) = GetNativeQuery
  { gnqSource :: SourceName
  }

deriving instance Backend b => Show (GetNativeQuery b)

deriving instance Backend b => Eq (GetNativeQuery b)

instance Backend b => FromJSON (GetNativeQuery b) where
  parseJSON = withObject "GetNativeQuery" $ \o -> do
    gnqSource <- o .: "source"
    pure GetNativeQuery {..}

instance Backend b => ToJSON (GetNativeQuery b) where
  toJSON GetNativeQuery {..} =
    object
      [ "source" .= gnqSource
      ]

-- | Handler for the 'get_native_query' endpoint.
runGetNativeQuery ::
  forall b m.
  ( BackendMetadata b,
    MetadataM m,
    HasServerConfigCtx m,
    MonadIO m,
    MonadError QErr m
  ) =>
  GetNativeQuery b ->
  m EncJSON
runGetNativeQuery q = do
  throwIfFeatureDisabled

  metadata <- getMetadata

  let nativeQuery :: Maybe (NativeQueries b)
      nativeQuery = metadata ^? metaSources . ix (gnqSource q) . toSourceMetadata . smNativeQueries @b

  pure (encJFromJValue nativeQuery)

-- | Handler for the 'track_native_query' endpoint. The type 'TrackNativeQuery
-- b' (appearing here in wrapped as 'BackendTrackNativeQuery b' for 'AnyBackend'
-- compatibility) is defined in 'class NativeQueryMetadata'.
runTrackNativeQuery ::
  forall b m.
  ( BackendMetadata b,
    CacheRWM m,
    MetadataM m,
    MonadError QErr m,
    HasServerConfigCtx m,
    MonadIO m
  ) =>
  BackendTrackNativeQuery b ->
  m EncJSON
runTrackNativeQuery (BackendTrackNativeQuery trackNativeQueryRequest) = do
  throwIfFeatureDisabled

  sourceConnConfig <-
    maybe (throw400 NotFound $ "Source " <> sourceNameToText source <> " not found.") pure
      . preview (metaSources . ix source . toSourceMetadata @b . smConfiguration)
      =<< getMetadata

  (metadata :: NativeQueryInfo b) <- do
    r <- liftIO $ runExceptT $ nativeQueryTrackToInfo @b sourceConnConfig trackNativeQueryRequest
    case r of
      Right nq -> pure nq
      Left (NativeQueryParseError e) -> throw400 ParseFailed e
      Left (NativeQueryValidationError e) -> throwError e

  let fieldName = nativeQueryInfoName @b metadata
      metadataObj =
        MOSourceObjId source $
          AB.mkAnyBackend $
            SMONativeQuery @b fieldName

  buildSchemaCacheFor metadataObj $
    MetadataModifier $
      (metaSources . ix source . toSourceMetadata @b . smNativeQueries)
        %~ \nqs -> metadata : (filter ((/= fieldName) . nativeQueryInfoName @b) nqs)

  pure successMsg
  where
    source = trackNativeQuerySource @b trackNativeQueryRequest

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
    MetadataM m,
    HasServerConfigCtx m,
    MonadIO m
  ) =>
  UntrackNativeQuery b ->
  m EncJSON
runUntrackNativeQuery q = do
  throwIfFeatureDisabled

  let metadataObj =
        MOSourceObjId source $
          AB.mkAnyBackend $
            SMONativeQuery @b fieldName

  buildSchemaCacheFor metadataObj $
    dropNativeQueryInMetadata @b source fieldName

  pure successMsg
  where
    source = utnqSource q
    fieldName = utnqRootFieldName q

dropNativeQueryInMetadata :: forall b. BackendMetadata b => SourceName -> NativeQueryName -> MetadataModifier
dropNativeQueryInMetadata source rootFieldName =
  MetadataModifier $
    metaSources . ix source . toSourceMetadata @b . smNativeQueries %~ filter ((/= rootFieldName) . nativeQueryInfoName @b)

-- | check feature flag is enabled before carrying out any actions
throwIfFeatureDisabled :: (HasServerConfigCtx m, MonadIO m, MonadError QErr m) => m ()
throwIfFeatureDisabled = do
  configCtx <- askServerConfigCtx

  enableNativeQuery <- liftIO (_sccCheckFeatureFlag configCtx FF.nativeQueryInterface)

  unless enableNativeQuery (throw500 "NativeQuery is disabled!")
