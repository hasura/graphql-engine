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
    TrackNativeQuery (..),
    UntrackNativeQuery (..),
    runGetNativeQuery,
    runTrackNativeQuery,
    runUntrackNativeQuery,
    dropNativeQueryInMetadata,
    module Hasura.NativeQuery.Types,
  )
where

import Autodocodec (HasCodec)
import Autodocodec qualified as AC
import Control.Lens (preview, (^?))
import Data.Aeson
import Data.Environment qualified as Env
import Hasura.Base.Error
import Hasura.CustomReturnType (CustomReturnType)
import Hasura.EncJSON
import Hasura.Metadata.DTO.Utils (codecNamePrefix)
import Hasura.NativeQuery.Metadata (NativeQueryArgumentName, NativeQueryInfo (..), parseInterpolatedQuery)
import Hasura.NativeQuery.Types
import Hasura.Prelude
import Hasura.RQL.Types.Backend (Backend, ScalarType, SourceConnConfiguration)
import Hasura.RQL.Types.Common (SourceName, sourceNameToText, successMsg)
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Backend
import Hasura.Server.Init.FeatureFlag as FF
import Hasura.Server.Types (HasServerConfigCtx (..), ServerConfigCtx (..))
import Language.GraphQL.Draft.Syntax (unName)

-- | Default implementation of the 'track_native_query' request payload.
data TrackNativeQuery (b :: BackendType) = TrackNativeQuery
  { tnqSource :: SourceName,
    tnqRootFieldName :: NativeQueryName,
    tnqCode :: Text,
    tnqArguments :: HashMap NativeQueryArgumentName (ScalarType b),
    tnqDescription :: Maybe Text,
    tnqReturns :: CustomReturnType b
  }

instance (Backend b) => HasCodec (TrackNativeQuery b) where
  codec =
    AC.CommentCodec
      ("A request to track a native query")
      $ AC.object (codecNamePrefix @b <> "TrackNativeQuery")
      $ TrackNativeQuery
        <$> AC.requiredField "source" sourceDoc
          AC..= tnqSource
        <*> AC.requiredField "root_field_name" rootFieldDoc
          AC..= tnqRootFieldName
        <*> AC.requiredField "code" codeDoc
          AC..= tnqCode
        <*> AC.optionalFieldWithDefault "arguments" mempty argumentsDoc
          AC..= tnqArguments
        <*> AC.optionalField "description" descriptionDoc
          AC..= tnqDescription
        <*> AC.requiredField "returns" returnsDoc
          AC..= tnqReturns
    where
      sourceDoc = "The source in whic this native query should be tracked"
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

-- | Default implementation of the method 'nativeQueryTrackToInfo'.
nativeQueryTrackToInfo ::
  forall b m.
  ( BackendMetadata b,
    MonadIO m,
    MonadError QErr m
  ) =>
  Env.Environment ->
  SourceConnConfiguration b ->
  TrackNativeQuery b ->
  m (NativeQueryInfo b)
nativeQueryTrackToInfo env sourceConnConfig TrackNativeQuery {..} = do
  nqiCode <- parseInterpolatedQuery tnqCode `onLeft` \e -> throw400 ParseFailed e
  let nqiRootFieldName = tnqRootFieldName
      nqiReturns = tnqReturns
      nqiArguments = tnqArguments
      nqiDescription = tnqDescription
      nqInfoImpl = NativeQueryInfo {..}

  validateNativeQuery @b env sourceConnConfig nqInfoImpl

  pure nqInfoImpl

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
  Env.Environment ->
  TrackNativeQuery b ->
  m EncJSON
runTrackNativeQuery env trackNativeQueryRequest = do
  throwIfFeatureDisabled

  sourceConnConfig <-
    maybe (throw400 NotFound $ "Source " <> sourceNameToText source <> " not found.") pure
      . preview (metaSources . ix source . toSourceMetadata @b . smConfiguration)
      =<< getMetadata

  (metadata :: NativeQueryInfo b) <- do
    liftIO (runExceptT (nativeQueryTrackToInfo @b env sourceConnConfig trackNativeQueryRequest))
      `onLeftM` throwError

  let fieldName = nqiRootFieldName metadata
      metadataObj =
        MOSourceObjId source $
          AB.mkAnyBackend $
            SMONativeQuery @b fieldName

  buildSchemaCacheFor metadataObj $
    MetadataModifier $
      (metaSources . ix source . toSourceMetadata @b . smNativeQueries)
        %~ \nqs -> metadata : (filter ((/= fieldName) . nqiRootFieldName) nqs)

  pure successMsg
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
    MetadataM m,
    HasServerConfigCtx m,
    MonadIO m
  ) =>
  UntrackNativeQuery b ->
  m EncJSON
runUntrackNativeQuery q = do
  throwIfFeatureDisabled

  -- Check source exists
  sourceMetadata <-
    maybe (throw400 NotFound $ "Source " <> sourceNameToText source <> " not found.") pure
      . preview (metaSources . ix source . toSourceMetadata @b)
      =<< getMetadata

  -- Check native query exists
  unless (any ((== fieldName) . nqiRootFieldName) $ _smNativeQueries sourceMetadata) do
    throw400 NotFound $ "Native query '" <> unName (getNativeQueryName fieldName) <> "' not found in source '" <> sourceNameToText source <> "'."

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
    metaSources . ix source . toSourceMetadata @b . smNativeQueries %~ filter ((/= rootFieldName) . nqiRootFieldName)

-- | check feature flag is enabled before carrying out any actions
throwIfFeatureDisabled :: (HasServerConfigCtx m, MonadIO m, MonadError QErr m) => m ()
throwIfFeatureDisabled = do
  configCtx <- askServerConfigCtx

  enableNativeQuery <- liftIO (_sccCheckFeatureFlag configCtx FF.nativeQueryInterface)

  unless enableNativeQuery (throw500 "NativeQuery is disabled!")
