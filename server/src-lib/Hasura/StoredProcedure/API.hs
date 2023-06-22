{-# LANGUAGE UndecidableInstances #-}

-- | Define and handle v1/metadata API operations to track, untrack, and get stored procedures.
module Hasura.StoredProcedure.API
  ( GetStoredProcedure (..),
    TrackStoredProcedure (..),
    UntrackStoredProcedure (..),
    runGetStoredProcedure,
    runTrackStoredProcedure,
    runUntrackStoredProcedure,
    dropStoredProcedureInMetadata,
    module Hasura.StoredProcedure.Types,
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
import Hasura.LogicalModel.Metadata (LogicalModelName)
import Hasura.Prelude
import Hasura.RQL.Types.Backend (Backend, FunctionName)
import Hasura.RQL.Types.BackendTag
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
  ( SourceName,
    sourceNameToText,
    successMsg,
  )
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.StoredProcedure.Metadata (ArgumentName, StoredProcedureMetadata (..))
import Hasura.StoredProcedure.Types

-- | Default implementation of the 'track_stored_procedure_query' request payload.
data TrackStoredProcedure (b :: BackendType) = TrackStoredProcedure
  { tspSource :: SourceName,
    tspStoredProcedure :: FunctionName b,
    tspConfig :: StoredProcedureConfig,
    tspArguments :: HashMap ArgumentName (NullableScalarType b),
    tspDescription :: Maybe Text,
    tspReturns :: LogicalModelName
  }

instance (Backend b) => HasCodec (TrackStoredProcedure b) where
  codec =
    AC.CommentCodec
      ("A request to track a stored procedure")
      $ AC.object (backendPrefix @b <> "TrackStoredProcedure")
      $ TrackStoredProcedure
      <$> AC.requiredField "source" sourceDoc
      AC..= tspSource
        <*> AC.requiredField "stored_procedure" spDoc
      AC..= tspStoredProcedure
        <*> AC.requiredField "configuration" configDoc
      AC..= tspConfig
        <*> AC.optionalFieldWithDefault "arguments" mempty argumentsDoc
      AC..= tspArguments
        <*> AC.optionalField "description" descriptionDoc
      AC..= tspDescription
        <*> AC.requiredField "returns" returnsDoc
      AC..= tspReturns
    where
      sourceDoc = "The source in which this stored procedure should be tracked"
      configDoc = "The configuration for the SQL stored procedure"
      spDoc = "The name of the SQL stored procedure"
      argumentsDoc = "Free variables in the expression and their types"
      returnsDoc = "Return type (table) of the expression"
      descriptionDoc = "A description of the query which appears in the graphql schema"

deriving via
  (AC.Autodocodec (TrackStoredProcedure b))
  instance
    (Backend b) => FromJSON (TrackStoredProcedure b)

deriving via
  (AC.Autodocodec (TrackStoredProcedure b))
  instance
    (Backend b) => ToJSON (TrackStoredProcedure b)

-- Configuration

-- | API payload for the 'get_stored_procedure' endpoint.
data GetStoredProcedure (b :: BackendType) = GetStoredProcedure
  { gspSource :: SourceName
  }

deriving instance (Backend b) => Show (GetStoredProcedure b)

deriving instance (Backend b) => Eq (GetStoredProcedure b)

instance (Backend b) => FromJSON (GetStoredProcedure b) where
  parseJSON = withObject "GetStoredProcedure" $ \o -> do
    gspSource <- o .: "source"
    pure GetStoredProcedure {..}

instance (Backend b) => ToJSON (GetStoredProcedure b) where
  toJSON GetStoredProcedure {..} =
    object
      [ "source" .= gspSource
      ]

-- | Handler for the 'get_stored_procedure' endpoint.
runGetStoredProcedure ::
  forall b m.
  ( BackendMetadata b,
    MetadataM m
  ) =>
  GetStoredProcedure b ->
  m EncJSON
runGetStoredProcedure q = do
  metadata <- getMetadata

  let storedProcedure :: Maybe (StoredProcedures b)
      storedProcedure = metadata ^? metaSources . ix (gspSource q) . toSourceMetadata . smStoredProcedures @b

  pure (encJFromJValue (InsOrdHashMap.elems <$> storedProcedure))

-- | Handler for the 'track_stored_procedure' endpoint. The type 'TrackStoredProcedure b'
-- (appearing here in wrapped as 'BackendTrackStoredProcedure b' for 'AnyBackend'
-- compatibility) is defined in 'class StoredProcedureMetadata'.
runTrackStoredProcedure ::
  forall b m.
  ( BackendMetadata b,
    MonadError QErr m,
    CacheRWM m,
    MetadataM m
  ) =>
  TrackStoredProcedure b ->
  m EncJSON
runTrackStoredProcedure TrackStoredProcedure {..} = do
  sourceMetadata <-
    maybe
      ( throw400 NotFound
          $ "Source '"
          <> sourceNameToText tspSource
          <> "' of kind "
          <> toTxt (reify (backendTag @b))
          <> " not found."
      )
      pure
      . preview (metaSources . ix tspSource . toSourceMetadata @b)
      =<< getMetadata

  let metadata =
        StoredProcedureMetadata
          { _spmStoredProcedure = tspStoredProcedure,
            _spmConfig = tspConfig,
            _spmReturns = tspReturns,
            _spmArguments = tspArguments,
            _spmDescription = tspDescription
          }

  let storedProcedure = _spmStoredProcedure metadata
      metadataObj =
        MOSourceObjId tspSource
          $ AB.mkAnyBackend
          $ SMOStoredProcedure @b storedProcedure
      existingStoredProcedures = InsOrdHashMap.keys (_smStoredProcedures sourceMetadata)
  when (storedProcedure `elem` existingStoredProcedures) do
    throw400 AlreadyTracked $ "Stored procedure '" <> toTxt storedProcedure <> "' is already tracked."

  buildSchemaCacheFor metadataObj
    $ MetadataModifier
    $ (metaSources . ix tspSource . toSourceMetadata @b . smStoredProcedures)
    %~ InsOrdHashMap.insert storedProcedure metadata

  pure successMsg

-- | API payload for the 'untrack_stored_procedure' endpoint.
data UntrackStoredProcedure (b :: BackendType) = UntrackStoredProcedure
  { utspSource :: SourceName,
    utspStoredProcedure :: FunctionName b
  }

deriving instance (Backend b) => Show (UntrackStoredProcedure b)

deriving instance (Backend b) => Eq (UntrackStoredProcedure b)

instance (Backend b) => FromJSON (UntrackStoredProcedure b) where
  parseJSON = withObject "UntrackStoredProcedure" $ \o -> do
    utspSource <- o .: "source"
    utspStoredProcedure <- o .: "stored_procedure"
    pure UntrackStoredProcedure {..}

instance (Backend b) => ToJSON (UntrackStoredProcedure b) where
  toJSON UntrackStoredProcedure {..} =
    object
      [ "source" .= utspSource,
        "stored_procedure" .= utspStoredProcedure
      ]

-- | Handler for the 'untrack_stored_procedure' endpoint.
runUntrackStoredProcedure ::
  forall b m.
  ( BackendMetadata b,
    MonadError QErr m,
    CacheRWM m,
    MetadataM m
  ) =>
  UntrackStoredProcedure b ->
  m EncJSON
runUntrackStoredProcedure q = do
  -- we do not check for feature flag here as we always want users to be able
  -- to remove stored procedures if they'd like
  assertStoredProcedureExists @b source storedProcedure

  let metadataObj =
        MOSourceObjId source
          $ AB.mkAnyBackend
          $ SMOStoredProcedure @b storedProcedure

  buildSchemaCacheFor metadataObj
    $ dropStoredProcedureInMetadata @b source storedProcedure
  pure successMsg
  where
    source = utspSource q
    storedProcedure = utspStoredProcedure q

dropStoredProcedureInMetadata ::
  forall b.
  (BackendMetadata b) =>
  SourceName ->
  FunctionName b ->
  MetadataModifier
dropStoredProcedureInMetadata source rootFieldName = do
  MetadataModifier
    $ metaSources
    . ix source
    . toSourceMetadata @b
    . smStoredProcedures
    %~ InsOrdHashMap.delete rootFieldName

-- | Check whether a stored procedure exists for the given source.
assertStoredProcedureExists ::
  forall b m.
  (Backend b, MetadataM m, MonadError QErr m) =>
  SourceName ->
  FunctionName b ->
  m ()
assertStoredProcedureExists sourceName storedProcedure = do
  metadata <- getMetadata

  let sourceMetadataTraversal :: Traversal' Metadata (SourceMetadata b)
      sourceMetadataTraversal = metaSources . ix sourceName . toSourceMetadata @b

  sourceMetadata <-
    preview sourceMetadataTraversal metadata
      `onNothing` throw400 NotFound ("Source " <> sourceName <<> " not found.")

  let desiredStoredProcedure :: Traversal' (SourceMetadata b) (StoredProcedureMetadata b)
      desiredStoredProcedure = smStoredProcedures . ix storedProcedure

  unless (has desiredStoredProcedure sourceMetadata) do
    throw400 NotFound ("Stored Procedure " <> toTxt storedProcedure <<> " not found in source " <> sourceName <<> ".")
