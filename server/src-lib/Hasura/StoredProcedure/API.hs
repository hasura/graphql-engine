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
import Data.Environment qualified as Env
import Data.HashMap.Strict.InsOrd.Extended qualified as InsOrdHashMap
import Data.Text.Extended (toTxt, (<<>))
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.LogicalModel.API (getCustomTypes)
import Hasura.LogicalModel.Metadata (LogicalModelName)
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
import Hasura.RQL.Types.Relationships.Local (RelDef, RelManualConfig)
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.Init.FeatureFlag (HasFeatureFlagChecker (..))
import Hasura.Server.Init.FeatureFlag qualified as FF
import Hasura.StoredProcedure.Metadata (ArgumentName, StoredProcedureMetadata (..), parseInterpolatedQuery)
import Hasura.StoredProcedure.Types (NullableScalarType, StoredProcedureName, storedProcedureArrayRelationshipsCodec)

-- | Default implementation of the 'track_stored_procedure_query' request payload.
data TrackStoredProcedure (b :: BackendType) = TrackStoredProcedure
  { tspSource :: SourceName,
    tspRootFieldName :: StoredProcedureName,
    tspCode :: Text,
    tspArguments :: HashMap ArgumentName (NullableScalarType b),
    tspArrayRelationships :: InsOrdHashMap.InsOrdHashMap RelName (RelDef (RelManualConfig b)),
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
        <*> AC.requiredField "root_field_name" rootFieldDoc
          AC..= tspRootFieldName
        <*> AC.requiredField "code" codeDoc
          AC..= tspCode
        <*> AC.optionalFieldWithDefault "arguments" mempty argumentsDoc
          AC..= tspArguments
        <*> AC.optionalFieldWithDefaultWith "array_relationships" storedProcedureArrayRelationshipsCodec mempty arrayRelationshipsDoc
          AC..= tspArrayRelationships
        <*> AC.optionalField "description" descriptionDoc
          AC..= tspDescription
        <*> AC.requiredField "returns" returnsDoc
          AC..= tspReturns
    where
      arrayRelationshipsDoc = "Any relationships between an output value and multiple values in another data source"
      sourceDoc = "The source in which this stored procedure should be tracked"
      rootFieldDoc = "Root field name for the stored procedure"
      codeDoc = "Native code expression (SQL) to run"
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

-- | Validate a stored procedure and extract the stored procedure info from the request.
storedProcedureTrackToMetadata ::
  forall b m.
  ( BackendMetadata b,
    MonadError QErr m,
    MonadIO m,
    MetadataM m
  ) =>
  Env.Environment ->
  SourceConnConfiguration b ->
  TrackStoredProcedure b ->
  m (StoredProcedureMetadata b)
storedProcedureTrackToMetadata env sourceConnConfig TrackStoredProcedure {..} = do
  code <- parseInterpolatedQuery tspCode `onLeft` \e -> throw400 ParseFailed e

  let storedProcedureMetadata =
        StoredProcedureMetadata
          { _spmRootFieldName = tspRootFieldName,
            _spmCode = code,
            _spmReturns = tspReturns,
            _spmArguments = tspArguments,
            _spmArrayRelationships = tspArrayRelationships,
            _spmDescription = tspDescription
          }

  metadata <- getMetadata

  -- lookup logical model in existing metadata
  case metadata ^? getCustomTypes tspSource . ix tspReturns of
    Just logicalModel ->
      validateStoredProcedure @b env sourceConnConfig logicalModel storedProcedureMetadata
    Nothing -> throw400 NotFound ("Logical model " <> tspReturns <<> " not found.")

  pure storedProcedureMetadata

-- | API payload for the 'get_stored_procedure' endpoint.
data GetStoredProcedure (b :: BackendType) = GetStoredProcedure
  { gspSource :: SourceName
  }

deriving instance Backend b => Show (GetStoredProcedure b)

deriving instance Backend b => Eq (GetStoredProcedure b)

instance Backend b => FromJSON (GetStoredProcedure b) where
  parseJSON = withObject "GetStoredProcedure" $ \o -> do
    gspSource <- o .: "source"
    pure GetStoredProcedure {..}

instance Backend b => ToJSON (GetStoredProcedure b) where
  toJSON GetStoredProcedure {..} =
    object
      [ "source" .= gspSource
      ]

-- | Handler for the 'get_stored_procedure' endpoint.
runGetStoredProcedure ::
  forall b m.
  ( BackendMetadata b,
    MetadataM m,
    HasFeatureFlagChecker m,
    MonadError QErr m
  ) =>
  GetStoredProcedure b ->
  m EncJSON
runGetStoredProcedure q = do
  throwIfFeatureDisabled

  metadata <- getMetadata

  let storedProcedure :: Maybe (StoredProcedures b)
      storedProcedure = metadata ^? metaSources . ix (gspSource q) . toSourceMetadata . smStoredProcedures @b

  pure (encJFromJValue (InsOrdHashMap.elems <$> storedProcedure))

-- | Handler for the 'track_native_query' endpoint. The type 'TrackStoredProcedure b'
-- (appearing here in wrapped as 'BackendTrackStoredProcedure b' for 'AnyBackend'
-- compatibility) is defined in 'class StoredProcedureMetadata'.
runTrackStoredProcedure ::
  forall b m.
  ( BackendMetadata b,
    MonadError QErr m,
    MonadIO m,
    CacheRWM m,
    MetadataM m,
    HasFeatureFlagChecker m
  ) =>
  Env.Environment ->
  TrackStoredProcedure b ->
  m EncJSON
runTrackStoredProcedure env trackStoredProcedureRequest = do
  throwIfFeatureDisabled

  sourceMetadata <-
    maybe
      ( throw400 NotFound $
          "Source '"
            <> sourceNameToText source
            <> "' of kind "
            <> toTxt (reify (backendTag @b))
            <> " not found."
      )
      pure
      . preview (metaSources . ix source . toSourceMetadata @b)
      =<< getMetadata
  let sourceConnConfig = _smConfiguration sourceMetadata

  (metadata :: StoredProcedureMetadata b) <- do
    storedProcedureTrackToMetadata @b env sourceConnConfig trackStoredProcedureRequest

  let fieldName = _spmRootFieldName metadata
      metadataObj =
        MOSourceObjId source $
          AB.mkAnyBackend $
            SMOStoredProcedure @b fieldName
      existingStoredProcedures = InsOrdHashMap.keys (_smStoredProcedures sourceMetadata)
  when (fieldName `elem` existingStoredProcedures) do
    throw400 AlreadyTracked $ "Stored procedure '" <> toTxt fieldName <> "' is already tracked."

  buildSchemaCacheFor metadataObj $
    MetadataModifier $
      (metaSources . ix source . toSourceMetadata @b . smStoredProcedures)
        %~ InsOrdHashMap.insert fieldName metadata

  pure successMsg
  where
    source = tspSource trackStoredProcedureRequest

-- | API payload for the 'untrack_stored_procedure' endpoint.
data UntrackStoredProcedure (b :: BackendType) = UntrackStoredProcedure
  { utspSource :: SourceName,
    utspRootFieldName :: StoredProcedureName
  }

deriving instance Show (UntrackStoredProcedure b)

deriving instance Eq (UntrackStoredProcedure b)

instance FromJSON (UntrackStoredProcedure b) where
  parseJSON = withObject "UntrackStoredProcedure" $ \o -> do
    utspSource <- o .: "source"
    utspRootFieldName <- o .: "root_field_name"
    pure UntrackStoredProcedure {..}

instance ToJSON (UntrackStoredProcedure b) where
  toJSON UntrackStoredProcedure {..} =
    object
      [ "source" .= utspSource,
        "root_field_name" .= utspRootFieldName
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
  assertStoredProcedureExists @b source fieldName

  let metadataObj =
        MOSourceObjId source $
          AB.mkAnyBackend $
            SMOStoredProcedure @b fieldName

  buildSchemaCacheFor metadataObj $
    dropStoredProcedureInMetadata @b source fieldName -- TODO pass `@b` again
  pure successMsg
  where
    source = utspSource q
    fieldName = utspRootFieldName q

dropStoredProcedureInMetadata ::
  forall b.
  BackendMetadata b =>
  SourceName ->
  StoredProcedureName ->
  MetadataModifier
dropStoredProcedureInMetadata source rootFieldName = do
  MetadataModifier $
    metaSources . ix source . toSourceMetadata @b . smStoredProcedures
      %~ InsOrdHashMap.delete rootFieldName

-- | check feature flag is enabled before carrying out any actions
throwIfFeatureDisabled :: (HasFeatureFlagChecker m, MonadError QErr m) => m ()
throwIfFeatureDisabled = do
  enableStoredProcedures <- checkFlag FF.nativeQueryInterface -- TODO: make our own feature flag
  unless enableStoredProcedures (throw500 "Stored Procedures are disabled!")

-- | Check whether a native query with the given root field name exists for
-- the given source.
assertStoredProcedureExists :: forall b m. (Backend b, MetadataM m, MonadError QErr m) => SourceName -> StoredProcedureName -> m ()
assertStoredProcedureExists sourceName rootFieldName = do
  metadata <- getMetadata

  let sourceMetadataTraversal :: Traversal' Metadata (SourceMetadata b)
      sourceMetadataTraversal = metaSources . ix sourceName . toSourceMetadata @b

  sourceMetadata <-
    preview sourceMetadataTraversal metadata
      `onNothing` throw400 NotFound ("Source " <> sourceName <<> " not found.")

  let desiredStoredProcedure :: Traversal' (SourceMetadata b) (StoredProcedureMetadata b)
      desiredStoredProcedure = smStoredProcedures . ix rootFieldName

  unless (has desiredStoredProcedure sourceMetadata) do
    throw400 NotFound ("Stored Procedure " <> rootFieldName <<> " not found in source " <> sourceName <<> ".")
