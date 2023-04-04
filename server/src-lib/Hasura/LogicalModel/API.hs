{-# LANGUAGE UndecidableInstances #-}

-- | Define and handle v1/metadata API operations to track, untrack, and get logical models.
module Hasura.LogicalModel.API
  ( GetLogicalModel (..),
    TrackLogicalModel (..),
    UntrackLogicalModel (..),
    runGetLogicalModel,
    runTrackLogicalModel,
    runUntrackLogicalModel,
    dropLogicalModelInMetadata,
    module Hasura.LogicalModel.Types,
  )
where

import Autodocodec (HasCodec)
import Autodocodec qualified as AC
import Control.Lens (Traversal', has, preview, (^?))
import Data.Aeson
import Data.Environment qualified as Env
import Data.HashMap.Strict.InsOrd.Extended qualified as OMap
import Data.Text.Extended (toTxt, (<<>))
import Hasura.Base.Error
import Hasura.CustomReturnType.API (getCustomTypes)
import Hasura.CustomReturnType.Metadata (CustomReturnTypeName)
import Hasura.EncJSON
import Hasura.LogicalModel.Metadata (LogicalModelArgumentName, LogicalModelMetadata (..), parseInterpolatedQuery)
import Hasura.LogicalModel.Types (LogicalModelName, NullableScalarType)
import Hasura.Metadata.DTO.Utils (codecNamePrefix)
import Hasura.Prelude
import Hasura.RQL.Types.Backend (Backend, SourceConnConfiguration)
import Hasura.RQL.Types.Common (SourceName, sourceNameToText, successMsg)
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Backend
import Hasura.SQL.Tag
import Hasura.Server.Init.FeatureFlag as FF
import Hasura.Server.Types (HasServerConfigCtx (..), ServerConfigCtx (..))

-- | Default implementation of the 'track_logical_model' request payload.
data TrackLogicalModel (b :: BackendType) = TrackLogicalModel
  { tlmSource :: SourceName,
    tlmRootFieldName :: LogicalModelName,
    tlmCode :: Text,
    tlmArguments :: HashMap LogicalModelArgumentName (NullableScalarType b),
    tlmDescription :: Maybe Text,
    tlmReturns :: CustomReturnTypeName
  }

instance (Backend b) => HasCodec (TrackLogicalModel b) where
  codec =
    AC.CommentCodec
      ("A request to track a logical model")
      $ AC.object (codecNamePrefix @b <> "TrackLogicalModel")
      $ TrackLogicalModel
        <$> AC.requiredField "source" sourceDoc
          AC..= tlmSource
        <*> AC.requiredField "root_field_name" rootFieldDoc
          AC..= tlmRootFieldName
        <*> AC.requiredField "code" codeDoc
          AC..= tlmCode
        <*> AC.optionalFieldWithDefault "arguments" mempty argumentsDoc
          AC..= tlmArguments
        <*> AC.optionalField "description" descriptionDoc
          AC..= tlmDescription
        <*> AC.requiredField "returns" returnsDoc
          AC..= tlmReturns
    where
      sourceDoc = "The source in which this logical model should be tracked"
      rootFieldDoc = "Root field name for the logical model"
      codeDoc = "Native code expression (SQL) to run"
      argumentsDoc = "Free variables in the expression and their types"
      returnsDoc = "Return type (table) of the expression"
      descriptionDoc = "A description of the query which appears in the graphql schema"

deriving via
  (AC.Autodocodec (TrackLogicalModel b))
  instance
    (Backend b) => FromJSON (TrackLogicalModel b)

deriving via
  (AC.Autodocodec (TrackLogicalModel b))
  instance
    (Backend b) => ToJSON (TrackLogicalModel b)

-- | Validate a logical model and extract the logical model info from the request.
logicalModelTrackToMetadata ::
  forall b m.
  ( BackendMetadata b,
    MetadataM m,
    MonadIO m,
    MonadError QErr m
  ) =>
  Env.Environment ->
  SourceConnConfiguration b ->
  TrackLogicalModel b ->
  m (LogicalModelMetadata b)
logicalModelTrackToMetadata env sourceConnConfig TrackLogicalModel {..} = do
  code <- parseInterpolatedQuery tlmCode `onLeft` \e -> throw400 ParseFailed e

  let logicalModelMetadata =
        LogicalModelMetadata
          { _lmmRootFieldName = tlmRootFieldName,
            _lmmCode = code,
            _lmmReturns = tlmReturns,
            _lmmArguments = tlmArguments,
            _lmmDescription = tlmDescription
          }

  metadata <- getMetadata

  -- lookup custom return type in existing metadata
  case metadata ^? getCustomTypes tlmSource . ix tlmReturns of
    Just customReturnType ->
      validateLogicalModel @b env sourceConnConfig customReturnType logicalModelMetadata
    Nothing -> throw400 NotFound ("Custom return type " <> tlmReturns <<> " not found.")

  pure logicalModelMetadata

-- | API payload for the 'get_logical_model' endpoint.
data GetLogicalModel (b :: BackendType) = GetLogicalModel
  { glmSource :: SourceName
  }

deriving instance Backend b => Show (GetLogicalModel b)

deriving instance Backend b => Eq (GetLogicalModel b)

instance Backend b => FromJSON (GetLogicalModel b) where
  parseJSON = withObject "GetLogicalModel" $ \o -> do
    glmSource <- o .: "source"
    pure GetLogicalModel {..}

instance Backend b => ToJSON (GetLogicalModel b) where
  toJSON GetLogicalModel {..} =
    object
      [ "source" .= glmSource
      ]

-- | Handler for the 'get_logical_model' endpoint.
runGetLogicalModel ::
  forall b m.
  ( BackendMetadata b,
    MetadataM m,
    HasServerConfigCtx m,
    MonadIO m,
    MonadError QErr m
  ) =>
  GetLogicalModel b ->
  m EncJSON
runGetLogicalModel q = do
  throwIfFeatureDisabled

  metadata <- getMetadata

  let logicalModel :: Maybe (LogicalModels b)
      logicalModel = metadata ^? metaSources . ix (glmSource q) . toSourceMetadata . smLogicalModels @b

  pure (encJFromJValue (OMap.elems <$> logicalModel))

-- | Handler for the 'track_logical_model' endpoint. The type 'TrackLogicalModel b'
-- (appearing here in wrapped as 'BackendTrackLogicalModel b' for 'AnyBackend'
-- compatibility) is defined in 'class LogicalModelMetadata'.
runTrackLogicalModel ::
  forall b m.
  ( BackendMetadata b,
    CacheRWM m,
    MetadataM m,
    MonadError QErr m,
    HasServerConfigCtx m,
    MonadIO m
  ) =>
  Env.Environment ->
  TrackLogicalModel b ->
  m EncJSON
runTrackLogicalModel env trackLogicalModelRequest = do
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

  (metadata :: LogicalModelMetadata b) <- do
    logicalModelTrackToMetadata @b env sourceConnConfig trackLogicalModelRequest

  let fieldName = _lmmRootFieldName metadata
      metadataObj =
        MOSourceObjId source $
          AB.mkAnyBackend $
            SMOLogicalModel @b fieldName
      existingLogicalModels = OMap.keys (_smLogicalModels sourceMetadata)

  when (fieldName `elem` existingLogicalModels) do
    throw400 AlreadyTracked $ "Logical model '" <> toTxt fieldName <> "' is already tracked."

  buildSchemaCacheFor metadataObj $
    MetadataModifier $
      (metaSources . ix source . toSourceMetadata @b . smLogicalModels)
        %~ OMap.insert fieldName metadata

  pure successMsg
  where
    source = tlmSource trackLogicalModelRequest

-- | API payload for the 'untrack_logical_model' endpoint.
data UntrackLogicalModel (b :: BackendType) = UntrackLogicalModel
  { utlmSource :: SourceName,
    utlmRootFieldName :: LogicalModelName
  }

deriving instance Show (UntrackLogicalModel b)

deriving instance Eq (UntrackLogicalModel b)

instance FromJSON (UntrackLogicalModel b) where
  parseJSON = withObject "UntrackLogicalModel" $ \o -> do
    utlmSource <- o .: "source"
    utlmRootFieldName <- o .: "root_field_name"
    pure UntrackLogicalModel {..}

instance ToJSON (UntrackLogicalModel b) where
  toJSON UntrackLogicalModel {..} =
    object
      [ "source" .= utlmSource,
        "root_field_name" .= utlmRootFieldName
      ]

-- | Handler for the 'untrack_logical_model' endpoint.
runUntrackLogicalModel ::
  forall b m.
  ( BackendMetadata b,
    MonadError QErr m,
    CacheRWM m,
    MetadataM m
  ) =>
  UntrackLogicalModel b ->
  m EncJSON
runUntrackLogicalModel q = do
  -- we do not check for feature flag here as we always want users to be able
  -- to remove logical models if they'd like
  assertLogicalModelExists @b source fieldName

  let metadataObj =
        MOSourceObjId source $
          AB.mkAnyBackend $
            SMOLogicalModel @b fieldName

  buildSchemaCacheFor metadataObj $
    dropLogicalModelInMetadata @b source fieldName

  pure successMsg
  where
    source = utlmSource q
    fieldName = utlmRootFieldName q

dropLogicalModelInMetadata :: forall b. BackendMetadata b => SourceName -> LogicalModelName -> MetadataModifier
dropLogicalModelInMetadata source rootFieldName = do
  MetadataModifier $
    metaSources . ix source . toSourceMetadata @b . smLogicalModels
      %~ OMap.delete rootFieldName

-- | check feature flag is enabled before carrying out any actions
throwIfFeatureDisabled :: (HasServerConfigCtx m, MonadIO m, MonadError QErr m) => m ()
throwIfFeatureDisabled = do
  configCtx <- askServerConfigCtx
  let CheckFeatureFlag runCheckFeatureFlag = _sccCheckFeatureFlag configCtx

  enableLogicalModels <- liftIO (runCheckFeatureFlag FF.logicalModelInterface)

  unless enableLogicalModels (throw500 "LogicalModels is disabled!")

-- | Check whether a logical model with the given root field name exists for
-- the given source.
assertLogicalModelExists :: forall b m. (Backend b, MetadataM m, MonadError QErr m) => SourceName -> LogicalModelName -> m ()
assertLogicalModelExists sourceName rootFieldName = do
  metadata <- getMetadata

  let sourceMetadataTraversal :: Traversal' Metadata (SourceMetadata b)
      sourceMetadataTraversal = metaSources . ix sourceName . toSourceMetadata @b

  sourceMetadata <-
    preview sourceMetadataTraversal metadata
      `onNothing` throw400 NotFound ("Source " <> sourceName <<> " not found.")

  let desiredLogicalModel :: Traversal' (SourceMetadata b) (LogicalModelMetadata b)
      desiredLogicalModel = smLogicalModels . ix rootFieldName

  unless (has desiredLogicalModel sourceMetadata) do
    throw400 NotFound ("Logical model " <> rootFieldName <<> " not found in source " <> sourceName <<> ".")
