{-# LANGUAGE UndecidableInstances #-}

-- | Define and handle v1/metadata API operations to track, untrack, and get custom return types.
module Hasura.CustomReturnType.API
  ( GetCustomReturnType (..),
    TrackCustomReturnType (..),
    UntrackCustomReturnType (..),
    runGetCustomReturnType,
    runTrackCustomReturnType,
    runUntrackCustomReturnType,
    dropCustomReturnTypeInMetadata,
    CreateCustomReturnTypePermission (..),
    DropCustomReturnTypePermission (..),
    runCreateSelectCustomReturnTypePermission,
    runDropSelectCustomReturnTypePermission,
    getCustomTypes,
    module Hasura.CustomReturnType.Types,
  )
where

import Autodocodec (HasCodec)
import Autodocodec qualified as AC
import Control.Lens (Traversal', has, preview, traversed, (^..), (^?))
import Data.Aeson
import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Data.HashMap.Strict.InsOrd.Extended qualified as OMap
import Data.Text.Extended (toTxt, (<<>))
import Hasura.Base.Error
import Hasura.CustomReturnType.Metadata (CustomReturnTypeMetadata (..), crtmSelectPermissions)
import Hasura.CustomReturnType.Types (CustomReturnTypeName)
import Hasura.EncJSON
import Hasura.LogicalModel.Metadata (LogicalModelMetadata (..))
import Hasura.LogicalModel.Types (NullableScalarType, nullableScalarTypeMapCodec)
import Hasura.Metadata.DTO.Utils (codecNamePrefix)
import Hasura.Prelude
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.RQL.Types.Common (SourceName, defaultSource, sourceNameToText, successMsg)
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.Permission (PermDef (_pdRole), SelPerm)
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Backend
import Hasura.Server.Init.FeatureFlag (HasFeatureFlagChecker (..))
import Hasura.Server.Init.FeatureFlag qualified as FF
import Hasura.Session (RoleName)

-- | Default implementation of the 'track_custom_return_type' request payload.
data TrackCustomReturnType (b :: BackendType) = TrackCustomReturnType
  { tctSource :: SourceName,
    tctName :: CustomReturnTypeName,
    tctDescription :: Maybe Text,
    tctFields :: InsOrd.InsOrdHashMap (Column b) (NullableScalarType b)
  }

instance (Backend b) => HasCodec (TrackCustomReturnType b) where
  codec =
    AC.CommentCodec
      ("A request to track a custom return type")
      $ AC.object (codecNamePrefix @b <> "TrackCustomReturnType")
      $ TrackCustomReturnType
        <$> AC.requiredField "source" sourceDoc
          AC..= tctSource
        <*> AC.requiredField "name" nameDoc
          AC..= tctName
        <*> AC.optionalField "description" descriptionDoc
          AC..= tctDescription
        <*> AC.requiredFieldWith "fields" nullableScalarTypeMapCodec fieldsDoc
          AC..= tctFields
    where
      sourceDoc = "The source in which this custom return type should be tracked"
      nameDoc = "Root field name for the custom return type"
      fieldsDoc = "Return type of the expression"
      descriptionDoc = "A description of the query which appears in the graphql schema"

deriving via
  (AC.Autodocodec (TrackCustomReturnType b))
  instance
    (Backend b) => FromJSON (TrackCustomReturnType b)

deriving via
  (AC.Autodocodec (TrackCustomReturnType b))
  instance
    (Backend b) => ToJSON (TrackCustomReturnType b)

-- | Validate a custom return type and extract the custom return type info from the request.
customTypeTrackToMetadata ::
  forall b.
  TrackCustomReturnType b ->
  CustomReturnTypeMetadata b
customTypeTrackToMetadata TrackCustomReturnType {..} =
  CustomReturnTypeMetadata
    { _crtmName = tctName,
      _crtmFields = tctFields,
      _crtmSelectPermissions = mempty,
      _crtmDescription = tctDescription
    }

-- | API payload for the 'get_custom_return_type' endpoint.
data GetCustomReturnType (b :: BackendType) = GetCustomReturnType
  { gcrtSource :: SourceName
  }

deriving instance Backend b => Show (GetCustomReturnType b)

deriving instance Backend b => Eq (GetCustomReturnType b)

instance Backend b => FromJSON (GetCustomReturnType b) where
  parseJSON = withObject "GetCustomReturnType" $ \o -> do
    gcrtSource <- o .: "source"
    pure GetCustomReturnType {..}

instance Backend b => ToJSON (GetCustomReturnType b) where
  toJSON GetCustomReturnType {..} =
    object
      [ "source" .= gcrtSource
      ]

-- | Handler for the 'get_custom_return_type' endpoint.
runGetCustomReturnType ::
  forall b m.
  ( BackendMetadata b,
    MetadataM m,
    HasFeatureFlagChecker m,
    MonadError QErr m
  ) =>
  GetCustomReturnType b ->
  m EncJSON
runGetCustomReturnType q = do
  throwIfFeatureDisabled

  metadata <- getMetadata

  let customTypes :: Maybe (CustomReturnTypes b)
      customTypes = metadata ^? getCustomTypes (gcrtSource q)

  pure (encJFromJValue (OMap.elems <$> customTypes))

getCustomTypes :: forall b. (Backend b) => SourceName -> Traversal' Metadata (CustomReturnTypes b)
getCustomTypes sourceName =
  metaSources . ix sourceName . toSourceMetadata . smCustomReturnTypes @b

-- | Handler for the 'track_custom_return_type' endpoint. The type 'TrackCustomReturnType b'
-- (appearing here in wrapped as 'BackendTrackCustomReturnType b' for 'AnyBackend'
-- compatibility) is defined in 'class CustomReturnTypeMetadata'.
runTrackCustomReturnType ::
  forall b m.
  ( BackendMetadata b,
    CacheRWM m,
    MetadataM m,
    MonadError QErr m,
    HasFeatureFlagChecker m
  ) =>
  TrackCustomReturnType b ->
  m EncJSON
runTrackCustomReturnType trackCustomReturnTypeRequest = do
  throwIfFeatureDisabled

  sourceMetadata <-
    maybe (throw400 NotFound $ "Source " <> sourceNameToText source <> " not found.") pure
      . preview (metaSources . ix source . toSourceMetadata @b)
      =<< getMetadata

  let (metadata :: CustomReturnTypeMetadata b) = customTypeTrackToMetadata trackCustomReturnTypeRequest

  let fieldName = _crtmName metadata
      metadataObj =
        MOSourceObjId source $
          AB.mkAnyBackend $
            SMOCustomReturnType @b fieldName
      existingCustomReturnTypes = OMap.keys (_smCustomReturnTypes sourceMetadata)

  when (fieldName `elem` existingCustomReturnTypes) do
    throw400 AlreadyTracked $ "Custom return type '" <> toTxt fieldName <> "' is already tracked."

  buildSchemaCacheFor metadataObj $
    MetadataModifier $
      (metaSources . ix source . toSourceMetadata @b . smCustomReturnTypes)
        %~ OMap.insert fieldName metadata

  pure successMsg
  where
    source = tctSource trackCustomReturnTypeRequest

-- | API payload for the 'untrack_custom_return_type' endpoint.
data UntrackCustomReturnType (b :: BackendType) = UntrackCustomReturnType
  { utctSource :: SourceName,
    utctName :: CustomReturnTypeName
  }

deriving instance Show (UntrackCustomReturnType b)

deriving instance Eq (UntrackCustomReturnType b)

instance FromJSON (UntrackCustomReturnType b) where
  parseJSON = withObject "UntrackCustomReturnType" $ \o -> do
    utctSource <- o .: "source"
    utctName <- o .: "name"
    pure UntrackCustomReturnType {..}

instance ToJSON (UntrackCustomReturnType b) where
  toJSON UntrackCustomReturnType {..} =
    object
      [ "source" .= utctSource,
        "name" .= utctName
      ]

-- | Handler for the 'untrack_custom_return_type' endpoint.
runUntrackCustomReturnType ::
  forall b m.
  ( BackendMetadata b,
    MonadError QErr m,
    CacheRWM m,
    MetadataM m
  ) =>
  UntrackCustomReturnType b ->
  m EncJSON
runUntrackCustomReturnType q = do
  -- we do not check for feature flag here as we always want users to be able
  -- to remove custom return types if they'd like
  assertCustomReturnTypeExists @b source fieldName

  let metadataObj =
        MOSourceObjId source $
          AB.mkAnyBackend $
            SMOCustomReturnType @b fieldName

  metadata <- getMetadata

  let logicalModels :: [LogicalModelMetadata b]
      logicalModels = metadata ^.. metaSources . ix source . toSourceMetadata @b . smLogicalModels . traversed

  case find ((== fieldName) . _lmmReturns) logicalModels of
    Just LogicalModelMetadata {_lmmRootFieldName} ->
      throw400 ConstraintViolation $
        "Custom type "
          <> fieldName
            <<> " still being used by logical model "
          <> _lmmRootFieldName <<> "."
    Nothing -> pure ()

  buildSchemaCacheFor metadataObj $
    dropCustomReturnTypeInMetadata @b source fieldName

  pure successMsg
  where
    source = utctSource q
    fieldName = utctName q

-- | A permission for logical models is tied to a specific root field name and
-- source. This wrapper adds both of those things to the JSON object that
-- describes the permission.
data CreateCustomReturnTypePermission a (b :: BackendType) = CreateCustomReturnTypePermission
  { ccrtpSource :: SourceName,
    ccrtpName :: CustomReturnTypeName,
    ccrtpInfo :: PermDef b a
  }
  deriving stock (Generic)

instance
  FromJSON (PermDef b a) =>
  FromJSON (CreateCustomReturnTypePermission a b)
  where
  parseJSON = withObject "CreateCustomReturnTypePermission" \obj -> do
    ccrtpSource <- obj .:? "source" .!= defaultSource
    ccrtpName <- obj .: "name"
    ccrtpInfo <- parseJSON (Object obj)

    pure CreateCustomReturnTypePermission {..}

runCreateSelectCustomReturnTypePermission ::
  forall b m.
  (Backend b, CacheRWM m, MetadataM m, MonadError QErr m, HasFeatureFlagChecker m) =>
  CreateCustomReturnTypePermission SelPerm b ->
  m EncJSON
runCreateSelectCustomReturnTypePermission CreateCustomReturnTypePermission {..} = do
  throwIfFeatureDisabled
  assertCustomReturnTypeExists @b ccrtpSource ccrtpName

  let metadataObj :: MetadataObjId
      metadataObj =
        MOSourceObjId ccrtpSource $
          AB.mkAnyBackend $
            SMOCustomReturnType @b ccrtpName

  buildSchemaCacheFor metadataObj $
    MetadataModifier $
      customReturnTypeMetadataSetter @b ccrtpSource ccrtpName . crtmSelectPermissions
        %~ OMap.insert (_pdRole ccrtpInfo) ccrtpInfo

  pure successMsg

-- | To drop a permission, we need to know the source and root field name of
-- the logical model, as well as the role whose permission we want to drop.
data DropCustomReturnTypePermission (b :: BackendType) = DropCustomReturnTypePermission
  { dcrtpSource :: SourceName,
    dcrtpName :: CustomReturnTypeName,
    dcrtpRole :: RoleName
  }
  deriving stock (Generic)

instance FromJSON (DropCustomReturnTypePermission b) where
  parseJSON = withObject "DropCustomReturnTypePermission" \obj -> do
    dcrtpSource <- obj .:? "source" .!= defaultSource
    dcrtpName <- obj .: "name"
    dcrtpRole <- obj .: "role"

    pure DropCustomReturnTypePermission {..}

runDropSelectCustomReturnTypePermission ::
  forall b m.
  (Backend b, CacheRWM m, MetadataM m, MonadError QErr m, HasFeatureFlagChecker m) =>
  DropCustomReturnTypePermission b ->
  m EncJSON
runDropSelectCustomReturnTypePermission DropCustomReturnTypePermission {..} = do
  throwIfFeatureDisabled
  assertCustomReturnTypeExists @b dcrtpSource dcrtpName

  let metadataObj :: MetadataObjId
      metadataObj =
        MOSourceObjId dcrtpSource $
          AB.mkAnyBackend $
            SMOCustomReturnType @b dcrtpName

  buildSchemaCacheFor metadataObj $
    MetadataModifier $
      customReturnTypeMetadataSetter @b dcrtpSource dcrtpName . crtmSelectPermissions
        %~ OMap.delete dcrtpRole

  pure successMsg

-- | TODO: should this cascade and also delete associated permissions?
dropCustomReturnTypeInMetadata :: forall b. BackendMetadata b => SourceName -> CustomReturnTypeName -> MetadataModifier
dropCustomReturnTypeInMetadata source name = do
  MetadataModifier $
    metaSources . ix source . toSourceMetadata @b . smCustomReturnTypes
      %~ OMap.delete name

-- | check feature flag is enabled before carrying out any actions
throwIfFeatureDisabled :: (HasFeatureFlagChecker m, MonadError QErr m) => m ()
throwIfFeatureDisabled = do
  enableCustomReturnTypes <- checkFlag FF.logicalModelInterface
  unless enableCustomReturnTypes $ throw500 "CustomReturnTypes is disabled!"

-- | Check whether a custom return type with the given root field name exists for
-- the given source.
assertCustomReturnTypeExists :: forall b m. (Backend b, MetadataM m, MonadError QErr m) => SourceName -> CustomReturnTypeName -> m ()
assertCustomReturnTypeExists sourceName name = do
  metadata <- getMetadata

  let sourceMetadataTraversal :: Traversal' Metadata (SourceMetadata b)
      sourceMetadataTraversal = metaSources . ix sourceName . toSourceMetadata @b

  sourceMetadata <-
    preview sourceMetadataTraversal metadata
      `onNothing` throw400 NotFound ("Source " <> sourceName <<> " not found.")

  let desiredCustomReturnType :: Traversal' (SourceMetadata b) (CustomReturnTypeMetadata b)
      desiredCustomReturnType = smCustomReturnTypes . ix name

  unless (has desiredCustomReturnType sourceMetadata) do
    throw400 NotFound ("Custom return type " <> name <<> " not found in source " <> sourceName <<> ".")
