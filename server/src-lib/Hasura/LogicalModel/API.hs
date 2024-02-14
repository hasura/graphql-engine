{-# LANGUAGE UndecidableInstances #-}

-- | Define and handle v1/metadata API operations to track, untrack, and get logical models.
module Hasura.LogicalModel.API
  ( GetLogicalModel (..),
    TrackLogicalModel (..),
    UntrackLogicalModel (..),
    runGetLogicalModel,
    execTrackLogicalModel,
    execUntrackLogicalModel,
    dropLogicalModelInMetadata,
    CreateLogicalModelPermission (..),
    DropLogicalModelPermission (..),
    runCreateSelectLogicalModelPermission,
    runDropSelectLogicalModelPermission,
    module Hasura.LogicalModel.Types,
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
import Hasura.LogicalModel.Lenses (lmmSelectPermissions)
import Hasura.LogicalModel.Metadata (LogicalModelMetadata (..))
import Hasura.LogicalModel.Types (LogicalModelField, LogicalModelLocation (..), LogicalModelName, logicalModelFieldMapCodec)
import Hasura.LogicalModelResolver.Lenses
import Hasura.NativeQuery.API (assertNativeQueryExists)
import Hasura.NativeQuery.Lenses (nqmReturns)
import Hasura.Prelude
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.RQL.Types.BackendTag (backendPrefix, backendTag, reify)
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common (SourceName, defaultSource, sourceNameToText, successMsg)
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.Permission (PermDef (_pdRole), SelPerm)
import Hasura.RQL.Types.Roles (RoleName)
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.SQL.AnyBackend qualified as AB

-- | Default implementation of the 'track_logical_model' request payload.
data TrackLogicalModel (b :: BackendType) = TrackLogicalModel
  { tlmSource :: SourceName,
    tlmName :: LogicalModelName,
    tlmDescription :: Maybe Text,
    tlmFields :: InsOrdHashMap.InsOrdHashMap (Column b) (LogicalModelField b)
  }

instance (Backend b) => HasCodec (TrackLogicalModel b) where
  codec =
    AC.CommentCodec
      ("A request to track a logical model")
      $ AC.object (backendPrefix @b <> "TrackLogicalModel")
      $ TrackLogicalModel
      <$> AC.requiredField "source" sourceDoc
      AC..= tlmSource
        <*> AC.requiredField "name" nameDoc
      AC..= tlmName
        <*> AC.optionalField "description" descriptionDoc
      AC..= tlmDescription
        <*> AC.requiredFieldWith "fields" logicalModelFieldMapCodec fieldsDoc
      AC..= tlmFields
    where
      sourceDoc = "The source in which this logical model should be tracked"
      nameDoc = "Root field name for the logical model"
      fieldsDoc = "Return type of the expression"
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
  forall b.
  TrackLogicalModel b ->
  LogicalModelMetadata b
logicalModelTrackToMetadata TrackLogicalModel {..} =
  LogicalModelMetadata
    { _lmmName = tlmName,
      _lmmFields = tlmFields,
      _lmmSelectPermissions = mempty,
      _lmmDescription = tlmDescription
    }

-- | API payload for the 'get_logical_model' endpoint.
data GetLogicalModel (b :: BackendType) = GetLogicalModel
  { glmSource :: SourceName
  }

deriving instance (Backend b) => Show (GetLogicalModel b)

deriving instance (Backend b) => Eq (GetLogicalModel b)

instance (Backend b) => FromJSON (GetLogicalModel b) where
  parseJSON = withObject "GetLogicalModel" $ \o -> do
    glmSource <- o .: "source"
    pure GetLogicalModel {..}

instance (Backend b) => ToJSON (GetLogicalModel b) where
  toJSON GetLogicalModel {..} =
    object
      [ "source" .= glmSource
      ]

-- | Handler for the 'get_logical_model' endpoint.
runGetLogicalModel ::
  forall b m.
  ( BackendMetadata b,
    MonadError QErr m,
    MetadataM m
  ) =>
  GetLogicalModel b ->
  m EncJSON
runGetLogicalModel q = do
  maybe
    ( throw400 NotFound
        $ "Source '"
        <> sourceNameToText (glmSource q)
        <> "' of kind "
        <> toTxt (reify (backendTag @b))
        <> " not found."
    )
    (const $ pure ())
    . preview (metaSources . ix (glmSource q) . toSourceMetadata @b)
    =<< getMetadata
  metadata <- getMetadata

  let logicalModels :: Maybe (LogicalModels b)
      logicalModels = metadata ^? getLogicalModels (glmSource q)

  pure (encJFromJValue (InsOrdHashMap.elems <$> logicalModels))

getLogicalModels :: forall b. (Backend b) => SourceName -> Traversal' Metadata (LogicalModels b)
getLogicalModels sourceName =
  metaSources . ix sourceName . toSourceMetadata . smLogicalModels @b

-- | Handler for the 'track_logical_model' endpoint. The type 'TrackLogicalModel b'
-- (appearing here in wrapped as 'BackendTrackLogicalModel b' for 'AnyBackend'
-- compatibility) is defined in 'class LogicalModelMetadata'.
execTrackLogicalModel ::
  forall b m.
  ( BackendMetadata b,
    MonadError QErr m
  ) =>
  TrackLogicalModel b ->
  Metadata ->
  m (MetadataObjId, MetadataModifier)
execTrackLogicalModel trackLogicalModelRequest metadata = do
  -- validation
  sourceMetadata <-
    maybe (throw400 NotFound $ "Source " <> sourceNameToText source <> " not found.") pure
      . preview (metaSources . ix source . toSourceMetadata @b)
      $ metadata

  let (lmMetadata :: LogicalModelMetadata b) = logicalModelTrackToMetadata trackLogicalModelRequest
      fieldName = _lmmName lmMetadata
      existingLogicalModels = InsOrdHashMap.keys (_smLogicalModels sourceMetadata)

  when (fieldName `elem` existingLogicalModels) do
    throw400 AlreadyTracked $ "Logical model '" <> toTxt fieldName <> "' is already tracked."

  let metadataObj =
        MOSourceObjId source
          $ AB.mkAnyBackend
          $ SMOLogicalModel @b fieldName

  let metadataModifier =
        MetadataModifier
          $ (metaSources . ix source . toSourceMetadata @b . smLogicalModels)
          %~ InsOrdHashMap.insert fieldName lmMetadata

  pure (metadataObj, metadataModifier)
  where
    source = tlmSource trackLogicalModelRequest

-- | API payload for the 'untrack_logical_model' endpoint.
data UntrackLogicalModel (b :: BackendType) = UntrackLogicalModel
  { utlmSource :: SourceName,
    utlmName :: LogicalModelName
  }

deriving instance Show (UntrackLogicalModel b)

deriving instance Eq (UntrackLogicalModel b)

instance FromJSON (UntrackLogicalModel b) where
  parseJSON = withObject "UntrackLogicalModel" $ \o -> do
    utlmSource <- o .: "source"
    utlmName <- o .: "name"
    pure UntrackLogicalModel {..}

instance ToJSON (UntrackLogicalModel b) where
  toJSON UntrackLogicalModel {..} =
    object
      [ "source" .= utlmSource,
        "name" .= utlmName
      ]

-- | Handler for the 'untrack_logical_model' endpoint.
execUntrackLogicalModel ::
  forall b m.
  ( BackendMetadata b,
    MonadError QErr m
  ) =>
  UntrackLogicalModel b ->
  Metadata ->
  m (MetadataObjId, MetadataModifier)
execUntrackLogicalModel q metadata = do
  -- we do not check for feature flag here as we always want users to be able
  -- to remove logical models if they'd like
  assertLogicalModelExists @b source fieldName metadata

  let metadataObj =
        MOSourceObjId source
          $ AB.mkAnyBackend
          $ SMOLogicalModel @b fieldName

  pure (metadataObj, dropLogicalModelInMetadata @b source fieldName)
  where
    source = utlmSource q
    fieldName = utlmName q

-- type for the `type` field in permissions
data LogicalModelSource
  = LMLogicalModel
  | LMNativeQuery

instance FromJSON LogicalModelSource where
  parseJSON (String "logical_model") = pure LMLogicalModel
  parseJSON (String "native_query") = pure LMNativeQuery
  parseJSON _ = fail "Expected string"

-- | A permission for logical models is tied to a specific name and
-- source. This wrapper adds both of those things to the JSON object that
-- describes the permission.
data CreateLogicalModelPermission a (b :: BackendType) = CreateLogicalModelPermission
  { clmpSource :: SourceName,
    clmpLocation :: LogicalModelLocation,
    clmpInfo :: PermDef b a
  }
  deriving stock (Generic)

instance
  (FromJSON (PermDef b a)) =>
  FromJSON (CreateLogicalModelPermission a b)
  where
  parseJSON = withObject "CreateLogicalModelPermission" \obj -> do
    clmpSource <- obj .:? "source" .!= defaultSource
    lmType <- obj .:? "type" .!= LMLogicalModel

    clmpLocation <- case lmType of
      LMLogicalModel -> LMLLogicalModel <$> obj .: "name"
      LMNativeQuery -> LMLNativeQuery <$> obj .: "name"

    clmpInfo <- parseJSON (Object obj)

    pure CreateLogicalModelPermission {..}

runCreateSelectLogicalModelPermission ::
  forall b m.
  (Backend b, CacheRWM m, MetadataM m, MonadError QErr m) =>
  CreateLogicalModelPermission SelPerm b ->
  m EncJSON
runCreateSelectLogicalModelPermission CreateLogicalModelPermission {..} = do
  metadata <- getMetadata

  case clmpLocation of
    LMLNativeQuery nativeQueryName -> do
      assertNativeQueryExists @b clmpSource nativeQueryName metadata

      let metadataObj :: MetadataObjId
          metadataObj =
            MOSourceObjId clmpSource
              $ AB.mkAnyBackend
              $ SMONativeQuery @b nativeQueryName

      buildSchemaCacheFor metadataObj
        $ MetadataModifier
        $ nativeQueryMetadataSetter @b clmpSource nativeQueryName
        . nqmReturns
        . _LMIInlineLogicalModel
        . ilmmSelectPermissions
        %~ InsOrdHashMap.insert (_pdRole clmpInfo) clmpInfo
    LMLLogicalModel logicalModelName -> do
      assertLogicalModelExists @b clmpSource logicalModelName metadata

      let metadataObj :: MetadataObjId
          metadataObj =
            MOSourceObjId clmpSource
              $ AB.mkAnyBackend
              $ SMOLogicalModel @b logicalModelName

      buildSchemaCacheFor metadataObj
        $ MetadataModifier
        $ logicalModelMetadataSetter @b clmpSource logicalModelName
        . lmmSelectPermissions
        %~ InsOrdHashMap.insert (_pdRole clmpInfo) clmpInfo

  pure successMsg

-- | To drop a permission, we need to know the source and name of
-- the logical model, as well as the role whose permission we want to drop.
data DropLogicalModelPermission (b :: BackendType) = DropLogicalModelPermission
  { dlmpSource :: SourceName,
    dlmpLocation :: LogicalModelLocation,
    dlmpRole :: RoleName
  }
  deriving stock (Generic)

instance FromJSON (DropLogicalModelPermission b) where
  parseJSON = withObject "DropLogicalModelPermission" \obj -> do
    dlmpSource <- obj .:? "source" .!= defaultSource

    lmType <- obj .:? "type" .!= LMLogicalModel

    dlmpLocation <- case lmType of
      LMLogicalModel -> LMLLogicalModel <$> obj .: "name"
      LMNativeQuery -> LMLNativeQuery <$> obj .: "name"

    dlmpRole <- obj .: "role"

    pure DropLogicalModelPermission {..}

runDropSelectLogicalModelPermission ::
  forall b m.
  (Backend b, CacheRWM m, MetadataM m, MonadError QErr m) =>
  DropLogicalModelPermission b ->
  m EncJSON
runDropSelectLogicalModelPermission DropLogicalModelPermission {..} = do
  metadata <- getMetadata
  case dlmpLocation of
    LMLNativeQuery nativeQueryName -> do
      assertNativeQueryExists @b dlmpSource nativeQueryName metadata

      let metadataObj :: MetadataObjId
          metadataObj =
            MOSourceObjId dlmpSource
              $ AB.mkAnyBackend
              $ SMONativeQuery @b nativeQueryName

      buildSchemaCacheFor metadataObj
        $ MetadataModifier
        $ nativeQueryMetadataSetter @b dlmpSource nativeQueryName
        . nqmReturns
        . _LMIInlineLogicalModel
        . ilmmSelectPermissions
        %~ InsOrdHashMap.delete dlmpRole
    LMLLogicalModel logicalModelName -> do
      assertLogicalModelExists @b dlmpSource logicalModelName metadata

      let metadataObj :: MetadataObjId
          metadataObj =
            MOSourceObjId dlmpSource
              $ AB.mkAnyBackend
              $ SMOLogicalModel @b logicalModelName

      buildSchemaCacheFor metadataObj
        $ MetadataModifier
        $ logicalModelMetadataSetter @b dlmpSource logicalModelName
        . lmmSelectPermissions
        %~ InsOrdHashMap.delete dlmpRole

  pure successMsg

-- | TODO: should this cascade and also delete associated permissions?
dropLogicalModelInMetadata :: forall b. (BackendMetadata b) => SourceName -> LogicalModelName -> MetadataModifier
dropLogicalModelInMetadata source name = do
  MetadataModifier
    $ metaSources
    . ix source
    . toSourceMetadata @b
    . smLogicalModels
    %~ InsOrdHashMap.delete name

-- | Check whether a logical model with the given root field name exists for
-- the given source.
assertLogicalModelExists ::
  forall b m. (Backend b, MonadError QErr m) => SourceName -> LogicalModelName -> Metadata -> m ()
assertLogicalModelExists sourceName name metadata = do
  let sourceMetadataTraversal :: Traversal' Metadata (SourceMetadata b)
      sourceMetadataTraversal = metaSources . ix sourceName . toSourceMetadata @b

  sourceMetadata <-
    preview sourceMetadataTraversal metadata
      `onNothing` throw400 NotFound ("Source " <> sourceName <<> " not found.")

  let desiredLogicalModel :: Traversal' (SourceMetadata b) (LogicalModelMetadata b)
      desiredLogicalModel = smLogicalModels . ix name

  unless (has desiredLogicalModel sourceMetadata) do
    throw400 NotFound ("Logical model " <> name <<> " not found in source " <> sourceName <<> ".")
