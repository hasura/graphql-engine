{-# LANGUAGE UndecidableInstances #-}

-- | Metadata V1 commands (and their types) for handling user-specified custom
-- SQL fragments.
module Hasura.RQL.DDL.CustomSQL
  ( GetCustomSQL (..),
    TrackCustomSQL (..),
    UntrackCustomSQL (..),
    runGetCustomSQL,
    runTrackCustomSQL,
    runUntrackCustomSQL,
    dropCustomSQLInMetadata,
  )
where

import Control.Lens ((^?))
import Data.Aeson
import Data.HashMap.Strict.InsOrd qualified as OMap
import Hasura.Base.Error
import Hasura.CustomSQL
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.RQL.Types.Backend (Backend, TableName)
import Hasura.RQL.Types.Common (SourceName, successMsg)
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Backend
import Hasura.Server.Init.FeatureFlag as FF
import Hasura.Server.Types (HasServerConfigCtx (..), ServerConfigCtx (..))
import Hasura.Session
import Language.GraphQL.Draft.Syntax qualified as G

---------------------------------

data GetCustomSQL (b :: BackendType) = GetCustomSQL
  { gcsSource :: SourceName
  }

deriving instance Backend b => Show (GetCustomSQL b)

deriving instance Backend b => Eq (GetCustomSQL b)

instance Backend b => FromJSON (GetCustomSQL b) where
  parseJSON = withObject "GetCustomSQL" $ \o -> do
    gcsSource <- o .: "source"
    pure GetCustomSQL {..}

instance Backend b => ToJSON (GetCustomSQL b) where
  toJSON GetCustomSQL {..} =
    object
      [ "source" .= gcsSource
      ]

runGetCustomSQL ::
  forall b m.
  ( BackendMetadata b,
    MetadataM m,
    HasServerConfigCtx m,
    MonadIO m,
    UserInfoM m,
    MonadError QErr m
  ) =>
  GetCustomSQL b ->
  m EncJSON
runGetCustomSQL q = do
  throwIfFeatureDisabled
  throwIfNotAdmin

  metadata <- getMetadata

  let customSQL :: Maybe (CustomSQLFields b)
      customSQL = metadata ^? metaSources . ix (gcsSource q) . toSourceMetadata . smCustomSQL

  pure (encJFromJValue customSQL)

----------------------------------

data TrackCustomSQL (b :: BackendType) = TrackCustomSQL
  { tcsSource :: SourceName,
    tcsType :: Text,
    tcsRootFieldName :: G.Name,
    tcsSql :: Text,
    tcsParameters :: NonEmpty CustomSQLParameter,
    tcsReturns :: TableName b
  }

deriving instance Backend b => Show (TrackCustomSQL b)

deriving instance Backend b => Eq (TrackCustomSQL b)

instance Backend b => FromJSON (TrackCustomSQL b) where
  parseJSON = withObject "TrackCustomSQL" $ \o -> do
    tcsSource <- o .: "source"
    tcsType <- o .: "type"
    tcsRootFieldName <- o .: "root_field_name"
    tcsSql <- o .: "sql"
    tcsParameters <- o .: "parameters"
    tcsReturns <- o .: "returns"
    pure TrackCustomSQL {..}

instance Backend b => ToJSON (TrackCustomSQL b) where
  toJSON TrackCustomSQL {..} =
    object
      [ "source" .= tcsSource,
        "type" .= tcsType,
        "root_field_name" .= tcsRootFieldName,
        "sql" .= tcsSql,
        "parameters" .= tcsParameters,
        "returns" .= tcsReturns
      ]

runTrackCustomSQL ::
  forall b m.
  ( BackendMetadata b,
    CacheRWM m,
    MetadataM m,
    MonadError QErr m,
    HasServerConfigCtx m,
    UserInfoM m,
    MonadIO m
  ) =>
  TrackCustomSQL b ->
  m EncJSON
runTrackCustomSQL q = do
  throwIfFeatureDisabled
  throwIfNotAdmin

  let metadataObj =
        MOSourceObjId source $
          AB.mkAnyBackend $
            SMOCustomSQL @b fieldName
      metadata =
        CustomSQLMetadata
          { _csmType = tcsType q,
            _csmRootFieldName = tcsRootFieldName q,
            _csmSql = tcsSql q,
            _csmParameters = tcsParameters q,
            _csmReturns = tcsReturns q
          }

  buildSchemaCacheFor metadataObj $
    MetadataModifier $
      (metaSources . ix source . toSourceMetadata @b . smCustomSQL)
        %~ OMap.insert fieldName metadata

  pure successMsg
  where
    source = tcsSource q
    fieldName = tcsRootFieldName q

---------------------------------

data UntrackCustomSQL (b :: BackendType) = UntrackCustomSQL
  { utcsSource :: SourceName,
    utcsRootFieldName :: G.Name
  }

deriving instance Backend b => Show (UntrackCustomSQL b)

deriving instance Backend b => Eq (UntrackCustomSQL b)

instance Backend b => FromJSON (UntrackCustomSQL b) where
  parseJSON = withObject "UntrackCustomSQL" $ \o -> do
    utcsSource <- o .: "source"
    utcsRootFieldName <- o .: "root_field_name"
    pure UntrackCustomSQL {..}

instance Backend b => ToJSON (UntrackCustomSQL b) where
  toJSON UntrackCustomSQL {..} =
    object
      [ "source" .= utcsSource,
        "root_field_name" .= utcsRootFieldName
      ]

runUntrackCustomSQL ::
  forall b m.
  ( BackendMetadata b,
    MonadError QErr m,
    CacheRWM m,
    MetadataM m,
    HasServerConfigCtx m,
    UserInfoM m,
    MonadIO m
  ) =>
  UntrackCustomSQL b ->
  m EncJSON
runUntrackCustomSQL q = do
  throwIfFeatureDisabled
  throwIfNotAdmin

  let metadataObj =
        MOSourceObjId source $
          AB.mkAnyBackend $
            SMOCustomSQL @b fieldName

  buildSchemaCacheFor metadataObj $
    dropCustomSQLInMetadata @b source (fieldName :: G.Name)

  pure successMsg
  where
    source = utcsSource q
    fieldName = utcsRootFieldName q

dropCustomSQLInMetadata :: forall b. BackendMetadata b => SourceName -> G.Name -> MetadataModifier
dropCustomSQLInMetadata source rootFieldName =
  MetadataModifier $
    metaSources . ix source . toSourceMetadata @b . smCustomSQL %~ OMap.delete rootFieldName

-- | check feature flag is enabled before carrying out any actions
throwIfFeatureDisabled :: (HasServerConfigCtx m, MonadIO m, MonadError QErr m) => m ()
throwIfFeatureDisabled = do
  configCtx <- askServerConfigCtx

  enableCustomSQL <- liftIO (_sccCheckFeatureFlag configCtx FF.nativeQueryInterface)

  unless enableCustomSQL (throw500 "CustomSQL is disabled!")

throwIfNotAdmin :: (MonadError QErr m, UserInfoM m) => m ()
throwIfNotAdmin = do
  uRole <- _uiRole <$> askUserInfo
  unless (uRole == adminRoleName) $
    throw400 AccessDenied "You have to be an admin to access this endpoint"
