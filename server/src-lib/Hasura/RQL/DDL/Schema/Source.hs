{-# LANGUAGE TemplateHaskell #-}

module Hasura.RQL.DDL.Schema.Source
  ( -- * Add Source
    AddSource,
    runAddSource,
    -- Drop Source
    DropSource (..),
    runDropSource,
    dropSource,
    runPostDropSourceHook,

    -- * Rename Source
    RenameSource,
    runRenameSource,

    -- * Update Source
    UpdateSource,
    runUpdateSource,

    -- * Get Source Tables
    GetSourceTables (..),
    runGetSourceTables,

    -- * Get Table Name
    GetTableInfo (..),
    runGetTableInfo,
  )
where

--------------------------------------------------------------------------------

import Control.Lens (at, (.~), (^.))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson qualified as Aeson
import Data.Aeson.Extended
import Data.Aeson.Extended qualified as J
import Data.Aeson.TH
import Data.Has
import Data.HashMap.Strict qualified as HM
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text.Extended
import Data.Text.Extended qualified as Text.E
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Adapter.Types qualified as DC.Types
import Hasura.Backends.DataConnector.Agent.Client qualified as Agent.Client
import Hasura.Base.Error
import Hasura.Base.Error qualified as Error
import Hasura.EncJSON
import Hasura.EncJSON qualified as EncJSON
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Backend qualified as RQL.Types
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Common qualified as Common
import Hasura.RQL.Types.HealthCheck (HealthCheckConfig)
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata qualified as Metadata
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Metadata.Instances ()
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.SchemaCacheTypes
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.AnyBackend qualified as AnyBackend
import Hasura.SQL.Backend
import Hasura.SQL.Backend qualified as Backend
import Hasura.SQL.BackendMap qualified as BackendMap
import Hasura.Server.Logging (MetadataLog (..))
import Hasura.Tracing qualified as Tracing
import Network.HTTP.Client.Manager qualified as HTTP.Manager
import Servant.Client ((//))
import Servant.Client.Generic qualified as Servant.Client

--------------------------------------------------------------------------------
-- Add source

data AddSource b = AddSource
  { _asName :: SourceName,
    _asBackendKind :: BackendSourceKind b,
    _asConfiguration :: SourceConnConfiguration b,
    _asReplaceConfiguration :: Bool,
    _asCustomization :: SourceCustomization,
    _asHealthCheckConfig :: Maybe (HealthCheckConfig b)
  }

instance (Backend b) => FromJSONWithContext (BackendSourceKind b) (AddSource b) where
  parseJSONWithContext backendKind = withObject "AddSource" $ \o ->
    AddSource
      <$> o .: "name"
      <*> pure backendKind
      <*> o .: "configuration"
      <*> o .:? "replace_configuration" .!= False
      <*> o .:? "customization" .!= emptySourceCustomization
      <*> o .:? "health_check"

runAddSource ::
  forall m b.
  (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b) =>
  AddSource b ->
  m EncJSON
runAddSource (AddSource name backendKind sourceConfig replaceConfiguration sourceCustomization healthCheckConfig) = do
  sources <- scSources <$> askSchemaCache

  metadataModifier <-
    MetadataModifier
      <$> if HM.member name sources
        then
          if replaceConfiguration
            then do
              let sMetadata = metaSources . ix name . toSourceMetadata @b
                  updateConfig = sMetadata . smConfiguration .~ sourceConfig
                  updateCustomization = sMetadata . smCustomization .~ sourceCustomization
              pure $ updateConfig . updateCustomization
            else throw400 AlreadyExists $ "source with name " <> name <<> " already exists"
        else do
          let sourceMetadata =
                mkSourceMetadata @b name backendKind sourceConfig sourceCustomization healthCheckConfig
          pure $ metaSources %~ OMap.insert name sourceMetadata

  buildSchemaCacheFor (MOSource name) metadataModifier
  pure successMsg

--------------------------------------------------------------------------------
-- Rename source

data RenameSource = RenameSource
  { _rmName :: SourceName,
    _rmNewName :: SourceName
  }

$(deriveFromJSON hasuraJSON ''RenameSource)

runRenameSource ::
  forall m.
  (MonadError QErr m, CacheRWM m, MetadataM m) =>
  RenameSource ->
  m EncJSON
runRenameSource RenameSource {..} = do
  sources <- scSources <$> askSchemaCache

  unless (HM.member _rmName sources) $
    throw400 NotExists $ "Could not find source with name " <>> _rmName

  when (HM.member _rmNewName sources) $
    throw400 AlreadyExists $ "Source with name " <> _rmNewName <<> " already exists"

  let metadataModifier =
        MetadataModifier $
          metaSources %~ renameBackendSourceMetadata _rmName _rmNewName
  buildSchemaCacheFor (MOSource _rmNewName) metadataModifier

  pure successMsg
  where
    renameBackendSourceMetadata ::
      SourceName ->
      SourceName ->
      OMap.InsOrdHashMap SourceName BackendSourceMetadata ->
      OMap.InsOrdHashMap SourceName BackendSourceMetadata
    renameBackendSourceMetadata oldKey newKey m =
      case OMap.lookup oldKey m of
        Just val ->
          let renamedSource = BackendSourceMetadata (AB.mapBackend (unBackendSourceMetadata val) (renameSource newKey))
           in OMap.insert newKey renamedSource $ OMap.delete oldKey $ m
        Nothing -> m

    renameSource :: forall b. SourceName -> SourceMetadata b -> SourceMetadata b
    renameSource newName metadata = metadata {_smName = newName}

--------------------------------------------------------------------------------
-- Drop source

data DropSource = DropSource
  { _dsName :: SourceName,
    _dsCascade :: Bool
  }
  deriving (Show, Eq)

instance FromJSON DropSource where
  parseJSON = withObject "DropSource" $ \o ->
    DropSource <$> o .: "name" <*> o .:? "cascade" .!= False

runDropSource ::
  forall m r.
  ( MonadError QErr m,
    CacheRWM m,
    MonadIO m,
    MonadBaseControl IO m,
    MetadataM m,
    MonadReader r m,
    Has (L.Logger L.Hasura) r
  ) =>
  DropSource ->
  m EncJSON
runDropSource dropSourceInfo@(DropSource name cascade) = do
  schemaCache <- askSchemaCache
  let sources = scSources schemaCache
  case HM.lookup name sources of
    Just backendSourceInfo ->
      AB.dispatchAnyBackend @BackendMetadata backendSourceInfo $ dropSource schemaCache dropSourceInfo
    Nothing -> do
      metadata <- getMetadata
      void $
        onNothing (metadata ^. metaSources . at name) $
          throw400 NotExists $ "source with name " <> name <<> " does not exist"
      if cascade
        then -- Without sourceInfo we can't cascade, so throw an error
          throw400 Unexpected $ "source with name " <> name <<> " is inconsistent"
        else -- Drop source from metadata
          buildSchemaCacheFor (MOSource name) (dropSourceMetadataModifier name)
  pure successMsg

dropSourceMetadataModifier :: SourceName -> MetadataModifier
dropSourceMetadataModifier sourceName = MetadataModifier $ metaSources %~ OMap.delete sourceName

dropSource ::
  forall m r b.
  ( MonadError QErr m,
    CacheRWM m,
    MonadIO m,
    MonadBaseControl IO m,
    MetadataM m,
    MonadReader r m,
    Has (L.Logger L.Hasura) r,
    BackendMetadata b
  ) =>
  SchemaCache ->
  DropSource ->
  SourceInfo b ->
  m ()
dropSource _schemaCache (DropSource sourceName cascade) sourceInfo = do
  schemaCache <- askSchemaCache
  let remoteDeps = getRemoteDependencies schemaCache sourceName

  unless (cascade || null remoteDeps) $
    reportDependentObjectsExist remoteDeps

  metadataModifier <- execWriterT $ do
    traverse_ purgeSourceAndSchemaDependencies remoteDeps
    tell $ dropSourceMetadataModifier sourceName

  buildSchemaCacheFor (MOSource sourceName) metadataModifier
  runPostDropSourceHook sourceName sourceInfo

runPostDropSourceHook ::
  forall m r b.
  ( MonadError QErr m,
    MonadIO m,
    MonadBaseControl IO m,
    MonadReader r m,
    Has (L.Logger L.Hasura) r,
    BackendMetadata b
  ) =>
  SourceName ->
  SourceInfo b ->
  m ()
runPostDropSourceHook sourceName sourceInfo = do
  logger :: (L.Logger L.Hasura) <- asks getter
  let sourceConfig = _siConfiguration sourceInfo
  -- Create a hashmap: {TableName: [Triggers]}
  let tableTriggersMap = HM.map (HM.keys . _tiEventTriggerInfoMap) (_siTables sourceInfo)
  -- We only log errors that arise from 'postDropSourceHook' here, and not
  -- surface them as end-user errors. See comment
  -- https://github.com/hasura/graphql-engine/issues/7092#issuecomment-873845282
  runExceptT (postDropSourceHook @b sourceConfig tableTriggersMap) >>= either (logDropSourceHookError logger) pure
  where
    logDropSourceHookError logger err =
      let msg =
            "Error executing cleanup actions after removing source '" <> toTxt sourceName
              <> "'. Consider cleaning up tables in hdb_catalog schema manually."
       in L.unLogger logger $ MetadataLog L.LevelWarn msg (J.toJSON err)

--------------------------------------------------------------------------------
-- update source

data UpdateSource b = UpdateSource
  { _usName :: SourceName,
    _usConfiguration :: Maybe (SourceConnConfiguration b),
    _usCustomization :: Maybe SourceCustomization,
    _usHealthCheckConfig :: Maybe (HealthCheckConfig b)
  }

instance (Backend b) => FromJSONWithContext (BackendSourceKind b) (UpdateSource b) where
  parseJSONWithContext _ = withObject "UpdateSource" $ \o ->
    UpdateSource
      <$> o .: "name"
      <*> o .:? "configuration"
      <*> o .:? "customization"
      <*> o .:? "health_check"

runUpdateSource ::
  forall m b.
  (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b) =>
  UpdateSource b ->
  m EncJSON
runUpdateSource (UpdateSource name sourceConfig sourceCustomization healthCheckConfig) = do
  sources <- scSources <$> askSchemaCache

  metadataModifier <-
    MetadataModifier
      <$> if HM.member name sources
        then do
          let sMetadata = metaSources . ix name . toSourceMetadata @b
              updateConfig = maybe id (\scc -> sMetadata . smConfiguration .~ scc) sourceConfig
              updateCustomization = maybe id (\scc -> sMetadata . smCustomization .~ scc) sourceCustomization
              updateHealthCheckConfig = maybe id (\hcc -> sMetadata . smHealthCheckConfig .~ Just hcc) healthCheckConfig
          pure $ updateHealthCheckConfig . updateConfig . updateCustomization
        else do
          throw400 NotExists $ "source with name " <> name <<> " does not exist"

  buildSchemaCacheFor (MOSource name) metadataModifier
  pure successMsg

--------------------------------------------------------------------------------

newtype GetSourceTables = GetSourceTables {_gstSourceName :: Common.SourceName}

instance FromJSON GetSourceTables where
  parseJSON = Aeson.withObject "GetSourceTables" \o -> do
    _gstSourceName <- o .: "source"
    pure $ GetSourceTables {..}

-- | Fetch a list of tables for the request data source. Currently
-- this is only supported for Data Connectors.
runGetSourceTables ::
  ( Has (L.Logger L.Hasura) r,
    HTTP.Manager.HasHttpManagerM m,
    MonadReader r m,
    MonadError Error.QErr m,
    Metadata.MetadataM m,
    MonadIO m
  ) =>
  GetSourceTables ->
  m EncJSON
runGetSourceTables GetSourceTables {..} = do
  metadata <- Metadata.getMetadata

  let sources = fmap Metadata.unBackendSourceMetadata $ Metadata._metaSources metadata
      bmap = Metadata._metaBackendConfigs metadata

  abSourceMetadata <-
    InsOrdHashMap.lookup _gstSourceName sources
      `onNothing` Error.throw400 Error.DataConnectorError ("Source '" <> Text.E.toTxt _gstSourceName <> "' not found")

  AnyBackend.dispatchAnyBackend @RQL.Types.Backend abSourceMetadata $ \Metadata.SourceMetadata {_smKind, _smConfiguration} -> do
    case _smKind of
      Backend.DataConnectorKind dcName -> do
        logger :: L.Logger L.Hasura <- asks getter
        manager <- HTTP.Manager.askHttpManager
        let timeout = DC.Types.timeout _smConfiguration
            apiConfig = DC.Types.value _smConfiguration

        DC.Types.DataConnectorOptions {..} <- do
          let backendConfig = Metadata.unBackendConfigWrapper <$> BackendMap.lookup @'Backend.DataConnector bmap
          onNothing
            (InsOrdHashMap.lookup dcName =<< backendConfig)
            (Error.throw400 Error.DataConnectorError ("Data connector named " <> Text.E.toTxt dcName <> " was not found in the data connector backend config"))

        schemaResponse <-
          Tracing.runTraceTWithReporter Tracing.noReporter "resolve source"
            . flip Agent.Client.runAgentClientT (Agent.Client.AgentClientContext logger _dcoUri manager (DC.Types.sourceTimeoutMicroseconds <$> timeout))
            $ (Servant.Client.genericClient // API._schema) (Text.E.toTxt _gstSourceName) apiConfig

        let fullyQualifiedTableNames = fmap API.dtiName $ API.srTables schemaResponse
            tableNames = fmap (fold . NonEmpty.intersperse "." . API.unTableName) fullyQualifiedTableNames
        pure $ EncJSON.encJFromJValue tableNames
      backend -> Error.throw500 ("Schema fetching is not supported for '" <> Text.E.toTxt backend <> "'")

--------------------------------------------------------------------------------

data GetTableInfo = GetTableInfo
  { _gtiSourceName :: Common.SourceName,
    _gtiTableName :: API.TableName
  }

instance FromJSON GetTableInfo where
  parseJSON = Aeson.withObject "GetSourceTables" \o -> do
    _gtiSourceName <- o .: "source"
    _gtiTableName <- o .: "table"
    pure $ GetTableInfo {..}

-- | Fetch a list of tables for the request data source. Currently
-- this is only supported for Data Connectors.
runGetTableInfo ::
  ( Has (L.Logger L.Hasura) r,
    HTTP.Manager.HasHttpManagerM m,
    MonadReader r m,
    MonadError Error.QErr m,
    Metadata.MetadataM m,
    MonadIO m
  ) =>
  GetTableInfo ->
  m EncJSON
runGetTableInfo GetTableInfo {..} = do
  metadata <- Metadata.getMetadata

  let sources = fmap Metadata.unBackendSourceMetadata $ Metadata._metaSources metadata
      bmap = Metadata._metaBackendConfigs metadata

  abSourceMetadata <-
    InsOrdHashMap.lookup _gtiSourceName sources
      `onNothing` Error.throw400 Error.DataConnectorError ("Source '" <> Text.E.toTxt _gtiSourceName <> "' not found")

  AnyBackend.dispatchAnyBackend @RQL.Types.Backend abSourceMetadata $ \Metadata.SourceMetadata {_smKind, _smConfiguration} -> do
    case _smKind of
      Backend.DataConnectorKind dcName -> do
        logger :: L.Logger L.Hasura <- asks getter
        manager <- HTTP.Manager.askHttpManager
        let timeout = DC.Types.timeout _smConfiguration
            apiConfig = DC.Types.value _smConfiguration

        DC.Types.DataConnectorOptions {..} <- do
          let backendConfig = Metadata.unBackendConfigWrapper <$> BackendMap.lookup @'Backend.DataConnector bmap
          onNothing
            (InsOrdHashMap.lookup dcName =<< backendConfig)
            (Error.throw400 Error.DataConnectorError ("Data connector named " <> Text.E.toTxt dcName <> " was not found in the data connector backend config"))

        schemaResponse <-
          Tracing.runTraceTWithReporter Tracing.noReporter "resolve source"
            . flip Agent.Client.runAgentClientT (Agent.Client.AgentClientContext logger _dcoUri manager (DC.Types.sourceTimeoutMicroseconds <$> timeout))
            $ (Servant.Client.genericClient // API._schema) (Text.E.toTxt _gtiSourceName) apiConfig

        let table = find ((== _gtiTableName) . API.dtiName) $ API.srTables schemaResponse
        pure $ EncJSON.encJFromJValue table
      backend -> Error.throw500 ("Schema fetching is not supported for '" <> Text.E.toTxt backend <> "'")
