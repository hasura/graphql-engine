module Hasura.RQL.DDL.Schema.Source
  ( -- * Add Source
    AddSource,
    runAddSource,

    -- * Drop Source
    DropSource (..),
    runDropSource,
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

    -- * Get Source Functions
    GetSourceTrackables (..),
    runGetSourceTrackables,

    -- * Get Table Info
    GetTableInfo (..),
    runGetTableInfo,

    -- * Legacy Get Table Info
    GetTableInfo_ (..),
    runGetTableInfo_,
  )
where

--------------------------------------------------------------------------------

import Control.Lens (at, (.~), (^.))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson qualified as J
import Data.Aeson.Extended
import Data.Environment qualified as Env
import Data.Has
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Text.Extended
import Data.Text.Extended qualified as Text.E
import Hasura.Base.Error
import Hasura.Base.Error qualified as Error
import Hasura.EncJSON
import Hasura.EncJSON qualified as EncJSON
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Backend qualified as RQL.Types
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.BackendType qualified as Backend
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
import Hasura.SQL.AnyBackend (AnyBackend)
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.AnyBackend qualified as AnyBackend
import Hasura.Server.Logging (MetadataLog (..))
import Hasura.Services

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
      <$> o
      .: "name"
      <*> pure backendKind
      <*> o
      .: "configuration"
      <*> o
      .:? "replace_configuration"
      .!= False
      <*> o
      .:? "customization"
      .!= emptySourceCustomization
      <*> o
      .:? "health_check"

runAddSource ::
  forall m b.
  (MonadIO m, MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b) =>
  Env.Environment ->
  AddSource b ->
  m EncJSON
runAddSource env (AddSource name backendKind sourceConfig replaceConfiguration sourceCustomization healthCheckConfig) = do
  sources <- scSources <$> askSchemaCache
  do
    -- version check
    result <- liftIO $ versionCheckImplementation @b env name sourceConfig
    liftEither result

  metadataModifier <-
    MetadataModifier
      <$> if HashMap.member name sources
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
          pure $ metaSources %~ InsOrdHashMap.insert name sourceMetadata

  buildSchemaCacheFor (MOSource name) metadataModifier
  pure successMsg

--------------------------------------------------------------------------------
-- Rename source

data RenameSource = RenameSource
  { _rmName :: SourceName,
    _rmNewName :: SourceName
  }
  deriving stock (Generic)

instance FromJSON RenameSource where
  parseJSON = genericParseJSON hasuraJSON

runRenameSource ::
  forall m.
  (MonadError QErr m, CacheRWM m, MetadataM m) =>
  RenameSource ->
  m EncJSON
runRenameSource RenameSource {..} = do
  sources <- scSources <$> askSchemaCache

  unless (HashMap.member _rmName sources)
    $ throw400 NotExists
    $ "Could not find source with name "
    <>> _rmName

  when (HashMap.member _rmNewName sources)
    $ throw400 AlreadyExists
    $ "Source with name "
    <> _rmNewName
    <<> " already exists"

  let metadataModifier =
        MetadataModifier
          $ metaSources
          %~ renameBackendSourceMetadata _rmName _rmNewName
  buildSchemaCacheFor (MOSource _rmNewName) metadataModifier

  pure successMsg
  where
    renameBackendSourceMetadata ::
      SourceName ->
      SourceName ->
      InsOrdHashMap.InsOrdHashMap SourceName BackendSourceMetadata ->
      InsOrdHashMap.InsOrdHashMap SourceName BackendSourceMetadata
    renameBackendSourceMetadata oldKey newKey m =
      case InsOrdHashMap.lookup oldKey m of
        Just val ->
          let renamedSource = BackendSourceMetadata (AB.mapBackend (unBackendSourceMetadata val) (renameSource newKey))
           in InsOrdHashMap.insert newKey renamedSource $ InsOrdHashMap.delete oldKey $ m
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
  case HashMap.lookup name sources of
    Just backendSourceInfo ->
      AB.dispatchAnyBackend @BackendMetadata backendSourceInfo $ dropSource dropSourceInfo
    Nothing -> do
      metadata <- getMetadata
      void
        $ onNothing (metadata ^. metaSources . at name)
        $ throw400 NotExists
        $ "source with name "
        <> name
        <<> " does not exist"
      if cascade
        then -- Without sourceInfo we can't cascade, so throw an error
          throw400 Unexpected $ "source with name " <> name <<> " is inconsistent"
        else -- Drop source from metadata
          buildSchemaCacheFor (MOSource name) (dropSourceMetadataModifier name)
  pure successMsg

dropSourceMetadataModifier :: SourceName -> MetadataModifier
dropSourceMetadataModifier sourceName = MetadataModifier $ metaSources %~ InsOrdHashMap.delete sourceName

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
  DropSource ->
  SourceInfo b ->
  m ()
dropSource (DropSource sourceName cascade) sourceInfo = do
  schemaCache <- askSchemaCache
  let remoteDeps = getRemoteDependencies schemaCache sourceName

  unless (cascade || null remoteDeps)
    $ reportDependentObjectsExist remoteDeps

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
  let tableTriggersMap = HashMap.map (HashMap.keys . _tiEventTriggerInfoMap) (_siTables sourceInfo)
  -- We only log errors that arise from 'postDropSourceHook' here, and not
  -- surface them as end-user errors. See comment
  -- https://github.com/hasura/graphql-engine/issues/7092#issuecomment-873845282
  runExceptT (postDropSourceHook @b sourceConfig tableTriggersMap) >>= either (logDropSourceHookError logger) pure
  where
    logDropSourceHookError logger err =
      let msg =
            "Error executing cleanup actions after removing source '"
              <> toTxt sourceName
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
      <$> o
      .: "name"
      <*> o
      .:? "configuration"
      <*> o
      .:? "customization"
      <*> o
      .:? "health_check"

runUpdateSource ::
  forall m b.
  (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b) =>
  UpdateSource b ->
  m EncJSON
runUpdateSource (UpdateSource name sourceConfig sourceCustomization healthCheckConfig) = do
  sources <- scSources <$> askSchemaCache

  metadataModifier <-
    MetadataModifier
      <$> if HashMap.member name sources
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

newtype GetSourceTables (b :: BackendType) = GetSourceTables {_gstSourceName :: SourceName}

instance FromJSON (GetSourceTables b) where
  parseJSON = J.withObject "GetSourceTables" \o -> do
    _gstSourceName <- o .: "source"
    pure $ GetSourceTables {..}

-- | Fetch a list of tables for the request data source.
runGetSourceTrackables ::
  forall b m r.
  ( BackendMetadata b,
    CacheRM m,
    MonadError Error.QErr m,
    Metadata.MetadataM m,
    MonadIO m,
    MonadBaseControl IO m,
    MonadReader r m,
    Has (L.Logger L.Hasura) r,
    ProvidesNetwork m
  ) =>
  GetSourceTrackables b ->
  m EncJSON
runGetSourceTrackables GetSourceTrackables {..} = do
  fmap EncJSON.encJFromJValue (listAllTrackables @b _gstrSourceName)

newtype GetSourceTrackables (b :: BackendType) = GetSourceTrackables {_gstrSourceName :: SourceName}

instance FromJSON (GetSourceTrackables b) where
  parseJSON = J.withObject "GetSourceFunctions" \o -> do
    _gstrSourceName <- o .: "source"
    pure $ GetSourceTrackables {..}

-- | Fetch a list of tables for the request data source.
runGetSourceTables ::
  forall b m r.
  ( BackendMetadata b,
    CacheRM m,
    MonadError Error.QErr m,
    Metadata.MetadataM m,
    MonadIO m,
    MonadBaseControl IO m,
    MonadReader r m,
    Has (L.Logger L.Hasura) r,
    ProvidesNetwork m
  ) =>
  GetSourceTables b ->
  m EncJSON
runGetSourceTables GetSourceTables {..} = do
  fmap EncJSON.encJFromJValue (listAllTables @b _gstSourceName)

--------------------------------------------------------------------------------

data GetTableInfo_ = GetTableInfo_
  { _gtiSourceName_ :: Common.SourceName,
    _gtiTableName_ :: TableName 'DataConnector
  }

instance FromJSON GetTableInfo_ where
  parseJSON = J.withObject "GetTableInfo_" \o -> do
    _gtiSourceName_ <- o .: "source"
    _gtiTableName_ <- o .: "table"
    pure $ GetTableInfo_ {..}

-- | Legacy data connector command. This doesn't use the DataConnector
-- 'ScalarType' to represent types.
runGetTableInfo_ ::
  ( CacheRM m,
    MonadError Error.QErr m,
    Metadata.MetadataM m,
    MonadBaseControl IO m,
    MonadIO m
  ) =>
  GetTableInfo_ ->
  m EncJSON
runGetTableInfo_ GetTableInfo_ {..} = do
  metadata <- Metadata.getMetadata

  let sources = fmap Metadata.unBackendSourceMetadata $ Metadata._metaSources metadata
  abSourceMetadata <- lookupSourceMetadata _gtiSourceName_ sources

  AnyBackend.dispatchAnyBackend @RQL.Types.Backend abSourceMetadata $ \Metadata.SourceMetadata {_smKind} -> do
    case _smKind of
      Backend.DataConnectorKind _dcName -> do
        fmap EncJSON.encJFromJValue (getTableInfo @'DataConnector _gtiSourceName_ _gtiTableName_)
      backend ->
        Error.throw500 ("Schema fetching is not supported for '" <> Text.E.toTxt backend <> "'")

data GetTableInfo (b :: BackendType) = GetTableInfo
  { _gtiSourceName :: Common.SourceName,
    _gtiTableName :: TableName b
  }

instance (Backend b) => FromJSON (GetTableInfo b) where
  parseJSON = J.withObject "GetTableInfo_" \o -> do
    _gtiSourceName <- o .: "source"
    _gtiTableName <- o .: "table"
    pure $ GetTableInfo {..}

-- | Get information about the given table.
runGetTableInfo ::
  forall b m.
  ( BackendMetadata b,
    CacheRM m,
    MonadError Error.QErr m,
    Metadata.MetadataM m,
    MonadBaseControl IO m,
    MonadIO m
  ) =>
  GetTableInfo b ->
  m EncJSON
runGetTableInfo GetTableInfo {..} = do
  fmap EncJSON.encJFromJValue (getTableInfo @b _gtiSourceName _gtiTableName)

--------------------------------------------------------------------------------
-- Internal helper functions

lookupSourceMetadata :: (MonadError QErr m) => SourceName -> InsOrdHashMap SourceName (AnyBackend SourceMetadata) -> m (AnyBackend SourceMetadata)
lookupSourceMetadata sourceName sources =
  InsOrdHashMap.lookup sourceName sources
    `onNothing` Error.throw400 Error.DataConnectorError ("Source '" <> Text.E.toTxt sourceName <> "' not found")
