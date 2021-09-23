module Hasura.RQL.DDL.Schema.Source where

import Control.Lens (at, (.~), (^.))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson
import Data.Aeson qualified as J
import Data.Aeson.TH
import Data.Has
import Data.HashMap.Strict qualified as HM
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.DDL.Deps
import Hasura.RQL.DDL.Schema.Common
import Hasura.RQL.Types
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.Logging (MetadataLog (..))

--------------------------------------------------------------------------------
-- Add source

data AddSource b = AddSource
  { _asName :: !SourceName,
    _asConfiguration :: !(SourceConnConfiguration b),
    _asReplaceConfiguration :: !Bool
  }

instance (Backend b) => FromJSON (AddSource b) where
  parseJSON = withObject "AddSource" $ \o ->
    AddSource
      <$> o .: "name"
      <*> o .: "configuration"
      <*> o .:? "replace_configuration" .!= False

runAddSource ::
  forall m b.
  (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b) =>
  AddSource b ->
  m EncJSON
runAddSource (AddSource name sourceConfig replaceConfiguration) = do
  sources <- scSources <$> askSchemaCache

  metadataModifier <-
    MetadataModifier
      <$> if HM.member name sources
        then
          if replaceConfiguration
            then pure $ metaSources . ix name . toSourceMetadata @b . smConfiguration .~ sourceConfig
            else throw400 AlreadyExists $ "source with name " <> name <<> " already exists"
        else do
          let sourceMetadata = mkSourceMetadata @b name sourceConfig
          pure $ metaSources %~ OMap.insert name sourceMetadata

  buildSchemaCacheFor (MOSource name) metadataModifier
  pure successMsg

--------------------------------------------------------------------------------
-- Rename source

data RenameSource = RenameSource
  { _rmName :: !SourceName,
    _rmNewName :: !SourceName
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
          OMap.insert
            newKey
            (AB.mapBackend val (renameSource newKey))
            . OMap.delete oldKey
            $ m
        Nothing -> m

    renameSource :: forall b. SourceName -> SourceMetadata b -> SourceMetadata b
    renameSource newName metadata = metadata {_smName = newName}

--------------------------------------------------------------------------------
-- Drop source

data DropSource = DropSource
  { _dsName :: !SourceName,
    _dsCascade :: !Bool
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
runDropSource (DropSource name cascade) = do
  sc <- askSchemaCache
  logger <- asks getter
  let sources = scSources sc
  case HM.lookup name sources of
    Just backendSourceInfo ->
      AB.dispatchAnyBackend @BackendMetadata backendSourceInfo $ dropSource logger sc
    Nothing -> do
      metadata <- getMetadata
      void $
        onNothing (metadata ^. metaSources . at name) $
          throw400 NotExists $ "source with name " <> name <<> " does not exist"
      if cascade
        then -- Without sourceInfo we can't cascade, so throw an error
          throw400 Unexpected $ "source with name " <> name <<> " is inconsistent"
        else -- Drop source from metadata
          buildSchemaCacheFor (MOSource name) dropSourceMetadataModifier
  pure successMsg
  where
    dropSource :: forall b. (BackendMetadata b) => L.Logger L.Hasura -> SchemaCache -> SourceInfo b -> m ()
    dropSource logger sc sourceInfo = do
      let sourceConfig = _siConfiguration sourceInfo
      let indirectDeps =
            mapMaybe getIndirectDep $
              getDependentObjs sc (SOSource name)

      when (not cascade && indirectDeps /= []) $
        reportDepsExt
          (map (SOSourceObj name . AB.mkAnyBackend) indirectDeps)
          []

      metadataModifier <- execWriterT $ do
        mapM_ (purgeDependentObject name >=> tell) indirectDeps
        tell dropSourceMetadataModifier

      buildSchemaCacheFor (MOSource name) metadataModifier

      -- We only log errors that arise from 'postDropSourceHook' here, and not
      -- surface them as end-user errors. See comment
      -- https://github.com/hasura/graphql-engine/issues/7092#issuecomment-873845282
      runExceptT (postDropSourceHook @b sourceConfig) >>= either logDropSourceHookError pure
      where
        logDropSourceHookError err =
          let msg =
                "Error executing cleanup actions after removing source '" <> toTxt name
                  <> "'. Consider cleaning up tables in hdb_catalog schema manually."
           in L.unLogger logger $ MetadataLog L.LevelWarn msg (J.toJSON err)

        getIndirectDep :: SchemaObjId -> Maybe (SourceObjId b)
        getIndirectDep = \case
          SOSourceObj s o ->
            if s == name
              then Nothing
              else -- consider only *this* backend specific dependencies
                AB.unpackAnyBackend o
          _ -> Nothing

    dropSourceMetadataModifier = MetadataModifier $ metaSources %~ OMap.delete name
