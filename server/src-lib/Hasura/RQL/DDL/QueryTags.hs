module Hasura.RQL.DDL.QueryTags where

import           Hasura.Prelude

import           Control.Lens
import qualified Data.Aeson.TH              as J
import qualified Data.HashMap.Strict.InsOrd as OM
import           Data.Text.Extended         (toTxt, (<<>))

import           Data.Aeson

import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.RQL.Types
import           Hasura.SQL.Tag

import qualified Hasura.SQL.AnyBackend      as AB

data SetQueryTagsConfig
  = SetQueryTagsConfig
  { _sqtSourceName :: !SourceName
  , _sqtConfig     :: !QueryTagsConfig
  }
$(J.deriveToJSON hasuraJSON{J.omitNothingFields=True} ''SetQueryTagsConfig)
$(makeLenses ''SetQueryTagsConfig)

instance FromJSON SetQueryTagsConfig where
  parseJSON = withObject "SetQueryTagsConfig" $ \o -> do
    sourceName <- o.: "source_name"
    queryTagsConfig <- parseJSON $ Object o
    pure $ SetQueryTagsConfig sourceName queryTagsConfig

runSetQueryTagsConfig
  :: (MonadError QErr m, MetadataM m, CacheRWM m)
  => SetQueryTagsConfig -> m EncJSON
runSetQueryTagsConfig (SetQueryTagsConfig sourceName queryTagsConfig) = do
  oldMetadata <- getMetadata
  case OM.lookup sourceName (_metaSources oldMetadata) of
    Nothing -> throw400 NotExists $ "source with name " <> sourceName <<> " does not exist"
    Just exists -> do
      let backendType = getBackendType exists
      case backendType of
        Postgres Vanilla -> setQueryTagsConfigInMetadata exists (Just queryTagsConfig)
        Postgres Citus   -> setQueryTagsConfigInMetadata exists (Just queryTagsConfig)
        _                -> queryTagsNotSupported backendType
  where
    getBackendType :: BackendSourceMetadata -> BackendType
    getBackendType exists =
      AB.dispatchAnyBackend @BackendMetadata exists $ \(_sourceMetadata :: SourceMetadata b) ->
        reify $ backendTag @b

    setQueryTagsConfigInMetadata exists qtConfig = do
      let metadataModifier = queryTagsMetadataModifier exists qtConfig
      buildSchemaCacheFor (MOSource sourceName) metadataModifier
      return successMsg

    queryTagsNotSupported backendType = throw400 NotSupported $ toTxt backendType <> " sources do not support query-tags yet"

    queryTagsMetadataModifier exists qtConfig =
      AB.dispatchAnyBackend @BackendMetadata exists $ \(_sourceMetadata :: SourceMetadata b) ->
        MetadataModifier $ metaSources.ix sourceName.toSourceMetadata @b.smQueryTags .~ qtConfig
