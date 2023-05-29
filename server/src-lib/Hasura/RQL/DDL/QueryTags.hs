module Hasura.RQL.DDL.QueryTags
  ( SetQueryTagsConfig,
    runSetQueryTagsConfig,
  )
where

import Control.Lens
import Data.Aeson
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Text.Extended (toTxt, (<<>))
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.QueryTags.Types
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendTag
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.SQL.AnyBackend qualified as AB

data SetQueryTagsConfig = SetQueryTagsConfig
  { _sqtSourceName :: SourceName,
    _sqtConfig :: QueryTagsConfig
  }
  deriving stock (Generic)

instance ToJSON SetQueryTagsConfig where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}
  toEncoding = genericToEncoding hasuraJSON {omitNothingFields = True}

instance FromJSON SetQueryTagsConfig where
  parseJSON = withObject "SetQueryTagsConfig" $ \o -> do
    sourceName <- o .: "source_name"
    queryTagsConfig <- parseJSON $ Object o
    pure $ SetQueryTagsConfig sourceName queryTagsConfig

runSetQueryTagsConfig ::
  (MonadError QErr m, MetadataM m, CacheRWM m) =>
  SetQueryTagsConfig ->
  m EncJSON
runSetQueryTagsConfig (SetQueryTagsConfig sourceName queryTagsConfig) = do
  oldMetadata <- getMetadata
  case InsOrdHashMap.lookup sourceName (_metaSources oldMetadata) of
    Nothing -> throw400 NotExists $ "source with name " <> sourceName <<> " does not exist"
    Just exists -> do
      let backendType = getBackendType exists
      case backendType of
        Postgres _ -> setQueryTagsConfigInMetadata (unBackendSourceMetadata exists) (Just queryTagsConfig)
        _ -> queryTagsNotSupported backendType
  where
    getBackendType :: BackendSourceMetadata -> BackendType
    getBackendType exists =
      AB.dispatchAnyBackend @Backend (unBackendSourceMetadata exists) $ \(_sourceMetadata :: SourceMetadata b) ->
        reify $ backendTag @b

    setQueryTagsConfigInMetadata exists qtConfig = do
      let metadataModifier = queryTagsMetadataModifier exists qtConfig
      buildSchemaCacheFor (MOSource sourceName) metadataModifier
      return successMsg

    queryTagsNotSupported backendType = throw400 NotSupported $ toTxt backendType <> " sources do not support query-tags yet"

    queryTagsMetadataModifier exists qtConfig =
      AB.dispatchAnyBackend @Backend exists $ \(_sourceMetadata :: SourceMetadata b) ->
        MetadataModifier $ metaSources . ix sourceName . toSourceMetadata @b . smQueryTags .~ qtConfig
