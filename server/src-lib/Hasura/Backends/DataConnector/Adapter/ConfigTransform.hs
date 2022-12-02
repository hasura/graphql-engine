module Hasura.Backends.DataConnector.Adapter.ConfigTransform
  ( transformSourceConfig,
    transformConnSourceConfig,
    validateConfiguration,
    getConfigSchemaResponse,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson qualified as J
import Data.Aeson.Kriti.Functions qualified as KFunc
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as Text
import Data.Text.Extended qualified as Text
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Adapter.Types (ConnSourceConfig (ConnSourceConfig, template, value), SourceConfig (..))
import Hasura.Backends.DataConnector.Adapter.Types qualified as DC
import Hasura.Base.Error (Code (DataConnectorError, NotSupported), QErr, throw400)
import Hasura.Prelude
import Hasura.RQL.Types.Common as Common
import Hasura.RQL.Types.SchemaCache
import Hasura.SQL.Backend qualified as Backend
import Kriti.Error qualified as Kriti

--------------------------------------------------------------------------------

transformConfig :: (MonadError QErr m) => API.Config -> Maybe Text -> [(Text, J.Value)] -> Env.Environment -> m API.Config
transformConfig config maybeTemplate scope env = do
  case maybeTemplate of
    Nothing -> pure config
    (Just t) ->
      case KFunc.runKritiWith t (("$config", J.toJSON config) : scope) (additionalFunctions env) of
        Left e -> throw400 NotSupported $ "transformConfig: Kriti template transform failed - " <> tshow e
        Right (J.Object r) -> pure $ API.Config r
        Right o -> throw400 NotSupported $ "transformConfig: Kriti did not decode into Object - " <> tshow o

transformSourceConfig :: (MonadError QErr m) => SourceConfig -> [(Text, J.Value)] -> Env.Environment -> m SourceConfig
transformSourceConfig sc@SourceConfig {_scConfig, _scTemplate} scope env = do
  transformedConfig <- transformConfig _scConfig _scTemplate scope env
  pure sc {_scConfig = transformedConfig}

transformConnSourceConfig :: (MonadError QErr m) => ConnSourceConfig -> [(Text, J.Value)] -> Env.Environment -> m API.Config
transformConnSourceConfig ConnSourceConfig {value, template} scope env = transformConfig value template scope env

--------------------------------------------------------------------------------

-- | Given a 'DC.DataConnectorName' fetch the associated
-- 'DC.DataConnectorInfo' from the SchemaCache.
getDataConnectorInfo' :: CacheRM m => DC.DataConnectorName -> m (Maybe DC.DataConnectorInfo)
getDataConnectorInfo' dataConnectorName = do
  bmap <- getBackendInfo @'Backend.DataConnector
  pure $ bmap >>= HashMap.lookup dataConnectorName

-- | Given a 'DC.DataConnectorName' fetch the associated
-- 'DC.DataConnectorInfo' from the SchemaCache. Lookup failures are
-- pushed into 'MonadError QErr m'.
getDataConnectorInfo :: (CacheRM m, MonadError QErr m) => DC.DataConnectorName -> m DC.DataConnectorInfo
getDataConnectorInfo dataConnectorName = do
  onNothingM (getDataConnectorInfo' dataConnectorName) $
    throw400 DataConnectorError ("Data connector named " <> Text.dquote dataConnectorName <> " was not found in the data connector backend info")

-- | Given a 'DC.DataConnectorName' fetch the associated
-- 'API.ConfigSchemaResponse' from the SchemaCache. Lookup failures
-- are pushed into 'MonadError QErr m'.
getConfigSchemaResponse :: (CacheRM m, MonadError QErr m) => DC.DataConnectorName -> m API.ConfigSchemaResponse
getConfigSchemaResponse = fmap DC._dciConfigSchemaResponse . getDataConnectorInfo

--------------------------------------------------------------------------------

validateConfiguration ::
  MonadError QErr m =>
  Common.SourceName ->
  DC.DataConnectorName ->
  API.ConfigSchemaResponse ->
  API.Config ->
  m ()
validateConfiguration sourceName dataConnectorName configSchema config = do
  let errors = API.validateConfigAgainstConfigSchema configSchema config
  unless (null errors) $
    let errorsText = Text.unlines (("- " <>) . Text.pack <$> errors)
     in throw400
          DataConnectorError
          ("Configuration for source " <> Text.dquote sourceName <> " is not valid based on the configuration schema declared by the " <> Text.dquote dataConnectorName <> " data connector agent. Errors:\n" <> errorsText)

additionalFunctions :: Env.Environment -> HashMap Text (J.Value -> Either Kriti.CustomFunctionError J.Value)
additionalFunctions env = KFunc.environmentFunctions env
