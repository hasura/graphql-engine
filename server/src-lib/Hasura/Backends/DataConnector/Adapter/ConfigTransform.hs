module Hasura.Backends.DataConnector.Adapter.ConfigTransform
  ( transformSourceConfig,
    validateConnSourceConfig,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson qualified as J
import Data.Aeson.Kriti.Functions qualified as KFunc
import Data.Environment qualified as Env
import Data.Text qualified as Text
import Data.Text.Extended qualified as Text
import Hasura.Backends.DataConnector.API (ConfigSchemaResponse)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Adapter.Types (ConnSourceConfig (ConnSourceConfig, template, value), SourceConfig (..))
import Hasura.Backends.DataConnector.Adapter.Types qualified as DC
import Hasura.Base.Error (Code (DataConnectorError, NotSupported), QErr, throw400)
import Hasura.Prelude
import Hasura.RQL.Types.Common as Common
import Hasura.Session (SessionVariables)
import Kriti.Error qualified as Kriti

--------------------------------------------------------------------------------

transformConfig :: (MonadError QErr m) => API.Config -> Maybe Text -> Maybe SessionVariables -> Env.Environment -> m API.Config
transformConfig config maybeTemplate sessionVariables env = do
  case maybeTemplate of
    Nothing -> pure config
    (Just t) ->
      case KFunc.runKritiWith t ([("$config", J.toJSON config), ("$env", J.toJSON env), ("$session", maybe (J.object []) J.toJSON sessionVariables)]) (additionalFunctions env) of
        Left e -> throw400 NotSupported $ "transformConfig: Kriti template transform failed - " <> tshow e
        Right (J.Object r) -> pure $ API.Config r
        Right o -> throw400 NotSupported $ "transformConfig: Kriti did not decode into Object - " <> tshow o

transformSourceConfig :: (MonadError QErr m) => SourceConfig -> Maybe SessionVariables -> m SourceConfig
transformSourceConfig sc@SourceConfig {_scConfig, _scTemplate, _scEnvironment} sessionVariables = do
  transformedConfig <- transformConfig _scConfig _scTemplate sessionVariables _scEnvironment
  pure sc {_scConfig = transformedConfig}

-- | Apply a transformation to a 'ConnSourceConfig' without validating the result.
transformConnSourceConfigUnsafe :: (MonadError QErr m) => ConnSourceConfig -> Maybe SessionVariables -> Env.Environment -> m API.Config
transformConnSourceConfigUnsafe ConnSourceConfig {value, template} sessionVariables env = transformConfig value template sessionVariables env

-- | Apply a transformation to a 'ConnSourceConfig' and validate the result.
validateConnSourceConfig ::
  (MonadError QErr m) =>
  DC.DataConnectorName ->
  Common.SourceName ->
  ConfigSchemaResponse ->
  ConnSourceConfig ->
  Maybe SessionVariables ->
  Env.Environment ->
  m ()
validateConnSourceConfig dcName sourceName configSchemaResponse connSourceConfig sessionVariables env = do
  transformedConfig <- transformConnSourceConfigUnsafe connSourceConfig sessionVariables env
  validateConfiguration sourceName dcName configSchemaResponse transformedConfig

validateConfiguration ::
  (MonadError QErr m) =>
  Common.SourceName ->
  DC.DataConnectorName ->
  API.ConfigSchemaResponse ->
  API.Config ->
  m ()
validateConfiguration sourceName dataConnectorName configSchema config = do
  let errors = API.validateConfigAgainstConfigSchema configSchema config
  unless (null errors)
    $ let errorsText = Text.unlines (("- " <>) . Text.pack <$> errors)
       in throw400
            DataConnectorError
            ("Configuration for source " <> Text.dquote sourceName <> " is not valid based on the configuration schema declared by the " <> Text.dquote dataConnectorName <> " data connector agent. Errors:\n" <> errorsText)

additionalFunctions :: Env.Environment -> HashMap Text (J.Value -> Either Kriti.CustomFunctionError J.Value)
additionalFunctions env = KFunc.environmentFunctions env
