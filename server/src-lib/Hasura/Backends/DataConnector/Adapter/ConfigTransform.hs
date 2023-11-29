module Hasura.Backends.DataConnector.Adapter.ConfigTransform
  ( transformSourceConfig,
    validateConnSourceConfig,
  )
where

--------------------------------------------------------------------------------

import Control.Exception.Safe (SomeException (..), catch)
import Data.Aeson qualified as J
import Data.Aeson.Kriti.Functions qualified as KFunc
import Data.ByteString qualified as BS
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as HashMap
import Data.List (isPrefixOf)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Extended qualified as Text
import Hasura.Backends.DataConnector.API (ConfigSchemaResponse)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Adapter.Types (ConnSourceConfig (..), SourceConfig (..))
import Hasura.Backends.DataConnector.Adapter.Types qualified as DC
import Hasura.Base.Error (Code (..), QErr, internalError, throw400)
import Hasura.Prelude
import Hasura.RQL.Types.Common as Common
import Hasura.Session (SessionVariables)
import Kriti.Error qualified as Kriti
import System.Directory (canonicalizePath)

--------------------------------------------------------------------------------

transformConfig :: (MonadError QErr m) => API.Config -> Maybe Text -> Maybe SessionVariables -> Env.Environment -> HashMap DC.TemplateVariableName Text -> m API.Config
transformConfig config maybeTemplate sessionVariables env templateVariables = do
  case maybeTemplate of
    Nothing -> pure config
    (Just t) ->
      case KFunc.runKritiWith t ([("$config", J.toJSON config), ("$env", J.toJSON env), ("$session", maybe (J.object []) J.toJSON sessionVariables), ("$vars", J.toJSON templateVariables)]) (additionalFunctions env) of
        Left e -> throw400 NotSupported $ "transformConfig: Kriti template transform failed - " <> tshow e
        Right (J.Object r) -> pure $ API.Config r
        Right o -> throw400 NotSupported $ "transformConfig: Kriti did not decode into Object - " <> tshow o

transformSourceConfig :: (MonadError QErr m, MonadIO m) => SourceConfig -> Maybe SessionVariables -> m SourceConfig
transformSourceConfig sc@SourceConfig {_scConfig, _scTemplate, _scTemplateVariables, _scEnvironment} sessionVariables = do
  resolvedTemplateVariables <- resolveTemplateVariables _scEnvironment _scTemplateVariables
  transformedConfig <- transformConfig _scConfig _scTemplate sessionVariables _scEnvironment resolvedTemplateVariables
  pure sc {_scConfig = transformedConfig}

-- | Apply a transformation to a 'ConnSourceConfig' without validating the result.
transformConnSourceConfigUnsafe :: (MonadError QErr m, MonadIO m) => ConnSourceConfig -> Maybe SessionVariables -> Env.Environment -> m API.Config
transformConnSourceConfigUnsafe ConnSourceConfig {..} sessionVariables env = do
  resolvedTemplateVariables <- resolveTemplateVariables env (fromMaybe mempty _cscTemplateVariables)
  transformConfig _cscValue _cscTemplate sessionVariables env resolvedTemplateVariables

-- | Apply a transformation to a 'ConnSourceConfig' and validate the result.
validateConnSourceConfig ::
  (MonadError QErr m, MonadIO m) =>
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

resolveTemplateVariables :: (MonadIO m, MonadError QErr m) => Env.Environment -> HashMap DC.TemplateVariableName DC.TemplateVariableSource -> m (HashMap DC.TemplateVariableName Text)
resolveTemplateVariables env templateVariables = do
  flip HashMap.traverseWithKey templateVariables $ \(DC.TemplateVariableName variableName) variableSource ->
    case variableSource of
      DC.TemplateVariableDynamicFromFile filepath -> do
        allowedPrefixNonCanon <-
          Env.lookupEnv env "HASURA_GRAPHQL_DYNAMIC_SECRETS_ALLOWED_PATH_PREFIX"
            `onNothing` throw400 PermissionError "'dynamic_from_file' template variables are not allowed because your administrator has not set the HASURA_GRAPHQL_DYNAMIC_SECRETS_ALLOWED_PATH_PREFIX environment variable."
        when (allowedPrefixNonCanon == "")
          $ throw400 PermissionError "'dynamic_from_file' template variables are not allowed because your administrator has not set the HASURA_GRAPHQL_DYNAMIC_SECRETS_ALLOWED_PATH_PREFIX environment variable to a non-empty value."

        allowedPrefixCanon <- liftIO $ canonicalizePath allowedPrefixNonCanon
        canonFilepath <- liftIO $ canonicalizePath filepath
        unless (allowedPrefixCanon `isPrefixOf` canonFilepath)
          $ throw400 PermissionError ("For the template variable " <> variableName <> ", the supplied dynamic_from_file file path, when canonicalized, does not match the allowed prefix set by your administrator via the HASURA_GRAPHQL_DYNAMIC_SECRETS_ALLOWED_PATH_PREFIX environment variable")

        result <- liftIO $ readFromFile canonFilepath
        liftEither result
  where
    readFromFile :: FilePath -> IO (Either QErr Text)
    readFromFile canonFilepath =
      (Right . Text.strip . Text.decodeUtf8 <$> BS.readFile canonFilepath) `catch` \(SomeException e) ->
        pure . Left . internalError $ "Error reading template variable dynamically from file '" <> Text.pack canonFilepath <> "': " <> tshow e
