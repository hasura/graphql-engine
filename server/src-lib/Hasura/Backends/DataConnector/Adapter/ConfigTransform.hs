module Hasura.Backends.DataConnector.Adapter.ConfigTransform
  ( transformSourceConfig,
    transformConnSourceConfig,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson qualified as J
import Data.Aeson.Kriti.Functions qualified as KFunc
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as M
import Data.Text qualified as T
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Adapter.Types (ConnSourceConfig (ConnSourceConfig, template, value), SourceConfig (..))
import Hasura.Base.Error (Code (NotSupported), QErr, throw400)
import Hasura.Prelude
import Kriti.Error qualified as Kriti

transformConfig :: (MonadError QErr m) => API.Config -> Maybe Text -> [(T.Text, J.Value)] -> Env.Environment -> m API.Config
transformConfig config maybeTemplate scope env = do
  case maybeTemplate of
    Nothing -> pure config
    (Just t) ->
      case KFunc.runKritiWith t (("$config", J.toJSON config) : scope) (additionalFunctions env) of
        Left e -> throw400 NotSupported $ "transformConfig: Kriti template transform failed - " <> tshow e
        Right (J.Object r) -> pure $ API.Config r
        Right o -> throw400 NotSupported $ "transformConfig: Kriti did not decode into Object - " <> tshow o

transformSourceConfig :: (MonadError QErr m) => SourceConfig -> [(T.Text, J.Value)] -> Env.Environment -> m SourceConfig
transformSourceConfig sc@SourceConfig {_scConfig, _scTemplate} scope env = do
  transformedConfig <- transformConfig _scConfig _scTemplate scope env
  pure sc {_scConfig = transformedConfig}

transformConnSourceConfig :: (MonadError QErr m) => ConnSourceConfig -> [(T.Text, J.Value)] -> Env.Environment -> m API.Config
transformConnSourceConfig ConnSourceConfig {value, template} scope env = transformConfig value template scope env

additionalFunctions :: Env.Environment -> M.HashMap T.Text (J.Value -> Either Kriti.CustomFunctionError J.Value)
additionalFunctions env = KFunc.environmentFunctions env
