-- | Module of reusable functions for Kriti transforms.
--
-- NOTE: This defines an alternative `runKritiWith` that includes the basicFunctions by default.
--       You should probably invoke Kriti through this module rather than directly in order to
--       make updating the functions available only require touching this module.
--
-- TODO: This should be added to the documentation and referenced in (for-example) REST Connectors once
--       the documentation refactor project is complete.
module Data.Aeson.Kriti.Functions (runKriti, runKritiWith, basicFunctions, environmentFunctions, sessionFunctions) where

import Control.Arrow (left)
import Data.Aeson qualified as J
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Hasura.Prelude
import Hasura.Session (SessionVariables, getSessionVariableValue, mkSessionVariable)
import Kriti qualified
import Kriti.CustomFunctions qualified as Kriti
import Kriti.Error (SerializeError (serialize), SerializedError)
import Kriti.Error qualified as Kriti

type KritiFunc = J.Value -> Either Kriti.CustomFunctionError J.Value

-- | `Data.Aeson.Kriti.Functions.runKriti` attaches the basicFunctions by default
--   NOTE: The error type is SerializedError due to KritiError not currently being exported
runKriti :: Text -> [(Text, J.Value)] -> Either SerializedError J.Value
runKriti t m = left serialize $ Kriti.runKritiWith t m basicFunctions

-- | `Data.Aeson.Kriti.Functions.runKritiWith` attaches the basicFunctions by default.
runKritiWith :: Text -> [(Text, J.Value)] -> HashMap Text KritiFunc -> Either SerializedError J.Value
runKritiWith t m f = left serialize $ Kriti.runKritiWith t m (basicFunctions <> f)

-- | Re-Export of the Kriti 'stdlib'
basicFunctions :: HashMap.HashMap Text KritiFunc
basicFunctions = Kriti.basicFuncMap

-- | Functions that interact with environment variables
environmentFunctions :: Env.Environment -> HashMap.HashMap Text KritiFunc
environmentFunctions env =
  HashMap.fromList
    [ ("getEnvironmentVariable", getEnvVar)
    ]
  where
    getEnvVar :: J.Value -> Either Kriti.CustomFunctionError J.Value
    getEnvVar = \case
      J.Null -> Right $ J.Null
      J.String k -> Right $ J.toJSON $ Env.lookupEnv env (T.unpack k)
      _ -> Left $ Kriti.CustomFunctionError "Environment variable name should be a string"

-- | Functions that interact with HGE session during requests
sessionFunctions :: Maybe SessionVariables -> HashMap.HashMap Text KritiFunc
sessionFunctions sessionVars = HashMap.singleton "getSessionVariable" getSessionVar
  where
    -- Returns Null if session-variables aren't passed in
    -- Throws an error if session variable isn't found. Perhaps a version that returns null would also be useful.
    -- Lookups are case-insensitive
    getSessionVar :: J.Value -> Either Kriti.CustomFunctionError J.Value
    getSessionVar = \case
      J.Null -> Right $ J.Null
      J.String txt ->
        case sessionVars >>= getSessionVariableValue (mkSessionVariable txt) of
          Just x -> Right $ J.String x
          Nothing -> Left . Kriti.CustomFunctionError $ "Session variable \"" <> txt <> "\" not found"
      _ -> Left $ Kriti.CustomFunctionError "Session variable name should be a string"
