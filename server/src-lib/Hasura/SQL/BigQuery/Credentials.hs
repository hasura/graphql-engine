-- |

module Hasura.SQL.BigQuery.Credentials where

import           Data.Text (Text)
import qualified Data.Text as T
import           Prelude
import           System.Environment

--------------------------------------------------------------------------------
-- Types

data Credentials = Credentials
  { accessToken :: !Text
  , apiToken :: !Text
  , projectName :: !Text
  }

instance Show Credentials where
  show Credentials {projectName} =
    "Credentials { accessToken = _, apiToken = _, projectName = " <>
    show projectName <>
    " }"

--------------------------------------------------------------------------------
-- Handy testing

getCredentialsEnv :: IO Credentials
getCredentialsEnv = do
  accessToken <- getEnvUnline "BIGQUERYACCESSTOKEN"
  apiToken <- getEnvUnline "BIGQUERYAPITOKEN"
  projectName <- getEnvUnline "BIGQUERYPROJECTNAME"
  pure Credentials {..}
  where
    getEnvUnline key = fmap (T.pack . concat . take 1 . lines) (getEnv key)
