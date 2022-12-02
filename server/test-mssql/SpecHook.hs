module SpecHook (hook) where

import Data.Text qualified as Text
import Database.MSSQL.Pool (ConnectionString (..))
import Hasura.Prelude
import System.Environment (getEnv)
import Test.Hspec

hook :: SpecWith ConnectionString -> Spec
hook = beforeWith (\_ -> ConnectionString . Text.pack <$> getEnv connectionStringEnvironmentVariable)

connectionStringEnvironmentVariable :: String
connectionStringEnvironmentVariable = "HASURA_MSSQL_CONN_STR"
