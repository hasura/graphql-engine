module Harness.Services.Composed
  ( module I,
    TestServicesConfig (..),
    mkTestServicesConfig,
  )
where

import Data.Has
import Data.Text qualified as T
import Harness.Services.GraphqlEngine as I
import Harness.Services.Postgres as I
import Hasura.Prelude
import System.Directory
import System.Environment

-- | Keys/addresses to all resources managed externally to the test harness.
data TestServicesConfig = TestServicesConfig
  { tscHgeBinPath :: HgeBinPath,
    tscPostgresServerUrl :: PostgresServerUrl
    -- Cockroach/Citus/...
    -- Bigquery credentials?
  }

instance Has HgeBinPath TestServicesConfig where
  getter = tscHgeBinPath
  modifier f x = x {tscHgeBinPath = f (tscHgeBinPath x)}

instance Has PostgresServerUrl TestServicesConfig where
  getter = tscPostgresServerUrl
  modifier f x = x {tscPostgresServerUrl = f (tscPostgresServerUrl x)}

mkTestServicesConfig :: IO TestServicesConfig
mkTestServicesConfig = do
  let var = "GRAPHQL_ENGINE"
  hgeBinPath <-
    lookupEnv var
      `onNothingM` error ("Environment variable '" ++ var ++ "' not specified.")
  let tscHgeBinPath = HgeBinPath hgeBinPath

  exists <- doesFileExist hgeBinPath
  unless exists $ error ("(" ++ var ++ ") The file '" ++ hgeBinPath ++ "' does not exist.")
  permissions <- getPermissions hgeBinPath
  unless (executable permissions) $ error ("(" ++ var ++ ") The file '" ++ hgeBinPath ++ "' is not executable.")

  let tscPostgresServerUrl = PostgresServerUrl $ T.pack postgresqlInitialConnectionString
  pure TestServicesConfig {..}

postgresqlInitialConnectionString :: String
postgresqlInitialConnectionString = "postgres://hasura:hasura@127.0.0.1:65002/hasura_metadata"
