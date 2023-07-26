{-# OPTIONS_GHC -Wno-unused-imports #-}

module Harness.Services.Composed
  ( module E,
    TestServicesConfig (..),
    mkTestServicesConfig,
    teardownServices,
  )
where

import Data.Has
import Data.Text qualified as T
import Harness.Constants qualified as Constants
import Harness.Logging
import Harness.Services.Database.Postgres as E
import Harness.Services.ExternalProcess.GraphqlEngine as E hiding (drawFromPool)
import Harness.Services.GraphqlEngine as E
import Harness.Services.GraphqlEngine.API as E
import Harness.Services.Schema as E
import Harness.Services.Source.Postgres as E
import Hasura.Prelude
import System.Directory
import System.Environment

-- | Keys/addresses to all resources managed externally to the test harness.
data TestServicesConfig = TestServicesConfig
  { tscHgeBinPath :: HgeBinPath,
    tscHgePool :: HgePool,
    tscPassthroughEnvVars :: PassthroughEnvVars,
    tscPostgresServerUrl :: PostgresServerUrl
    -- Cockroach/Citus/...
    -- Bigquery credentials?
  }

instance Has HgePool TestServicesConfig where
  getter = tscHgePool
  modifier f x = x {tscHgePool = f (tscHgePool x)}

instance Has HgeBinPath TestServicesConfig where
  getter = tscHgeBinPath
  modifier f x = x {tscHgeBinPath = f (tscHgeBinPath x)}

instance Has PostgresServerUrl TestServicesConfig where
  getter = tscPostgresServerUrl
  modifier f x = x {tscPostgresServerUrl = f (tscPostgresServerUrl x)}

instance Has PassthroughEnvVars TestServicesConfig where
  getter = tscPassthroughEnvVars
  modifier f x = x {tscPassthroughEnvVars = f (tscPassthroughEnvVars x)}

mkTestServicesConfig :: Logger -> IO TestServicesConfig
mkTestServicesConfig logger = do
  tscHgeBinPath <- HgeBinPath <$> getExeFromEnvVar "GRAPHQL_ENGINE"
  tscHgePool <- mkHgeInstancePool logger
  tscPassthroughEnvVars <- mkPassthroughEnv

  let tscPostgresServerUrl = PostgresServerUrl $ T.pack postgresqlInitialConnectionString
  pure TestServicesConfig {..}

teardownServices :: TestServicesConfig -> IO ()
teardownServices servicesConfig = do
  hgePoolDestroy (tscHgePool servicesConfig)

getExeFromEnvVar :: String -> IO String
getExeFromEnvVar envVar = do
  binPath <-
    lookupEnv envVar
      `onNothingM` error ("Environment variable '" ++ envVar ++ "' not specified.")

  exists <- doesFileExist binPath
  unless exists $ error ("(" ++ envVar ++ ") The file '" ++ binPath ++ "' does not exist.")
  permissions <- getPermissions binPath
  unless (executable permissions) $ error ("(" ++ envVar ++ ") The file '" ++ binPath ++ "' is not executable.")

  return binPath

postgresqlInitialConnectionString :: String
postgresqlInitialConnectionString = "postgresql://hasura:hasura@127.0.0.1:65002/hasura_metadata"

-- | when running HGE via a binary, what should we pass through from the test's
-- environment into the fresh HGE?
envToPassthrough :: [String]
envToPassthrough =
  [ Constants.bigqueryServiceKeyVar,
    "HASURA_GRAPHQL_EE_LICENSE_KEY"
  ]

-- | grab items from env to pass through to new HGE instances
mkPassthroughEnv :: IO PassthroughEnvVars
mkPassthroughEnv =
  let lookup' env = do
        value <- fromMaybe "" <$> lookupEnv env
        pure (env, value)
   in PassthroughEnvVars <$> traverse lookup' envToPassthrough
