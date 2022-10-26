{-# LANGUAGE QuasiQuotes #-}

-- | Sqlite Chinook Agent Configuration
--
-- NOTE: This module is intended to be imported qualified.
module Harness.Backend.DataConnector.Chinook.Sqlite
  ( agentConfig,
    sourceConfiguration,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture

--------------------------------------------------------------------------------

-- | Reference Agent @backend_configs@ field.
agentConfig :: Aeson.Value
agentConfig =
  let backendType = Fixture.defaultBackendTypeString Fixture.DataConnectorSqlite
   in [yaml|
dataconnector:
  *backendType:
    uri: "http://127.0.0.1:65007/"
|]

-- | Sqlite Agent specific @sources@ entry @configuration@ field.
sourceConfiguration :: Aeson.Value
sourceConfiguration =
  [yaml|
db: "/db.chinook.sqlite"
|]
