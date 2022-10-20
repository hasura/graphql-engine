{-# LANGUAGE QuasiQuotes #-}

-- | In Memory Chinook Reference Agent Configuration.
--
-- NOTE: This module is intended to be imported qualified.
module Harness.Backend.DataConnector.Chinook.Reference
  ( agentConfig,
    sourceConfiguration,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture
import Hasura.Prelude

--------------------------------------------------------------------------------

-- | Reference Agent @backend_configs@ field.
agentConfig :: Aeson.Value
agentConfig =
  let backendType = Fixture.defaultBackendTypeString $ Fixture.DataConnectorReference
   in [yaml|
dataconnector:
  *backendType:
    uri: "http://127.0.0.1:65005/"
|]

-- | Reference Agent specific @sources@ entry @configuration@ field.
sourceConfiguration :: Aeson.Value
sourceConfiguration = [yaml| {} |]
