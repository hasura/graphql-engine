{-# LANGUAGE QuasiQuotes #-}

-- | Data Connector helpers.
module Harness.Backend.DataConnector
  ( mkLocalTestEnvironment,
    setup,
    teardown,
    defaultSourceMetadata,
  )
where

--------------------------------------------------------------------------------

import Control.Concurrent (ThreadId, forkIO, killThread)
import Data.Aeson (Value)
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Context (BackendType (DataConnector), defaultBackendTypeString, defaultSource)
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Backends.DataConnector.Agent.Server (runDcServer)
import Prelude

--------------------------------------------------------------------------------

defaultSourceMetadata :: Value
defaultSourceMetadata =
  let source = defaultSource DataConnector
      backendType = defaultBackendTypeString DataConnector
   in [yaml|
name : *source
kind: *backendType
tables:
  - table: "albums"
    object_relationships:
      - name: "artist"
        using:
          manual_configuration:
            remote_table: "artists"
            column_mapping:
              artist_id: "id"
  - table: "artists"
    array_relationships:
      - name: "albums"
        using:
          manual_configuration:
            remote_table: "albums"
            column_mapping:
              id: "artist_id"
configuration: {}
|]

defaultBackendConfig :: Value
defaultBackendConfig =
  let backendType = defaultBackendTypeString DataConnector
   in [yaml|
dataconnector:
  *backendType:
    uri: "http://localhost:8100/"
|]

mkLocalTestEnvironment :: TestEnvironment -> IO ThreadId
mkLocalTestEnvironment _ = forkIO runDcServer

-- | Setup the schema in the most expected way.
setup :: (TestEnvironment, ThreadId) -> IO ()
setup (testEnvironment, _) = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource testEnvironment defaultSourceMetadata (Just defaultBackendConfig)

-- | Teardown the schema and tracking in the most expected way.
teardown :: (TestEnvironment, ThreadId) -> IO ()
teardown (testEnvironment, agentThread) = do
  GraphqlEngine.clearMetadata testEnvironment
  killThread agentThread
