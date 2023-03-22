{-# LANGUAGE QuasiQuotes #-}

-- | This module houses low-level functions and types to help access and work
-- with the hge metadata api.
module Harness.Services.Metadata (export_metadata, replace_metadata) where

import Data.Aeson
import Data.Has
import Harness.Logging
import Harness.Quoter.Yaml (yaml)
import Harness.Services.GraphqlEngine
import Hasura.Prelude

export_metadata :: (Has Logger env, Has HgeServerInstance env) => env -> IO Value
export_metadata env = do
  hgePost
    env
    200
    "/v1/metadata"
    []
    [yaml|
        type: export_metadata
        args: null
      |]

replace_metadata :: (Has Logger env, Has HgeServerInstance env) => env -> Value -> IO Value
replace_metadata env newMetadata = do
  hgePost
    env
    200
    "/v1/metadata"
    []
    [yaml|
        type: replace_metadata
        args:
          allow_inconsistent_metadata: true
          metadata: *newMetadata
      |]
