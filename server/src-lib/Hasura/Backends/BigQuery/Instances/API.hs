{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.BigQuery.Instances.API where

import           Hasura.Prelude

import           Hasura.SQL.Backend
import           Hasura.Server.API.Backend


instance BackendAPI 'BigQuery where
  parseBackendMetadataV1 = curry $ concat <$> sequenceA
    [ sourceCommands           @'BigQuery
    , tableCommands            @'BigQuery
    , tablePermissionsCommands @'BigQuery
    , relationshipCommands     @'BigQuery
    ]
