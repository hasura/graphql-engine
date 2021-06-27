{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.SQLite.Instances.API where

import           Hasura.Prelude

import           Hasura.SQL.Backend
import           Hasura.Server.API.Backend


instance BackendAPI 'SQLite where
  parseBackendMetadataV1 = curry $ concat <$> sequenceA
    [ sourceCommands @'SQLite
    , tableCommands  @'SQLite
    ]
