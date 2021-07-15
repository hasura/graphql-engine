{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.MySQL.Instances.API where

import           Hasura.Prelude

import           Hasura.SQL.Backend
import           Hasura.Server.API.Backend


instance BackendAPI 'MySQL where
  metadataV1CommandParsers = concat
    [ sourceCommands           @'MySQL
    , tableCommands            @'MySQL
    , tablePermissionsCommands @'MySQL
    , relationshipCommands     @'MySQL
    ]
