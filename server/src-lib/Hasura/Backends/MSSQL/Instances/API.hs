{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.MSSQL.Instances.API where

import           Hasura.Prelude

import           Hasura.SQL.Backend
import           Hasura.Server.API.Backend


instance BackendAPI 'MSSQL where
  metadataV1CommandParsers = concat
    [ sourceCommands           @'MSSQL
    , tableCommands            @'MSSQL
    , tablePermissionsCommands @'MSSQL
    , relationshipCommands     @'MSSQL
    ]
