{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataConnector.Adapter.API () where

--------------------------------------------------------------------------------

import Hasura.Prelude
import Hasura.SQL.Backend (BackendType (DataConnector))
import Hasura.Server.API.Backend (BackendAPI (..), sourceCommands, tableCommands)

--------------------------------------------------------------------------------

instance BackendAPI 'DataConnector where
  metadataV1CommandParsers =
    concat
      [ sourceCommands @'DataConnector,
        tableCommands @'DataConnector
      ]
