{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataWrapper.Adapter.API () where

--------------------------------------------------------------------------------

import Hasura.Prelude
import Hasura.SQL.Backend (BackendType (DataWrapper))
import Hasura.Server.API.Backend (BackendAPI (..), sourceCommands, tableCommands)

--------------------------------------------------------------------------------

instance BackendAPI 'DataWrapper where
  metadataV1CommandParsers =
    concat
      [ sourceCommands @'DataWrapper,
        tableCommands @'DataWrapper
      ]
