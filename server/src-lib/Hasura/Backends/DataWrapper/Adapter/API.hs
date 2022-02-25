{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataWrapper.Adapter.API () where

--------------------------------------------------------------------------------

import Hasura.SQL.Backend (BackendType (DataWrapper))
import Hasura.Server.API.Backend (BackendAPI (..))

--------------------------------------------------------------------------------

instance BackendAPI 'DataWrapper where
  metadataV1CommandParsers = []
