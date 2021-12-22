{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Experimental.Adapter.API () where

--------------------------------------------------------------------------------

import Hasura.SQL.Backend (BackendType (Experimental))
import Hasura.Server.API.Backend (BackendAPI (..))

--------------------------------------------------------------------------------

instance BackendAPI 'Experimental where
  metadataV1CommandParsers = []
