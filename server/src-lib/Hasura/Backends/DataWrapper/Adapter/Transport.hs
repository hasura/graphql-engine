{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataWrapper.Adapter.Transport () where

--------------------------------------------------------------------------------

import Control.Exception.Safe (throwIO)
import Hasura.Backends.DataWrapper.Adapter.Execute ()
import Hasura.Base.Error (Code (NotSupported), throw400)
import Hasura.GraphQL.Transport.Backend (BackendTransport (..))
import Hasura.Prelude
import Hasura.SQL.Backend (BackendType (DataWrapper))

--------------------------------------------------------------------------------

instance BackendTransport 'DataWrapper where
  runDBQuery _ _ _ _ _ _ _ _ =
    throw400 NotSupported "runDBQuery: not implemented for GraphQL Data Wrappers."
  runDBQueryExplain _ =
    throw400 NotSupported "runDBQueryExplain: not implemented for GraphQL Data Wrappers."
  runDBMutation _ _ _ _ _ _ _ _ =
    throw400 NotSupported "runDBMutation: not implemented for GraphQL Data Wrappers."
  runDBSubscription _ _ _ =
    liftIO . throwIO $ userError "runDBSubscription: not implemented for GraphQL Data Wrappers."
