{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Experimental.Adapter.Transport () where

--------------------------------------------------------------------------------

import Control.Exception (throwIO)
import Hasura.Base.Error (Code (NotSupported), throw400)
import Hasura.Experimental.Adapter.Execute ()
import Hasura.GraphQL.Transport.Backend (BackendTransport (..))
import Hasura.Prelude
import Hasura.SQL.Backend (BackendType (Experimental))

--------------------------------------------------------------------------------

instance BackendTransport 'Experimental where
  runDBQuery _ _ _ _ _ _ _ _ =
    throw400 NotSupported "runDBQuery: not implemented for Experimental"
  runDBQueryExplain _ =
    throw400 NotSupported "runDBQueryExplain: not implemented for Experimental"
  runDBMutation _ _ _ _ _ _ _ _ =
    throw400 NotSupported "runDBMutation: not implemented for Experimental"
  runDBSubscription _ _ _ =
    liftIO $ throwIO $ userError "runDBSubscription: not implemented for Experimental"
