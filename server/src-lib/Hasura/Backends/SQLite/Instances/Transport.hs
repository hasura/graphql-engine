{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.SQLite.Instances.Transport () where

import           Hasura.Prelude

import           Hasura.Backends.SQLite.Instances.Execute ()
import           Hasura.GraphQL.Execute.Backend
import           Hasura.GraphQL.Transport.Backend
import           Hasura.RQL.Types


instance BackendTransport 'SQLite  where
  runDBQuery        = \_ _ _ _ _ _ a _ -> withElapsedTime $ liftIO a
  runDBQueryExplain = \a               -> liftIO $ dbsiAction a
  runDBMutation     = error "not implemented"
  runDBSubscription = error "not implemented"
