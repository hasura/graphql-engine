{-# OPTIONS_GHC -fno-warn-orphans #-}


module Hasura.Backends.MySQL.Instances.Transport where


import           Hasura.Backends.MySQL.Instances.Execute ()
import           Hasura.GraphQL.Transport.Backend
import           Hasura.Prelude
import           Hasura.RQL.Types


instance BackendTransport 'MySQL where
  runDBQuery        = error "MySQL backend does not support this operation yet."
  runDBQueryExplain = error "MySQL backend does not support this operation yet."
  runDBMutation     = error "MySQL backend does not support this operation yet."
  runDBSubscription = error "MySQL backend does not support this operation yet."
