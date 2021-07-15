{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.MySQL.Instances.Execute where


import           Hasura.Base.Error
import           Hasura.GraphQL.Execute.Backend
import           Hasura.Prelude
import           Hasura.RQL.Types
import qualified Hasura.Tracing                 as Tracing


instance BackendExecute 'MySQL where
  type PreparedQuery    'MySQL = Text
  type MultiplexedQuery 'MySQL = Void
  type ExecutionMonad   'MySQL = Tracing.TraceT (ExceptT QErr IO)
  mkDBQueryPlan                = error "MySQL backend does not support this operation yet."
  mkDBMutationPlan             = error "MySQL backend does not support this operation yet."
  mkDBSubscriptionPlan _ _ _ _ = error "MySQL backend does not support this operation yet."
  mkDBQueryExplain             = error "MySQL backend does not support this operation yet."
  mkLiveQueryExplain _         = error "MySQL backend does not support this operation yet."
