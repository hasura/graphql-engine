{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataWrapper.Adapter.Execute
  (
  )
where

--------------------------------------------------------------------------------

import Hasura.Base.Error (Code (NotSupported), QErr, throw400, throw500)
import Hasura.GraphQL.Execute.Backend (BackendExecute (..))
import Hasura.Prelude
import Hasura.SQL.Backend (BackendType (DataWrapper))
import Hasura.Tracing qualified as Tracing

--------------------------------------------------------------------------------

instance BackendExecute 'DataWrapper where
  type PreparedQuery 'DataWrapper = ()
  type MultiplexedQuery 'DataWrapper = Void
  type ExecutionMonad 'DataWrapper = Tracing.TraceT (ExceptT QErr IO)

  mkDBQueryPlan _ _ _ _ =
    throw400 NotSupported "mkDBQueryPlan: not implemented for GraphQL Data Wrappers."
  mkDBQueryExplain _ _ _ _ _ =
    throw400 NotSupported "mkDBQueryExplain: not implemented for GraphQL Data Wrappers."
  mkDBMutationPlan _ _ _ _ _ =
    throw400 NotSupported "mkDBMutationPlan: not implemented for GraphQL Data Wrappers."
  mkDBSubscriptionPlan _ _ _ _ _ =
    throw400 NotSupported "mkDBSubscriptionPlan: not implemented for GraphQL Data Wrappers."
  mkDBRemoteRelationshipPlan _ _ _ _ _ _ _ =
    throw500 "mkDBRemoteRelationshipPlan: not implemented for GraphQL Data Wrappers."
  mkSubscriptionExplain _ =
    throw400 NotSupported "mkSubscriptionExplain: not implemented for GraphQL Data Wrappers."
