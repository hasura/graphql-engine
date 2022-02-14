{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Experimental.Adapter.Execute
  (
  )
where

--------------------------------------------------------------------------------

import Hasura.Base.Error (Code (NotSupported), QErr, throw400, throw500)
import Hasura.GraphQL.Execute.Backend (BackendExecute (..))
import Hasura.Prelude
import Hasura.SQL.Backend (BackendType (Experimental))
import Hasura.Tracing qualified as Tracing

--------------------------------------------------------------------------------

instance BackendExecute 'Experimental where
  type PreparedQuery 'Experimental = ()
  type MultiplexedQuery 'Experimental = Void
  type ExecutionMonad 'Experimental = Tracing.TraceT (ExceptT QErr IO)

  mkDBQueryPlan _ _ _ _ =
    throw400 NotSupported "mkDBQueryPlan: not implemented for Experimental"
  mkDBQueryExplain _ _ _ _ _ =
    throw400 NotSupported "mkDBQueryExplain: not implemented for Experimental"
  mkDBMutationPlan _ _ _ _ _ =
    throw400 NotSupported "mkDBMutationPlan: Experimental backend does not support this operation yet."
  mkDBSubscriptionPlan _ _ _ _ _ =
    throw400 NotSupported "mkDBSubscriptionPlan: Experimental backend does not support this operation yet."
  mkDBRemoteRelationshipPlan _ _ _ _ _ _ _ =
    throw500 "mkDBRemoteRelationshipPlan: Experimental backend does not currently support generalized joins."
  mkLiveQueryExplain _ =
    throw400 NotSupported "mkLiveQueryExplain: Experimental backend does not support this operation yet."
