{-# LANGUAGE StandaloneKindSignatures #-}

-- |
-- This module holds functions and data types used for logging at the GraphQL
-- layer. Unlike QueryLog, these are fired after queries are finished so could
-- include things like execution time in future.
module Hasura.GraphQL.Logging.ExecutionLog
  ( ExecutionLog (..),
    ExecutionStats (..),
    statsToAnyBackend,
    MonadExecutionLog (..),
  )
where

import Data.Aeson qualified as J
import Data.Kind (Type)
import Hasura.EncJSON (EncJSON)
import Hasura.GraphQL.Execute.Backend (ActionResult (..))
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types.Backend (Backend (ExecutionStatistics))
import Hasura.RQL.Types.BackendTag (HasTag)
import Hasura.RQL.Types.BackendType (BackendType)
import Hasura.SQL.AnyBackend (AnyBackend, dispatchAnyBackend', mkAnyBackend)
import Hasura.Server.Types (RequestId)
import Hasura.Tracing (TraceT)

-- | A GraphQL query, optionally generated SQL, and the request id makes up the
-- | 'ExecutionLog'
data ExecutionLog = ExecutionLog
  { _elRequestId :: !RequestId,
    _elStatistics :: !(Maybe (AnyBackend ExecutionStats))
  }

-- | 'ExecutionStatistics' is a type family, which means we can't partially
-- apply it (in 'AnyBackend', for example). To get round this, we have a
-- newtype that really just wraps the type family.
type ExecutionStats :: BackendType -> Type
newtype ExecutionStats b = ExecutionStats (ExecutionStatistics b)

-- | When we want to log anything from 'DBStepInfo', we first need to transform
-- the backend-specific execution statistics into 'AnyBackend' statistics. This
-- is fine in practice because all we do with it is log it as JSON.
statsToAnyBackend :: forall b. (HasTag b) => ActionResult b -> (Maybe (AnyBackend ExecutionStats), EncJSON)
statsToAnyBackend ActionResult {..} =
  (fmap (mkAnyBackend @b . ExecutionStats) arStatistics, arResult)

deriving newtype instance (Backend b) => J.ToJSON (ExecutionStats b)

instance J.ToJSON ExecutionLog where
  toJSON (ExecutionLog reqId mstatistics) =
    J.object
      $ [ "request_id" J..= reqId,
          "statistics" J..= case mstatistics of
            Just statistics -> dispatchAnyBackend' @J.ToJSON statistics J.toJSON
            Nothing -> J.toJSON ()
        ]

instance L.ToEngineLog ExecutionLog L.Hasura where
  toEngineLog ql = (L.LevelInfo, L.ELTExecutionLog, J.toJSON ql)

class (Monad m) => MonadExecutionLog m where
  logExecutionLog ::
    L.Logger L.Hasura ->
    ExecutionLog ->
    m ()

instance (MonadExecutionLog m) => MonadExecutionLog (ExceptT e m) where
  logExecutionLog logger l = lift $ logExecutionLog logger l

instance (MonadExecutionLog m) => MonadExecutionLog (ReaderT r m) where
  logExecutionLog logger l = lift $ logExecutionLog logger l

instance (MonadExecutionLog m) => MonadExecutionLog (TraceT m) where
  logExecutionLog logger l = lift $ logExecutionLog logger l
