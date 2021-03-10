module Hasura.GraphQL.Execute.Action.Types where

import           Hasura.Prelude

import           Control.Monad.Trans.Control (MonadBaseControl)

import qualified Hasura.Tracing              as Tracing
import qualified Network.HTTP.Types          as HTTP

import           Hasura.EncJSON
import           Hasura.RQL.Types


newtype ActionExecution =
  ActionExecution {
    unActionExecution
      :: forall m
       . (MonadIO m, MonadBaseControl IO m, MonadError QErr m, Tracing.MonadTrace m) => m EncJSON
  }

-- A plan to execute any action
data ActionExecutionPlan
  = AEPSync !ActionExecution
  | AEPAsyncQuery !ActionId !(ActionLogResponse -> ActionExecution)
  | AEPAsyncMutation !EncJSON

data ActionExecuteResult
  = ActionExecuteResult
  { _aerExecution :: !ActionExecution
  , _aerHeaders   :: !HTTP.ResponseHeaders
  }
