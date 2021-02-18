module Hasura.GraphQL.Execute.Action where

import           Hasura.Prelude

import           Control.Monad.Trans.Control                 (MonadBaseControl)

import qualified Hasura.Tracing                              as Tracing

import           Hasura.EncJSON
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.Action


newtype ActionExecution =
  ActionExecution {
    unActionExecution
      :: forall m
       . (MonadIO m, MonadBaseControl IO m, MonadError QErr m, Tracing.MonadTrace m) => m EncJSON
  }

data ActionExecutionPlan
  = AEPSync !ActionExecution
  | AEPAsyncQuery !ActionId !(ActionLogResponse -> ActionExecution)
  | AEPAsyncMutation !EncJSON
