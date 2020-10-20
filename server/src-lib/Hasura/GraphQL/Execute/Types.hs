module Hasura.GraphQL.Execute.Types where

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.Types.Error

import           Control.Monad.Trans.Control (MonadBaseControl)

import qualified Data.Aeson                  as J
import qualified Hasura.Tracing              as Tracing

-- graphql-engine supports two GraphQL interfaces: one at v1/graphql, and a Relay one at v1beta1/relay
data GraphQLQueryType
  = QueryHasura
  | QueryRelay
  deriving (Show, Eq, Ord, Generic)
instance Hashable GraphQLQueryType

instance J.ToJSON GraphQLQueryType where
  toJSON = \case
    QueryHasura -> "hasura"
    QueryRelay  -> "relay"

newtype ActionExecution =
  ActionExecution {
  unActionExecution
    :: forall m
     . ( MonadIO m
       , MonadBaseControl IO m
       , MonadError QErr m
       , Tracing.MonadTrace m
       ) => m EncJSON
  }
