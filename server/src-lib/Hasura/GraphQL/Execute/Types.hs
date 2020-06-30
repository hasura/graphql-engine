module Hasura.GraphQL.Execute.Types (GraphQLQueryType(..)) where

import           Hasura.Prelude

import qualified Data.Aeson     as J

-- graphql-engine supports two GraphQL interfaces: one at v1/graphql, and a Relay one at v1/relay
data GraphQLQueryType
  = QueryHasura
  | QueryRelay
  deriving (Show, Eq, Ord, Generic)
instance Hashable GraphQLQueryType

instance J.ToJSON GraphQLQueryType where
  toJSON = \case
    QueryHasura -> "hasura"
    QueryRelay  -> "relay"
