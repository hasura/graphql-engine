module Hasura.Server.Context
  ( HttpResponse(..)
  , GraphQLResponseConfig(..)
  , noDebugResponseConfig
  )
  where

import           Hasura.Prelude

import qualified Network.HTTP.Types as HTTP

data HttpResponse a
  = HttpResponse
  { _hrBody    :: !a
  , _hrHeaders :: !HTTP.ResponseHeaders
  } deriving (Functor, Foldable, Traversable)

-- | @'GraphQLResponseConfig' has configuration for debugging GraphQL error response.
-- See the github comment https://github.com/hasura/graphql-engine/issues/4031#issuecomment-609747705 for more details.
data GraphQLResponseConfig
  = GraphQLResponseConfig
  { _gscDeveloperMode       :: !Bool
  , _gscAdminInternalErrors :: !Bool
  } deriving (Show, Eq)

noDebugResponseConfig :: GraphQLResponseConfig
noDebugResponseConfig = GraphQLResponseConfig False False
