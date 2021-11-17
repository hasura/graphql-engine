-- | Helpers for talking to graphql engine.
module Harness.GraphqlEngine
  ( post,
    post_,
    postGraphql,
    postGraphqlYaml,
  )
where

import Data.Aeson
import Data.Functor
import GHC.Stack
import Harness.Constants qualified as Constants
import Harness.Http qualified as Http
import Prelude

-- | Post some JSON to graphql-engine, getting back more
-- JSON. Optimistically assumes success. Use another function if you
-- want to test for failure.
post :: HasCallStack => String -> Value -> IO Value
post path = Http.postValue_ (Constants.graphqlEngineUrlPrefix ++ path)

-- | Same as post, but ignores the value.
post_ :: HasCallStack => String -> Value -> IO ()
post_ path = void . post path

-- | Same as post, but defaults to the graphql end-point.
postGraphqlYaml :: HasCallStack => Value -> IO Value
postGraphqlYaml = post "/v1/graphql"

-- | Same as 'postGraphqlYaml', but adds the {query:..} wrapper.
postGraphql :: HasCallStack => Value -> IO Value
postGraphql value = postGraphqlYaml (object ["query" .= value])
