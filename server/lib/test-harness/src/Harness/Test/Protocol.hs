module Harness.Test.Protocol
  ( withEachProtocol,
  )
where

import Harness.TestEnvironment (GlobalTestEnvironment, Protocol (..))
import Harness.TestEnvironment qualified as TestEnvironment
import Hasura.Prelude
import Test.Hspec (ActionWith, SpecWith, aroundAllWith, describe)
import Test.Hspec.Core.Spec (Item (..), mapSpecItem_)

-- | Query and mutation requests to @graphql-engine@ can be performed with both
-- HTTP and web socket requests. This function takes a set of tests that make
-- @graphql-engine@ requests and runs them twice: once with those requests
-- being made via HTTP, and again with them being made via web sockets.
withEachProtocol :: SpecWith GlobalTestEnvironment -> SpecWith GlobalTestEnvironment
withEachProtocol spec = do
  let withHTTP :: (ActionWith GlobalTestEnvironment -> IO ()) -> (ActionWith GlobalTestEnvironment -> IO ())
      withHTTP k test = k \globalTestEnvironment -> test globalTestEnvironment {TestEnvironment.requestProtocol = HTTP}

  describe "Over HTTP" $ flip mapSpecItem_ spec \item ->
    item {itemExample = \params -> itemExample item params . withHTTP}

  let connectWS :: ActionWith GlobalTestEnvironment -> ActionWith GlobalTestEnvironment
      connectWS k globalTestEnvironment = do
        k
          globalTestEnvironment
            { TestEnvironment.requestProtocol = WebSocket
            }

  aroundAllWith connectWS $ describe "Over WebSockets" $ flip mapSpecItem_ spec \item ->
    item {itemExample = \params -> itemExample item params}
