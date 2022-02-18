module SpecHook
  ( hook,
  )
where

import Control.Exception.Safe (bracket)
import Harness.GraphqlEngine (startServerThread, stopServer)
import Harness.State (State (..))
import System.Environment (lookupEnv)
import Test.Hspec (Spec, SpecWith, aroundAllWith)
import Text.Read (readMaybe)
import Prelude

setupState :: IO State
setupState = do
  murlPrefix <- lookupEnv "HASURA_TEST_URLPREFIX"
  mport <- fmap (>>= readMaybe) (lookupEnv "HASURA_TEST_PORT")
  server <- startServerThread ((,) <$> murlPrefix <*> mport)
  pure $ State server

teardownState :: State -> IO ()
teardownState State {server} =
  stopServer server

hook :: SpecWith State -> Spec
hook = aroundAllWith (const . bracket setupState teardownState)
