module Main (main) where

import ArrayRelationshipsSpec qualified
import BasicFieldsSpec qualified
import Control.Exception
import DirectivesSpec qualified
import Harness.GraphqlEngine (startServerThread, stopServer)
import Harness.State (State (..))
import LimitOffsetSpec qualified
import ObjectRelationshipsSpec qualified
import OrderingSpec qualified
import ServiceLivenessSpec qualified
import System.Environment
import Test.Hspec
import Text.Read
import ViewsSpec qualified
import WhereSpec qualified
import Prelude

setupState :: IO State
setupState = do
  murlPrefix <- lookupEnv "HASURA_TEST_URLPREFIX"
  mport <- fmap (>>= readMaybe) (lookupEnv "HASURA_TEST_PORT")
  server <- startServerThread ((,) <$> murlPrefix <*> mport)
  pure State {server}

teardownState :: State -> IO ()
teardownState State {server} =
  stopServer server

main :: IO ()
main =
  hspec $
    aroundAllWith (const . bracket setupState teardownState) $ do
      describe "ServiceLiveness" ServiceLivenessSpec.spec
      describe "BasicFields" BasicFieldsSpec.spec
      describe "Ordering" OrderingSpec.spec
      describe "Where" WhereSpec.spec
      describe "LimitOffset" LimitOffsetSpec.spec
      describe "ObjectRelationships" ObjectRelationshipsSpec.spec
      describe "ArrayRelationships" ArrayRelationshipsSpec.spec
      describe "Directives" DirectivesSpec.spec
      describe "Views" ViewsSpec.spec
