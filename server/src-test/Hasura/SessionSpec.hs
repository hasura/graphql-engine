module Hasura.SessionSpec (spec) where

import Hasura.Generator ()
import Hasura.Prelude
import Hasura.Server.Utils
import Hasura.Session
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = describe "SessionVariable" $ do
  prop "Arbitrary instance generates valid session variables" $ \v ->
    sessionVariableToText v `shouldSatisfy` isSessionVariable
