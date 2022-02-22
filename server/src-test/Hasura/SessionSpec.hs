module Hasura.SessionSpec (spec) where

import Hasura.Prelude
import Hasura.QuickCheck.Instances ()
import Hasura.Server.Utils (isSessionVariable)
import Hasura.Session (sessionVariableToText)
import Test.Hspec (Spec, describe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = describe "SessionVariable" $ do
  prop "Arbitrary instance generates valid session variables" $ \v ->
    sessionVariableToText v `shouldSatisfy` isSessionVariable
