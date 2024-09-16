module Hasura.Authentication.SessionSpec (spec) where

import Hasura.Authentication.Session
import Hasura.Prelude
import Hasura.QuickCheck.Instances ()
import Test.Hspec (Spec, describe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = describe "SessionVariable" $ do
  prop "Arbitrary instance generates valid session variables" $ \v ->
    (fromSessionVariable v :: Text) `shouldSatisfy` isSessionVariable
