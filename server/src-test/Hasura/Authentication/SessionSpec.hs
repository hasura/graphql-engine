module Hasura.Authentication.SessionSpec (spec) where

import Data.ByteString qualified as BS
import Hasura.Authentication.Session
import Hasura.Prelude
import Hasura.QuickCheck.Instances ()
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = describe "SessionVariable" $ do
  prop "Arbitrary instance generates valid session variables" $ \v ->
    (fromSessionVariable v :: Text) `shouldSatisfy` isSessionVariable

  describe "mkSessionVariablesHeaders" $ do
    let customHeader = unsafeMkSessionVariable ("x-hasura-bruker-navn" :: Text)

    it "decodes UTF-8 header values" $ do
      let headerValue = BS.pack [84, 101, 115, 116, 195, 165, 195, 165]
      getSessionVariableValue customHeader (mkSessionVariablesHeaders [("x-hasura-bruker-navn", headerValue)])
        `shouldBe` Just "Test\229\229"

    it "decodes legacy Latin-1 header values without replacement characters" $ do
      let headerValue = BS.pack [84, 101, 115, 116, 229, 229]
      getSessionVariableValue customHeader (mkSessionVariablesHeaders [("x-hasura-bruker-navn", headerValue)])
        `shouldBe` Just "Test\229\229"
