module Hasura.GraphQL.Parser.MonadParseTest (spec) where

import Data.Aeson.Internal
import Hasura.Base.Error
import Hasura.Base.Error.TestInstances ()
import Hasura.GraphQL.Parser.Class.Parse
import Hasura.GraphQL.Parser.Monad
import Hasura.Prelude
import Test.Hspec

runParse' :: Parse () -> Either QErr ()
runParse' = runParse @_ @()

errorStr :: Text
errorStr = "oh no"

spec :: Spec
spec = do
  describe "withKey" $ do
    it "Path is empty when no keys added" $ do
      let parser = parseError errorStr
          expected = (err400 ValidationFailed errorStr) {qePath = mempty}
      runParse' parser `shouldBe` Left expected

    it "Path has two items in the order they were added" $ do
      let parser = withKey (Key "dog") (withKey (Key "log") (parseError errorStr))
          expected = (err400 ValidationFailed errorStr) {qePath = [Key "dog", Key "log"]}
      runParse' parser `shouldBe` Left expected
