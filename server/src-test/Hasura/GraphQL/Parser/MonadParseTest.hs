module Hasura.GraphQL.Parser.MonadParseTest (spec) where

import Data.Aeson.Internal
import Hasura.Base.Error
import Hasura.Base.Error.TestInstances ()
import Hasura.Base.ErrorMessage
import Hasura.GraphQL.Parser.Class.Parse
import Hasura.GraphQL.Parser.Monad
import Hasura.Prelude
import Test.Hspec

runParse' :: Parse () -> Either QErr ()
runParse' = runParse @_ @()

errorMessage :: ErrorMessage
errorMessage = "oh no"

spec :: Spec
spec = do
  describe "withKey" $ do
    it "Path is empty when no keys added" $ do
      let parser = parseError errorMessage
          expected = (err400 ValidationFailed (fromErrorMessage errorMessage)) {qePath = mempty}
      runParse' parser `shouldBe` Left expected

    it "Path has two items in the order they were added" $ do
      let parser = withKey (Key "dog") (withKey (Key "log") (parseError errorMessage))
          expected = (err400 ValidationFailed (fromErrorMessage errorMessage)) {qePath = [Key "dog", Key "log"]}
      runParse' parser `shouldBe` Left expected
