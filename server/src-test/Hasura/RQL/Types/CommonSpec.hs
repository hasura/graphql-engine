module Hasura.RQL.Types.CommonSpec (spec) where

import Control.Arrow.Extended
import Control.Monad.Trans.Writer
import Data.Text qualified as Text
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.QuickCheck.Instances ()
import Hasura.RQL.Types
import Network.URI (isAbsoluteURI)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

noPasswordParams :: PGConnectionParams
noPasswordParams =
  PGConnectionParams
    "localhost"
    "postgres"
    Nothing
    5432
    "postgres"

passwordParams :: PGConnectionParams
passwordParams =
  PGConnectionParams
    "localhost"
    "postgres"
    (Just "password")
    5432
    "postgres"

escapeCharParams :: PGConnectionParams
escapeCharParams =
  PGConnectionParams
    "loc@lhost"
    "r00t"
    (Just "p@ssw0rd")
    5432
    "test//db"

spec :: Spec
spec = do
  pgConnectionStringFromParamsSpec
  commentSpec
  withRecordInconsistencyEqualSpec

pgConnectionStringFromParamsSpec :: Spec
pgConnectionStringFromParamsSpec =
  describe "getPGConnectionStringFromParams" $ do
    it "should return a valid connection string when provided with no password within params" $ do
      let connectionString = getPGConnectionStringFromParams noPasswordParams

      connectionString `shouldBe` "postgresql://postgres@localhost:5432/postgres"
      isAbsoluteURI connectionString `shouldBe` True

    it "should return a valid connection string when provided with a password within params" $ do
      let connectionString = getPGConnectionStringFromParams passwordParams

      connectionString `shouldBe` "postgresql://postgres:password@localhost:5432/postgres"
      isAbsoluteURI connectionString `shouldBe` True

    it "should return a valid connection string when the input contains characters that need to be escaped" $ do
      let connectionString = getPGConnectionStringFromParams escapeCharParams

      connectionString `shouldBe` "postgresql://r00t:p%40ssw0rd@loc%40lhost:5432/test%2F%2Fdb"
      isAbsoluteURI connectionString `shouldBe` True

commentSpec :: Spec
commentSpec =
  describe "Comment" $ do
    prop "should roundtrip between Comment and Maybe Text" $
      \str ->
        let text = Text.pack <$> str
         in (commentToMaybeText . commentFromMaybeText) text `shouldBe` text

withRecordInconsistencyEqualSpec :: Spec
withRecordInconsistencyEqualSpec =
  describe "withRecordInconsistency" do
    prop "Should equal withRecordInconsistencyM" $
      \inputMetadata (errOrUnit :: Either QErr ()) ->
        let arrowInputArr = ErrorA (arr (const errOrUnit))
            arrow = withRecordInconsistency @(Kleisli (Writer (Seq InconsistentMetadata))) arrowInputArr
            arrowOutput =
              runWriter $ runKleisli arrow ((), (inputMetadata, ()))
            monadInput = liftEither errOrUnit
            monadOutput = runWriter $ withRecordInconsistencyM inputMetadata monadInput
         in arrowOutput `shouldBe` monadOutput
