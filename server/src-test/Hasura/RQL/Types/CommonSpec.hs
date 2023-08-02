module Hasura.RQL.Types.CommonSpec (spec) where

--------------------------------------------------------------------------------

import Control.Arrow.Extended
import Control.Lens qualified as Lens
import Control.Monad.Trans.Writer
import Data.Aeson qualified as J
import Data.Aeson.Lens
import Data.Text qualified as Text
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.Base.Error.TestInstances ()
import Hasura.Generator.Common
import Hasura.Prelude
import Hasura.QuickCheck.Instances ()
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.SchemaCache.Build
import Hedgehog.Gen qualified as Gen
import Hedgehog.Generic
import Network.URI (isAbsoluteURI)
import Test.Hspec
import Test.Hspec.Hedgehog
import Test.Hspec.QuickCheck (prop)

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  pgConnectionStringFromParamsSpec
  commentSpec
  withRecordInconsistencyEqualSpec
  aesonSpecs

--------------------------------------------------------------------------------

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
    prop "should roundtrip between Comment and Maybe Text"
      $ \str ->
        let text = Text.pack <$> str
         in (commentToMaybeText . commentFromMaybeText) text `shouldBe` text

withRecordInconsistencyEqualSpec :: Spec
withRecordInconsistencyEqualSpec =
  describe "withRecordInconsistency" do
    prop "Should equal withRecordInconsistencyM"
      $ \inputMetadata (errOrUnit :: Either QErr ()) ->
        let arrowInputArr = ErrorA (arr (const errOrUnit))
            arrow = withRecordInconsistency arrowInputArr
            arrowOutput =
              runWriter $ runKleisli arrow ((), (inputMetadata, ()))
            monadInput = liftEither errOrUnit
            monadOutput = runWriter $ withRecordInconsistencyM inputMetadata monadInput
         in arrowOutput `shouldBe` monadOutput

--------------------------------------------------------------------------------

aesonSpecs :: Spec
aesonSpecs = describe "AesonSpecs" do
  envRecordSpec
  remoteRelationshipGSpec
  aesonRoundTrips

envRecordSpec :: Spec
envRecordSpec = describe "EnvRecord" do
  it "Serializes to JSON" $ hedgehog do
    term <- forAll genEnvRecord
    let value :: J.Value = J.toJSON term

    Lens.preview (key "env_var" . _String) value === Just (_envVarName term)

remoteRelationshipGSpec :: Spec
remoteRelationshipGSpec = describe "RemoteRelationshipG" do
  it "Serializes to JSON" $ hedgehog $ do
    term <- forAll genRemoteRelationshipG
    let value :: J.Value = J.toJSON term

    Lens.preview (key "name" . _String) value === Just (toTxt (_rrName term))
    Lens.preview (key "definition" . _JSON) value === Just ()

-- | JSON Round Trip Serialization tests for non derived instances.
--
-- TODO: 'InputWebhook' and 'UrlConf' have deeply nested types that all will need generator instances.
aesonRoundTrips :: Spec
aesonRoundTrips = describe "JSON RoundTrip:" $ do
  jsonRoundTrip (hgen @RelType) "RelType"
  jsonRoundTrip (hgen @InsertOrder) "InsertOrder"
  jsonRoundTrip genSourceName "SourceName"
  -- jsonRoundTrip (hgen @InputWebhook) "InputWebhook"
  jsonRoundTrip genTimeout "Timeout"
  jsonRoundTrip genPGConnectionParams "PGConnectionParams"
  -- jsonRoundTrip (hgen @UrlConf) "UrlConf"
  jsonRoundTrip (hgen @MetricsConfig) "MetricsConfig"
  jsonRoundTrip genComment "Comment"
  jsonRoundTrip (hgen @ApolloFederationVersion) "ApolloFederationVersion"
  jsonRoundTrip (hgen @ApolloFederationConfig) "ApolloFederationConfig"
  jsonRoundTrip (hgen @TriggerOnReplication) "TriggerOnReplication"

--------------------------------------------------------------------------------
-- Generators

genSourceName :: Gen SourceName
genSourceName = choice [pure SNDefault, SNName <$> genNonEmptyText defaultRange]

genTimeout :: Gen Timeout
genTimeout = Timeout <$> genInt

genEnvRecord :: Gen (EnvRecord ())
genEnvRecord = EnvRecord <$> genText <*> pure ()

genComment :: Gen Comment
genComment = Gen.choice [pure Automatic, Explicit <$> Gen.maybe (genNonEmptyText defaultRange)]

genRelName :: Gen RelName
genRelName = RelName <$> genNonEmptyText defaultRange

genPGConnectionParams :: Gen PGConnectionParams
genPGConnectionParams = do
  _pgcpHost <- genText
  _pgcpUsername <- genText
  _pgcpPassword <- Gen.maybe genText
  _pgcpPort <- genInt
  _pgcpDatabase <- genText
  pure PGConnectionParams {..}

genRemoteRelationshipG :: Gen (RemoteRelationshipG ())
genRemoteRelationshipG = do
  _rrName <- genRelName
  let _rrDefinition = ()
  pure RemoteRelationship {..}
