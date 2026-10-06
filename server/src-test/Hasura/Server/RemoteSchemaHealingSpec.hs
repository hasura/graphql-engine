module Hasura.Server.RemoteSchemaHealingSpec (spec) where

import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as HashMap
import Data.Text.NonEmpty (mkNonEmptyTextUnsafe)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime)
import Hasura.Authentication.Role (mkRoleNameSafe)
import Hasura.Prelude
import Hasura.RQL.Types.Metadata.Object
import Hasura.RemoteSchema.Metadata (RemoteSchemaName (..))
import Hasura.Server.RemoteSchemaHealing
import Test.Hspec

spec :: Spec
spec = describe "RemoteSchemaHealing" do
  describe "nextBackoff" do
    it "doubles the delay after each failure, up to the maximum" do
      let delays = map heDelay $ take 9 $ iterate (nextBackoff t0) (newEntry (seconds 5) t0 J.Null)
      delays `shouldBe` map seconds [5, 10, 20, 40, 80, 120, 120, 120, 120]

    it "schedules the next attempt using the current delay" do
      let entry = nextBackoff t0 $ nextBackoff t0 $ newEntry (seconds 5) t0 J.Null
      heNextAttempt entry `shouldBe` addUTCTime 10 t0
      heAttempts entry `shouldBe` 2

    it "is due immediately when first seen" do
      heNextAttempt (newEntry (seconds 5) t0 J.Null) `shouldBe` t0

    it "never exceeds the maximum, even for a large initial interval" do
      heDelay (nextBackoff t0 $ newEntry (seconds 600) t0 J.Null) `shouldBe` maxBackoff

  describe "inconsistentRemoteSchemas" do
    it "only includes remote schemas which are themselves inconsistent" do
      let role = mkRoleNameSafe $ mkNonEmptyTextUnsafe "user"
          inconsistencies =
            [ InconsistentObject "unreachable" Nothing (MetadataObject (MORemoteSchema rsA) (J.String "a")),
              InconsistentObject "bad permission" Nothing (MetadataObject (MORemoteSchemaPermissions rsB role) (J.String "b-perm")),
              ConflictingObjects "conflict" [MetadataObject (MORemoteSchema rsC) (J.String "c")]
            ]
      inconsistentRemoteSchemas inconsistencies `shouldBe` HashMap.fromList [(rsA, J.String "a")]
  where
    t0 = UTCTime (fromGregorian 2026 1 1) 0
    rsA = RemoteSchemaName $ mkNonEmptyTextUnsafe "a"
    rsB = RemoteSchemaName $ mkNonEmptyTextUnsafe "b"
    rsC = RemoteSchemaName $ mkNonEmptyTextUnsafe "c"
