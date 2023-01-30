module Hasura.Backends.Postgres.Connection.VersionCheckSpec (spec) where

import Hasura.Backends.Postgres.Connection.VersionCheck
import Hasura.Prelude
import Test.Hspec

spec :: Spec
spec = do
  describe "CockroachDB" crdbVersionSpec

crdbVersionSpec :: Spec
crdbVersionSpec = do
  describe "The CockroachDB version parser" $ do
    it "parses 'v22.2.0-beta.4'" $ do
      parseCrdbVersion
        "CockroachDB CCL v22.2.0-beta.4 (x86_64-pc-linux-gnu, built 2022/10/17 14:34:07, go1.19.1)"
        `shouldBe` Right (CockroachDbVersion 22 2 0 "-beta.4")

    it "parses 'v22.1.0'" $ do
      parseCrdbVersion
        "CockroachDB CCL v22.1.0 (x86_64-pc-linux-gnu, built 2022/05/23 16:27:47, go1.17.6)"
        `shouldBe` Right (CockroachDbVersion 22 1 0 "")

  describe "The CockroachDB version checker" $ do
    for_ ["", "-beta.4"] $ \rest -> do
      it ("rejects 'v5.5.3" <> rest <> "'") $ do
        CockroachDbVersion 5 5 3 rest `shouldNotSatisfy` crdbVersionIsSupported

      it ("rejects 'v19.5.3" <> rest <> "'") $ do
        CockroachDbVersion 19 5 3 rest `shouldNotSatisfy` crdbVersionIsSupported

      it ("rejects 'v22.0.4" <> rest <> "'") $ do
        CockroachDbVersion 22 0 4 rest `shouldNotSatisfy` crdbVersionIsSupported

      it ("rejects 'v22.1.0" <> rest <> "'") $ do
        CockroachDbVersion 22 1 0 rest `shouldNotSatisfy` crdbVersionIsSupported

      it ("approves 'v22.2.0" <> rest <> "'") $ do
        CockroachDbVersion 22 2 0 rest `shouldSatisfy` crdbVersionIsSupported

      it ("approves 'v22.2.1" <> rest <> "'") $ do
        CockroachDbVersion 22 2 1 rest `shouldSatisfy` crdbVersionIsSupported

      it ("approves 'v22.2.2" <> rest <> "'") $ do
        CockroachDbVersion 22 2 2 rest `shouldSatisfy` crdbVersionIsSupported

      it ("approves 'v22.3.0" <> rest <> "'") $ do
        CockroachDbVersion 22 3 0 rest `shouldSatisfy` crdbVersionIsSupported

      it ("approves 'v23.0.0" <> rest <> "'") $ do
        CockroachDbVersion 23 0 0 rest `shouldSatisfy` crdbVersionIsSupported

      it ("approves 'v30.0.0" <> rest <> "'") $ do
        CockroachDbVersion 30 0 0 rest `shouldSatisfy` crdbVersionIsSupported
