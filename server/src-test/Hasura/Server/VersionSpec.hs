module Hasura.Server.VersionSpec (spec) where

import Data.Text qualified as T
import Data.Text.Conversions (FromText (..))
import Hasura.Prelude
import Hasura.Server.Version
import Test.Hspec

versions :: [(Text, Text)]
versions =
  [ ("v1.2.4", "channel/stable/v1.2"),
    ("2.3.4", "channel/stable/v2.3"),
    ("v1.0.0-alpha45", "channel/alpha/v1.0"),
    ("v1.0.0-beta.01", "channel/beta/v1.0"),
    ("v2.3.1-rc.11", "channel/rc/v2.3"),
    ("v1.2.0-rj.1", "channel/rj/v1.2"),
    -- Apparently this one was generated in a different way?
    -- ("v2.0.0-pro.9", "channel/stable/v2.0"),
    ("v2.0.0-alpha.pro.9", "channel/alpha/v2.0"),
    ("v2.12.0-ce", "channel/versioned/v2.12.0-ce"),
    ("v2.12.1-ce", "channel/versioned/v2.12.1-ce"),
    ("v2.12.0-beta.1-ce", "channel/versioned/v2.12.0-beta.1-ce"),
    ("v2.12.0-beta.2-ce", "channel/versioned/v2.12.0-beta.2-ce"),
    ("v2.12.0", "channel/stable/v2.12"),
    ("v2.12.1", "channel/stable/v2.12"),
    ("v2.12.0-beta.1", "channel/beta/v2.12"),
    ("v2.12.0-beta.2", "channel/beta/v2.12"),
    ("v2.12.0-alpha.1", "channel/alpha/v2.12"),
    ("v2.12.0-alpha.2", "channel/alpha/v2.12")
    -- The Haskell server does not seem to be responsible for generating version strings for Cloud
    -- ("v2.12.0-cloud.1", "channel/versioned/v2.12.0-cloud.1")
    -- ("v2.0.0-cloud.9", "channel/stable/v2.0"),
    -- ("v2.0.0-alpha.cloud.9", "channel/alpha/v2.0"),
  ]

spec :: Spec
spec = describe "console assets version" do
  parallel
    $ for_ versions \(input, output) ->
      it ("versionToAssetsVersion returns expected output for " <> T.unpack input)
        $ versionToAssetsVersion (fromText input)
        `shouldBe` output
