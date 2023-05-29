{-# LANGUAGE QuasiQuotes #-}

module Test.API.Metadata.TransparentDefaultsSpec (spec) where

import Control.Lens qualified as CL
import Data.Aeson (Value (Object))
import Data.Aeson.Lens qualified as AL
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postMetadata, postMetadata_)
import Harness.Quoter.Yaml
import Harness.Test.BackendType qualified as BT
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment
import Hasura.Prelude
import Test.Hspec

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ setupMetadata testEnvironment
                ]
            }
        ]
    )
    tests

tests :: SpecWith TestEnvironment
tests = do
  describe "properties of metadata in the presence of defaults" do
    describe "without metadata modifications" do
      -- Note that this requires that the foobar agent is running. Reuses sqlite service.
      it "includes defaults in list_source_kinds" \testEnvironment -> do
        response <- postMetadata testEnvironment listSourceKinds
        let response' = response CL.^.. AL.key "sources" . AL._Array . CL.folded . CL.filtered (\x -> (x CL.^? AL.key "kind") == Just "foobar")
            expected =
              [yaml|
            builtin: false
            display_name: "FOOBARDB"
            kind: foobar
            available: true
          |]
        response' `shouldBe` [expected]

      it "does not include defaults on stand alone export" \testEnvironment -> do
        response <- postMetadata testEnvironment exportMetadata
        let response' = Object $ response CL.^. AL.key "metadata" . AL._Object & CL.sans "sources"
            expected = [yaml| version: 3 |]
        -- Doesn't include defaults
        response' `shouldBe` expected

    describe "with metadata modifications" do
      it "does not include defaults on stand alone export" \testEnvironment -> do
        addSourceResults <- postMetadata testEnvironment addSource
        addSourceResults `shouldBe` [yaml| message: success |]

        response <- postMetadata testEnvironment exportMetadata
        let response' = Object $ response CL.^. AL.key "metadata" . AL._Object & CL.sans "sources"
            expected = [yaml| version: 3 |]
        -- Shouldn't include defaults
        response' `shouldBe` expected

exportMetadata :: Value
exportMetadata =
  [yaml|
    type: export_metadata
    version: 2
    args: {}
  |]

-- Only works if the defaults are applied.
addSource :: Value
addSource =
  [yaml|
    type: foobar_add_source
    args:
      driver: foobar
      name: myfoobar
      replace_configuration: false
      configuration:
        value: {}
  |]

listSourceKinds :: Value
listSourceKinds =
  [yaml|
    type: list_source_kinds
    version: 1
    args: {}
  |]

setupMetadata :: TestEnvironment -> Fixture.SetupAction
setupMetadata testEnvironment = do
  let sourceName = BT.backendSourceName btc
      sourceConfiguration = Postgres.defaultSourceConfiguration testEnvironment
      btc = fromMaybe (error "Couldn't find backendTypeConfig") (getBackendTypeConfig testEnvironment)

      setup :: IO ()
      setup =
        postMetadata_
          testEnvironment
          [yaml|
            type: replace_metadata
            args:
              metadata:
                version: 3
                sources:
                  - name: *sourceName
                    kind: postgres
                    tables: []
                    configuration: *sourceConfiguration
          |]

      teardown :: IO ()
      teardown = setup

  Fixture.SetupAction setup \_ -> teardown
