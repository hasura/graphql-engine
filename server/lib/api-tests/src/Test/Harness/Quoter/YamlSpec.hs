{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}

-- who tests the test framework?
module Test.Harness.Quoter.YamlSpec (spec) where

import Data.Aeson qualified as J
import Data.Aeson.KeyMap qualified as KM
import GHC.Generics
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.TestEnvironment
import Hasura.Prelude
import Test.Hspec

--------------------------------------------------------------------------------

-- test datatype we will use to insert some yaml
data MakeSomeYaml = MakeSomeYaml
  { msyString :: String,
    msyBool :: Bool,
    msyNumber :: Int,
    msyArray :: [MakeSomeYaml]
  }
  deriving stock (Generic)
  deriving anyclass (J.ToJSON)

-- ** Preamble

spec :: SpecWith GlobalTestEnvironment
spec = describe "Yaml quasiquoters" $ do
  describe "yaml quoter" $ do
    it "Interpolates a simple value using a Yaml anchor" $ const do
      let interpolatedValue = (100 :: Int)
      let input =
            [yaml|
              type: pg_create_select_permission
              args:
                limit: *interpolatedValue
            |]
      let expected =
            [yaml|
              type: pg_create_select_permission
              args:
                limit: 100
            |]
      input `shouldBe` expected

    it "Interpolates a complex value using a Yaml anchor" $ const do
      let complexValue =
            MakeSomeYaml
              { msyString = "hello",
                msyNumber = 100,
                msyBool = True,
                msyArray = []
              }
      let expected =
            J.Object
              ( KM.fromList
                  [ ("type", J.String "thing"),
                    ("complex", J.toJSON complexValue)
                  ]
              )

      let input =
            [yaml|
              type: thing
              complex: *complexValue
            |]
      input `shouldBe` expected

    it "Interpolates a key using a Yaml anchor" $ const do
      let interpolatedValue = ("limit" :: String)
      let input =
            [yaml|
              type: pg_create_select_permission
              args:
                *interpolatedValue: 100
            |]
      let expected =
            [yaml|
              type: pg_create_select_permission
              args:
                limit: 100
            |]
      input `shouldBe` expected

  describe "interpolateYaml quoter" $ do
    it "Copes with input that contains lots of single quotes" $ const do
      let interpolateValue = ("hello" :: String)
      let input =
            [interpolateYaml|
              errors:
              - extensions:
                  code: validation-failed
                  path: $.selectionSet.hasura_author.selectionSet.notPresentCol
                message: |-
                  field 'notPresentCol' not found in type: '#{interpolateValue}_author'
            |]
      let expected =
            [interpolateYaml|
              errors:
              - extensions:
                  code: validation-failed
                  path: $.selectionSet.hasura_author.selectionSet.notPresentCol
                message: |-
                  field 'notPresentCol' not found in type: 'hello_author'
            |]

      input `shouldBe` expected

    it "Interpolates a Haskell value as expected" $ const do
      let interpolatedValue = (100 :: Int)
      let input :: J.Value
          input =
            [interpolateYaml|
              type: pg_create_select_permission
              args:
                limit: #{interpolatedValue}
            |]
      let expected :: J.Value
          expected =
            [yaml|
              type: pg_create_select_permission
              args:
                limit: 100
            |]
      input `shouldBe` expected

    it "Interpolates a Haskell expression as expected" $ const do
      let input :: J.Value
          input =
            [interpolateYaml|
              type: pg_create_select_permission
              args:
                limit: #{ (1 + 2 + 3 :: Int) }
            |]
      let expected :: J.Value
          expected =
            [yaml|
              type: pg_create_select_permission
              args:
                limit: 6
            |]
      input `shouldBe` expected

    it "Interpolation does not fail when parsing a '*'" $ const do
      let input :: J.Value
          input =
            [interpolateYaml|
              "*"
            |]
          expected =
            [yaml|
              "*"
            |]
      input `shouldBe` expected
