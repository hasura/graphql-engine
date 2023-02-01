{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasura.Backends.Postgres.NativeQuery.NativeQuerySpec
  ( spec,
  )
where

import Hasura.Base.Error.TestInstances ()
import Hasura.NativeQuery.Metadata
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude

spec :: Spec
spec = do
  describe "Parses RawQuery into InterpolatedQuery" do
    it "Parses SQL with no parameters in it" $ do
      let rawSQL = "SELECT * FROM dogs"
      parseInterpolatedQuery rawSQL `shouldBe` Right (InterpolatedQuery [IIText "SELECT * FROM dogs"])

    it "Parses only a variable" $ do
      let rawSQL = "{{dogs}}"
      parseInterpolatedQuery rawSQL `shouldBe` Right (InterpolatedQuery [IIVariable (NativeQueryArgumentName "dogs")])

    it "Parses SQL with one parameter in it" $ do
      let rawSQL = "SELECT * FROM dogs WHERE name = {{name}}"
      parseInterpolatedQuery rawSQL
        `shouldBe` Right
          ( InterpolatedQuery
              [ IIText "SELECT * FROM dogs WHERE name = ",
                IIVariable (NativeQueryArgumentName "name")
              ]
          )

    it "Parses SQL with one parameter in the middle of the string" $ do
      let rawSQL = "SELECT * FROM dogs WHERE {{name}} = name"
      parseInterpolatedQuery rawSQL
        `shouldBe` Right
          ( InterpolatedQuery
              [ IIText "SELECT * FROM dogs WHERE ",
                IIVariable (NativeQueryArgumentName "name"),
                IIText " = name"
              ]
          )

    it "Parses SQL with one parameter and a loose { hanging around" $ do
      let rawSQL = "SELECT * FROM dogs WHERE {{name}} = '{doggy friend}'"
      parseInterpolatedQuery rawSQL
        `shouldBe` Right
          ( InterpolatedQuery
              [ IIText "SELECT * FROM dogs WHERE ",
                IIVariable (NativeQueryArgumentName "name"),
                IIText " = '{doggy friend}'"
              ]
          )

    it "What should happen for unclosed variable" $ do
      let rawSQL = "SELECT * FROM dogs WHERE {{name"
      parseInterpolatedQuery rawSQL `shouldBe` Left "Found '{{' without a matching closing '}}'"
