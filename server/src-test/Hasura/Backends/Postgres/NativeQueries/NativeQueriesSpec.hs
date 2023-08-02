{-# HLINT ignore "avoid Language.GraphQL.Draft.Syntax.unsafeMkName" #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Hasura.Backends.Postgres.NativeQueries.NativeQueriesSpec (spec) where

import Data.Bifunctor
import Data.Either
import Data.HashMap.Strict qualified as HashMap
import Hasura.Backends.Postgres.Instances.NativeQueries
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Base.Error
import Hasura.LogicalModel.Cache (LogicalModelInfo (..))
import Hasura.LogicalModel.Metadata
import Hasura.LogicalModelResolver.Metadata
import Hasura.NativeQuery.InterpolatedQuery (trimQueryEnd)
import Hasura.NativeQuery.Metadata
import Hasura.Prelude hiding (first)
import Language.GraphQL.Draft.Syntax qualified as G
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
  describe "Parses raw query into InterpolatedQuery" do
    it "Parses SQL with no parameters in it" $ do
      let rawSQL = "SELECT * FROM dogs"
      parseInterpolatedQuery rawSQL `shouldBe` Right (InterpolatedQuery [IIText "SELECT * FROM dogs"])

    it "Parses only a variable" $ do
      let rawSQL = "{{dogs}}"
      parseInterpolatedQuery rawSQL `shouldBe` Right (InterpolatedQuery [IIVariable (ArgumentName "dogs")])

    it "Parses SQL with one parameter in it" $ do
      let rawSQL = "SELECT * FROM dogs WHERE name = {{name}}"
      parseInterpolatedQuery rawSQL
        `shouldBe` Right
          ( InterpolatedQuery
              [ IIText "SELECT * FROM dogs WHERE name = ",
                IIVariable (ArgumentName "name")
              ]
          )

    it "Parses SQL with one parameter in the middle of the string" $ do
      let rawSQL = "SELECT * FROM dogs WHERE {{name}} = name"
      parseInterpolatedQuery rawSQL
        `shouldBe` Right
          ( InterpolatedQuery
              [ IIText "SELECT * FROM dogs WHERE ",
                IIVariable (ArgumentName "name"),
                IIText " = name"
              ]
          )

    it "Parses SQL with one parameter and a loose { hanging around" $ do
      let rawSQL = "SELECT * FROM dogs WHERE {{name}} = '{doggy friend}'"
      parseInterpolatedQuery rawSQL
        `shouldBe` Right
          ( InterpolatedQuery
              [ IIText "SELECT * FROM dogs WHERE ",
                IIVariable (ArgumentName "name"),
                IIText " = '{doggy friend}'"
              ]
          )

    it "What should happen for unclosed variable" $ do
      let rawSQL = "SELECT * FROM dogs WHERE {{name"
      parseInterpolatedQuery rawSQL `shouldBe` Left "Found '{{' without a matching closing '}}'"

  describe "Validation" do
    let lmi =
          LogicalModelInfo
            { _lmiName = LogicalModelName (G.unsafeMkName "logical_model_name"),
              _lmiFields = mempty,
              _lmiDescription = Nothing,
              _lmiPermissions = mempty
            }

        nqm =
          NativeQueryMetadata
            { _nqmRootFieldName = NativeQueryName (G.unsafeMkName "root_field_name"),
              _nqmCode = InterpolatedQuery mempty,
              _nqmReturns = LMILogicalModelName $ LogicalModelName (G.unsafeMkName "logical_model_name"),
              _nqmArguments = mempty,
              _nqmArrayRelationships = mempty,
              _nqmObjectRelationships = mempty,
              _nqmDescription = mempty
            }

    it "Rejects undeclared variables" do
      let Right code = parseInterpolatedQuery "SELECT {{hey}}"
      let actual :: Either QErr Text = fmap snd $ runExcept $ nativeQueryToPreparedStatement lmi nqm {_nqmCode = code}

      (first showQErr actual) `shouldSatisfy` isLeft
      let Left err = actual
      qeCode err `shouldBe` ValidationFailed

    it "Handles multiple occurences of variables " do
      let Right code = parseInterpolatedQuery "SELECT {{hey}}, {{hey}}"
      let actual :: Either QErr Text =
            fmap snd
              $ runExcept
              $ nativeQueryToPreparedStatement
                lmi
                nqm
                  { _nqmCode = code,
                    _nqmArguments =
                      HashMap.fromList
                        [ (ArgumentName "hey", NullableScalarType PGVarchar False Nothing)
                        ]
                  }

      (first showQErr actual) `shouldSatisfy` isRight
      let Right rendered = actual
      rendered
        `shouldBe` "PREPARE _logimo_vali_(varchar) AS WITH _cte_logimo_vali_ AS (\nSELECT $1, $1\n)\nSELECT \nFROM _cte_logimo_vali_"

    it "Handles multiple variables " do
      let Right code = parseInterpolatedQuery "SELECT {{hey}}, {{ho}}"
      let actual :: Either QErr Text =
            fmap snd
              $ runExcept
              $ nativeQueryToPreparedStatement
                lmi
                nqm
                  { _nqmCode = code,
                    _nqmArguments =
                      HashMap.fromList
                        [ (ArgumentName "hey", NullableScalarType PGVarchar False Nothing),
                          (ArgumentName "ho", NullableScalarType PGInteger False Nothing)
                        ]
                  }

      (first showQErr actual) `shouldSatisfy` isRight
      let Right rendered = actual
      rendered
        `shouldBe` "PREPARE _logimo_vali_(varchar, integer) AS WITH _cte_logimo_vali_ AS (\nSELECT $1, $2\n)\nSELECT \nFROM _cte_logimo_vali_"

  describe "trimQueryEnd" do
    it "only adds a newline when query does not end in trimmable characters" do
      let Right query =
            parseInterpolatedQuery "SELECT * FROM dogs WHERE {{name}} = '{doggy friend}'"
      trimQueryEnd query
        `shouldBe` ( InterpolatedQuery
                       [ IIText "SELECT * FROM dogs WHERE ",
                         IIVariable (ArgumentName "name"),
                         IIText " = '{doggy friend}'",
                         IIText "\n"
                       ]
                   )

    it "It trims spaces from the end" do
      let Right query =
            parseInterpolatedQuery "SELECT * FROM dogs WHERE {{name}} = '{doggy friend}'   "
      trimQueryEnd query
        `shouldBe` ( InterpolatedQuery
                       [ IIText "SELECT * FROM dogs WHERE ",
                         IIVariable (ArgumentName "name"),
                         IIText " = '{doggy friend}'",
                         IIText "\n"
                       ]
                   )

    it "It trims spaces, newlines and tabs from the end from the end" do
      let Right query =
            parseInterpolatedQuery "SELECT * FROM dogs WHERE {{name}} = '{doggy friend}'   \n \n\n \t \n \t  "
      trimQueryEnd query
        `shouldBe` ( InterpolatedQuery
                       [ IIText "SELECT * FROM dogs WHERE ",
                         IIVariable (ArgumentName "name"),
                         IIText " = '{doggy friend}'",
                         IIText "\n"
                       ]
                   )
    it "It trims spaces, newlines and tabs, and then semicolon from the end from the end" do
      let Right query =
            parseInterpolatedQuery "SELECT * FROM dogs WHERE {{name}} = '{doggy friend}' ;  \n \n\n \t \n \t  "
      trimQueryEnd query
        `shouldBe` ( InterpolatedQuery
                       [ IIText "SELECT * FROM dogs WHERE ",
                         IIVariable (ArgumentName "name"),
                         IIText " = '{doggy friend}' ",
                         IIText "\n"
                       ]
                   )
