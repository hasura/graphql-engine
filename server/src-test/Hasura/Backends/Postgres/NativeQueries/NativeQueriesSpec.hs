{-# HLINT ignore "avoid Language.GraphQL.Draft.Syntax.unsafeMkName" #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Hasura.Backends.Postgres.NativeQueries.NativeQueriesSpec (spec) where

import Data.Bifunctor
import Data.Either
import Data.HashMap.Strict qualified as HM
import Hasura.Backends.Postgres.Instances.NativeQueries
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Base.Error
import Hasura.CustomReturnType.Metadata
import Hasura.NativeQuery.Metadata
import Hasura.NativeQuery.Types
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

  describe "Validation" do
    let crtm =
          CustomReturnTypeMetadata
            { _crtmName = CustomReturnTypeName (G.unsafeMkName "custom_return_type_name"),
              _crtmFields = mempty,
              _crtmDescription = Nothing,
              _crtmSelectPermissions = mempty
            }

        nqm =
          NativeQueryMetadata
            { _nqmRootFieldName = NativeQueryName (G.unsafeMkName "root_field_name"),
              _nqmCode = InterpolatedQuery mempty,
              _nqmReturns = CustomReturnTypeName (G.unsafeMkName "custom_return_type_name"),
              _nqmArguments = mempty,
              _nqmDescription = mempty
            }

    it "Rejects undeclared variables" do
      let Right code = parseInterpolatedQuery "SELECT {{hey}}"
      let actual :: Either QErr Text = fmap snd $ runExcept $ nativeQueryToPreparedStatement crtm nqm {_nqmCode = code}

      (first showQErr actual) `shouldSatisfy` isLeft
      let Left err = actual
      qeCode err `shouldBe` ValidationFailed

    it "Handles multiple occurences of variables " do
      let Right code = parseInterpolatedQuery "SELECT {{hey}}, {{hey}}"
      let actual :: Either QErr Text =
            fmap snd $
              runExcept $
                nativeQueryToPreparedStatement
                  crtm
                  nqm
                    { _nqmCode = code,
                      _nqmArguments =
                        HM.fromList
                          [ (NativeQueryArgumentName "hey", NullableScalarType PGVarchar False Nothing)
                          ]
                    }

      (first showQErr actual) `shouldSatisfy` isRight
      let Right rendered = actual
      rendered
        `shouldBe` "PREPARE _logimo_vali_root_field_name(varchar) AS WITH _cte_logimo_vali_root_field_name AS (\nSELECT $1, $1\n)\nSELECT \nFROM _cte_logimo_vali_root_field_name"

    it "Handles multiple variables " do
      let Right code = parseInterpolatedQuery "SELECT {{hey}}, {{ho}}"
      let actual :: Either QErr Text =
            fmap snd $
              runExcept $
                nativeQueryToPreparedStatement
                  crtm
                  nqm
                    { _nqmCode = code,
                      _nqmArguments =
                        HM.fromList
                          [ (NativeQueryArgumentName "hey", NullableScalarType PGVarchar False Nothing),
                            (NativeQueryArgumentName "ho", NullableScalarType PGInteger False Nothing)
                          ]
                    }

      (first showQErr actual) `shouldSatisfy` isRight
      let Right rendered = actual
      rendered
        `shouldBe` "PREPARE _logimo_vali_root_field_name(varchar, integer) AS WITH _cte_logimo_vali_root_field_name AS (\nSELECT $1, $2\n)\nSELECT \nFROM _cte_logimo_vali_root_field_name"
