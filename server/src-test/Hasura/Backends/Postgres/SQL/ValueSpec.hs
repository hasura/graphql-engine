{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.Postgres.SQL.ValueSpec (spec) where

import Data.Text.RawString (raw)
import Hasura.Backends.Postgres.SQL.DML
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.SQL.Value (PGScalarValue (..), parsePGValue, pgScalarValueToJson, txtEncoder)
import Hasura.Base.Error (Code (ParseFailed), QErr (..), runAesonParser)
import Hasura.Base.Error.TestInstances ()
import Hasura.Prelude
import Network.HTTP.Types qualified as HTTP
import Test.Hspec

spec :: Spec
spec = do
  txtEncoderSpec
  jsonValueSpec

singleElement, multiElement, edgeCaseStrings, nestedArray, nestedArray', malformedArray, nonLatinArray :: PGScalarValue
singleElement = PGValArray [PGValInteger 1]
multiElement = PGValArray [PGValVarchar "a", PGValVarchar "b"]
edgeCaseStrings = PGValArray $ map PGValVarchar ["a", "", [raw|"|], [raw|\|], [raw|,|], [raw|}|]]
nestedArray = PGValArray [multiElement, multiElement]
nestedArray' = PGValArray [nestedArray]
malformedArray = PGValArray [PGValInteger 1]
nonLatinArray = PGValArray [PGValVarchar "שלום"]

txtEncoderSpec :: Spec
txtEncoderSpec =
  describe "txtEncoder should encode a valid Postgres array of:" $ do
    it "a single element" $ do
      txtEncoder singleElement `shouldBe` SELit [raw|{1}|]
    it "multiple elements" $ do
      txtEncoder multiElement `shouldBe` SELit [raw|{"a","b"}|]
    it "simple nested arrays" $ do
      txtEncoder nestedArray `shouldBe` SELit [raw|{{"a","b"},{"a","b"}}|]
    it "more deeply nested arrays" $ do
      txtEncoder nestedArray' `shouldBe` SELit [raw|{{{"a","b"},{"a","b"}}}|]
    it "edge case strings" $ do
      txtEncoder edgeCaseStrings `shouldBe` SELit [raw|{"a","","\"","\\",",","}"}|]
    it "non-latin characters" $ do
      txtEncoder nonLatinArray `shouldBe` SELit [raw|{"שלום"}|]

pgArrayRoundtrip :: PGScalarValue -> PGScalarType -> Expectation
pgArrayRoundtrip v t = do
  let parsedValue = runExcept $ runAesonParser (parsePGValue t) (pgScalarValueToJson v)
  parsedValue `shouldBe` Right v

jsonValueSpec :: Spec
jsonValueSpec = describe "JSON Roundtrip: PGArray" do
  describe "parsePGValue" $ do
    it "singleElement PGArray" $ do
      pgArrayRoundtrip singleElement (PGArray PGInteger)
    it "multiElement PGArray" $ do
      pgArrayRoundtrip multiElement (PGArray PGVarchar)
    it "nestedArray PGArray" $ do
      pgArrayRoundtrip nestedArray (PGArray (PGArray PGVarchar))
    it "nestedArray' PGArray" $ do
      pgArrayRoundtrip nestedArray' (PGArray (PGArray (PGArray PGVarchar)))
    it "malformedArray PGArray" $ do
      let parsedValue = runExcept $ runAesonParser (parsePGValue PGVarchar) (pgScalarValueToJson malformedArray)
      parsedValue
        `shouldBe` Left
          QErr
            { qePath = [],
              qeStatus =
                HTTP.Status
                  { statusCode = 400,
                    statusMessage = "Bad Request"
                  },
              qeError = "parsing Text failed, expected String, but encountered Array",
              qeCode = ParseFailed,
              qeInternal = Nothing
            }
