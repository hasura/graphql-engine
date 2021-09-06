module Hasura.RQL.Types.CommonSpec (spec) where

import           Hasura.Prelude

import           Hasura.RQL.Types.Common (PGConnectionParams (..), getPGConnectionStringFromParams)
import           Network.URI             (isAbsoluteURI)

import           Test.Hspec

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

spec :: Spec
spec = do
  pgConnectionStringFromParamsSpec

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
