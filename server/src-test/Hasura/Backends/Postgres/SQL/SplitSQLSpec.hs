module Hasura.Backends.Postgres.SQL.SplitSQLSpec (spec) where

import Hasura.Backends.Postgres.DDL.RunSQL (splitSQLStatements)
import Hasura.Prelude
import Test.Hspec

spec :: Spec
spec = describe "splitSQLStatements" $ do
  it "splits simple SQL statements" $ do
    let sql = "SELECT 1; SELECT 2; "
    splitSQLStatements sql `shouldBe` ["SELECT 1", "SELECT 2"]

  it "split simplq SQL statements newline" $ do
    let sql = "SELECT 1;\n SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT 1", "SELECT 2"]

  it "handles empty statements" $ do
    let sql = "SELECT 1; ; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT 1", "SELECT 2"]

  it "preserves strings with semicolons" $ do
    let sql = "SELECT 'a; b'; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT 'a; b'", "SELECT 2"]

  it "handles dollar-quoted strings" $ do
    let sql = "SELECT $$hello;world$$; SELECT $tag$foo;bar$tag$;"
    splitSQLStatements sql `shouldBe` ["SELECT $$hello;world$$", "SELECT $tag$foo;bar$tag$"]

  it "ignores comments" $ do
    let sql = "SELECT 1; -- comment here\nSELECT 2; /* multi-line\n comment */SELECT 3;"
    splitSQLStatements sql `shouldBe` ["SELECT 1", "SELECT 2", "SELECT 3"]

  it "handles whitespace" $ do
    let sql = "  SELECT 1  ;  \n  SELECT 2  ;  "
    splitSQLStatements sql `shouldBe` ["SELECT 1", "SELECT 2"]

  it "preserves empty result for empty input" $ do
    splitSQLStatements "" `shouldBe` []
    splitSQLStatements "   " `shouldBe` []
    splitSQLStatements "\n\t" `shouldBe` []
