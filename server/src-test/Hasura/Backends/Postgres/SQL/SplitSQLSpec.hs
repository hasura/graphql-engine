module Hasura.Backends.Postgres.SQL.SplitSQLSpec (spec) where

import Data.Text qualified as T
import Hasura.Backends.Postgres.DDL.RunSQL (splitSQLStatements)
import Hasura.Prelude
import Test.Hspec

spec :: Spec
spec = describe "splitSQLStatements" $ do
  it "splits simple SQL statements" $ do
    let sql = "SELECT 1; SELECT 2; "
    splitSQLStatements sql `shouldBe` ["SELECT 1", "SELECT 2"]

  it "splits simple SQL statements separated by newlines" $ do
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

  it "preserves regex end anchors in untagged dollar-quoted blocks" $ do
    let sql = "DO $$ BEGIN IF 'x' ~ '^x$' THEN NULL; END IF; END $$;"
    splitSQLStatements sql `shouldBe` ["DO $$ BEGIN IF 'x' ~ '^x$' THEN NULL; END IF; END $$"]

  it "preserves positional parameters in untagged dollar-quoted function bodies" $ do
    let sql = "CREATE FUNCTION echo(text) RETURNS text AS $$ BEGIN RETURN $1; END; $$ LANGUAGE plpgsql; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["CREATE FUNCTION echo(text) RETURNS text AS $$ BEGIN RETURN $1; END; $$ LANGUAGE plpgsql", "SELECT 2"]

  it "preserves differently tagged dollar quotes inside untagged dollar quotes" $ do
    let sql = "SELECT $$before $inner$inside;body$inner$ after;body$$; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT $$before $inner$inside;body$inner$ after;body$$", "SELECT 2"]

  it "preserves untagged dollar quotes inside tagged dollar quotes" $ do
    let sql = "SELECT $outer$before $$inside;body$$ after;body$outer$; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT $outer$before $$inside;body$$ after;body$outer$", "SELECT 2"]

  it "does not close a dollar quote on a longer tag with the same prefix" $ do
    let sql = "SELECT $tag$before $tagged$inside;body$tagged$ after;body$tag$; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT $tag$before $tagged$inside;body$tagged$ after;body$tag$", "SELECT 2"]

  it "does not close a dollar quote on a tag missing its final dollar sign" $ do
    let sql = "SELECT $tag$before $tag; after$tag$; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT $tag$before $tag; after$tag$", "SELECT 2"]

  it "splits statements after matching complete dollar-quote delimiters" $ do
    let sql = "DO $$ BEGIN IF 'x' ~ '^x$' THEN NULL; END IF; END $$; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["DO $$ BEGIN IF 'x' ~ '^x$' THEN NULL; END IF; END $$", "SELECT 2"]

  it "preserves comments attached to statements" $ do
    let sql = "SELECT 1; -- comment here\nSELECT 2; /* multi-line\n comment */SELECT 3;"
    splitSQLStatements sql `shouldBe` ["SELECT 1", "-- comment here\nSELECT 2", "/* multi-line\n comment */SELECT 3"]

  it "handles whitespace" $ do
    let sql = "  SELECT 1  ;  \n  SELECT 2  ;  "
    splitSQLStatements sql `shouldBe` ["SELECT 1", "SELECT 2"]

  it "preserves empty result for empty input" $ do
    splitSQLStatements "" `shouldBe` []
    splitSQLStatements "   " `shouldBe` []
    splitSQLStatements "\n\t" `shouldBe` []

  it "preserves line comment markers in strings" $ do
    let sql = "SELECT 'http://x--y'; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT 'http://x--y'", "SELECT 2"]

  it "preserves block comment markers in strings" $ do
    let sql = "SELECT '/* literal */'; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT '/* literal */'", "SELECT 2"]

  it "preserves doubled single quotes" $ do
    let sql = "SELECT 'it''s;--/* fine'; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT 'it''s;--/* fine'", "SELECT 2"]

  it "preserves escaped quotes and semicolons in escape strings" $ do
    let sql = "SELECT E'it\\'s;--/* fine'; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT E'it\\'s;--/* fine'", "SELECT 2"]

  it "recognizes lower-case escape string prefixes" $ do
    let sql = "SELECT e'it\\'s;ok'; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT e'it\\'s;ok'", "SELECT 2"]

  it "handles escaped backslashes before closing escape strings" $ do
    let sql = "SELECT E'backslash\\\\'; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT E'backslash\\\\'", "SELECT 2"]

  it "does not treat backslashes as escapes in ordinary strings" $ do
    let sql = "SELECT '\\'; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT '\\'", "SELECT 2"]

  it "carries escape mode into newline-concatenated strings" $ do
    let sql = "SELECT E'first'\n'it\\'s;ok'; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT E'first'\n'it\\'s;ok'", "SELECT 2"]

  it "carries escape mode across line comments before continuation" $ do
    let sql = "SELECT E'first' -- comment\r\n 'it\\'s;ok'; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT E'first' -- comment\r\n 'it\\'s;ok'", "SELECT 2"]

  it "does not continue escape mode across block comments" $ do
    let sql = "SELECT E'first' /*\n*/ '\\'; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT E'first' /*\n*/ '\\'", "SELECT 2"]

  it "does not recognize escape prefixes inside identifiers" $ do
    let sql = "SELECT nameE'\\'; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT nameE'\\'", "SELECT 2"]

  it "preserves semicolons and comment markers in quoted identifiers" $ do
    let sql = "SELECT 1 AS \"a;--/*b\"; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT 1 AS \"a;--/*b\"", "SELECT 2"]

  it "preserves doubled quotes in quoted identifiers" $ do
    let sql = "SELECT 1 AS \"a\"\";b\"; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT 1 AS \"a\"\";b\"", "SELECT 2"]

  it "preserves Unicode escape strings and identifiers" $ do
    let sql = "SELECT U&'d\\0061;t', 1 AS U&\"a;\\0062\"; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT U&'d\\0061;t', 1 AS U&\"a;\\0062\"", "SELECT 2"]

  it "preserves bit and hexadecimal string literals" $ do
    let sql = "SELECT B'10', X'ff'; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT B'10', X'ff'", "SELECT 2"]

  it "preserves dollar signs in unquoted identifiers" $ do
    let sql = "SELECT 1 AS a$b$c, 2 AS name$$; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT 1 AS a$b$c, 2 AS name$$", "SELECT 2"]

  it "preserves Unicode identifiers containing dollar signs" $ do
    let sql = "SELECT 1 AS café$tag$; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT 1 AS café$tag$", "SELECT 2"]

  it "accepts Unicode dollar-quote tags" $ do
    let sql = "SELECT $café$one;two$café$; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT $café$one;two$café$", "SELECT 2"]

  it "accepts underscores and digits after the start of dollar tags" $ do
    let sql = "SELECT $_tag12$one;two$_tag12$; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT $_tag12$one;two$_tag12$", "SELECT 2"]

  it "distinguishes positional parameters from dollar quotes" $ do
    let sql = "PREPARE p(text) AS SELECT $1 || $tag$body;text$tag$; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["PREPARE p(text) AS SELECT $1 || $tag$body;text$tag$", "SELECT 2"]

  it "rejects dollar tags that start with digits" $ do
    let sql = "SELECT $1$; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT $1$", "SELECT 2"]

  it "does not scan ahead across invalid dollar tags" $ do
    let sql = "SELECT $not-a-tag$; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT $not-a-tag$", "SELECT 2"]

  it "matches dollar tags case sensitively" $ do
    let sql = "SELECT $Tag$before $tag$inside;body$tag$ after$Tag$; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT $Tag$before $tag$inside;body$tag$ after$Tag$", "SELECT 2"]

  it "preserves quote and comment markers in dollar-quoted blocks" $ do
    let sql = "DO $$ BEGIN RAISE NOTICE 'a -- b /* c */'; END $$; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["DO $$ BEGIN RAISE NOTICE 'a -- b /* c */'; END $$", "SELECT 2"]

  it "does not strip line comments from dollar-quoted bodies" $ do
    let sql = "DO $$ BEGIN -- keep ; $tag$\n NULL; END $$; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["DO $$ BEGIN -- keep ; $tag$\n NULL; END $$", "SELECT 2"]

  it "preserves nested block comments without joining tokens" $ do
    let sql = "SELECT/* outer ; /* inner */ tail */1; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT/* outer ; /* inner */ tail */1", "SELECT 2"]

  it "does not interpret quotes or dollar signs in comments" $ do
    let sql = "SELECT 1 /* ' \" $$ ; -- */; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT 1 /* ' \" $$ ; -- */", "SELECT 2"]

  it "ends line comments at carriage returns" $ do
    let sql = "SELECT 1 -- ; ignore\r; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT 1 -- ; ignore", "SELECT 2"]

  it "ignores comment-only fragments before and after statements" $ do
    let sql = "-- first\n; /* only */; SELECT 1; -- trailing"
    splitSQLStatements sql `shouldBe` ["SELECT 1"]

  it "does not return trailing comments as the last statement" $ do
    let sql = "SELECT 1; /* trailing */ -- trailing\n"
    splitSQLStatements sql `shouldBe` ["SELECT 1"]

  it "ignores input containing only comments and empty statements" $ do
    let sql = "/* outer /* nested */ end */; -- comment\n;"
    splitSQLStatements sql `shouldBe` []

  it "forwards unterminated block comments after completed statements" $ do
    let sql = "SELECT 1; /* unterminated"
    splitSQLStatements sql `shouldBe` ["SELECT 1", "/* unterminated"]

  it "forwards unterminated nested block comments" $ do
    let sql = "/* outer /* inner */"
    splitSQLStatements sql `shouldBe` ["/* outer /* inner */"]

  it "forwards unterminated strings unchanged" $ do
    let sql = "SELECT 'unterminated; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT 'unterminated; SELECT 2;"]

  it "forwards unterminated quoted identifiers unchanged" $ do
    let sql = "SELECT \"unterminated; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT \"unterminated; SELECT 2;"]

  it "forwards unterminated dollar quotes unchanged" $ do
    let sql = "SELECT $tag$unterminated; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT $tag$unterminated; SELECT 2;"]

  it "does not trim non-ASCII identifier characters as whitespace" $ do
    let sql = " ; SELECT 2;"
    splitSQLStatements sql `shouldBe` [" ", "SELECT 2"]

  it "preserves long ordinary and escape-string runs around escaped quotes" $ do
    let body = T.replicate 4096 "é;--/* text */"
        ordinary = "SELECT '" <> body <> "''tail'"
        escaped = "SELECT E'" <> body <> "\\'tail'"
    splitSQLStatements (ordinary <> "; " <> escaped <> "; SELECT 2;") `shouldBe` [ordinary, escaped, "SELECT 2"]

  it "counts Unicode line comments correctly in escape-string continuations" $ do
    let sql = "SELECT E'first' -- café 🙂\r\n -- 次\n 'it\\'s;ok'; SELECT 2;"
    splitSQLStatements sql `shouldBe` ["SELECT E'first' -- café 🙂\r\n -- 次\n 'it\\'s;ok'", "SELECT 2"]
