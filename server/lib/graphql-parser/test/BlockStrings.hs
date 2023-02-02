-- | Regression tests for issue #20 https://github.com/hasura/graphql-parser-hs/issues/20
module BlockStrings
  ( blockTest,
  )
where

-------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog
  ( Group (..),
    Property,
    checkParallel,
    failure,
    footnote,
    property,
    success,
    withTests,
    (===),
  )
import Language.GraphQL.Draft.Parser (blockString, runParser)
import Prelude

-------------------------------------------------------------------------------

blockTest :: IO Bool
blockTest = do
  checkParallel $
    Group
      "Test.parser.block-string.unit"
      [ ("parses the specExample", "\n    Hello,\n      World!\n\n    Yours,\n      GraphQL.\n  " `shouldParseTo` "Hello,\n  World!\n\nYours,\n  GraphQL."),
        ("do not remove WS from the end of lines", "\nFoo \nbar  " `shouldParseTo` "Foo \nbar  "),
        ("tabs are WS as well", "\n\t\tFoo\n\t\tbar\n\t\t\tqux" `shouldParseTo` "Foo\nbar\n\tqux"),
        ("tabs work with spaces", "\n\t Foo\n \tbar\n\t\t qux" `shouldParseTo` "Foo\nbar\n qux"),
        ("parses newline", "\n" `shouldParseTo` ""),
        ("parses very simples not-empty block", "x" `shouldParseTo` "x"),
        ("common indentation is removed", "\n  a \n   b \n  c " `shouldParseTo` "a \n b \nc "),
        ("zero common indentation is possible", "\na \n b \nc " `shouldParseTo` "a \n b \nc "),
        ("no whitespace is removed from the first line", "  abc " `shouldParseTo` "  abc "),
        ("ignores escaping", "  \\  " `shouldParseTo` "  \\  "), -- this is a single \
        ("\n in first characters is parsed", "\n hey  " `shouldParseTo` "hey  "),
        ("simple case", "\nx\n" `shouldParseTo` "x"),
        ("empty single line", "" `shouldParseTo` ""),
        ("empty two lines", "\n" `shouldParseTo` ""),
        ("empty three lines", "\n\n" `shouldParseTo` ""),
        ("empty X lines", "\n\n\n\n\n\n" `shouldParseTo` ""),
        ("preserves escaped newlines", "\nhello\\nworld\n" `shouldParseTo` "hello\\nworld"),
        ("double-quotes are parsed normally", "\n\"\n" `shouldParseTo` "\""),
        ("escaped triple-quotes are ignored as block terminator", "\n   \\\"\"\"hey\n   friends\n" `shouldParseTo` "\"\"\"hey\nfriends"),
        ("fails for normal string", blockParseFail "\"hey\""),
        ("fails for block string that is not closed", blockParseFail "\"\"\" hey"),
        ("fails for block string that is not closed when there are escaped triple-quotes", blockParseFail "\"\"\" hey\\\"\"\"hey"),
        ("does not ignore escaping when it's part of an escaped triple-quotes", blockParseFail "\"\"\"\\\"\"\"") -- this is a single \, but it touches the """ at the end
      ]

-- | We use this function to tests cases that we know should
-- fail, when we pass a function to construct wrapped the
-- body in a delimiter, where we will probably be testing
-- for errors using it.
blockParseFail :: Text -> Property
blockParseFail unparsed = withTests 1 $
  property $ do
    case runParser blockString ("\"\"\"" <> unparsed <> "\"\"\"") of
      Left _ -> success
      Right _ -> do
        footnote ("Should have failed for: " <> T.unpack ("\"\"\"" <> unparsed <> "\"\"\""))
        failure

-- | Test whether certain block string content parses to the expected value.
shouldParseTo :: Text -> Text -> Property
shouldParseTo unparsed expected = withTests 1 $
  property $ do
    case runParser blockString ("\"\"\"" <> unparsed <> "\"\"\"") of
      Right r -> expected === r
      Left l -> do
        footnote $ T.unpack $ "Block parser failed: " <> l
        failure
