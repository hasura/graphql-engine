{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-------------------------------------------------------------------------------

-- | Regression tests for issue #20 https://github.com/hasura/graphql-parser-hs/issues/20
module Keywords
  ( primitiveTests,
  )
where

-------------------------------------------------------------------------------

import Data.Foldable (for_)
import Data.Text (Text, singleton)
import Data.Void (Void)
import Hedgehog
  ( MonadTest,
    Property,
    PropertyName,
    liftTest,
    property,
    tripping,
    withTests,
  )
import Language.GraphQL.Draft.Parser (Parser, nameParser, runParser, value)
import Language.GraphQL.Draft.Printer qualified as Printer
import Language.GraphQL.Draft.Syntax (EnumValue (..), Value (..), addSuffixes, litName, litSuffix)
import Text.Builder (Builder, run)
import Prelude

-------------------------------------------------------------------------------

primitiveTests :: [(PropertyName, Property)]
primitiveTests =
  [ ("a \"null\" prefix doesn't prevent parsing a name", withTests 1 propNullNameName),
    ("a \"null\" prefix doesn't prevent parsing an enum name", withTests 1 propNullNameValue),
    ("a \"true\" prefix doesn't prevent parsing an enum name", withTests 1 propBoolNameValue),
    ("a string containing \\NUL is handled correctly", withTests 1 propHandleNulString),
    ("a string containing \\n is handled correctly", withTests 1 propHandleNewlineString),
    ("a string containing \\x0011 is handled correctly", withTests 1 propHandleControlString),
    ("all unicode characters are supported", withTests 1 propHandleUnicodeCharacters),
    ("triple quotes is a valid string", withTests 1 propHandleTripleQuote),
    ("name with a suffix should be a valid name", withTests 1 propNameWithSuffix)
  ]

propNullNameValue :: Property
propNullNameValue =
  property . roundtripValue $
    VList [VEnum $ EnumValue $$(litName "nullColumn")]

propBoolNameValue :: Property
propBoolNameValue =
  property . roundtripValue $
    VList [VEnum $ EnumValue $$(litName "trueColumn")]

propNullNameName :: Property
propNullNameName =
  property $
    roundtripParser nameParser Printer.nameP $$(litName "nullColumntwo")

propHandleNulString :: Property
propHandleNulString = property . roundtripValue $ VString "\NUL"

propHandleNewlineString :: Property
propHandleNewlineString = property . roundtripValue $ VString "\n"

propHandleControlString :: Property
propHandleControlString = property . roundtripValue $ VString "\x0011"

-- NB: 'liftTest' is explicitly used to restrict the 'for_' block to operate in
-- the 'Test' type (i.e. 'type Test = TestT Identity'), as opposed to 'PropertyT
-- IO'.  The 'Test' monad is a thinner monad stack & therefore doesn't suffer
-- from memory leakage caused by, among others, Hedgehog's 'TreeT', which is
-- used for automatic shrinking (which we don't need in this test).
propHandleUnicodeCharacters :: Property
propHandleUnicodeCharacters = property . liftTest $
  for_ [minBound .. maxBound] $ \char ->
    roundtripValue . VString $ singleton char

propHandleTripleQuote :: Property
propHandleTripleQuote = property . roundtripValue $ VString "\"\"\""

propNameWithSuffix :: Property
propNameWithSuffix =
  property . roundtripValue $
    VList [VEnum $ EnumValue (addSuffixes $$(litName "prefix") [$$(litSuffix "1suffix"), $$(litSuffix "2suffix")])]

-- | Test that a given 'Value'@ @'Void' passes round-trip tests as expected.
roundtripValue :: (MonadTest m) => Value Void -> m ()
roundtripValue = roundtripParser value Printer.value

-- | Test that a pair of parsing/printing functions are compatible with one
-- another.
--
-- That is: given a 'Parser'@ a@ and some @a -> @'Builder', ensure that any
-- valid @a@ round-trips through the printer and parser to yield the same @a@.
roundtripParser ::
  forall a m.
  (MonadTest m, Eq a, Show a) =>
  Parser a ->
  (a -> Builder) ->
  a ->
  m ()
roundtripParser parser printer ast = tripping ast printAST parseAST
  where
    parseAST :: Text -> Either Text a
    parseAST = runParser parser

    printAST :: a -> Text
    printAST = run . printer
