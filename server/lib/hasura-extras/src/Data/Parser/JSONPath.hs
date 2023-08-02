module Data.Parser.JSONPath
  ( encodeJSONPath,
    parseJSONPath,
  )
where

import Control.Applicative
import Data.Aeson (Key)
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.Types (JSONPath, JSONPathElement (..))
import Data.Attoparsec.Text
import Data.Bifunctor qualified as Bifunctor
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Hasura.Prelude

-- | Encodes a JSON path as text that looks like code you would write
-- in order to traverse that path in JavaScript.
encodeJSONPath :: JSONPath -> Text
encodeJSONPath path = "$" <> foldMap formatPart path
  where
    formatPart (Index idx) = "[" <> tshow idx <> "]"
    formatPart (Key key)
      | specialChars stringKey = TL.toStrict ("[" <> TL.decodeUtf8 (J.encode (J.String textKey)) <> "]")
      | otherwise = "." <> textKey
      where
        textKey = K.toText key
        stringKey = T.unpack textKey
        specialChars [] = True
        -- first char must not be number
        specialChars (c : xs) =
          notElem c (alphabet ++ "_")
            || any (`notElem` (alphaNumerics ++ "_-")) xs

parseJSONPath :: Text -> Either Text JSONPath
parseJSONPath "$" = Right []
parseJSONPath txt =
  Bifunctor.first (const invalidMessage)
    $ parseOnly (optional (char '$') *> many1' element <* endOfInput) txt
  where
    invalidMessage =
      txt
        <> ". Accept letters, digits, underscore (_) or hyphen (-) only"
        <> ". Use quotes enclosed in bracket ([\"...\"]) if there is any special character"

element :: Parser JSONPathElement
element =
  Key <$> (optional (char '.') *> name) -- field or .field
    <|> bracketElement -- [42], ["field"], or ['field']

name :: Parser Key
name = go <?> "property name"
  where
    go = do
      firstChar <-
        letter
          <|> char '_'
          <?> "first character of property name must be a letter or underscore"
      otherChars <- many' (letter <|> digit <|> satisfy (inClass "-_"))
      pure $ K.fromText $ T.pack (firstChar : otherChars)

-- | Parses a JSON property key or index in square bracket format, e.g.
-- > [42]
-- > ["hello"]
-- > ['你好']
bracketElement :: Parser JSONPathElement
bracketElement = do
  void $ optional (char '.') *> char '['
  result <-
    Index <$> decimal
      <|> Key <$> doubleQuotedString
      <|> Key <$> singleQuotedString
  void $ char ']'
  pure result
  where
    parseJSONString inQuotes =
      maybe (fail "Invalid JSON string") (pure . K.fromText)
        . J.decode
        . TL.encodeUtf8
        $ "\""
        <> inQuotes
        <> "\""

    doubleQuotedString = do
      void $ char '"'
      inQuotes <- TL.concat <$> many doubleQuotedChar
      void $ char '"'
      parseJSONString inQuotes

    doubleQuotedChar = jsonChar '"'

    -- Converts `'foo'` to `"foo"` and then parses it.
    singleQuotedString = do
      void $ char '\''
      inQuotes <- TL.concat <$> many singleQuotedChar
      void $ char '\''
      parseJSONString inQuotes

    -- Un-escapes single quotes, and escapes double quotes.
    singleQuotedChar =
      (string "\\'" *> pure "'")
        <|> (string "\"" *> pure "\\\"")
        <|> jsonChar '\''

    jsonChar delimiter =
      (("\\" <>) . TL.singleton <$> (char '\\' *> anyChar))
        <|> (TL.singleton <$> notChar delimiter)
