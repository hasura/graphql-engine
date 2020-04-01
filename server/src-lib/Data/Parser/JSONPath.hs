module Data.Parser.JSONPath
  ( parseJSONPath
  , JSONPathElement(..)
  , JSONPath
  ) where

import           Prelude

import qualified Data.Text            as T

import           Control.Applicative
import           Data.Aeson.Internal  (JSONPath, JSONPathElement (..))
import           Data.Attoparsec.Text

parseJSONPath :: T.Text -> Either String JSONPath
parseJSONPath = parseOnly (optional (char '$') *> many1' element <* endOfInput)

element :: Parser JSONPathElement
element = Key <$> (optional (char '.') *> name)   -- field or .field
      <|> bracketElement                          -- [42] or ['field']

name :: Parser T.Text
name = go <?> "property name" where
  go = do
    firstChar <- letter
             <|> char '_'
             <|> fail "first character of property name must be a letter or underscore"
    otherChars <- many' (letter <|> digit <|> satisfy (inClass "-_"))
    pure $ T.pack (firstChar:otherChars)

-- | Parses a JSON property key or index in square bracket format, e.g.
-- > [42]
-- > ["hello"]
-- > ['你好']
bracketElement :: Parser JSONPathElement
bracketElement = do
  char '['
  result <- Index <$> decimal
        <|>   Key <$> quotedString '"'
        <|>   Key <$> quotedString '\''
  char ']'
  pure result
  where
    quotedString delimiter = do
      char delimiter
      result <- T.pack <$> many' (charOrEscape delimiter)
      char delimiter
      pure result

    charOrEscape delimiter = (char '\\' *> anyChar) <|> notChar delimiter
