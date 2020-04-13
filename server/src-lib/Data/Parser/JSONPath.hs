module Data.Parser.JSONPath
  ( parseJSONPath
  , JSONPathElement(..)
  , JSONPath
  ) where

import           Control.Applicative  ((<|>))
import           Data.Aeson.Internal  (JSONPath, JSONPathElement (..))
import           Data.Attoparsec.Text
import           Data.Bool            (bool)
import           Data.Char
import           Prelude              hiding (takeWhile)
import           Text.Read            (readMaybe)
import qualified Data.Text            as T

parseSimpleKeyText :: Parser T.Text
parseSimpleKeyText = takeWhile1 (inClass "a-zA-Z0-9_-")

parseKey :: Parser JSONPathElement
parseKey = Key <$>
  ( (char '.' *> parseSimpleKeyText) -- Parse `.key`
   <|> T.pack <$> ((string ".['" <|> string "['") *> manyTill anyChar (string "']")) -- Parse `['key']` or `.['key']`
   <|> fail "invalid key element"
  )

parseIndex :: Parser JSONPathElement
parseIndex = Index <$>
  ( ((char '[' *> manyTill anyChar (char ']')) >>= maybe (fail "invalid array index") pure . readMaybe) -- Parse `[Int]`
   <|> fail "invalid index element"
  )

parseElements :: Parser JSONPath
parseElements = skipWhile (== '$') *> parseRemaining
  where
    parseFirstKey = Key <$> parseSimpleKeyText
    parseElements' = many1 (parseIndex <|> parseKey)
    parseRemaining = do
      maybeFirstChar <- peekChar
      case maybeFirstChar of
        Nothing -> pure []
        Just firstChar ->
          -- If first char is not any of '.' and '[', then parse first key
          -- Eg:- Parse "key1.key2[0]"
          if firstChar `notElem` (".[" :: String) then do
            firstKey <- parseFirstKey
            remainingElements <- parseElements'
            pure $ firstKey:remainingElements
          else parseElements'

-- | Parse jsonpath String value
parseJSONPath :: T.Text -> Either String JSONPath
parseJSONPath = parseResult . parse parseElements
  where
    parseResult = \case
      Fail _ pos err ->
        Left $ bool (head pos) err (null pos)
      Partial p ->  parseResult $ p ""
      Done remain r ->
        if not $ T.null remain then
          Left $ invalidMessage remain
        else
          Right r
    invalidMessage s = "invalid property name: "  ++ T.unpack s
      ++ ". Accept letters, digits, underscore (_) or hyphen (-) only"
      ++ ". Use single quotes enclosed in bracket if there are any special characters"
