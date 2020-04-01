module Data.Parser.JSONPath
  ( parseJSONPath
  , JSONPathElement(..)
  , JSONPath
  ) where

import           Control.Applicative  ((<|>))
import           Data.Aeson.Internal  (JSONPath, JSONPathElement (..))
import           Data.Attoparsec.Text
import           Data.Bool            (bool)
import qualified Data.Text            as T
import           Prelude              hiding (takeWhile)
import           Text.Read            (readMaybe)


parseKey :: Parser JSONPathElement
parseKey = do
  firstChar <- letter
           <|> char '_'
           <?> "the first character of property name must be a letter or underscore."
  name <- many' (letter
           <|> digit
           <|> satisfy (`elem` ("-_" :: String))
            )
  return $ Key $ T.pack (firstChar:name)

-- parse json property key or index in squared bracket format
-- this parser support special characters
parseBracketElement :: Parser JSONPathElement
parseBracketElement = skip (== '[') *> anyChar >>= parseContent
  where
    parseContent :: Char -> Parser JSONPathElement
    parseContent ']'  = fail "empty array index"
    parseContent '\'' = parseTextContent '\''
    parseContent '"'  = parseTextContent '"'
    parseContent c    = parseIntContent c

    parseTextContent :: Char -> Parser JSONPathElement
    parseTextContent quote = do
      remain <- many' (notChar quote)
      skip (== quote)
      skip (== ']')
      return $ Key $ T.pack remain

    parseIntContent :: Char -> Parser JSONPathElement
    parseIntContent c = do
      remain <- many' (notChar ']')
      skip (== ']')
      let content = c:remain
      case (readMaybe content :: Maybe Int) of
        Nothing -> fail $ "invalid array index: " ++ content
        Just v  -> return $ Index v

parseElement :: Parser JSONPathElement
parseElement = do
  dotLen <- T.length <$> takeWhile (== '.')
  if dotLen > 1
    then fail "multiple dots in json path"
    else peekChar >>= \case
      Nothing ->  fail "empty json path"
      Just '[' -> parseBracketElement
      _ -> parseKey

parseElements :: Parser JSONPath
parseElements = skipWhile (== '$') *> many1 parseElement

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
      ++ "Accept letters, digits, underscore (_) or hyphen (-) only. "
      ++ "You should use bracket notation (e.g ['Hello!']) for special characters"
