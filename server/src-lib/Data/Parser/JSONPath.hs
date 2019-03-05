module Data.Parser.JSONPath
  ( parseJSONPath
  , JSONPathElement(..)
  , JSONPath
  ) where

import           Control.Applicative  ((<|>))
import           Data.Aeson.Internal  (JSONPath, JSONPathElement (..))
import           Data.Attoparsec.Text
import           Data.Bool            (bool)
import           Data.Char            (isDigit)
import qualified Data.Text            as T
import           Prelude              hiding (takeWhile)
import           Text.Read            (readMaybe)

parseKey :: Parser T.Text
parseKey = do
  firstChar <- letter
           <?> "the first character of property name must be a letter."
  name <- many' (letter
           <|> digit
           <|> satisfy (`elem` ("-_" :: String))
            )
  return $ T.pack (firstChar:name)

parseIndex :: Parser Int
parseIndex = skip (== '[') *> anyChar >>= parseDigits
  where
    parseDigits :: Char -> Parser Int
    parseDigits firstDigit
      | firstDigit == ']' = fail "empty array index"
      | not $ isDigit firstDigit =
          fail $ "invalid array index: " ++ [firstDigit]
      | otherwise = do
          remain <- many' (notChar ']')
          skip (== ']')
          let content = firstDigit:remain
          case (readMaybe content :: Maybe Int) of
            Nothing -> fail $ "invalid array index: " ++ content
            Just v  -> return v

parseElement :: Parser JSONPathElement
parseElement = do
  dotLen <- T.length <$> takeWhile (== '.')
  if dotLen > 1
    then fail "multiple dots in json path"
    else peekChar >>= \case
      Nothing ->  fail "empty json path"
      Just '[' -> Index <$> parseIndex
      _ -> Key <$> parseKey

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
      ++ ". Accept letters, digits, underscore (_) or hyphen (-) only"
