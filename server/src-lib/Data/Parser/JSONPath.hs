module Data.Parser.JSONPath
  ( parseJSONPath
  , JSONPathElement(..)
  ) where

import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text
import           Prelude

import qualified Data.Text            as T

data JSONPathElement
  = Key T.Text
  | Index !Int
  deriving (Show, Eq)

firstPathElementChar :: Parser Char
firstPathElementChar = letter
  <|> satisfy (`elem` ("_$" :: String))
  <?> "The first character must be a letter, an underscore (_) or a dollar sign ($)."

pathElement :: Parser JSONPathElement
pathElement = do
  firstChar <- firstPathElementChar
  remain <- many' ( letter
                <|> digit
                <|> satisfy (`elem` ("-_$" :: String))
                )
  return $ Key $ T.pack (firstChar:remain)

childPathElement :: Parser JSONPathElement
childPathElement = skip (== '.') *> pathElement

indexPathElement :: Parser JSONPathElement
indexPathElement = do
  skip (== '[')
  content <- many1' digit <?> "Invalid array JSON path. Expected digits, got empty or valid characters"
  skip (== ']')
  return $ Index (read content)

pathElements :: Parser [JSONPathElement]
pathElements = do
  firstPath <- pathElement
           <|> indexPathElement
  remain <- many' (childPathElement <|> indexPathElement)
  return (firstPath:remain)

parseJSONPath :: T.Text -> Either String [JSONPathElement]
parseJSONPath = parseOnly (pathElements <* (endOfInput <?> emptyMessage))
  where
    emptyMessage = "Invalid JSON path. Property name is empty"
