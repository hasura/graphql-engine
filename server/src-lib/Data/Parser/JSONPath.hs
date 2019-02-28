module Data.Parser.JSONPath
  ( parseJSONPath
  , JSONPathElement(..)
  ) where

import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text
import           Prelude
import           Data.Bool

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
pathElement = atEnd >>=
  bool (fail "Invalid JSON path. Property name is empty") (do
    firstChar <- firstPathElementChar
    remain <- many' ( letter
                  <|> digit
                  <|> satisfy (`elem` ("-_$" :: String))
                  )
    return $ Key $ T.pack (firstChar:remain)
  )

childPathElement :: Parser JSONPathElement
childPathElement = skip (== '.') *> pathElement

indexPathElement :: Parser JSONPathElement
indexPathElement = do
  skip (== '[')
  content <- many' digit
  skip (== ']')
  if null content
    then fail "Invalid array JSON path. Expected digits, got empty or valid characters"
    else return $ Index (read content)

pathElements :: Parser [JSONPathElement]
pathElements = do
  firstPath <- pathElement
           <|> indexPathElement
  remain <- many' (childPathElement <|> indexPathElement)
  return (firstPath:remain)

parseJSONPath :: T.Text -> Either String [JSONPathElement]
parseJSONPath = parseOnly (pathElements <* endOfInput)
