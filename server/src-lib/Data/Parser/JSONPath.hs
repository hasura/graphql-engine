module Data.Parser.JSONPath
  ( parseJSONPath
  , JSONPathElement(..)
  ) where

import           Control.Applicative  (liftA2, (<|>))
import           Data.Aeson.Internal  (JSONPathElement (..))
import           Data.Attoparsec.Text
import           Data.Bool            (bool)
import           Data.Char            (isDigit)
import           Data.List            (intercalate)
import qualified Data.Text            as T
import           Prelude
import           Text.Read            (readMaybe)

firstPathElementChar :: Parser Char
firstPathElementChar = letter
  <|> satisfy (`elem` ("_$" :: String))
  <?> "the first character of property name must be a letter, an underscore (_) or a dollar sign ($)."

propertyPathElement :: Parser JSONPathElement
propertyPathElement = do
  firstChar <- firstPathElementChar
  remain <- many'
    ( letter
    <|> digit
    <|> satisfy (`elem` ("-_$" :: String))
    <?> "invalid special characters, accept letters, digits, underscore (_), hyphen (-) or a dollar sign ($) only "
    )
  return $ Key $ T.pack (firstChar:remain)

indexPathElement :: Parser JSONPathElement
indexPathElement = skip (== '[') *> anyChar >>= parseDigits
  where
    parseDigits :: Char -> Parser JSONPathElement
    parseDigits firstDigit
      | firstDigit == ']' = fail "empty array index"
      | not $ isDigit firstDigit = fail $ "invalid index digit: " ++ [firstDigit]
                                        ++ ". Please make sure that index value is number."
      | otherwise = do
          remain <- many' (notChar ']')
          skip (== ']')
          let content = firstDigit:remain
          case (readMaybe content :: Maybe Int) of
            Nothing -> fail $ "invalid array index: " ++ content
            Just v  -> return $ Index v

pathElement :: Parser JSONPathElement
pathElement = peekChar >>= \case
  Nothing -> fail "empty json path"
  Just '[' -> indexPathElement
  _ -> propertyPathElement

pathElements :: Parser [JSONPathElement]
pathElements = do
  firstPath <- pathElement
  remain <- childPathElements
  return (firstPath:remain)

childPathElements  :: Parser [JSONPathElement]
childPathElements = peekChar >>= \case
  Nothing -> return []
  Just '.' -> liftA2 (:) (skip (== '.') *> propertyPathElement) childPathElements
  Just '[' -> liftA2 (:) indexPathElement childPathElements
  Just c -> fail $ "invalid character: " ++ [c]

parseJSONPath :: T.Text -> Either String [JSONPathElement]
parseJSONPath = parseResult . parse pathElements
  where
    parseResult = \case
      Fail _ pos err -> Left $ bool (intercalate ". " pos) err (null pos)
      Partial p -> parseResult $ p ""
      Done _ r -> Right r
