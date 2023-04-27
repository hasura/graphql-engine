{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Templating yaml files.
module Harness.Quoter.Yaml.InterpolateYaml
  ( interpolateYaml,
    ToYamlString (..),
  )
where

import Control.Exception.Safe (impureThrow)
import Data.Aeson qualified as J
import Data.Aeson.Text qualified as J.Text
import Data.Bifunctor qualified as Bifunctor
import Data.String
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as TL
import Data.Yaml qualified
import Hasura.Prelude
import Instances.TH.Lift ()
import Language.Haskell.Meta (parseExp)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

-- | a class for values that can be interpolated in Yaml strings
class ToYamlString a where
  showYml :: a -> String

instance ToYamlString String where
  showYml s = s

instance ToYamlString Text where
  showYml s = T.unpack s

instance ToYamlString Int where
  showYml = show

instance ToYamlString J.Value where
  showYml = TL.unpack . J.Text.encodeToLazyText

-- | Treats Yaml as a string, and allows #{ stuff } to be spliced in
-- '[interpolateYaml| hello #{ 1 + 2 + 3 } |]`
-- should make 'hello 6'
interpolateYaml :: QuasiQuoter
interpolateYaml =
  QuasiQuoter
    { quoteExp = interpolateYamlExp,
      quotePat = \_ -> fail "invalid",
      quoteType = \_ -> fail "invalid",
      quoteDec = \_ -> fail "invalid"
    }

-- | Template a YAML file contents.
interpolateYamlExp :: String -> Q Exp
interpolateYamlExp inputString = do
  appE [|stringToYaml|] (evalInterpolation inputString)

-- Produces 'Value'.
stringToYaml :: String -> J.Value
stringToYaml inputString =
  case Data.Yaml.decodeEither' (encodeUtf8 (T.pack $ inputString)) of
    Left e -> impureThrow e
    Right v -> v

data InterpolatePart
  = -- | any other string part
    IPRaw String
  | -- | expression found in '#{ <here> }'
    IPExpression Exp

-- | Parses raw QQ text to a list of 'InterpolatePart' then interprets it.
evalInterpolation :: String -> ExpQ
evalInterpolation txt =
  case parseInterpolated txt of
    Left err -> fail $ "Parsing error: " <> err
    Right result ->
      interpret result

parseInterpolated :: String -> Either String [InterpolatePart]
parseInterpolated = Bifunctor.first show . P.parse parseParts "yamlQQ"
  where
    -- This can probably be made more succinct. We start by trying to parse
    -- an interpolated expression, then we try to parse a comment. The reasoning
    -- behind it is that both start with a '#' character, and raw parsing does
    -- NOT parse '#' chars.
    parseParts :: Parser [InterpolatePart]
    parseParts =
      P.many
        ( P.try parseInterpolatedExpr
            <|> P.try parseComment
            <|> parseRaw
        )

    -- Parses raw text. Will never parse a '#'.
    parseRaw :: Parser InterpolatePart
    parseRaw = IPRaw <$> P.many1 (P.try $ P.noneOf "#")

    -- Parses a comment: wherever we encounter a '#', until the end of line.
    parseComment :: Parser InterpolatePart
    parseComment = do
      _ <- P.char '#'
      it <- P.manyTill P.anyChar P.endOfLine
      pure $ IPRaw ('#' : it <> "\n")

    -- Parses an interpolated expression in the format '#{ i am some haskell }'
    parseInterpolatedExpr :: Parser InterpolatePart
    parseInterpolatedExpr = do
      _ <- P.string "#{"
      code <- P.manyTill P.anyChar (P.char '}')
      case parseExp code of
        Left err -> fail $ "could not find name: " <> err
        Right r -> pure $ IPExpression r

interpret :: [InterpolatePart] -> ExpQ
interpret =
  appE [|concat|]
    . listE
    . fmap fromInterpolatePart
  where
    fromInterpolatePart :: InterpolatePart -> ExpQ
    fromInterpolatePart (IPRaw s) = stringE s
    fromInterpolatePart (IPExpression e) = appE [|exprToLiteral|] (pure e)

-- interpolated strings use their ToYamlString instances
exprToLiteral :: (ToYamlString a) => a -> String
exprToLiteral a = showYml a
