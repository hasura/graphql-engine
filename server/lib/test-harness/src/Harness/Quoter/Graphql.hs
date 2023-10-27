{-# LANGUAGE TemplateHaskellQuotes #-}

-- | GraphQL quasi quoter with built-in interpolation.
-- Interpolation works via the #{expression} syntax.
module Harness.Quoter.Graphql (graphql, ToGraphqlString (..)) where

import Data.Bifunctor qualified as Bifunctor
import Data.String (fromString)
import Data.Text (unpack)
import Hasura.Prelude
import Language.Haskell.Meta (parseExp)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

-- | a class for values that can be interpolated in GraphQL queries
class ToGraphqlString a where
  showGql :: a -> String

instance ToGraphqlString Bool where
  showGql True = "true"
  showGql False = "false"

instance ToGraphqlString String where
  showGql = id

instance ToGraphqlString Text where
  showGql = unpack

-- | Transforms GraphQL to its JSON representation. Does string interpolation.
-- For every expression enclosed as #{expression}, this Quasi Quoter will
-- evaluate 'expression' in the context it was written. For example:
--
-- let x = True
-- [graphql| Hello, #{not x}! |]
--
-- Will get translated to "Hello, false!". Note that we convert the Haskell
-- value to a string using Show. If the expression
-- fails to evaluate, the compilation will fail.
graphql :: QuasiQuoter
graphql =
  QuasiQuoter
    { quoteExp = evalGraphql,
      quotePat = \_ -> fail "invalid",
      quoteType = \_ -> fail "invalid",
      quoteDec = \_ -> fail "invalid"
    }

-- | We parse the raw string to this structure: raw strings or expressions to
-- be interpolated.
data GraphqlPart
  = GPRaw String
  | GPExpression Exp

-- | Parses raw QQ text to a list of 'GraphqlPart' then interprets it.
evalGraphql :: String -> ExpQ
evalGraphql txt =
  case parseInterpolatedGQL txt of
    Left err -> fail $ "Parsing error: " <> err
    Right result -> interpret result

parseInterpolatedGQL :: String -> Either String [GraphqlPart]
parseInterpolatedGQL = Bifunctor.first show . P.parse parseParts "graphqlQQ"
  where
    -- This can probably be made more succinct. We start by trying to parse
    -- an interpolated expression, then we try to parse a comment. The reasoning
    -- behind it is that both start with a '#' character, and raw parsing does
    -- NOT parse '#' chars.
    --
    -- TODO: is there a way to escape '#' chars such that they are valid in
    -- graphql? If yes, then this parser will fail on those.
    parseParts :: Parser [GraphqlPart]
    parseParts =
      P.many (P.try parseInterpolatedExpr P.<|> P.try parseComment P.<|> parseRaw)

    -- Parses raw text. Will never parse a '#'.
    parseRaw :: Parser GraphqlPart
    parseRaw = GPRaw <$> P.many1 (P.try $ P.noneOf "#")

    -- Parses a comment: wherever we encounter a '#', until the end of line.
    parseComment :: Parser GraphqlPart
    parseComment = do
      _ <- P.char '#'
      it <- P.manyTill P.anyChar P.newline
      pure $ GPRaw ('#' : it <> "\n")

    -- Parses an interpolated expression.
    parseInterpolatedExpr :: Parser GraphqlPart
    parseInterpolatedExpr = do
      _ <- P.string "#{"
      code <- P.manyTill P.anyChar (P.char '}')
      case parseExp code of
        Left err -> fail $ "could not find name: " <> err
        Right r -> pure $ GPExpression r

-- TODO: Is there a better way of doing this? I am not sure 'fromString' is
-- ideal here, but this is what was (implicitly) happening before.
interpret :: [GraphqlPart] -> ExpQ
interpret =
  appE [|fromString|]
    . appE [|concat|]
    . listE
    . fmap go
  where
    go :: GraphqlPart -> ExpQ
    go (GPRaw s) = stringE s
    go (GPExpression e) = appE [|exprToGql|] (pure e)

exprToGql :: (ToGraphqlString a) => a -> String
exprToGql a = showGql a
