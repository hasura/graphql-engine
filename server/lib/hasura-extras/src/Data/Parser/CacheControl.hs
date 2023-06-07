-- | Functions related to parsing @Cache-Control@ header as defined in
-- https://tools.ietf.org/html/rfc7234#section-5.2
--
-- To get @max-age@/@s-maxage@ from @Cache-Control@ header, use 'parseMaxAge'. If you need to check
-- other directives use 'parseCacheControl'.
--
-- Rules which starts with @obs-@ is not required to implement because they are maked as "obsolete" as
-- per https://tools.ietf.org/html/rfc7230#section-1.2
module Data.Parser.CacheControl
  ( CacheControl,
    CacheControlDirective (..),
    parseCacheControl,
    parseMaxAge,
    findMaxAge,
    noCacheExists,
    noStoreExists,
    mustRevalidateExists,
  )
where

import Data.Attoparsec.Text qualified as AT
import Data.Bifunctor (first)
import Data.Text qualified as T
import Hasura.Prelude hiding (first)

type CacheControl = [CacheControlDirective]

data CacheControlDirective
  = CCDOnlyToken !Text
  | CCDTokenWithVal !Text !Text
  deriving (Show, Eq)

-- | Tries to parse the @max-age@ or @s-maxage@ present in the value of @Cache-Control@ header
parseMaxAge :: (Integral a) => Text -> Either String a
parseMaxAge t =
  parseCacheControl t >>= findMaxAge >>= maybe (Left notFoundErr) Right
  where
    notFoundErr = "could not find max-age/s-maxage"

findMaxAge :: (Integral a) => CacheControl -> Either String (Maybe a)
findMaxAge cacheControl = do
  case findCCDTokenWithVal checkMaxAgeToken cacheControl of
    Just (_, val) -> Just <$> first parseErr (AT.parseOnly AT.decimal val)
    Nothing -> Right Nothing
  where
    parseErr _ = "could not parse max-age/s-maxage value"
    checkMaxAgeToken token = token == "max-age" || token == "s-maxage"

-- | Checks if the @no-cache@ directive is present
noCacheExists :: CacheControl -> Bool
noCacheExists cacheControl =
  isJust $ findCCDOnlyToken (== "no-cache") cacheControl

-- | Checks if the @no-store@ directive is present
noStoreExists :: CacheControl -> Bool
noStoreExists cacheControl =
  isJust $ findCCDOnlyToken (== "no-store") cacheControl

-- | Checks if the @must-revalidate@ directive is present
mustRevalidateExists :: CacheControl -> Bool
mustRevalidateExists cacheControl =
  isJust $ findCCDOnlyToken (== "must-revalidate") cacheControl

findCCDOnlyToken :: (Text -> Bool) -> CacheControl -> Maybe Text
findCCDOnlyToken tokenPredicate cacheControl =
  listToMaybe $ mapMaybe check cacheControl
  where
    check = \case
      CCDOnlyToken token
        | tokenPredicate token -> Just token
        | otherwise -> Nothing
      CCDTokenWithVal _ _ -> Nothing

findCCDTokenWithVal :: (Text -> Bool) -> CacheControl -> Maybe (Text, Text)
findCCDTokenWithVal tokenPredicate cacheControl =
  listToMaybe $ mapMaybe check cacheControl
  where
    check = \case
      CCDOnlyToken _ -> Nothing
      CCDTokenWithVal token value
        | tokenPredicate token -> Just (token, value)
        | otherwise -> Nothing

-- | Parses a @Cache-Control@ header and returns a list of directives
parseCacheControl :: Text -> Either String CacheControl
parseCacheControl = AT.parseOnly cacheControlParser

-- ABNF: cache-control = *( "," OWS) cache-directive *( OWS "," [OWS cache-directive])
-- https://tools.ietf.org/html/rfc7234#appendix-C
cacheControlParser :: AT.Parser CacheControl
cacheControlParser = do
  void $ AT.many' ("," *> optionalWhitespaceParser)
  cd <- cacheDirectiveParser
  cds <- AT.many' $ optionalWhitespaceParser *> "," *> AT.option Nothing (pure <$> optionalDirective)
  return $ cd : catMaybes cds
  where
    optionalDirective = optionalWhitespaceParser *> cacheDirectiveParser

-- ABNF: OWS = *( SP / HTAB ) ; optional whitespace
-- https://tools.ietf.org/html/rfc7230#section-3.2.3
optionalWhitespaceParser :: AT.Parser (Maybe Char)
optionalWhitespaceParser = AT.option Nothing (pure <$> AT.space)

-- ABNF: cache-directive = token [ "=" ( token / quoted-string ) ]
-- https://tools.ietf.org/html/rfc7230#section-3.2.6
cacheDirectiveParser :: AT.Parser CacheControlDirective
cacheDirectiveParser = tokenWithValue <|> onlyToken
  where
    onlyToken = CCDOnlyToken <$> tokenParser
    tokenWithValue = do
      tok <- tokenParser
      void $ AT.char '='
      val <- tokenParser <|> quotedStringParser
      return $ CCDTokenWithVal tok val

-- ABNF: 1*tchar
-- https://tools.ietf.org/html/rfc7230#section-3.2.6
tokenParser :: AT.Parser Text
tokenParser = T.pack <$> AT.many1 tcharParser

-- ABNF: tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "." / "^" / "_" / "`" / "|"
--               / "~" / DIGIT / ALPHA ; any VCHAR, except delimiters
-- https://tools.ietf.org/html/rfc7230#section-3.2.6
tcharParser :: AT.Parser Char
tcharParser =
  AT.char '!'
    <|> AT.char '#'
    <|> AT.char '$'
    <|> AT.char '%'
    <|> AT.char '&'
    <|> AT.char '\''
    <|> AT.char '*'
    <|> AT.char '+'
    <|> AT.char '-'
    <|> AT.char '.'
    <|> AT.char '^'
    <|> AT.char '_'
    <|> AT.char '`'
    <|> AT.char '|'
    <|> AT.char '~'
    <|> AT.digit
    <|> AT.letter

-- ABNF: DQUOTE = %x22 ; " (Double Quote)
-- https://tools.ietf.org/html/rfc5234#appendix-B.1
dquoteParser :: AT.Parser Char
dquoteParser = AT.char '"'

-- ABNF: VCHAR = %x21-7E ; visible (printing) characters
-- https://tools.ietf.org/html/rfc5234#appendix-B.1
vcharParser :: AT.Parser Char
vcharParser = AT.anyChar

-- ABNF: quoted-string = DQUOTE *( qdtext / quoted-pair ) DQUOTE
-- https://tools.ietf.org/html/rfc7230#section-3.2.6
quotedStringParser :: AT.Parser Text
quotedStringParser =
  dquoteParser *> fmap T.pack (AT.many' (qdTextParser <|> quotedPairParser)) <* dquoteParser

-- ABNF: quoted-pair = "\" ( HTAB / SP / VCHAR / obs-text )
-- https://tools.ietf.org/html/rfc7230#section-3.2.6
quotedPairParser :: AT.Parser Char
quotedPairParser =
  AT.string "\\" *> (AT.space <|> vcharParser)

-- ABNF: qdtext = HTAB / SP / %x21 / %x23-5B / %x5D-7E / obs-text
-- https://tools.ietf.org/html/rfc7230#section-3.2.6
qdTextParser :: AT.Parser Char
qdTextParser =
  AT.space
    <|> AT.char '!' -- %x21
    -- skip %x22 as it is '"'
    <|> AT.char '#' -- %x23
    <|> AT.char '$' -- %x24
    <|> AT.char '%' -- %x25
    <|> AT.char '&' -- %x26
    <|> AT.char '\'' -- %x27 single quote
    <|> AT.char '(' -- %x28
    <|> AT.char ')' -- %x29
    <|> AT.char '*' -- %x2A
    <|> AT.char '+' -- %x2B
    <|> AT.char ',' -- %x2C
    <|> AT.char '-' -- %x2D
    <|> AT.char '.' -- %x2E
    <|> AT.char '/' -- %x2F
    <|> AT.digit -- %x30-39
    <|> AT.char ':' -- %x3A
    <|> AT.char ';' -- %x3B
    <|> AT.char '<' -- %x3C
    <|> AT.char '=' -- %x3D
    <|> AT.char '>' -- %x3E
    <|> AT.char '?' -- %x3F
    <|> AT.char '@' -- %x40
    <|> AT.letter -- %x41-5A / %x61-7A
    <|> AT.char '[' -- %x5B
    -- skip %x5C as it is '\'
    <|> AT.char ']' -- %x5D
    <|> AT.char '^' -- %x5E
    <|> AT.char '_' -- %x5F
    <|> AT.char '`' -- %x60
    <|> AT.char '{' -- %x7B
    <|> AT.char '|' -- %x7C
    <|> AT.char '}' -- %x7D
    <|> AT.char '~' -- %x7E
