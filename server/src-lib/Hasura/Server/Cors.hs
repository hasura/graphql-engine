{-# LANGUAGE DeriveAnyClass #-}

-- | CORS (Cross Origin Resource Sharing) related configuration
module Hasura.Server.Cors
  ( CorsConfig (..),
    CorsPolicy (..),
    parseOrigin,
    readCorsDomains,
    mkDefaultCorsPolicy,
    isCorsDisabled,
    Domains (..),
    inWildcardList,
  )
where

import Control.Applicative (optional)
import Data.Aeson ((.:))
import Data.Aeson qualified as J
import Data.Attoparsec.Text qualified as AT
import Data.Char qualified as C
import Data.HashSet qualified as Set
import Data.Text qualified as T
import Hasura.Prelude
import Hasura.Server.Utils (fmapL)

data DomainParts = DomainParts
  { wdScheme :: !Text,
    wdHost :: !Text, -- the hostname part (without the *.)
    wdPort :: !(Maybe Int)
  }
  deriving (Show, Eq, Generic, Hashable)

instance J.FromJSON DomainParts where
  parseJSON = J.genericParseJSON hasuraJSON

instance J.ToJSON DomainParts where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

data Domains = Domains
  { dmFqdns :: !(Set.HashSet Text),
    dmWildcards :: !(Set.HashSet DomainParts)
  }
  deriving (Show, Eq, Generic)

instance J.FromJSON Domains where
  parseJSON = J.genericParseJSON hasuraJSON

instance J.ToJSON Domains where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

data CorsConfig
  = CCAllowAll
  | CCAllowedOrigins Domains
  | CCDisabled Bool -- should read cookie?
  deriving (Show, Eq)

instance J.ToJSON CorsConfig where
  toJSON c = case c of
    CCDisabled wsrc -> toJ True J.Null (Just wsrc)
    CCAllowAll -> toJ False (J.String "*") Nothing
    CCAllowedOrigins d -> toJ False (J.toJSON d) Nothing
    where
      toJ :: Bool -> J.Value -> Maybe Bool -> J.Value
      toJ dis origs mWsRC =
        J.object
          [ "disabled" J..= dis,
            "ws_read_cookie" J..= mWsRC,
            "allowed_origins" J..= origs
          ]

instance J.FromJSON CorsConfig where
  parseJSON = J.withObject "cors config" \o -> do
    let parseAllowAll "*" = pure CCAllowAll
        parseAllowAll _ = fail "unexpected string"
    o .: "disabled" >>= \case
      True -> CCDisabled <$> o .: "ws_read_cookie"
      False ->
        o .: "allowed_origins" >>= \v ->
          J.withText "origins" parseAllowAll v
            <|> CCAllowedOrigins
            <$> J.parseJSON v

isCorsDisabled :: CorsConfig -> Bool
isCorsDisabled = \case
  CCDisabled _ -> True
  _ -> False

readCorsDomains :: String -> Either String CorsConfig
readCorsDomains str
  | str == "*" = pure CCAllowAll
  | otherwise = do
      let domains = map T.strip $ T.splitOn "," (T.pack str)
      pDomains <- mapM parseOptWildcardDomain domains
      let (fqdns, wcs) = (lefts pDomains, rights pDomains)
      return $ CCAllowedOrigins $ Domains (Set.fromList fqdns) (Set.fromList wcs)

data CorsPolicy = CorsPolicy
  { cpConfig :: !CorsConfig,
    cpMethods :: ![Text],
    cpMaxAge :: !Int
  }
  deriving (Show, Eq)

mkDefaultCorsPolicy :: CorsConfig -> CorsPolicy
mkDefaultCorsPolicy cfg =
  CorsPolicy
    { cpConfig = cfg,
      cpMethods = ["GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS"],
      cpMaxAge = 1728000
    }

inWildcardList :: Domains -> Text -> Bool
inWildcardList (Domains _ wildcards) origin =
  any (`Set.member` wildcards) $ parseOrigin origin

-- | Parsers for wildcard domains
runParser :: AT.Parser a -> Text -> Either String a
runParser = AT.parseOnly

parseOrigin :: Text -> Either String DomainParts
parseOrigin = runParser originParser

originParser :: AT.Parser DomainParts
originParser =
  domainParser (Just ignoreSubdomain)
  where
    ignoreSubdomain = do
      s <- AT.takeTill (== '.')
      void $ AT.char '.'
      return s

parseOptWildcardDomain :: Text -> Either String (Either Text DomainParts)
parseOptWildcardDomain d =
  fmapL (const errMsg) $ runParser optWildcardDomainParser d
  where
    optWildcardDomainParser :: AT.Parser (Either Text DomainParts)
    optWildcardDomainParser =
      Right <$> wildcardDomainParser <|> Left <$> fqdnParser

    errMsg = "invalid domain: '" <> T.unpack d <> "'. " <> helpMsg
    helpMsg =
      "All domains should have scheme + (optional wildcard) host + "
        <> "(optional port)"

    wildcardDomainParser :: AT.Parser DomainParts
    wildcardDomainParser = domainParser $ Just (AT.string "*" *> AT.string ".")

    fqdnParser :: AT.Parser Text
    fqdnParser = do
      (DomainParts scheme host port) <- domainParser Nothing
      let sPort = maybe "" (\p -> ":" <> tshow p) port
      return $ scheme <> host <> sPort

domainParser :: Maybe (AT.Parser Text) -> AT.Parser DomainParts
domainParser parser = do
  scheme <- schemeParser
  forM_ parser void
  host <- hostPortParser
  port <- optional portParser
  return $ DomainParts scheme host port
  where
    schemeParser :: AT.Parser Text
    schemeParser = do
      -- supports a custom URI scheme, rather than just http:// or https:// (see OSS #5818)
      scheme <- AT.takeWhile1 (\x -> C.isAlphaNum x || elem x ['+', '.', '-'])
      sep <- AT.string "://"
      return $ scheme <> sep

    hostPortParser :: AT.Parser Text
    hostPortParser = hostWithPortParser <|> AT.takeText

    hostWithPortParser :: AT.Parser Text
    hostWithPortParser = do
      h <- AT.takeWhile1 (/= ':')
      void $ AT.char ':'
      return h

    portParser :: AT.Parser Int
    portParser = AT.decimal
