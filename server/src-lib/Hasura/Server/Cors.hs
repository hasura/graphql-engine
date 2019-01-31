-- | CORS (Cross Origin Resource Sharing) related configuration

module Hasura.Server.Cors where

import qualified Data.Aeson           as J
import qualified Data.Aeson.Casing    as J
import qualified Data.Aeson.TH        as J
import qualified Data.Attoparsec.Text as AT
import qualified Data.ByteString      as B
import qualified Data.HashSet         as Set
import qualified Data.Text            as T

import           Control.Applicative  (optional)
import           Data.List            (partition)

import           Hasura.Prelude
import           Hasura.Server.Utils


data Domains
  = Domains
  { dmFqdns     :: !(Set.HashSet Text)
  , dmWildcards :: !(Set.HashSet (Text, Text, Maybe Int))
  } deriving (Show, Eq)

$(J.deriveToJSON (J.aesonDrop 2 J.snakeCase) ''Domains)

data CorsDomain
  = CDStar -- corresponds to *
  | CDDomains Domains
  deriving (Show, Eq)

instance J.ToJSON CorsDomain where
  toJSON CDStar        = J.String "*"
  toJSON (CDDomains d) = J.toJSON d

data CorsConfigG a
  = CorsConfigG
  { ccDomain   :: !a
  , ccDisabled :: !Bool
  } deriving (Show, Eq)

type RawCorsConfig = CorsConfigG (Maybe CorsDomain)
type CorsConfig = CorsConfigG CorsDomain


wildcardDomainRegex :: B.ByteString
wildcardDomainRegex = "^http(s)?://(\\*\\.)?([a-zA-Z0-9]+\\.)*[a-zA-Z0-9]+(:[0-9]+)*$"

validateDomain :: Text -> Either String Bool
validateDomain = matchRegex wildcardDomainRegex False

readCorsDomains :: String -> Either String CorsDomain
readCorsDomains str
  | str == "*"               = Right CDStar
  | str == "" || str == ","  = Left "invalid domain"
  | otherwise = do
      let domains = map T.strip $ T.splitOn "," (T.pack str)
      forM_ domains $ \dom -> do
        res <- validateDomain dom
        bool (Left $ err dom) (Right dom) res
      let (wcs, fqdns) = partitionDomains domains
      wcsTriples <- mapM parseWildcardDomain wcs
      Right $ CDDomains $ Domains (Set.fromList fqdns) (Set.fromList wcsTriples)
  where
    err d = "invalid domain: '" <> T.unpack d <> "'"
    partitionDomains = partition (\d -> T.isPrefixOf "http://*." d || T.isPrefixOf "https://*." d)


data CorsPolicy
  = CorsPolicy
  { cpDomains :: !CorsDomain
  , cpMethods :: ![Text]
  , cpMaxAge  :: !Int
  } deriving (Show, Eq)

mkDefaultCorsPolicy :: CorsConfig -> CorsPolicy
mkDefaultCorsPolicy cfg =
  CorsPolicy
  { cpDomains = ccDomain cfg
  , cpMethods = ["GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS"]
  , cpMaxAge = 1728000
  }


parseOrigin :: Text -> Either String (Text, Text, Maybe Int)
parseOrigin = runParser originParser

originParser :: AT.Parser (Text, Text, Maybe Int)
originParser = do
  scheme <- schemeParser
  void $ AT.takeTill (== '.')
  void $ AT.char '.'
  host <- parseHostWPort <|> AT.takeText
  port <- optional portParser
  return (scheme, host, port)


parseWildcardDomain :: Text -> Either String (Text, Text, Maybe Int)
parseWildcardDomain = runParser wildcardDomainParser

runParser :: AT.Parser a -> Text -> Either String a
runParser = AT.parseOnly

wildcardDomainParser :: AT.Parser (Text, Text, Maybe Int)
wildcardDomainParser =
  (,,) <$> schemeParser <*> wildcardHostParser <*> optional portParser

schemeParser :: AT.Parser Text
schemeParser = AT.string "http://" <|> AT.string "https://"

wildcardHostParser :: AT.Parser Text
wildcardHostParser = do
  void $ "*" *> "."
  parseHostWPort <|> AT.takeText

parseHostWPort :: AT.Parser Text
parseHostWPort = do
  h <- AT.takeWhile1 (/= ':')
  void $ AT.char ':'
  return h

portParser :: AT.Parser Int
portParser = AT.decimal
