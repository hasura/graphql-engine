-- | CORS (Cross Origin Resource Sharing) related configuration

module Hasura.Server.Cors where

import qualified Data.Aeson          as J
import qualified Data.ByteString     as B
import qualified Data.Text           as T

import           Data.List           (partition)

import           Hasura.Prelude
import           Hasura.Server.Utils


data Domains
  = Domains
  { dFqdn      :: ![Text]
  , dWildcards :: ![Text]
  } deriving (Show, Eq)

instance J.ToJSON Domains where
  toJSON ds = J.toJSON $ dFqdn ds ++ dWildcards ds

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
wildcardDomainRegex = "^(\\*\\.)?([a-zA-Z0-9]+\\.)*[a-zA-Z0-9]+$"

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
      Right $ CDDomains $ Domains fqdns wcs
  where
    err d = "invalid domain: '" <> T.unpack d <> "'"
    partitionDomains = partition (T.isPrefixOf "*.")


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
