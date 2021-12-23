-- | graphql-engine used to have a query plan cache, which cached the execution
-- plan for a given graphql query (sans JSON variable values).  After the PDV
-- refactor (see
-- [hasura/graphql-engine#4111](https://github.com/hasura/graphql-engine/pull/4111)),
-- this query plan cache was not needed anymore.  For backwards compatibility
-- reasons, we still need to parse the configuration options from the CLI, although
-- the CLI option gets ignored.
--
-- Eventually, we can decide to stop parsing the CLI option
-- --query-plan-cache-size, at which point this module can be removed.
module Hasura.Cache.Bounded
  ( CacheSize (..),
    parseCacheSize,
  )
where

import Data.Aeson qualified as J
import Data.Word (Word16)
import GHC.Natural (Natural)
import Hasura.Prelude hiding (lookup)

newtype CacheSize = CacheSize {unCacheSize :: Word16}
  deriving (Show, Read, Eq, Ord, Bounded, Num, Real, Integral, Enum, J.ToJSON, J.FromJSON)

parseCacheSize :: String -> Either String CacheSize
parseCacheSize v =
  -- NOTE: naively using readMaybe Word16 will silently wrap
  case readMaybe v :: Maybe Natural of
    Just n | n <= max16 && n >= 0 -> return (CacheSize $ fromIntegral n)
    _ -> throwError "cache size must be given as a number between 0 and 65535"
  where
    max16 = fromIntegral (maxBound :: Word16) :: Natural
