-- | A module that defines the current catalog version and nothing else. This is necessary to
-- circumvent the unfortunate “GHC stage restriction,” which prevents us from using a binding in a
-- compile-time splice unless it is defined in a different module. The actual migration code is in
-- "Hasura.Server.Migrate".
module Hasura.Server.Migrate.Version
  ( latestCatalogVersion
  , latestCatalogVersionString
  ) where

import           Hasura.Prelude

import qualified Data.Text      as T

latestCatalogVersion :: Integer
latestCatalogVersion = 31

latestCatalogVersionString :: T.Text
latestCatalogVersionString = T.pack $ show latestCatalogVersion
