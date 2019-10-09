-- | A module that defines the current catalog version and nothing else. This is necessary to
-- circumvent the unfortunate “GHC stage restriction,” which prevents us from using a binding in a
-- compile-time splice unless it is defined in a different module. The actual migration code is in
-- "Migrate".
module Migrate.Version (latestCatalogVersion) where

import           Hasura.Prelude

latestCatalogVersion :: Integer
latestCatalogVersion = 24
