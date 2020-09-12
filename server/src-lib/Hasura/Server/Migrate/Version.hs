-- | A module that defines the current catalog version and nothing else. This is necessary to
-- circumvent the unfortunate “GHC stage restriction,” which prevents us from using a binding in a
-- compile-time splice unless it is defined in a different module. The actual migration code is in
-- "Hasura.Server.Migrate".
module Hasura.Server.Migrate.Version
  ( latestCatalogVersion
  , latestCatalogVersionString
  ) where

import           Hasura.Prelude

import qualified Data.Text                  as T
import qualified Language.Haskell.TH.Syntax as TH

import           Data.FileEmbed             (embedStringFile)

-- | The current catalog schema version. We store this in a file
-- because we want to append the current verson to the catalog_versions file
-- when tagging a new release, in @tag-release.sh@.
latestCatalogVersion :: Integer
latestCatalogVersion =
  $(do let s = $(embedStringFile "src-rsr/catalog_version.txt")
       TH.lift (read s :: Integer))

latestCatalogVersionString :: T.Text
latestCatalogVersionString = T.pack $ show latestCatalogVersion
