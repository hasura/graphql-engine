{-# LANGUAGE TemplateHaskell #-}

-- | A module that defines the current catalog version and nothing else. This is necessary to
-- circumvent the unfortunate “GHC stage restriction,” which prevents us from using a binding in a
-- compile-time splice unless it is defined in a different module. The actual migration code is in
-- "Hasura.Server.Migrate".
module Hasura.Server.Migrate.Version
  ( latestCatalogVersion,
    latestCatalogVersionString,
  )
where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.Text qualified as T
import Hasura.Prelude
import Language.Haskell.TH.Syntax qualified as TH

-- | The current catalog schema version. We store this in a file
-- because we want to append the current verson to the catalog_versions file
-- when tagging a new release, in @tag-release.sh@.
latestCatalogVersion :: Integer
latestCatalogVersion =
  $( do
       let s = $(makeRelativeToProject "src-rsr/catalog_version.txt" >>= embedStringFile)
       TH.lift (read s :: Integer)
   )

latestCatalogVersionString :: Text
latestCatalogVersionString = T.pack $ show latestCatalogVersion
