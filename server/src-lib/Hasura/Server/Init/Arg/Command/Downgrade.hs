{-# LANGUAGE TemplateHaskell #-}

-- | The Downgrade Command Parser
module Hasura.Server.Init.Arg.Command.Downgrade
  ( downgradeCommandParser,
  )
where

--------------------------------------------------------------------------------

import Data.FileEmbed qualified as Embed
import Data.String qualified as String
import Hasura.Prelude
import Hasura.Server.Init.Config qualified as Config
import Language.Haskell.TH.Syntax qualified as TH
import Options.Applicative qualified as Opt

--------------------------------------------------------------------------------

-- | This implements the mapping between application versions
-- and catalog schema versions.
downgradeShortcuts :: [(String, String)]
downgradeShortcuts =
  $( do
       let s = $(Embed.makeRelativeToProject "src-rsr/catalog_versions.txt" >>= Embed.embedStringFile)

           parseVersions = map (parseVersion . words) . lines

           parseVersion [tag, version] = (tag, version)
           parseVersion other = error ("unrecognized tag/catalog mapping " ++ show other)
       TH.lift (parseVersions s)
   )

--------------------------------------------------------------------------------

downgradeCommandParser :: Opt.Parser Config.DowngradeOptions
downgradeCommandParser =
  Config.DowngradeOptions
    <$> choice
      ( Opt.strOption
          ( Opt.long "to-catalog-version"
              <> Opt.metavar "<VERSION>"
              <> Opt.help "The target catalog schema version (e.g. 31)"
          )
          : map (uncurry shortcut) downgradeShortcuts
      )
    <*> Opt.switch
      ( Opt.long "dryRun"
          <> Opt.help "Don't run any migrations, just print out the SQL."
      )
  where
    shortcut v catalogVersion =
      Opt.flag'
        (String.fromString catalogVersion)
        ( Opt.long ("to-" <> v)
            <> Opt.help ("Downgrade to graphql-engine version " <> v <> " (equivalent to --to-catalog-version " <> catalogVersion <> ")")
        )
