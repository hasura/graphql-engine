module Hasura.UpgradeTests.Options
  ( Options (..),
    parseOptions,
  )
where

import Hasura.Prelude
import Options.Applicative

-- | Test suite options.
data Options = Options
  { -- | The path to the root of the HGE repository
    --   (default: the current working directory).
    optionsRepositoryRoot :: FilePath,
    -- | The version of HGE to upgrade from (default: "latest").
    --   This is a Docker image tag.
    optionsBaseVersion :: Text,
    -- | Any further arguments, to be passed directly to Hspec.
    optionsHspecArgs :: [String]
  }
  deriving stock (Show)

-- | Parse 'Options' from the command-line arguments.
parseOptions :: IO Options
parseOptions = execParser optionsParserInfo

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption
      ( long "repository-root"
          <> metavar "PATH"
          <> value "."
          <> showDefault
          <> help "the path to the root of the HGE repository"
      )
    <*> strOption
      ( long "base-version"
          <> metavar "VERSION"
          <> value "latest"
          <> showDefault
          <> help "the version of HGE to upgrade from"
      )
    <*> many (strArgument (metavar "ARG" <> help "arguments to Hspec"))

optionsParserInfo :: ParserInfo Options
optionsParserInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Test that upgrading HGE from the last released version works"
        <> forwardOptions
    )
