{-# LANGUAGE ApplicativeDo #-}

module Hasura.Server.Init.Arg
  ( -- * Main Opt.Parser
    parseHgeOpts,
    parsePostgresConnInfo,
    parseMetadataDbUrl,
    mainCmdFooter,
    metadataDbUrlOption,
    retriesNumOption,
    databaseUrlOption,

    -- * Command Opt.Parsers
    module Downgrade,
    module Serve,
  )
where

--------------------------------------------------------------------------------

import Data.URL.Template qualified as Template
import Hasura.Logging qualified as Logging
import Hasura.Prelude
import Hasura.Server.Init.Arg.Command.Downgrade as Downgrade
import Hasura.Server.Init.Arg.Command.Serve as Serve
import Hasura.Server.Init.Arg.PrettyPrinter qualified as PP
import Hasura.Server.Init.Config (HGECommand, HGEOptionsRaw, Option, PostgresConnDetailsRaw, PostgresConnInfo, PostgresConnInfoRaw, ServeOptionsRaw)
import Hasura.Server.Init.Config qualified as Config
import Hasura.Server.Init.Env qualified as Env
import Options.Applicative qualified as Opt

--------------------------------------------------------------------------------

-- | The Main Arg 'Opt.Parser'. It constructs a 'HGEOptionsRaw' term:
--
-- 1. '(Config.PostgresConnInfo (Maybe PostgresConnInfoRaw))' - The DB connection.
-- 2: 'Maybe String' - Representing the metadata connection.
-- 3: 'Config.HGECommand' @a@ - The result of the supplied Subcommand.
parseHgeOpts :: (Logging.EnabledLogTypes impl) => Opt.Parser (HGEOptionsRaw (ServeOptionsRaw impl))
parseHgeOpts =
  Config.HGEOptionsRaw <$> parsePostgresConnInfo <*> parseMetadataDbUrl <*> parseHGECommand

parseHGECommand :: (Logging.EnabledLogTypes impl) => Opt.Parser (HGECommand (ServeOptionsRaw impl))
parseHGECommand =
  Opt.subparser
    ( Opt.command
        "serve"
        ( Opt.info
            (Opt.helper <*> (Config.HCServe <$> serveCommandParser))
            ( Opt.progDesc "Start the GraphQL Engine Server"
                <> Opt.footerDoc (Just serveCmdFooter)
            )
        )
        <> Opt.command
          "export"
          ( Opt.info
              (pure Config.HCExport)
              (Opt.progDesc "Export graphql-engine's metadata to stdout")
          )
        <> Opt.command
          "clean"
          ( Opt.info
              (pure Config.HCClean)
              (Opt.progDesc "Clean graphql-engine's metadata to start afresh")
          )
        <> Opt.command
          "downgrade"
          ( Opt.info
              (Config.HCDowngrade <$> downgradeCommandParser)
              (Opt.progDesc "Downgrade the GraphQL Engine schema to the specified version")
          )
        <> Opt.command
          "version"
          ( Opt.info
              (pure Config.HCVersion)
              (Opt.progDesc "Prints the version of GraphQL Engine")
          )
    )

--------------------------------------------------------------------------------

parsePostgresConnInfo :: Opt.Parser (PostgresConnInfo (Maybe PostgresConnInfoRaw))
parsePostgresConnInfo = do
  retries' <- retries
  maybeRawConnInfo <-
    (fmap Config.PGConnDatabaseUrl <$> parseDatabaseUrl)
      <|> (fmap Config.PGConnDetails <$> parseRawConnDetails)
  pure $ Config.PostgresConnInfo maybeRawConnInfo retries'
  where
    retries =
      Opt.optional
        $ Opt.option
          Opt.auto
          ( Opt.long "retries"
              <> Opt.metavar "NO OF RETRIES"
              <> Opt.help (Config._helpMessage retriesNumOption)
          )

retriesNumOption :: Option ()
retriesNumOption =
  Config.Option
    { Config._default = (),
      Config._envVar = "HASURA_GRAPHQL_NO_OF_RETRIES",
      Config._helpMessage = "No.of retries if Postgres connection error occurs (default: 1)"
    }

parseDatabaseUrl :: Opt.Parser (Maybe Template.Template)
parseDatabaseUrl =
  Opt.optional
    $ Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "database-url"
          <> Opt.metavar "<DATABASE-URL>"
          <> Opt.help (Config._helpMessage databaseUrlOption)
      )

databaseUrlOption :: Option ()
databaseUrlOption =
  Config.Option
    { Config._default = (),
      Config._envVar = "HASURA_GRAPHQL_DATABASE_URL",
      Config._helpMessage = "Postgres database URL. Example postgres://foo:bar@example.com:2345/database"
    }

parseRawConnDetails :: Opt.Parser (Maybe PostgresConnDetailsRaw)
parseRawConnDetails = do
  host' <- host
  port' <- port
  user' <- user
  password' <- password
  dbName' <- dbName
  options' <- options
  pure
    $ Config.PostgresConnDetailsRaw
    <$> host'
    <*> port'
    <*> user'
    <*> pure password'
    <*> dbName'
    <*> pure options'
  where
    host =
      Opt.optional
        $ Opt.strOption
          ( Opt.long "host"
              <> Opt.metavar "<HOST>"
              <> Opt.help "Postgres server host"
          )

    port =
      Opt.optional
        $ Opt.option
          Opt.auto
          ( Opt.long "port"
              <> Opt.short 'p'
              <> Opt.metavar "<PORT>"
              <> Opt.help "Postgres server port"
          )

    user =
      Opt.optional
        $ Opt.strOption
          ( Opt.long "user"
              <> Opt.short 'u'
              <> Opt.metavar "<USER>"
              <> Opt.help "Database user name"
          )

    password =
      Opt.strOption
        ( Opt.long "password"
            <> Opt.metavar "<PASSWORD>"
            <> Opt.value ""
            <> Opt.help "Password of the user"
        )

    dbName =
      Opt.optional
        $ Opt.strOption
          ( Opt.long "dbname"
              <> Opt.short 'd'
              <> Opt.metavar "<DBNAME>"
              <> Opt.help "Database name to connect to"
          )

    options =
      Opt.optional
        $ Opt.strOption
          ( Opt.long "pg-connection-options"
              <> Opt.short 'o'
              <> Opt.metavar "<DATABASE-OPTIONS>"
              <> Opt.help "PostgreSQL options"
          )

-- Currently this MUST ultimately end up getting parsed in initBasicConnectionInfo
parseMetadataDbUrl :: Opt.Parser (Maybe String)
parseMetadataDbUrl =
  Opt.optional
    $ Opt.strOption
      ( Opt.long "metadata-database-url"
          <> Opt.metavar "<METADATA-DATABASE-URL>"
          <> Opt.help (Config._helpMessage metadataDbUrlOption)
      )

metadataDbUrlOption :: Option ()
metadataDbUrlOption =
  Config.Option
    { Config._default = (),
      Config._envVar = "HASURA_GRAPHQL_METADATA_DATABASE_URL",
      Config._helpMessage = "Postgres database URL for Metadata storage. Example postgres://foo:bar@example.com:2345/database\nThis can also be a URI of the form ‘dynamic-from-file:///path/to/file’,  where the referenced file contains a postgres connection string, which will be read dynamically every time a new connection is established."
    }

--------------------------------------------------------------------------------
-- Pretty Printer

mainCmdFooter :: PP.Doc
mainCmdFooter =
  examplesDoc PP.<$> PP.text "" PP.<$> envVarDoc
  where
    examplesDoc = PP.mkExamplesDoc examples
    examples =
      [ [ "# Serve GraphQL Engine on default port (8080) with console disabled",
          "graphql-engine --database-url <database-url> serve"
        ],
        [ "# For more options, checkout",
          "graphql-engine serve --help"
        ]
      ]

    envVarDoc =
      PP.mkEnvVarDoc
        [ Config.optionPP databaseUrlOption,
          Config.optionPP metadataDbUrlOption,
          Config.optionPP retriesNumOption
        ]
