{-# LANGUAGE ApplicativeDo #-}

-- TODO(SOLOMON): Minimize Exports
module Hasura.Server.Init.Arg
  ( module Downgrade,
    module Serve,
    parseHgeOpts,
    parsePostgresConnInfo,
    parseMetadataDbUrl,
    mainCmdFooter,
    metadataDbUrlEnv,
    retriesNumEnv,
    databaseUrlEnv,
  )
where

--------------------------------------------------------------------------------

import Data.URL.Template qualified as Template
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.Server.Init.Arg.Command.Downgrade as Downgrade
import Hasura.Server.Init.Arg.Command.Serve as Serve
import Hasura.Server.Init.Arg.PrettyPrinter qualified as PP
import Hasura.Server.Init.Config qualified as Config
import Hasura.Server.Init.Env qualified as Env
import Options.Applicative qualified as Opt

--------------------------------------------------------------------------------

-- | The Main Arg Parser. It constructs a 'Config.HGEOptionsRaw' term:
--
-- 1. '(Config.PostgresConnInfo (Maybe PostgresConnInfoRaw))' - The DB connection.
-- 2: 'Maybe String' - Representing the metadata connection.
-- 3: 'Config.HGECommand' @a@ - The result of the supplied Subcommand.
parseHgeOpts :: L.EnabledLogTypes impl => Opt.Parser (Config.HGEOptionsRaw (Config.ServeOptionsRaw impl))
parseHgeOpts =
  Config.HGEOptionsRaw <$> parsePostgresConnInfo <*> parseMetadataDbUrl <*> parseHGECommand

parseHGECommand :: L.EnabledLogTypes impl => Opt.Parser (Config.HGECommand (Config.ServeOptionsRaw impl))
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

parsePostgresConnInfo :: Opt.Parser (Config.PostgresConnInfo (Maybe Config.PostgresConnInfoRaw))
parsePostgresConnInfo = do
  retries' <- retries
  maybeRawConnInfo <-
    (fmap Config.PGConnDatabaseUrl <$> parseDatabaseUrl)
      <|> (fmap Config.PGConnDetails <$> parseRawConnDetails)
  pure $ Config.PostgresConnInfo maybeRawConnInfo retries'
  where
    retries =
      Opt.optional $
        Opt.option
          Opt.auto
          ( Opt.long "retries"
              <> Opt.metavar "NO OF RETRIES"
              <> Opt.help (snd retriesNumEnv)
          )

retriesNumEnv :: (String, String)
retriesNumEnv =
  ( "HASURA_GRAPHQL_NO_OF_RETRIES",
    "No.of retries if Postgres connection error occurs (default: 1)"
  )

parseDatabaseUrl :: Opt.Parser (Maybe Template.URLTemplate)
parseDatabaseUrl =
  Opt.optional $
    Opt.option
      (Opt.eitherReader Env.fromEnv)
      ( Opt.long "database-url"
          <> Opt.metavar "<DATABASE-URL>"
          <> Opt.help (snd databaseUrlEnv)
      )

databaseUrlEnv :: (String, String)
databaseUrlEnv =
  ( "HASURA_GRAPHQL_DATABASE_URL",
    "Postgres database URL. Example postgres://foo:bar@example.com:2345/database"
  )

parseRawConnDetails :: Opt.Parser (Maybe Config.PostgresConnDetailsRaw)
parseRawConnDetails = do
  host' <- host
  port' <- port
  user' <- user
  password' <- password
  dbName' <- dbName
  options' <- options
  pure $
    Config.PostgresConnDetailsRaw
      <$> host'
      <*> port'
      <*> user'
      <*> pure password'
      <*> dbName'
      <*> pure options'
  where
    host =
      Opt.optional $
        Opt.strOption
          ( Opt.long "host"
              <> Opt.metavar "<HOST>"
              <> Opt.help "Postgres server host"
          )

    port =
      Opt.optional $
        Opt.option
          Opt.auto
          ( Opt.long "port"
              <> Opt.short 'p'
              <> Opt.metavar "<PORT>"
              <> Opt.help "Postgres server port"
          )

    user =
      Opt.optional $
        Opt.strOption
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
      Opt.optional $
        Opt.strOption
          ( Opt.long "dbname"
              <> Opt.short 'd'
              <> Opt.metavar "<DBNAME>"
              <> Opt.help "Database name to connect to"
          )

    options =
      Opt.optional $
        Opt.strOption
          ( Opt.long "pg-connection-options"
              <> Opt.short 'o'
              <> Opt.metavar "<DATABASE-OPTIONS>"
              <> Opt.help "PostgreSQL options"
          )

-- TODO(SOLOMON): Should we parse the URL here?
parseMetadataDbUrl :: Opt.Parser (Maybe String)
parseMetadataDbUrl =
  Opt.optional $
    Opt.strOption
      ( Opt.long "metadata-database-url"
          <> Opt.metavar "<METADATA-DATABASE-URL>"
          <> Opt.help (snd metadataDbUrlEnv)
      )

metadataDbUrlEnv :: (String, String)
metadataDbUrlEnv =
  ( "HASURA_GRAPHQL_METADATA_DATABASE_URL",
    "Postgres database URL for Metadata storage. Example postgres://foo:bar@example.com:2345/database"
  )

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

    envVarDoc = PP.mkEnvVarDoc [databaseUrlEnv, retriesNumEnv]
