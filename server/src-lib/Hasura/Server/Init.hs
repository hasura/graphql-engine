{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasura.Server.Init where

import qualified Database.PG.Query    as Q

import           Options.Applicative
import           System.Exit          (exitFailure)

import qualified Data.Text            as T

import           Hasura.Prelude
import           Hasura.RQL.DDL.Utils
import           Hasura.Server.Auth
import           Hasura.Server.Utils


data InitError
  = InitError !String
  deriving (Show, Eq)

instance Q.FromPGConnErr InitError where
  fromPGConnErr = InitError . show

instance Q.FromPGTxErr InitError where
  fromPGTxErr = InitError . show


initErrExit :: (Show e) => e -> IO a
initErrExit e = print e >> exitFailure

-- clear the hdb_views schema
initStateTx :: Q.Tx ()
initStateTx = clearHdbViews

data RawConnInfo =
  RawConnInfo
  { connHost     :: !(Maybe String)
  , connPort     :: !(Maybe Int)
  , connUser     :: !(Maybe String)
  , connPassword :: !String
  , connUrl      :: !(Maybe String)
  , connDatabase :: !(Maybe String)
  , connOptions  :: !(Maybe String)
  } deriving (Eq, Read, Show)

data CorsConfigG a
  = CorsConfigG
  { ccDomain   :: !a
  , ccDisabled :: !Bool
  } deriving (Show, Eq)

type CorsConfigFlags = CorsConfigG (Maybe T.Text)
type CorsConfig = CorsConfigG T.Text


parseRawConnInfo :: Parser RawConnInfo
parseRawConnInfo =
  RawConnInfo
  <$> optional (strOption ( long "host" <>
                  metavar "HOST" <>
                  help "Postgres server host" ))
  <*> optional (option auto ( long "port" <>
                  short 'p' <>
                  metavar "PORT" <>
                  help "Postgres server port" ))
  <*> optional (strOption ( long "user" <>
                  short 'u' <>
                  metavar "USER" <>
                  help "Database user name" ))
  <*> strOption ( long "password" <>
                  short 'p' <>
                  metavar "PASSWORD" <>
                  value "" <>
                  help "Password of the user" )
  <*> optional (strOption ( long "database-url" <>
                  metavar "DATABASE-URL" <>
                  help "Postgres database URL. Example postgres://foo:bar@example.com:2345/database"))
  <*> optional (strOption ( long "dbname" <>
                  short 'd' <>
                  metavar "NAME" <>
                  help "Database name to connect to" ))
  <*> pure Nothing

connInfoErrModifier :: String -> String
connInfoErrModifier s = "Fatal Error : " ++ s

mkConnInfo :: Maybe String -> RawConnInfo -> Either String Q.ConnInfo
mkConnInfo mEnvDbUrl (RawConnInfo mHost mPort mUser pass mURL mDB opts) = do
  let mFinalDBUrl = ifNothingTakeEnv mURL
  case (mHost, mPort, mUser, mDB, mFinalDBUrl) of

    (Just host, Just port, Just user, Just db, Nothing) ->
      return $ Q.ConnInfo host port user pass db opts

    (_, _, _, _, Just dbURL) -> maybe (throwError invalidUrlMsg)
                                return $ parseDatabaseUrl dbURL opts
    _ -> throwError $ "Invalid options. "
                    ++ "Expecting all database connection params "
                    ++ "(host, port, user, dbname, password) or "
                    ++ "database-url (HASURA_GRAPHQL_DATABASE_URL)"
  where
    invalidUrlMsg = "Invalid database-url (HASURA_GRAPHQL_DATABASE_URL). "
                    ++ "Example postgres://foo:bar@example.com:2345/database"
    ifNothingTakeEnv Nothing = mEnvDbUrl
    ifNothingTakeEnv t       = t

readIsoLevel :: String -> Either String Q.TxIsolation
readIsoLevel isoS =
  case isoS of
    "read-comitted" -> return Q.ReadCommitted
    "repeatable-read" -> return Q.RepeatableRead
    "serializable" -> return Q.ReadCommitted
    _ -> Left "Only expecting read-comitted / repeatable-read / serializable"

parseTxIsolation :: Parser Q.TxIsolation
parseTxIsolation =
  option (eitherReader readIsoLevel) ( long "tx-iso" <>
                  short 'i' <>
                  value Q.ReadCommitted <>
                  metavar "TXISO" <>
                  help "transaction isolation. read-committed / repeatable-read / serializable" )

parseRootDir :: Parser (Maybe String)
parseRootDir =
  optional $ strOption ( long "root-dir" <>
               metavar "STATIC-DIR" <>
               help "this static dir is served at / and takes precedence over all routes" )

parseConnParams :: Parser Q.ConnParams
parseConnParams =
  Q.ConnParams
  <$> option auto ( long "stripes" <>
               short 's' <>
               metavar "NO OF STRIPES" <>
               value 1 <>
               help "Number of stripes" )
  <*> option auto ( long "connections" <>
               short 'c' <>
               metavar "NO OF CONNS" <>
               value 50 <>
               help "Number of conns that need to be opened to Postgres" )
  <*> option auto ( long "timeout" <>
               short 'c' <>
               metavar "SECONDS" <>
               value 180 <>
               help "Each connection's idle time before it is closed" )

parseServerPort :: Parser Int
parseServerPort =
  option auto ( long "server-port" <>
           metavar "PORT" <>
           value 8080 <>
           showDefault <>
           help "Port on which graphql-engine should be served")

parseAccessKey :: Parser (Maybe AccessKey)
parseAccessKey =
  optional $ AccessKey <$>
    strOption ( long "access-key" <>
                metavar "SECRET ACCESS KEY" <>
                help "Secret access key, required to access this instance"
              )

parseWebHook :: Parser (Maybe Webhook)
parseWebHook =
  optional $ Webhook <$>
    strOption ( long "auth-hook" <>
                metavar "AUTHENTICATION WEB HOOK" <>
                help "The authentication webhook, required to authenticate requests"
              )


parseJwtSecret :: Parser (Maybe Text)
parseJwtSecret =
  optional $ strOption ( long "jwt-secret" <>
                         metavar "JWK" <>
                         help jwtSecretHelp
                       )

jwtSecretHelp :: String
jwtSecretHelp = "The JSON containing type and the JWK used for verifying. e.g: "
              <> "`{\"type\": \"HS256\", \"key\": \"<your-hmac-shared-secret>\", \"claims_namespace\": \"<optional-custom-claims-key-name>\"}`,"
              <> "`{\"type\": \"RS256\", \"key\": \"<your-PEM-RSA-public-key>\", \"claims_namespace\": \"<optional-custom-claims-key-name>\"}`"


parseCorsConfig :: Parser CorsConfigFlags
parseCorsConfig =
  CorsConfigG
  <$> optional (strOption ( long "cors-domain" <>
                  metavar "CORS DOMAIN" <>
                  help "The domain, including scheme and port, to allow CORS for"
                 ))
  <*> switch ( long "disable-cors" <>
               help "Disable CORS handling"
             )

parseEnableConsole :: Parser Bool
parseEnableConsole = switch ( long "enable-console" <>
                              help "Enable API Console"
                            )
