module Hasura.Server.Init where

import qualified Database.PG.Query            as Q

import           Options.Applicative
import           Options.Applicative.Types
import           System.Exit                  (exitFailure)

import qualified Data.Text                    as T
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           Hasura.Prelude
import           Hasura.RQL.DDL.Utils
import           Hasura.RQL.Types             (RoleName (..))
import           Hasura.Server.Auth
import           Hasura.Server.Utils

newtype InitError
  = InitError String
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

data CorsConfig
  = CorsConfig
  { ccDomain   :: !T.Text
  , ccDisabled :: !Bool
  } deriving (Show, Eq)

type Env = [(String, String)]

class FromConfig a where
  fromConfig :: String -> Either String a

instance FromConfig String where
  fromConfig = Right

instance FromConfig Text where
  fromConfig = Right . T.pack

instance FromConfig AuthHookType where
  fromConfig = readHookType

instance FromConfig Int where
  fromConfig = maybe (Left "Expecting Int value") Right . readMaybe

instance FromConfig AccessKey where
  fromConfig = Right . AccessKey . T.pack

instance FromConfig RoleName where
  fromConfig = Right . RoleName . T.pack

instance FromConfig Bool where
  fromConfig = parseStrAsBool

instance FromConfig Q.TxIsolation where
  fromConfig = readIsoLevel

parseStrAsBool :: String -> Either String Bool
parseStrAsBool t
  | t `elem` truthVals = Right True
  | t `elem` falseVals = Right False
  | otherwise = Left errMsg
  where
    truthVals = ["true", "t", "yes", "y"]
    falseVals = ["false", "f", "no", "n"]

    errMsg = " Not a valid boolean text. " ++ "True values are "
             ++ show truthVals ++ " and  False values are " ++ show falseVals
             ++ ". All values are case insensitive"

type ConfigM a = ReaderT Env (ExceptT String ParserM) a

runConfig :: Env -> ConfigM a -> Parser (Either String a)
runConfig env m = fromM $ runExceptT $ runReaderT m env

fromParser :: Parser a -> ConfigM a
fromParser = lift . lift . oneM

returnJust :: Monad m => a -> m (Maybe a)
returnJust = return . Just

withEnvOption :: FromConfig a
              => String -> Mod OptionFields a -> ConfigM (Maybe a)
withEnvOption envVar optFldsMod = do
  valM <- fromParser optParser
  maybe (considerEnv envVar) returnJust valM
  where
    optParser = optional $
                option (eitherReader fromConfig) optFldsMod

withEnvFlag :: String -> Mod FlagFields Bool -> ConfigM Bool
withEnvFlag envVar flagFldsMod = do
  boolVal <- fromParser flagParser
  bool considerEnv' (return True) boolVal
  where
    flagParser = switch flagFldsMod
    considerEnv' = fromMaybe False <$> considerEnv envVar

considerEnv :: FromConfig a => String -> ConfigM (Maybe a)
considerEnv envVar = do
  env <- ask
  let envValM = lookup envVar env
  case envValM of
    Nothing  -> return Nothing
    Just val -> either throwErr returnJust $ fromConfig val

  where
    throwErr s = throwError $
             "Fatal Error: Environment variable " ++ envVar ++ ": " ++ s

mkExamplesDoc :: [[String]] -> PP.Doc
mkExamplesDoc exampleLines =
  PP.text "Examples: " PP.<$> PP.indent 2 (PP.vsep examples)
  where
    examples = map PP.text $ intercalate [""] exampleLines

mkEnvVarDoc :: [(String, String)] -> PP.Doc
mkEnvVarDoc envVars =
  PP.text "Environment variables: " PP.<$>
  PP.indent 2 (PP.vsep $ map mkEnvVarLine envVars)
  where
    mkEnvVarLine (var, desc) =
      (PP.fillBreak 30 (PP.text var) PP.<+> prettifyDesc desc) <> PP.hardline
    prettifyDesc = PP.align . PP.fillSep . map PP.text . words

mainCmdFooter :: PP.Doc
mainCmdFooter =
  examplesDoc PP.<$> PP.text "" PP.<$> envVarDoc
  where
    examplesDoc = mkExamplesDoc examples
    examples =
      [
        [ "# Serve GraphQL Engine on default port (8080) with console disabled"
        , "graphql-engine --database-url <database-url> serve"
        ]
      , [ "# For more options, checkout"
        , "graphql-engine serve --help"
        ]
      ]

    envVarDoc = mkEnvVarDoc [databaseUrlEnv]

databaseUrlEnv :: (String, String)
databaseUrlEnv =
  ( "HASURA_GRAPHQL_DATABASE_URL"
  , "Postgres database URL. Example postgres://foo:bar@example.com:2345/database"
  )

serveCmdFooter :: PP.Doc
serveCmdFooter =
  examplesDoc PP.<$> PP.text "" PP.<$> envVarDoc
  where
    examplesDoc = mkExamplesDoc examples
    examples =
      [
        [ "# Start GraphQL Engine on default port (8080) with console enabled"
        , "graphql-engine --database-url <database-url> serve --enable-console"
        ]
      , [ "# Start GraphQL Engine on default port (8080) with console disabled"
        , "graphql-engine --database-url <database-url> serve"
        ]
      , [ "# Start GraphQL Engine on a different port (say 9090) with console disabled"
        , "graphql-engine --database-url <database-url> serve --server-port 9090"
        ]
      , [ "# Start GraphQL Engine with access key"
        , "graphql-engine --database-url <database-url> serve --access-key <secretaccesskey>"
        ]
      , [ "# Start GraphQL Engine with restrictive CORS policy (only allow https://example.com:8080)"
        , "graphql-engine --database-url <database-url> serve --cors-domain https://example.com:8080"
        ]
      , [ "# Start GraphQL Engine with Authentication Webhook (GET)"
        , "graphql-engine --database-url <database-url> serve --access-key <secretaccesskey>"
          <> " --auth-hook https://mywebhook.com/get"
        ]
      , [ "# Start GraphQL Engine with Authentication Webhook (POST)"
        , "graphql-engine --database-url <database-url> serve --access-key <secretaccesskey>"
          <> " --auth-hook https://mywebhook.com/post --auth-hook-mode POST"
        ]
      ]

    envVarDoc = mkEnvVarDoc $ envVars <> eventEnvs
    envVars =
      [ servePortEnv, pgStripesEnv, pgConnsEnv, pgTimeoutEnv
      , txIsoEnv, rootDirEnv, accessKeyEnv, authHookEnv , authHookTypeEnv
      , jwtSecretEnv , unAuthRoleEnv, corsDomainEnv , enableConsoleEnv
      ]

    eventEnvs =
      [ ( "HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE"
        , "Max event threads"
        )
      , ( "HASURA_GRAPHQL_EVENTS_FETCH_INTERVAL"
        , "Postgres events polling interval"
        )
      ]

servePortEnv :: (String, String)
servePortEnv =
  ( "HASURA_GRAPHQL_SERVER_PORT"
  , "Port on which graphql-engine should be served (default: 8080)"
  )

pgConnsEnv :: (String, String)
pgConnsEnv =
  ( "HASURA_GRAPHQL_PG_CONNECTIONS"
  , "Number of conns that need to be opened to Postgres (default: 50)"
  )

pgStripesEnv :: (String, String)
pgStripesEnv =
  ( "HASURA_GRAPHQL_PG_STRIPES"
  , "Number of conns that need to be opened to Postgres (default: 1)")

pgTimeoutEnv :: (String, String)
pgTimeoutEnv =
  ( "HASURA_GRAPHQL_PG_TIMEOUT"
  , "Each connection's idle time before it is closed (default: 180 sec)"
  )

txIsoEnv :: (String, String)
txIsoEnv =
  ( "HASURA_GRAPHQL_TX_ISOLATION"
  , "transaction isolation. read-committed / repeatable-read / serializable (default: read-commited)"
  )

rootDirEnv :: (String, String)
rootDirEnv =
  ( "HASURA_GRAPHQL_ROOT_DIR"
  , "this static dir is served at / and takes precedence over all routes")

accessKeyEnv :: (String, String)
accessKeyEnv =
  ( "HASURA_GRAPHQL_ACCESS_KEY"
  , "Secret access key, required to access this instance"
  )

authHookEnv :: (String, String)
authHookEnv =
  ( "HASURA_GRAPHQL_AUTH_HOOK"
  , "The authentication webhook, required to authenticate requests"
  )

authHookTypeEnv :: (String, String)
authHookTypeEnv =
  ( "HASURA_GRAPHQL_AUTH_HOOK_TYPE"
  , "The authentication webhook type (default: GET)"
  )

jwtSecretEnv :: (String, String)
jwtSecretEnv =
  ( "HASURA_GRAPHQL_JWT_SECRET"
  , jwtSecretHelp
  )

unAuthRoleEnv :: (String, String)
unAuthRoleEnv =
  ( "HASURA_GRAPHQL_UNAUTHORIZED_ROLE"
  , "Unauthorized role, used when access-key is not sent in access-key only mode "
                                 ++ "or \"Authorization\" header is absent in JWT mode"
  )

corsDomainEnv :: (String, String)
corsDomainEnv =
  ( "HASURA_GRAPHQL_CORS_DOMAIN"
  , "The domain, including scheme and port, to allow CORS for"
  )

enableConsoleEnv :: (String, String)
enableConsoleEnv =
  ( "HASURA_GRAPHQL_ENABLE_CONSOLE"
  , "Enable API Console"
  )

configRawConnInfo :: ConfigM RawConnInfo
configRawConnInfo =
  RawConnInfo <$> host <*> port <*> user <*> password
              <*> dbUrl <*> dbName <*> pure Nothing
  where
    host = fromParser $
      optional (strOption ( long "host" <>
                  metavar "HOST" <>
                  help "Postgres server host" ))

    port = fromParser $
      optional (option auto ( long "port" <>
                  short 'p' <>
                  metavar "PORT" <>
                  help "Postgres server port" ))

    user = fromParser $
      optional (strOption ( long "user" <>
                  short 'u' <>
                  metavar "USER" <>
                  help "Database user name" ))

    password = fromParser $
      strOption ( long "password" <>
                  metavar "PASSWORD" <>
                  value "" <>
                  help "Password of the user" )

    (dbUrlEnv, dbUrlHelp) = databaseUrlEnv
    dbUrl = withEnvOption dbUrlEnv
              ( long "database-url" <>
                metavar "DATABASE-URL" <>
                help dbUrlHelp
              )

    dbName = fromParser $
      optional (strOption ( long "dbname" <>
                  short 'd' <>
                  metavar "NAME" <>
                  help "Database name to connect to" )
               )

connInfoErrModifier :: String -> String
connInfoErrModifier s = "Fatal Error : " ++ s

mkConnInfo ::RawConnInfo -> Either String Q.ConnInfo
mkConnInfo (RawConnInfo mHost mPort mUser pass mURL mDB opts) =
  case (mHost, mPort, mUser, mDB, mURL) of

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

readIsoLevel :: String -> Either String Q.TxIsolation
readIsoLevel isoS =
  case isoS of
    "read-comitted" -> return Q.ReadCommitted
    "repeatable-read" -> return Q.RepeatableRead
    "serializable" -> return Q.ReadCommitted
    _ -> Left "Only expecting read-comitted / repeatable-read / serializable"

configTxIsolation :: ConfigM Q.TxIsolation
configTxIsolation = fromMaybe Q.ReadCommitted <$>
  withEnvOption envVar
    ( long "tx-iso" <>
      short 'i' <>
      metavar "TXISO" <>
      help helpDesc
    )
  where
    (envVar, helpDesc) = txIsoEnv

configRootDir :: ConfigM (Maybe String)
configRootDir = withEnvOption envVar
                ( long "root-dir" <>
                  metavar "STATIC-DIR" <>
                  help helpDesc
                )
  where
    (envVar, helpDesc) = rootDirEnv

configConnParams :: ConfigM Q.ConnParams
configConnParams =
  Q.ConnParams <$> stripes <*> conns <*> timeout
  where
    (stripesEnv, stripesHelp) = pgStripesEnv
    stripes = fromMaybe 1 <$> withEnvOption stripesEnv
              ( long "stripes" <>
                 short 's' <>
                 metavar "NO OF STRIPES" <>
                 help stripesHelp
              )

    (connEnv, connHelp) = pgConnsEnv
    conns = fromMaybe 50 <$> withEnvOption connEnv
            ( long "connections" <>
               short 'c' <>
               metavar "NO OF CONNS" <>
               help connHelp
            )

    (timeoutEnv, timeoutHelp) = pgTimeoutEnv
    timeout = fromMaybe 180 <$> withEnvOption timeoutEnv
              ( long "timeout" <>
                metavar "SECONDS" <>
                help timeoutHelp
              )

configServerPort :: ConfigM Int
configServerPort =
  fromMaybe 8080 <$> withEnvOption envVar
       ( long "server-port" <>
         metavar "PORT" <>
         help helpDesc
       )
  where
    (envVar, helpDesc) = servePortEnv

configAccessKey :: ConfigM (Maybe AccessKey)
configAccessKey =
  fmap AccessKey <$> withEnvOption envVar
                         ( long "access-key" <>
                           metavar "SECRET ACCESS KEY" <>
                           help helpDesc
                         )
  where
    (envVar, helpDesc) = accessKeyEnv

readHookType :: String -> Either String AuthHookType
readHookType tyS =
  case tyS of
    "GET"  -> Right AHTGet
    "POST" -> Right AHTPost
    _      -> Left "Only expecting GET / POST"

configWebHook :: ConfigM (Maybe AuthHook)
configWebHook =
  liftA2 mkAuthHook configUrl configEnablePost
  where
    mkAuthHook mUrl mTy = flip AuthHook (fromMaybe AHTGet mTy) <$> mUrl
    (urlEnvVar, urlHelp) = authHookEnv
    (urlTyEnvVar, urlTyHelp) = authHookTypeEnv
    configUrl = withEnvOption urlEnvVar
                  ( long "auth-hook" <>
                    metavar "AUTHENTICATION WEB HOOK" <>
                    help urlHelp
                  )
    configEnablePost = withEnvOption urlTyEnvVar
                  ( long "auth-hook-mode" <>
                    metavar "GET|POST" <>
                    help urlTyHelp
                  )


configJwtSecret :: ConfigM (Maybe Text)
configJwtSecret =
  withEnvOption envVar
             ( long "jwt-secret" <>
               metavar "JWK" <>
               help jwtSecretHelp
             )
  where
    envVar = fst jwtSecretEnv

jwtSecretHelp :: String
jwtSecretHelp = "The JSON containing type and the JWK used for verifying. e.g: "
              <> "`{\"type\": \"HS256\", \"key\": \"<your-hmac-shared-secret>\", \"claims_namespace\": \"<optional-custom-claims-key-name>\"}`,"
              <> "`{\"type\": \"RS256\", \"key\": \"<your-PEM-RSA-public-key>\", \"claims_namespace\": \"<optional-custom-claims-key-name>\"}`"

configUnAuthRole :: ConfigM (Maybe RoleName)
configUnAuthRole =
  fmap RoleName <$> withEnvOption envVar
                        ( long "unauthorized-role" <>
                          metavar "UNAUTHORIZED ROLE" <>
                          help helpDesc
                        )
  where
    (envVar, helpDesc) = unAuthRoleEnv

configCorsConfig :: ConfigM CorsConfig
configCorsConfig = do
  corsDomain <- fromMaybe "*" <$> corsDomainConfig
  CorsConfig corsDomain <$> disableCors
  where
    (corsDomainEnvVar, corsDomainHelp) = corsDomainEnv
    corsDomainConfig = withEnvOption corsDomainEnvVar
                       ( long "cors-domain" <>
                         metavar "CORS DOMAIN" <>
                         help corsDomainHelp
                       )
    disableCors = fromParser $
      switch ( long "disable-cors" <>
               help "Disable CORS handling"
             )

configEnableConsole :: ConfigM Bool
configEnableConsole =
  withEnvFlag envVar
             ( long "enable-console" <>
               help helpDesc
             )
  where
    (envVar, helpDesc) = enableConsoleEnv
