module Hasura.Server.Init where

import qualified Database.PG.Query            as Q

import           Options.Applicative
import           System.Exit                  (exitFailure)

import qualified Data.Aeson                   as J
import qualified Data.Text                    as T
import qualified Hasura.Logging               as L
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Data.String                  as DataString
import           Hasura.Prelude
import           Hasura.RQL.DDL.Utils
import           Hasura.RQL.Types             (RoleName (..))
import           Hasura.Server.Auth
import           Hasura.Server.Logging
import           Hasura.Server.Utils
import           Network.Wai.Handler.Warp


initErrExit :: (Show e) => e -> IO a
initErrExit e = print e >> exitFailure

-- clear the hdb_views schema
initStateTx :: Q.Tx ()
initStateTx = clearHdbViews

data RawConnParams
  = RawConnParams
  { rcpStripes      :: !(Maybe Int)
  , rcpConns        :: !(Maybe Int)
  , rcpIdleTime     :: !(Maybe Int)
  , rcpAllowPrepare :: !(Maybe Bool)
  } deriving (Show, Eq)

type RawAuthHook = AuthHookG (Maybe T.Text) (Maybe AuthHookType)

data RawServeOptions
  = RawServeOptions
  { rsoPort          :: !(Maybe Int)
  , rsoHost          :: !(Maybe HostPreference)
  , rsoConnParams    :: !RawConnParams
  , rsoTxIso         :: !(Maybe Q.TxIsolation)
  , rsoAccessKey     :: !(Maybe AccessKey)
  , rsoAuthHook      :: !RawAuthHook
  , rsoJwtSecret     :: !(Maybe Text)
  , rsoUnAuthRole    :: !(Maybe RoleName)
  , rsoCorsConfig    :: !RawCorsConfig
  , rsoEnableConsole :: !Bool
  } deriving (Show, Eq)

data CorsConfigG a
  = CorsConfigG
  { ccDomain   :: !a
  , ccDisabled :: !Bool
  } deriving (Show, Eq)

type RawCorsConfig = CorsConfigG (Maybe T.Text)
type CorsConfig = CorsConfigG T.Text

data ServeOptions
  = ServeOptions
  { soPort          :: !Int
  , soHost          :: !HostPreference
  , soConnParams    :: !Q.ConnParams
  , soTxIso         :: !Q.TxIsolation
  , soAccessKey     :: !(Maybe AccessKey)
  , soAuthHook      :: !(Maybe AuthHook)
  , soJwtSecret     :: !(Maybe Text)
  , soUnAuthRole    :: !(Maybe RoleName)
  , soCorsConfig    :: !CorsConfig
  , soEnableConsole :: !Bool
  } deriving (Show, Eq)

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

data HGECommandG a
  = HCServe !a
  | HCExport
  | HCClean
  | HCExecute
  | HCVersion
  deriving (Show, Eq)

type HGECommand = HGECommandG ServeOptions
type RawHGECommand = HGECommandG RawServeOptions

data HGEOptionsG a
  = HGEOptionsG
  { hoConnInfo :: !RawConnInfo
  , hoCommand  :: !(HGECommandG a)
  } deriving (Show, Eq)

type RawHGEOptions = HGEOptionsG RawServeOptions
type HGEOptions = HGEOptionsG ServeOptions

type Env = [(String, String)]

class FromEnv a where
  fromEnv :: String -> Either String a

instance FromEnv String where
  fromEnv = Right

instance FromEnv HostPreference where
  fromEnv = Right . DataString.fromString

instance FromEnv Text where
  fromEnv = Right . T.pack

instance FromEnv AuthHookType where
  fromEnv = readHookType

instance FromEnv Int where
  fromEnv = maybe (Left "Expecting Int value") Right . readMaybe

instance FromEnv AccessKey where
  fromEnv = Right . AccessKey . T.pack

instance FromEnv RoleName where
  fromEnv = Right . RoleName . T.pack

instance FromEnv Bool where
  fromEnv = parseStrAsBool

instance FromEnv Q.TxIsolation where
  fromEnv = readIsoLevel

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

readIsoLevel :: String -> Either String Q.TxIsolation
readIsoLevel isoS =
  case isoS of
    "read-comitted" -> return Q.ReadCommitted
    "repeatable-read" -> return Q.RepeatableRead
    "serializable" -> return Q.ReadCommitted
    _ -> Left "Only expecting read-comitted / repeatable-read / serializable"

type WithEnv a = ReaderT Env (ExceptT String Identity) a

runWithEnv :: Env -> WithEnv a -> Either String a
runWithEnv env m = runIdentity $ runExceptT $ runReaderT m env

returnJust :: Monad m => a -> m (Maybe a)
returnJust = return . Just

considerEnv :: FromEnv a => String -> WithEnv (Maybe a)
considerEnv envVar = do
  env <- ask
  case lookup envVar env of
    Nothing  -> return Nothing
    Just val -> either throwErr returnJust $ fromEnv val
  where
    throwErr s = throwError $
      "Fatal Error:- Environment variable " ++ envVar ++ ": " ++ s

withEnv :: FromEnv a => Maybe a -> String -> WithEnv (Maybe a)
withEnv mVal envVar =
  maybe (considerEnv envVar) returnJust mVal

withEnvBool :: Bool -> String -> WithEnv Bool
withEnvBool bVal envVar =
  bool considerEnv' (return True) bVal
  where
    considerEnv' = do
      mEnvVal <- considerEnv envVar
      maybe (return False) return mEnvVal

mkHGEOptions :: RawHGEOptions -> WithEnv HGEOptions
mkHGEOptions (HGEOptionsG rawConnInfo rawCmd) =
  HGEOptionsG <$> connInfo <*> cmd
  where
    connInfo = mkRawConnInfo rawConnInfo
    cmd = case rawCmd of
      HCServe rso -> HCServe <$> mkServeOptions rso
      HCExport    -> return HCExport
      HCClean     -> return HCClean
      HCExecute   -> return HCExecute
      HCVersion   -> return HCVersion

mkRawConnInfo :: RawConnInfo -> WithEnv RawConnInfo
mkRawConnInfo rawConnInfo = do
  withEnvUrl <- withEnv rawDBUrl $ fst databaseUrlEnv
  return $ rawConnInfo {connUrl = withEnvUrl}
  where
    rawDBUrl = connUrl rawConnInfo

mkServeOptions :: RawServeOptions -> WithEnv ServeOptions
mkServeOptions rso = do
  port <- fromMaybe 8080 <$>
          withEnv (rsoPort rso) (fst servePortEnv)
  host <- fromMaybe "*" <$>
          withEnv (rsoHost rso) (fst serveHostEnv)

  connParams <- mkConnParams $ rsoConnParams rso
  txIso <- fromMaybe Q.ReadCommitted <$>
           withEnv (rsoTxIso rso) (fst txIsoEnv)
  accKey <- withEnv (rsoAccessKey rso) $ fst accessKeyEnv
  authHook <- mkAuthHook $ rsoAuthHook rso
  jwtSecr <- withEnv (rsoJwtSecret rso) $ fst jwtSecretEnv
  unAuthRole <- withEnv (rsoUnAuthRole rso) $ fst unAuthRoleEnv
  corsCfg <- mkCorsConfig $ rsoCorsConfig rso
  enableConsole <- withEnvBool (rsoEnableConsole rso) $
                   fst enableConsoleEnv
  return $ ServeOptions port host connParams txIso accKey authHook
                        jwtSecr unAuthRole corsCfg enableConsole
  where
    mkConnParams (RawConnParams s c i p) = do
      stripes <- fromMaybe 1 <$> withEnv s (fst pgStripesEnv)
      conns <- fromMaybe 50 <$> withEnv c (fst pgConnsEnv)
      iTime <- fromMaybe 180 <$> withEnv i (fst pgTimeoutEnv)
      allowPrepare <- fromMaybe True <$> withEnv p (fst pgUsePrepareEnv)
      return $ Q.ConnParams stripes conns iTime allowPrepare

    mkAuthHook (AuthHookG mUrl mType) = do
      mUrlEnv <- withEnv mUrl $ fst authHookEnv
      authModeM <- withEnv mType (fst authHookModeEnv)
      ty <- maybe (authHookTyEnv mType) return authModeM
      return (flip AuthHookG ty <$> mUrlEnv)

    -- Also support HASURA_GRAPHQL_AUTH_HOOK_TYPE
    -- TODO:- drop this in next major update
    authHookTyEnv mType = fromMaybe AHTGet <$>
      withEnv mType "HASURA_GRAPHQL_AUTH_HOOK_TYPE"

    mkCorsConfig (CorsConfigG mDom isDis) = do
      domEnv <- fromMaybe "*" <$> withEnv mDom (fst corsDomainEnv)
      return $ CorsConfigG domEnv isDis

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
      [ servePortEnv, serveHostEnv, pgStripesEnv, pgConnsEnv, pgTimeoutEnv
      , txIsoEnv, accessKeyEnv, authHookEnv , authHookModeEnv
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

serveHostEnv :: (String, String)
serveHostEnv =
  ( "HASURA_GRAPHQL_SERVER_HOST"
  , "Host on which graphql-engine will listen (default: *)"
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

pgUsePrepareEnv :: (String, String)
pgUsePrepareEnv =
  ( "HASURA_GRAPHQL_USE_PREPARED_STATEMENTS"
  , "Use prepared statements for queries (default: True)"
  )

txIsoEnv :: (String, String)
txIsoEnv =
  ( "HASURA_GRAPHQL_TX_ISOLATION"
  , "transaction isolation. read-committed / repeatable-read / serializable (default: read-commited)"
  )

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

authHookModeEnv :: (String, String)
authHookModeEnv =
  ( "HASURA_GRAPHQL_AUTH_HOOK_MODE"
  , "The authentication webhook mode (default: GET)"
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

parseRawConnInfo :: Parser RawConnInfo
parseRawConnInfo =
  RawConnInfo <$> host <*> port <*> user <*> password
              <*> dbUrl <*> dbName <*> pure Nothing
  where
    host = optional $
      strOption ( long "host" <>
                  metavar "HOST" <>
                  help "Postgres server host" )

    port = optional $
      option auto ( long "port" <>
                  short 'p' <>
                  metavar "PORT" <>
                  help "Postgres server port" )

    user = optional $
      strOption ( long "user" <>
                  short 'u' <>
                  metavar "USER" <>
                  help "Database user name" )

    password =
      strOption ( long "password" <>
                  metavar "PASSWORD" <>
                  value "" <>
                  help "Password of the user"
                )

    dbUrl = optional $
      strOption
                ( long "database-url" <>
                  metavar "DATABASE-URL" <>
                  help (snd databaseUrlEnv)
                )

    dbName = optional $
      strOption ( long "dbname" <>
                  short 'd' <>
                  metavar "NAME" <>
                  help "Database name to connect to"
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

parseTxIsolation :: Parser (Maybe Q.TxIsolation)
parseTxIsolation = optional $
  option (eitherReader readIsoLevel)
           ( long "tx-iso" <>
             short 'i' <>
             metavar "TXISO" <>
             help (snd txIsoEnv)
           )

parseConnParams :: Parser RawConnParams
parseConnParams =
  RawConnParams <$> stripes <*> conns <*> timeout <*> allowPrepare
  where
    stripes = optional $
      option auto
              ( long "stripes" <>
                 short 's' <>
                 metavar "NO OF STRIPES" <>
                 help (snd pgStripesEnv)
              )

    conns = optional $
      option auto
            ( long "connections" <>
               short 'c' <>
               metavar "NO OF CONNS" <>
               help (snd pgConnsEnv)
            )

    timeout = optional $
      option auto
              ( long "timeout" <>
                metavar "SECONDS" <>
                help (snd pgTimeoutEnv)
              )
    allowPrepare = optional $
      option (eitherReader parseStrAsBool)
              ( long "use-prepared-statements" <>
                metavar "USE PREPARED STATEMENTS" <>
                help (snd pgUsePrepareEnv)
              )

parseServerPort :: Parser (Maybe Int)
parseServerPort = optional $
  option auto
       ( long "server-port" <>
         metavar "PORT" <>
         help (snd servePortEnv)
       )

parseServerHost :: Parser (Maybe HostPreference)
parseServerHost = optional $ strOption ( long "server-host" <>
                metavar "HOST" <>
                help "Host on which graphql-engine will listen (default: *)"
              )

parseAccessKey :: Parser (Maybe AccessKey)
parseAccessKey =
  optional $ AccessKey <$>
    strOption ( long "access-key" <>
                metavar "SECRET ACCESS KEY" <>
                help (snd accessKeyEnv)
              )

readHookType :: String -> Either String AuthHookType
readHookType tyS =
  case tyS of
    "GET"  -> Right AHTGet
    "POST" -> Right AHTPost
    _      -> Left "Only expecting GET / POST"

parseWebHook :: Parser RawAuthHook
parseWebHook =
  AuthHookG <$> url <*> urlType
  where
    url = optional $
      strOption ( long "auth-hook" <>
                  metavar "AUTHENTICATION WEB HOOK" <>
                  help (snd authHookEnv)
                )
    urlType = optional $
      option (eitherReader readHookType)
                  ( long "auth-hook-mode" <>
                    metavar "GET|POST" <>
                    help (snd authHookModeEnv)
                  )


parseJwtSecret :: Parser (Maybe Text)
parseJwtSecret =
  optional $ strOption
             ( long "jwt-secret" <>
               metavar "JWK" <>
               help (snd jwtSecretEnv)
             )

jwtSecretHelp :: String
jwtSecretHelp = "The JSON containing type and the JWK used for verifying. e.g: "
              <> "`{\"type\": \"HS256\", \"key\": \"<your-hmac-shared-secret>\", \"claims_namespace\": \"<optional-custom-claims-key-name>\"}`,"
              <> "`{\"type\": \"RS256\", \"key\": \"<your-PEM-RSA-public-key>\", \"claims_namespace\": \"<optional-custom-claims-key-name>\"}`"

parseUnAuthRole :: Parser (Maybe RoleName)
parseUnAuthRole = optional $
  RoleName <$> strOption ( long "unauthorized-role" <>
                          metavar "UNAUTHORIZED ROLE" <>
                          help (snd unAuthRoleEnv)
                        )

parseCorsConfig :: Parser RawCorsConfig
parseCorsConfig =
  CorsConfigG <$> corsDomain <*> disableCors
  where
    corsDomain =
      optional (strOption
                 ( long "cors-domain" <>
                   metavar "CORS DOMAIN" <>
                   help (snd corsDomainEnv)
                 )
               )
    disableCors =
      switch ( long "disable-cors" <>
               help "Disable CORS handling"
             )

parseEnableConsole :: Parser Bool
parseEnableConsole =
  switch ( long "enable-console" <>
           help (snd enableConsoleEnv)
         )

-- Init logging related
connInfoToLog :: Q.ConnInfo -> StartupLog
connInfoToLog (Q.ConnInfo host port user _ db _) =
  StartupLog L.LevelInfo "postgres_connection" infoVal
  where
    infoVal = J.object [ "host" J..= host
                       , "port" J..= port
                       , "user" J..= user
                       , "database" J..= db
                       ]

serveOptsToLog :: ServeOptions -> StartupLog
serveOptsToLog so =
  StartupLog L.LevelInfo "serve_options" infoVal
  where
    infoVal = J.object [ "port" J..= soPort so
                       , "accesskey_set" J..= isJust (soAccessKey so)
                       , "auth_hook" J..= (ahUrl <$> soAuthHook so)
                       , "auth_hook_mode" J..= (show . ahType <$> soAuthHook so)
                       , "unauth_role" J..= soUnAuthRole so
                       , "cors_domain" J..= (ccDomain . soCorsConfig) so
                       , "cors_disabled" J..= (ccDisabled . soCorsConfig) so
                       , "enable_console" J..= soEnableConsole so
                       , "use_prepared_statements" J..= (Q.cpAllowPrepare . soConnParams) so
                       ]

mkGenericStrLog :: T.Text -> String -> StartupLog
mkGenericStrLog k msg =
  StartupLog L.LevelInfo k $ J.toJSON msg
