{-# LANGUAGE CPP #-}
module Hasura.Server.Init where

import qualified Database.PG.Query                as Q

import           Data.Char                        (toLower)
import           Network.Wai.Handler.Warp         (HostPreference)
import           Options.Applicative

import qualified Data.Aeson                       as J
import qualified Data.Aeson.Casing                as J
import qualified Data.Aeson.TH                    as J
import qualified Data.ByteString.Lazy.Char8       as BLC
import qualified Data.HashSet                     as Set
import qualified Data.String                      as DataString
import qualified Data.Text                        as T
import qualified Hasura.GraphQL.Execute.LiveQuery as LQ
import qualified Hasura.Logging                   as L
import qualified Text.PrettyPrint.ANSI.Leijen     as PP

import           Hasura.Prelude
import           Hasura.RQL.Types                 (RoleName (..),
                                                   SchemaCache (..),
                                                   mkNonEmptyText)
import           Hasura.Server.Auth
import           Hasura.Server.Cors
import           Hasura.Server.Logging
import           Hasura.Server.Utils

newtype InstanceId
  = InstanceId { getInstanceId :: Text }
  deriving (Show, Eq, J.ToJSON, J.FromJSON, Q.FromCol, Q.ToPrepArg)

generateInstanceId :: IO InstanceId
generateInstanceId = InstanceId <$> generateFingerprint

data StartupTimeInfo
  = StartupTimeInfo
  { _stiMessage   :: !Text
  , _stiTimeTaken :: !Double
  }
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''StartupTimeInfo)

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
  { rsoPort               :: !(Maybe Int)
  , rsoHost               :: !(Maybe HostPreference)
  , rsoConnParams         :: !RawConnParams
  , rsoTxIso              :: !(Maybe Q.TxIsolation)
  , rsoAdminSecret        :: !(Maybe AdminSecret)
  , rsoAuthHook           :: !RawAuthHook
  , rsoJwtSecret          :: !(Maybe JWTConfig)
  , rsoUnAuthRole         :: !(Maybe RoleName)
  , rsoCorsConfig         :: !(Maybe CorsConfig)
  , rsoEnableConsole      :: !Bool
  , rsoConsoleAssetsDir   :: !(Maybe Text)
  , rsoEnableTelemetry    :: !(Maybe Bool)
  , rsoWsReadCookie       :: !Bool
  , rsoStringifyNum       :: !Bool
  , rsoEnabledAPIs        :: !(Maybe [API])
  , rsoMxRefetchInt       :: !(Maybe LQ.RefetchInterval)
  , rsoMxBatchSize        :: !(Maybe LQ.BatchSize)
  , rsoFallbackRefetchInt :: !(Maybe LQ.RefetchInterval)
  , rsoEnableAllowlist    :: !Bool
  , rsoEnabledLogTypes    :: !(Maybe [L.EngineLogType])
  , rsoLogLevel           :: !(Maybe L.LogLevel)
  } deriving (Show, Eq)

data ServeOptions
  = ServeOptions
  { soPort             :: !Int
  , soHost             :: !HostPreference
  , soConnParams       :: !Q.ConnParams
  , soTxIso            :: !Q.TxIsolation
  , soAdminSecret      :: !(Maybe AdminSecret)
  , soAuthHook         :: !(Maybe AuthHook)
  , soJwtSecret        :: !(Maybe JWTConfig)
  , soUnAuthRole       :: !(Maybe RoleName)
  , soCorsConfig       :: !CorsConfig
  , soEnableConsole    :: !Bool
  , soConsoleAssetsDir :: !(Maybe Text)
  , soEnableTelemetry  :: !Bool
  , soStringifyNum     :: !Bool
  , soEnabledAPIs      :: !(Set.HashSet API)
  , soLiveQueryOpts    :: !LQ.LQOpts
  , soEnableAllowlist  :: !Bool
  , soEnabledLogTypes  :: !(Set.HashSet L.EngineLogType)
  , soLogLevel         :: !L.LogLevel
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
  , connRetries  :: !(Maybe Int)
  } deriving (Eq, Read, Show)

data HGECommandG a
  = HCServe !a
  | HCExport
  | HCClean
  | HCExecute
  | HCVersion
  deriving (Show, Eq)

data API
  = METADATA
  | GRAPHQL
  | PGDUMP
  | DEVELOPER
  | CONFIG
  deriving (Show, Eq, Read, Generic)
$(J.deriveJSON (J.defaultOptions { J.constructorTagModifier = map toLower })
  ''API)

instance Hashable API

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

instance FromEnv AdminSecret where
  fromEnv = Right . AdminSecret . T.pack

instance FromEnv RoleName where
  fromEnv string = case mkNonEmptyText (T.pack string) of
    Nothing     -> Left "empty string not allowed"
    Just neText -> Right $ RoleName neText

instance FromEnv Bool where
  fromEnv = parseStrAsBool

instance FromEnv Q.TxIsolation where
  fromEnv = readIsoLevel

instance FromEnv CorsConfig where
  fromEnv = readCorsDomains

instance FromEnv [API] where
  fromEnv = readAPIs

instance FromEnv LQ.BatchSize where
  fromEnv = fmap LQ.mkBatchSize . readEither

instance FromEnv LQ.RefetchInterval where
  fromEnv = fmap LQ.refetchIntervalFromMilli . readEither

instance FromEnv JWTConfig where
  fromEnv = readJson

instance FromEnv [L.EngineLogType] where
  fromEnv = readLogTypes

instance FromEnv L.LogLevel where
  fromEnv = readLogLevel

parseStrAsBool :: String -> Either String Bool
parseStrAsBool t
  | map toLower t `elem` truthVals = Right True
  | map toLower t `elem` falseVals = Right False
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
    "read-committed" -> return Q.ReadCommitted
    "repeatable-read" -> return Q.RepeatableRead
    "serializable" -> return Q.Serializable
    _ -> Left "Only expecting read-committed / repeatable-read / serializable"

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

considerEnvs :: FromEnv a => [String] -> WithEnv (Maybe a)
considerEnvs envVars = foldl1 (<|>) <$> mapM considerEnv envVars

withEnv :: FromEnv a => Maybe a -> String -> WithEnv (Maybe a)
withEnv mVal envVar =
  maybe (considerEnv envVar) returnJust mVal

withEnvs :: FromEnv a => Maybe a -> [String] -> WithEnv (Maybe a)
withEnvs mVal envVars =
  maybe (considerEnvs envVars) returnJust mVal

withEnvBool :: Bool -> String -> WithEnv Bool
withEnvBool bVal envVar =
  bool considerEnv' (return True) bVal
  where
    considerEnv' = do
      mEnvVal <- considerEnv envVar
      maybe (return False) return mEnvVal

withEnvJwtConf :: Maybe JWTConfig -> String -> WithEnv (Maybe JWTConfig)
withEnvJwtConf jVal envVar =
  maybe (considerEnv envVar) returnJust jVal

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
  withEnvRetries <- withEnv retries $ fst retriesNumEnv
  return $ rawConnInfo { connUrl = withEnvUrl
                       , connRetries = withEnvRetries
                       }
  where
    rawDBUrl = connUrl rawConnInfo
    retries = connRetries rawConnInfo

mkServeOptions :: RawServeOptions -> WithEnv ServeOptions
mkServeOptions rso = do
  port <- fromMaybe 8080 <$>
          withEnv (rsoPort rso) (fst servePortEnv)
  host <- fromMaybe "*" <$>
          withEnv (rsoHost rso) (fst serveHostEnv)

  connParams <- mkConnParams $ rsoConnParams rso
  txIso <- fromMaybe Q.ReadCommitted <$> withEnv (rsoTxIso rso) (fst txIsoEnv)
  adminScrt <- withEnvs (rsoAdminSecret rso) $ map fst [adminSecretEnv, accessKeyEnv]
  authHook <- mkAuthHook $ rsoAuthHook rso
  jwtSecret <- withEnvJwtConf (rsoJwtSecret rso) $ fst jwtSecretEnv
  unAuthRole <- withEnv (rsoUnAuthRole rso) $ fst unAuthRoleEnv
  corsCfg <- mkCorsConfig $ rsoCorsConfig rso
  enableConsole <- withEnvBool (rsoEnableConsole rso) $
                   fst enableConsoleEnv
  consoleAssetsDir <- withEnv (rsoConsoleAssetsDir rso) (fst consoleAssetsDirEnv)
  enableTelemetry <- fromMaybe True <$>
                     withEnv (rsoEnableTelemetry rso) (fst enableTelemetryEnv)
  strfyNum <- withEnvBool (rsoStringifyNum rso) $ fst stringifyNumEnv
  enabledAPIs <- Set.fromList . fromMaybe defaultAPIs <$>
                     withEnv (rsoEnabledAPIs rso) (fst enabledAPIsEnv)
  lqOpts <- mkLQOpts
  enableAL <- withEnvBool (rsoEnableAllowlist rso) $ fst enableAllowlistEnv
  enabledLogs <- Set.fromList . fromMaybe (Set.toList L.defaultEnabledLogTypes) <$>
                 withEnv (rsoEnabledLogTypes rso) (fst enabledLogsEnv)
  serverLogLevel <- fromMaybe L.LevelInfo <$> withEnv (rsoLogLevel rso) (fst logLevelEnv)
  return $ ServeOptions port host connParams txIso adminScrt authHook jwtSecret
                        unAuthRole corsCfg enableConsole consoleAssetsDir
                        enableTelemetry strfyNum enabledAPIs lqOpts enableAL
                        enabledLogs serverLogLevel
  where
#ifdef DeveloperAPIs
    defaultAPIs = [METADATA,GRAPHQL,PGDUMP,CONFIG,DEVELOPER]
#else
    defaultAPIs = [METADATA,GRAPHQL,PGDUMP,CONFIG]
#endif
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

    mkCorsConfig mCfg = do
      corsCfg <- fromMaybe CCAllowAll <$> withEnv mCfg (fst corsDomainEnv)
      readCookVal <- withEnvBool (rsoWsReadCookie rso) (fst wsReadCookieEnv)
      wsReadCookie <- case (isCorsDisabled corsCfg, readCookVal) of
        (True, _)      -> return readCookVal
        (False, True)  -> throwError $ fst wsReadCookieEnv
                          <> " can only be used when CORS is disabled"
        (False, False) -> return False
      return $ case corsCfg of
        CCDisabled _ -> CCDisabled wsReadCookie
        _            -> corsCfg

    mkLQOpts = do
      mxRefetchIntM <- withEnv (rsoMxRefetchInt rso) $
                       fst mxRefetchDelayEnv
      mxBatchSizeM <- withEnv (rsoMxBatchSize rso) $
                      fst mxBatchSizeEnv
      fallbackRefetchIntM <- withEnv (rsoFallbackRefetchInt rso) $
                             fst fallbackRefetchDelayEnv
      return $ LQ.mkLQOpts (LQ.mkMxOpts mxBatchSizeM mxRefetchIntM)
        (LQ.mkFallbackOpts fallbackRefetchIntM)


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
      (PP.fillBreak 40 (PP.text var) PP.<+> prettifyDesc desc) <> PP.hardline
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

    envVarDoc = mkEnvVarDoc [databaseUrlEnv, retriesNumEnv]

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
      , [ "# Start GraphQL Engine with admin secret key"
        , "graphql-engine --database-url <database-url> serve --admin-secret <adminsecretkey>"
        ]
      , [ "# Start GraphQL Engine with restrictive CORS policy (only allow https://example.com:8080)"
        , "graphql-engine --database-url <database-url> serve --cors-domain https://example.com:8080"
        ]
      , [ "# Start GraphQL Engine with multiple domains for CORS (https://example.com, http://localhost:3000 and https://*.foo.bar.com)"
        , "graphql-engine --database-url <database-url> serve --cors-domain \"https://example.com, https://*.foo.bar.com, http://localhost:3000\""
        ]
      , [ "# Start GraphQL Engine with Authentication Webhook (GET)"
        , "graphql-engine --database-url <database-url> serve --admin-secret <adminsecretkey>"
          <> " --auth-hook https://mywebhook.com/get"
        ]
      , [ "# Start GraphQL Engine with Authentication Webhook (POST)"
        , "graphql-engine --database-url <database-url> serve --admin-secret <adminsecretkey>"
          <> " --auth-hook https://mywebhook.com/post --auth-hook-mode POST"
        ]
      , [ "# Start GraphQL Engine with telemetry enabled/disabled"
        , "graphql-engine --database-url <database-url> serve --enable-telemetry true|false"
        ]
      ]

    envVarDoc = mkEnvVarDoc $ envVars <> eventEnvs
    envVars =
      [ databaseUrlEnv, retriesNumEnv, servePortEnv, serveHostEnv
      , pgStripesEnv, pgConnsEnv, pgTimeoutEnv, pgUsePrepareEnv, txIsoEnv
      , adminSecretEnv , accessKeyEnv, authHookEnv, authHookModeEnv
      , jwtSecretEnv, unAuthRoleEnv, corsDomainEnv, enableConsoleEnv
      , enableTelemetryEnv, wsReadCookieEnv, stringifyNumEnv, enabledAPIsEnv
      , enableAllowlistEnv, enabledLogsEnv, logLevelEnv
      ]

    eventEnvs =
      [ ( "HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE"
        , "Max event threads"
        )
      , ( "HASURA_GRAPHQL_EVENTS_FETCH_INTERVAL"
        , "Postgres events polling interval"
        )
      ]

retriesNumEnv :: (String, String)
retriesNumEnv =
  ( "HASURA_GRAPHQL_NO_OF_RETRIES"
  , "No.of retries if Postgres connection error occurs (default: 1)"
  )

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
  , "Number of connections per stripe that need to be opened to Postgres (default: 50)"
  )

pgStripesEnv :: (String, String)
pgStripesEnv =
  ( "HASURA_GRAPHQL_PG_STRIPES"
  , "Number of stripes (distinct sub-pools) to maintain with Postgres (default: 1)"
  )

pgTimeoutEnv :: (String, String)
pgTimeoutEnv =
  ( "HASURA_GRAPHQL_PG_TIMEOUT"
  , "Each connection's idle time before it is closed (default: 180 sec)"
  )

pgUsePrepareEnv :: (String, String)
pgUsePrepareEnv =
  ( "HASURA_GRAPHQL_USE_PREPARED_STATEMENTS"
  , "Use prepared statements for queries (default: true)"
  )

txIsoEnv :: (String, String)
txIsoEnv =
  ( "HASURA_GRAPHQL_TX_ISOLATION"
  , "transaction isolation. read-committed / repeatable-read / serializable (default: read-commited)"
  )

accessKeyEnv :: (String, String)
accessKeyEnv =
  ( "HASURA_GRAPHQL_ACCESS_KEY"
  , "Admin secret key, required to access this instance (deprecated: use HASURA_GRAPHQL_ADMIN_SECRET instead)"
  )

adminSecretEnv :: (String, String)
adminSecretEnv =
  ( "HASURA_GRAPHQL_ADMIN_SECRET"
  , "Admin Secret key, required to access this instance"
  )

authHookEnv :: (String, String)
authHookEnv =
  ( "HASURA_GRAPHQL_AUTH_HOOK"
  , "URL of the authorization webhook required to authorize requests"
  )

authHookModeEnv :: (String, String)
authHookModeEnv =
  ( "HASURA_GRAPHQL_AUTH_HOOK_MODE"
  , "HTTP method to use for authorization webhook (default: GET)"
  )

jwtSecretEnv :: (String, String)
jwtSecretEnv =
  ( "HASURA_GRAPHQL_JWT_SECRET"
  , jwtSecretHelp
  )

unAuthRoleEnv :: (String, String)
unAuthRoleEnv =
  ( "HASURA_GRAPHQL_UNAUTHORIZED_ROLE"
  , "Unauthorized role, used when admin-secret is not sent in admin-secret only mode "
                                 ++ "or \"Authorization\" header is absent in JWT mode"
  )

corsDomainEnv :: (String, String)
corsDomainEnv =
  ( "HASURA_GRAPHQL_CORS_DOMAIN"
  , "CSV of list of domains, excluding scheme (http/https) and including  port, "
    ++ "to allow CORS for. Wildcard domains are allowed. See docs for details."
  )

enableConsoleEnv :: (String, String)
enableConsoleEnv =
  ( "HASURA_GRAPHQL_ENABLE_CONSOLE"
  , "Enable API Console"
  )

enableTelemetryEnv :: (String, String)
enableTelemetryEnv =
  ( "HASURA_GRAPHQL_ENABLE_TELEMETRY"
  -- TODO: better description
  , "Enable anonymous telemetry (default: true)"
  )

wsReadCookieEnv :: (String, String)
wsReadCookieEnv =
  ( "HASURA_GRAPHQL_WS_READ_COOKIE"
  , "Read cookie on WebSocket initial handshake, even when CORS is disabled."
  ++ " This can be a potential security flaw! Please make sure you know "
  ++ "what you're doing."
  ++ "This configuration is only applicable when CORS is disabled."
  )

stringifyNumEnv :: (String, String)
stringifyNumEnv =
  ( "HASURA_GRAPHQL_STRINGIFY_NUMERIC_TYPES"
  , "Stringify numeric types (default: false)"
  )

enabledAPIsEnv :: (String, String)
enabledAPIsEnv =
  ( "HASURA_GRAPHQL_ENABLED_APIS"
  , "Comma separated list of enabled APIs. (default: metadata,graphql,pgdump,config)"
  )

consoleAssetsDirEnv :: (String, String)
consoleAssetsDirEnv =
  ( "HASURA_GRAPHQL_CONSOLE_ASSETS_DIR"
  , "A directory from which static assets required for console is served at"
  ++ "'/console/assets' path. Can be set to '/srv/console-assets' on the"
  ++ " default docker image to disable loading assets from CDN."
  )

enabledLogsEnv :: (String, String)
enabledLogsEnv =
  ( "HASURA_GRAPHQL_ENABLED_LOG_TYPES"
  , "Comma separated list of enabled log types "
    <> "(default: startup,http-log,webhook-log,websocket-log)"
    <> "(all: startup,http-log,webhook-log,websocket-log,query-log)"
  )

logLevelEnv :: (String, String)
logLevelEnv =
  ( "HASURA_GRAPHQL_LOG_LEVEL"
  , "Server log level (default: info) (all: error, warn, info, debug)"
  )

parseRawConnInfo :: Parser RawConnInfo
parseRawConnInfo =
  RawConnInfo <$> host <*> port <*> user <*> password
              <*> dbUrl <*> dbName <*> pure Nothing
              <*> retries
  where
    host = optional $
      strOption ( long "host" <>
                  metavar "<HOST>" <>
                  help "Postgres server host" )

    port = optional $
      option auto ( long "port" <>
                  short 'p' <>
                  metavar "<PORT>" <>
                  help "Postgres server port" )

    user = optional $
      strOption ( long "user" <>
                  short 'u' <>
                  metavar "<USER>" <>
                  help "Database user name" )

    password =
      strOption ( long "password" <>
                  metavar "<PASSWORD>" <>
                  value "" <>
                  help "Password of the user"
                )

    dbUrl = optional $
      strOption
                ( long "database-url" <>
                  metavar "<DATABASE-URL>" <>
                  help (snd databaseUrlEnv)
                )

    dbName = optional $
      strOption ( long "dbname" <>
                  short 'd' <>
                  metavar "<DBNAME>" <>
                  help "Database name to connect to"
                )
    retries = optional $
      option auto ( long "retries" <>
                    metavar "NO OF RETRIES" <>
                    help (snd retriesNumEnv)
                  )

connInfoErrModifier :: String -> String
connInfoErrModifier s = "Fatal Error : " ++ s

mkConnInfo ::RawConnInfo -> Either String Q.ConnInfo
mkConnInfo (RawConnInfo mHost mPort mUser pass mURL mDB opts mRetries) =
  case (mHost, mPort, mUser, mDB, mURL) of

    (Just host, Just port, Just user, Just db, Nothing) ->
      return $ Q.ConnInfo host port user pass db opts retries

    (_, _, _, _, Just dbURL) -> maybe (throwError invalidUrlMsg)
                                withRetries $ parseDatabaseUrl dbURL opts
    _ -> throwError $ "Invalid options. "
                    ++ "Expecting all database connection params "
                    ++ "(host, port, user, dbname, password) or "
                    ++ "database-url (HASURA_GRAPHQL_DATABASE_URL)"
  where
    retries = fromMaybe 1 mRetries
    withRetries ci = return $ ci{Q.connRetries = retries}
    invalidUrlMsg = "Invalid database-url (HASURA_GRAPHQL_DATABASE_URL). "
                    ++ "Example postgres://foo:bar@example.com:2345/database"

parseTxIsolation :: Parser (Maybe Q.TxIsolation)
parseTxIsolation = optional $
  option (eitherReader readIsoLevel)
           ( long "tx-iso" <>
             short 'i' <>
             metavar "<TXISO>" <>
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
                 metavar "<NO OF STRIPES>" <>
                 help (snd pgStripesEnv)
              )

    conns = optional $
      option auto
            ( long "connections" <>
               short 'c' <>
               metavar "<NO OF CONNS>" <>
               help (snd pgConnsEnv)
            )

    timeout = optional $
      option auto
              ( long "timeout" <>
                metavar "<SECONDS>" <>
                help (snd pgTimeoutEnv)
              )
    allowPrepare = optional $
      option (eitherReader parseStrAsBool)
              ( long "use-prepared-statements" <>
                metavar "<true|false>" <>
                help (snd pgUsePrepareEnv)
              )

parseServerPort :: Parser (Maybe Int)
parseServerPort = optional $
  option auto
       ( long "server-port" <>
         metavar "<PORT>" <>
         help (snd servePortEnv)
       )

parseServerHost :: Parser (Maybe HostPreference)
parseServerHost = optional $ strOption ( long "server-host" <>
                metavar "<HOST>" <>
                help "Host on which graphql-engine will listen (default: *)"
              )

parseAccessKey :: Parser (Maybe AdminSecret)
parseAccessKey =
  optional $ AdminSecret <$>
    strOption ( long "access-key" <>
                metavar "ADMIN SECRET KEY (DEPRECATED: USE --admin-secret)" <>
                help (snd adminSecretEnv)
              )

parseAdminSecret :: Parser (Maybe AdminSecret)
parseAdminSecret =
  optional $ AdminSecret <$>
    strOption ( long "admin-secret" <>
                metavar "ADMIN SECRET KEY" <>
                help (snd adminSecretEnv)
              )

readHookType :: String -> Either String AuthHookType
readHookType tyS =
  case tyS of
    "GET"  -> Right AHTGet
    "POST" -> Right AHTPost
    _      -> Left "Only expecting GET / POST"

readAPIs :: String -> Either String [API]
readAPIs = mapM readAPI . T.splitOn "," . T.pack
  where readAPI si = case T.toUpper $ T.strip si of
          "METADATA"  -> Right METADATA
          "GRAPHQL"   -> Right GRAPHQL
          "PGDUMP"    -> Right PGDUMP
          "DEVELOPER" -> Right DEVELOPER
          "CONFIG"    -> Right CONFIG
          _            -> Left "Only expecting list of comma separated API types metadata,graphql,pgdump,developer,config"

readLogTypes :: String -> Either String [L.EngineLogType]
readLogTypes = mapM readLogType . T.splitOn "," . T.pack
  where readLogType si = case T.toLower $ T.strip si of
          "startup"       -> Right L.ELTStartup
          "http-log"      -> Right L.ELTHttpLog
          "webhook-log"   -> Right L.ELTWebhookLog
          "websocket-log" -> Right L.ELTWebsocketLog
          "query-log"     -> Right L.ELTQueryLog
          _               -> Left $ "Valid list of comma-separated log types: "
                             <> BLC.unpack (J.encode L.userAllowedLogTypes)

readLogLevel :: String -> Either String L.LogLevel
readLogLevel s = case T.toLower $ T.strip $ T.pack s of
  "debug" -> Right L.LevelDebug
  "info"  -> Right L.LevelInfo
  "warn"  -> Right L.LevelWarn
  "error" -> Right L.LevelError
  _       -> Left "Valid log levels: debug, info, warn or error"


readJson :: (J.FromJSON a) => String -> Either String a
readJson = J.eitherDecodeStrict . txtToBs . T.pack


parseWebHook :: Parser RawAuthHook
parseWebHook =
  AuthHookG <$> url <*> urlType
  where
    url = optional $
      strOption ( long "auth-hook" <>
                  metavar "<WEB HOOK URL>" <>
                  help (snd authHookEnv)
                )
    urlType = optional $
      option (eitherReader readHookType)
                  ( long "auth-hook-mode" <>
                    metavar "<GET|POST>" <>
                    help (snd authHookModeEnv)
                  )

parseJwtSecret :: Parser (Maybe JWTConfig)
parseJwtSecret =
  optional $
    option (eitherReader readJson)
    ( long "jwt-secret" <>
      metavar "<JSON CONFIG>" <>
      help (snd jwtSecretEnv)
    )

jwtSecretHelp :: String
jwtSecretHelp = "The JSON containing type and the JWK used for verifying. e.g: "
              <> "`{\"type\": \"HS256\", \"key\": \"<your-hmac-shared-secret>\", \"claims_namespace\": \"<optional-custom-claims-key-name>\"}`,"
              <> "`{\"type\": \"RS256\", \"key\": \"<your-PEM-RSA-public-key>\", \"claims_namespace\": \"<optional-custom-claims-key-name>\"}`"

parseUnAuthRole :: Parser (Maybe RoleName)
parseUnAuthRole = fmap mkRoleName $ optional $
  strOption ( long "unauthorized-role" <>
              metavar "<ROLE>" <>
              help (snd unAuthRoleEnv)
            )
  where
    mkRoleName mText = mText >>= (fmap RoleName . mkNonEmptyText)

parseCorsConfig :: Parser (Maybe CorsConfig)
parseCorsConfig = mapCC <$> disableCors <*> corsDomain
  where
    corsDomain = optional $
      option (eitherReader readCorsDomains)
      ( long "cors-domain" <>
        metavar "<DOMAINS>" <>
        help (snd corsDomainEnv)
      )

    disableCors =
      switch ( long "disable-cors" <>
               help "Disable CORS. Do not send any CORS headers on any request"
             )

    mapCC isDisabled domains =
      bool domains (Just $ CCDisabled False) isDisabled

parseEnableConsole :: Parser Bool
parseEnableConsole =
  switch ( long "enable-console" <>
           help (snd enableConsoleEnv)
         )

parseConsoleAssetsDir :: Parser (Maybe Text)
parseConsoleAssetsDir = optional $
    option (eitherReader fromEnv)
      ( long "console-assets-dir" <>
        help (snd consoleAssetsDirEnv)
      )

parseEnableTelemetry :: Parser (Maybe Bool)
parseEnableTelemetry = optional $
  option (eitherReader parseStrAsBool)
         ( long "enable-telemetry" <>
           help (snd enableTelemetryEnv)
         )

parseWsReadCookie :: Parser Bool
parseWsReadCookie =
  switch ( long "ws-read-cookie" <>
           help (snd wsReadCookieEnv)
         )

parseStringifyNum :: Parser Bool
parseStringifyNum =
  switch ( long "stringify-numeric-types" <>
           help (snd stringifyNumEnv)
         )

parseEnabledAPIs :: Parser (Maybe [API])
parseEnabledAPIs = optional $
  option (eitherReader readAPIs)
         ( long "enabled-apis" <>
           help (snd enabledAPIsEnv)
         )

parseMxRefetchInt :: Parser (Maybe LQ.RefetchInterval)
parseMxRefetchInt =
  optional $
    option (eitherReader fromEnv)
    ( long "live-queries-multiplexed-refetch-interval" <>
      metavar "<INTERVAL(ms)>" <>
      help (snd mxRefetchDelayEnv)
    )

parseMxBatchSize :: Parser (Maybe LQ.BatchSize)
parseMxBatchSize =
  optional $
    option (eitherReader fromEnv)
    ( long "live-queries-multiplexed-batch-size" <>
      metavar "BATCH_SIZE" <>
      help (snd mxBatchSizeEnv)
    )

parseEnableAllowlist :: Parser Bool
parseEnableAllowlist =
  switch ( long "enable-allowlist" <>
           help (snd enableAllowlistEnv)
         )

mxRefetchDelayEnv :: (String, String)
mxRefetchDelayEnv =
  ( "HASURA_GRAPHQL_LIVE_QUERIES_MULTIPLEXED_REFETCH_INTERVAL"
  , "results will only be sent once in this interval (in milliseconds) for "
  <> "live queries which can be multiplexed. Default: 1000 (1sec)"
  )

mxBatchSizeEnv :: (String, String)
mxBatchSizeEnv =
  ( "HASURA_GRAPHQL_LIVE_QUERIES_MULTIPLEXED_BATCH_SIZE"
  , "multiplexed live queries are split into batches of the specified "
  <> "size. Default 100. "
  )

enableAllowlistEnv :: (String, String)
enableAllowlistEnv =
  ( "HASURA_GRAPHQL_ENABLE_ALLOWLIST"
  , "Only accept allowed GraphQL queries"
  )

parseFallbackRefetchInt :: Parser (Maybe LQ.RefetchInterval)
parseFallbackRefetchInt =
  optional $
    option (eitherReader fromEnv)
    ( long "live-queries-fallback-refetch-interval" <>
      metavar "<INTERVAL(ms)>" <>
      help (snd mxRefetchDelayEnv)
    )

fallbackRefetchDelayEnv :: (String, String)
fallbackRefetchDelayEnv =
  ( "HASURA_GRAPHQL_LIVE_QUERIES_FALLBACK_REFETCH_INTERVAL"
  , "results will only be sent once in this interval (in milliseconds) for "
  <> "live queries which cannot be multiplexed. Default: 1000 (1sec)"
  )

parseEnabledLogs :: Parser (Maybe [L.EngineLogType])
parseEnabledLogs = optional $
  option (eitherReader readLogTypes)
         ( long "enabled-log-types" <>
           help (snd enabledLogsEnv)
         )

parseLogLevel :: Parser (Maybe L.LogLevel)
parseLogLevel = optional $
  option (eitherReader readLogLevel)
         ( long "log-level" <>
           help (snd logLevelEnv)
         )

-- Init logging related
connInfoToLog :: Q.ConnInfo -> StartupLog
connInfoToLog (Q.ConnInfo host port user _ db _ retries) =
  StartupLog L.LevelInfo "postgres_connection" infoVal
  where
    infoVal = J.object [ "host" J..= host
                       , "port" J..= port
                       , "user" J..= user
                       , "database" J..= db
                       , "retries" J..= retries
                       ]

serveOptsToLog :: ServeOptions -> StartupLog
serveOptsToLog so =
  StartupLog L.LevelInfo "server_configuration" infoVal
  where
    infoVal = J.object [ "port" J..= soPort so
                       , "server_host" J..= show (soHost so)
                       , "transaction_isolation" J..= show (soTxIso so)
                       , "admin_secret_set" J..= isJust (soAdminSecret so)
                       , "auth_hook" J..= (ahUrl <$> soAuthHook so)
                       , "auth_hook_mode" J..= (show . ahType <$> soAuthHook so)
                       , "jwt_secret" J..= (J.toJSON <$> soJwtSecret so)
                       , "unauth_role" J..= soUnAuthRole so
                       , "cors_config" J..= soCorsConfig so
                       , "enable_console" J..= soEnableConsole so
                       , "console_assets_dir" J..= soConsoleAssetsDir so
                       , "enable_telemetry" J..= soEnableTelemetry so
                       , "use_prepared_statements" J..= (Q.cpAllowPrepare . soConnParams) so
                       , "stringify_numeric_types" J..= soStringifyNum so
                       , "enabled_apis" J..= soEnabledAPIs so
                       , "live_query_options" J..= soLiveQueryOpts so
                       , "enable_allowlist" J..= soEnableAllowlist so
                       , "enabled_log_types" J..= soEnabledLogTypes so
                       , "log_level" J..= soLogLevel so
                       ]

mkGenericStrLog :: L.LogLevel -> T.Text -> String -> StartupLog
mkGenericStrLog logLevel k msg =
  StartupLog logLevel k $ J.toJSON msg

mkGenericLog :: (J.ToJSON a) => L.LogLevel -> Text -> a -> StartupLog
mkGenericLog logLevel k msg =
  StartupLog logLevel k $ J.toJSON msg

inconsistentMetadataLog :: SchemaCache -> StartupLog
inconsistentMetadataLog sc =
  StartupLog L.LevelWarn "inconsistent_metadata" infoVal
  where
    infoVal = J.object ["objects" J..= scInconsistentObjs sc]
