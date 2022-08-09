-- TODO(SOLOMON): Should this be moved into `Data.Environment`?
module Hasura.Server.Init.Env
  ( FromEnv (..),
    WithEnvT (..),
    WithEnv,
    runWithEnvT,
    runWithEnv,
    withOption,
    withOptionDefault,
    withOptions,
    withOptionSwitch,
    considerEnv,
    considerEnvs,
  )
where

--------------------------------------------------------------------------------

import Control.Monad.Morph (MFunctor (..))
import Data.Char qualified as C
import Data.HashSet qualified as Set
import Data.String qualified as String
import Data.Text qualified as T
import Data.Time (NominalDiffTime)
import Data.URL.Template (URLTemplate, parseURLTemplate)
import Database.PG.Query qualified as Q
import Hasura.Backends.Postgres.Connection.MonadTx (ExtensionsSchema (..))
import Hasura.Cache.Bounded qualified as Cache
import Hasura.GraphQL.Execute.Subscription.Options qualified as ES
import Hasura.GraphQL.Schema.NamingCase (NamingCase, parseNamingConventionFromText)
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types.Common (NonNegativeInt, mkNonNegativeInt)
import Hasura.Server.Auth (AdminSecretHash, AuthHookType (..), JWTConfig (..), hashAdminSecret)
import Hasura.Server.Cors (CorsConfig, readCorsDomains)
import Hasura.Server.Init.Config
import Hasura.Server.Logging qualified as Logging
import Hasura.Server.Types (ExperimentalFeature (..), MaintenanceMode (..))
import Hasura.Server.Utils (readIsoLevel)
import Hasura.Session (RoleName, mkRoleName)
import Network.Wai.Handler.Warp (HostPreference)

--------------------------------------------------------------------------------

-- | Lookup a key in the application environment then parse the value
-- with a 'FromEnv' instance'
considerEnv :: (Monad m, FromEnv a) => String -> WithEnvT m (Maybe a)
considerEnv envVar = do
  env <- ask
  case lookup envVar env of
    Nothing -> pure Nothing
    Just val -> either throwErr (pure . Just) $ fromEnv val
  where
    throwErr s =
      throwError $
        "Fatal Error:- Environment variable " ++ envVar ++ ": " ++ s

-- | Lookup a list of keys with 'considerEnv' and return the first
-- value to parse successfully.
considerEnvs :: (Monad m, FromEnv a) => [String] -> WithEnvT m (Maybe a)
considerEnvs envVars = foldl1 (<|>) <$> mapM considerEnv envVars

-- | Lookup a list of keys with 'withOption' and return the first
-- value to parse successfully.
withOptions :: (Monad m, FromEnv option) => Maybe option -> [Option ()] -> WithEnvT m (Maybe option)
withOptions parsed options = foldl1 (<|>) <$> traverse (withOption parsed) options

-- | Given the parse result for an option and the 'Option def' record
-- for that option, query the environment, and then merge the results
-- from the parser and environment.
withOption :: (Monad m, FromEnv option) => Maybe option -> Option () -> WithEnvT m (Maybe option)
withOption parsed option =
  let option' = option {_default = Nothing}
   in withOptionDefault (fmap Just parsed) option'

-- | Given the parse result for an option and the 'Option def' record
-- for that option, query the environment, and then merge the results
-- from the parser, environment, and the default.
withOptionDefault :: (Monad m, FromEnv option) => Maybe option -> Option option -> WithEnvT m option
withOptionDefault parsed Option {..} =
  onNothing parsed (fromMaybe _default <$> considerEnv _envVar)

-- | Switches in 'optparse-applicative' have different semantics then
-- ordinary flags. They are always optional and produce a 'False' when
-- absent rather then a 'Nothing'.
--
-- In HGE we give Env Vars a higher precedence then an absent Switch
-- but the ordinary 'withEnv' operation expects a 'Nothing' for an
-- absent arg parser result.
--
-- This function executes with 'withOption Nothing' when the Switch is
-- absent, otherwise it returns 'True'.
--
-- An alternative solution would be to make Switches return 'Maybe _',
-- where '_' is an option specific sum type. This would allow us to
-- use 'withOptionDefault' directly. Additionally, all fields of
-- 'ServeOptionsRaw' would become 'Maybe' or 'First' values which
-- would allow us to write a 'Monoid ServeOptionsRaw' instance for
-- combing different option sources.
withOptionSwitch :: Monad m => Bool -> Option Bool -> WithEnvT m Bool
withOptionSwitch parsed option = bool (withOptionDefault Nothing option) (pure True) parsed

--------------------------------------------------------------------------------

-- | A 'Read' style parser used for consuming envvars and building
-- 'ReadM' parsers for 'optparse-applicative'.
class FromEnv a where
  fromEnv :: String -> Either String a

-- TODO: Use `Data.Environment.Environment` for context
type WithEnv = WithEnvT Identity

newtype WithEnvT m a = WithEnvT {unWithEnvT :: ReaderT [(String, String)] (ExceptT String m) a}
  deriving newtype (Functor, Applicative, Monad, MonadReader [(String, String)], MonadError String, MonadIO)

instance MonadTrans WithEnvT where
  lift = WithEnvT . lift . lift

instance MFunctor WithEnvT where
  hoist f (WithEnvT m) = WithEnvT $ hoist (hoist f) m

-- | Given an environment run a 'WithEnv' action producing either a
-- parse error or an @a@.
runWithEnv :: [(String, String)] -> WithEnv a -> Either String a
runWithEnv env (WithEnvT m) = runIdentity $ runExceptT $ runReaderT m env

runWithEnvT :: [(String, String)] -> WithEnvT m a -> m (Either String a)
runWithEnvT env (WithEnvT m) = runExceptT $ runReaderT m env

--------------------------------------------------------------------------------

-- Deserialize from seconds, in the usual way
instance FromEnv NominalDiffTime where
  fromEnv s =
    case (readMaybe s :: Maybe Double) of
      Nothing -> Left "could not parse as a Double"
      Just i -> Right $ realToFrac i

instance FromEnv String where
  fromEnv = Right

instance FromEnv HostPreference where
  fromEnv = Right . String.fromString

instance FromEnv Text where
  fromEnv = Right . T.pack

instance FromEnv a => FromEnv (Maybe a) where
  fromEnv = fmap Just . fromEnv

instance FromEnv AuthHookType where
  fromEnv = \case
    "GET" -> Right AHTGet
    "POST" -> Right AHTPost
    _ -> Left "Only expecting GET / POST"

instance FromEnv Int where
  fromEnv s =
    case readMaybe s of
      Nothing -> Left "Expecting Int value"
      Just m -> Right m

instance FromEnv AdminSecretHash where
  fromEnv = Right . hashAdminSecret . T.pack

instance FromEnv RoleName where
  fromEnv string =
    case mkRoleName (T.pack string) of
      Nothing -> Left "empty string not allowed"
      Just roleName -> Right roleName

instance FromEnv Bool where
  fromEnv t
    | map C.toLower t `elem` truthVals = Right True
    | map C.toLower t `elem` falseVals = Right False
    | otherwise = Left errMsg
    where
      truthVals = ["true", "t", "yes", "y"]
      falseVals = ["false", "f", "no", "n"]

      errMsg =
        " Not a valid boolean text. True values are "
          ++ show truthVals
          ++ " and  False values are "
          ++ show falseVals
          ++ ". All values are case insensitive"

instance FromEnv Options.StringifyNumbers where
  fromEnv = fmap (bool Options.Don'tStringifyNumbers Options.StringifyNumbers) . fromEnv @Bool

instance FromEnv Options.RemoteSchemaPermissions where
  fromEnv = fmap (bool Options.DisableRemoteSchemaPermissions Options.EnableRemoteSchemaPermissions) . fromEnv @Bool

instance FromEnv Options.InferFunctionPermissions where
  fromEnv = fmap (bool Options.Don'tInferFunctionPermissions Options.InferFunctionPermissions) . fromEnv @Bool

instance FromEnv (MaintenanceMode ()) where
  fromEnv = fmap (bool MaintenanceModeDisabled (MaintenanceModeEnabled ())) . fromEnv @Bool

instance FromEnv Logging.MetadataQueryLoggingMode where
  fromEnv = fmap (bool Logging.MetadataQueryLoggingDisabled Logging.MetadataQueryLoggingEnabled) . fromEnv @Bool

instance FromEnv Q.TxIsolation where
  fromEnv = readIsoLevel

instance FromEnv CorsConfig where
  fromEnv = readCorsDomains

instance FromEnv (HashSet API) where
  fromEnv = fmap Set.fromList . traverse readAPI . T.splitOn "," . T.pack
    where
      readAPI si = case T.toUpper $ T.strip si of
        "METADATA" -> Right METADATA
        "GRAPHQL" -> Right GRAPHQL
        "PGDUMP" -> Right PGDUMP
        "DEVELOPER" -> Right DEVELOPER
        "CONFIG" -> Right CONFIG
        "METRICS" -> Right METRICS
        _ -> Left "Only expecting list of comma separated API types metadata,graphql,pgdump,developer,config,metrics"

instance FromEnv NamingCase where
  fromEnv = parseNamingConventionFromText . T.pack

instance FromEnv (HashSet ExperimentalFeature) where
  fromEnv = fmap Set.fromList . traverse readAPI . T.splitOn "," . T.pack
    where
      readAPI si = case T.toLower $ T.strip si of
        "inherited_roles" -> Right EFInheritedRoles
        "streaming_subscriptions" -> Right EFStreamingSubscriptions
        "optimize_permission_filters" -> Right EFOptimizePermissionFilters
        "naming_convention" -> Right EFNamingConventions
        "apollo_federation" -> Right EFApolloFederation
        _ ->
          Left $
            "Only expecting list of comma separated experimental features, options are:"
              ++ "inherited_roles, streaming_subscriptions, optimize_permission_filters, naming_convention, apollo_federation"

instance FromEnv ES.BatchSize where
  fromEnv s = do
    val <- readEither s
    maybeToEither "batch size should be a non negative integer" $ ES.mkBatchSize val

instance FromEnv ES.RefetchInterval where
  fromEnv x = do
    val <- fmap (milliseconds . fromInteger) . readEither $ x
    maybeToEither "refetch interval should be a non negative integer" $ ES.mkRefetchInterval val

instance FromEnv Milliseconds where
  fromEnv = readEither

instance FromEnv OptionalInterval where
  fromEnv x = do
    Milliseconds i <- fromEnv @Milliseconds x
    if i == 0
      then pure $ Skip
      else pure $ Interval $ Milliseconds i

instance FromEnv Seconds where
  fromEnv = fmap fromInteger . readEither

instance FromEnv WSConnectionInitTimeout where
  fromEnv = fmap (WSConnectionInitTimeout . fromIntegral) . fromEnv @Int

instance FromEnv KeepAliveDelay where
  fromEnv =
    fmap KeepAliveDelay . fromEnv @Seconds

instance FromEnv JWTConfig where
  fromEnv = readJson

instance FromEnv [JWTConfig] where
  fromEnv = readJson

instance L.EnabledLogTypes impl => FromEnv (HashSet (L.EngineLogType impl)) where
  fromEnv = fmap Set.fromList . L.parseEnabledLogTypes

instance FromEnv L.LogLevel where
  fromEnv s = case T.toLower $ T.strip $ T.pack s of
    "debug" -> Right L.LevelDebug
    "info" -> Right L.LevelInfo
    "warn" -> Right L.LevelWarn
    "error" -> Right L.LevelError
    _ -> Left "Valid log levels: debug, info, warn or error"

instance FromEnv URLTemplate where
  fromEnv = parseURLTemplate . T.pack

instance FromEnv NonNegativeInt where
  fromEnv s =
    maybeToEither "Only expecting a non negative integer" (mkNonNegativeInt =<< readMaybe s)

instance FromEnv Cache.CacheSize where
  fromEnv = Cache.parseCacheSize

instance FromEnv ExtensionsSchema where
  fromEnv = Right . ExtensionsSchema . T.pack
