-- TODO(SOLOMON): Should this be moved into `Data.Environment`?
module Hasura.Server.Init.Env
  ( -- * WithEnv
    WithEnvT (..),
    WithEnv,
    runWithEnvT,
    runWithEnv,
    withOption,
    withOptionDefault,
    withOptions,
    withOptionSwitch,
    withOptionSwitch',
    considerEnv,
    considerEnvs,

    -- * FromEnv
    FromEnv (..),
  )
where

--------------------------------------------------------------------------------

import Control.Monad.Morph qualified as Morph
import Data.Aeson qualified as J
import Data.ByteString.Lazy.UTF8 qualified as BLU
import Data.Char qualified as Char
import Data.HashSet qualified as HashSet
import Data.String qualified as String
import Data.Text qualified as Text
import Data.Time qualified as Time
import Data.URL.Template qualified as Template
import Database.PG.Query qualified as Query
import Hasura.Backends.Postgres.Connection.MonadTx (ExtensionsSchema)
import Hasura.Backends.Postgres.Connection.MonadTx qualified as MonadTx
import Hasura.Cache.Bounded qualified as Cache
import Hasura.GraphQL.Execute.Subscription.Options qualified as Subscription.Options
import Hasura.Logging qualified as Logging
import Hasura.Prelude
import Hasura.RQL.Types.Metadata (Metadata, MetadataDefaults (..))
import Hasura.RQL.Types.NamingCase (NamingCase)
import Hasura.RQL.Types.NamingCase qualified as NamingCase
import Hasura.RQL.Types.Roles (RoleName, mkRoleName)
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.Server.Auth qualified as Auth
import Hasura.Server.Cors qualified as Cors
import Hasura.Server.Init.Config qualified as Config
import Hasura.Server.Logging qualified as Server.Logging
import Hasura.Server.Types (GranularPrometheusMetricsState (..))
import Hasura.Server.Types qualified as Server.Types
import Hasura.Server.Utils qualified as Utils
import Network.Wai.Handler.Warp qualified as Warp
import Refined (NonNegative, Positive, Refined, refineFail, unrefine)

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
      throwError
        $ "Fatal Error:- Environment variable "
        ++ envVar
        ++ ": "
        ++ s

-- | Lookup a list of keys with 'considerEnv' and return the first
-- value to parse successfully.
considerEnvs :: (Monad m, FromEnv a) => [String] -> WithEnvT m (Maybe a)
considerEnvs envVars = foldl1 (<|>) <$> mapM considerEnv envVars

-- | Lookup a list of keys with 'withOption' and return the first
-- value to parse successfully.
withOptions :: (Monad m, FromEnv option) => Maybe option -> [Config.Option ()] -> WithEnvT m (Maybe option)
withOptions parsed options = foldl1 (<|>) <$> traverse (withOption parsed) options

-- | Given the parse result for an option and the 'Option' record
-- for that option, query the environment, and then merge the results
-- from the parser and environment.
withOption :: (Monad m, FromEnv option) => Maybe option -> Config.Option () -> WithEnvT m (Maybe option)
withOption parsed option =
  let option' = option {Config._default = Nothing}
   in withOptionDefault (fmap Just parsed) option'

-- | Given the parse result for an option and the 'Option' record for
-- that option, query the environment, and then merge the results from
-- the parser, environment, and the default.
withOptionDefault :: (Monad m, FromEnv option) => Maybe option -> Config.Option option -> WithEnvT m option
withOptionDefault parsed Config.Option {..} =
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
-- NOTE: An alternative solution might be to make Switches return
-- 'Maybe _', where '_' is an option specific sum type. This would
-- allow us to use 'withOptionDefault' directly. Additionally, all
-- fields of 'ServeOptionsRaw' would become 'Maybe' or 'First' values
-- which would allow us to write a 'Monoid ServeOptionsRaw' instance
-- for combing different option sources.
--
-- A 'Monoid' instance would be super valuable to cleanup arg/env
-- parsing but this solution feels somewhat unsatisfying.
withOptionSwitch :: (Monad m) => Bool -> Config.Option Bool -> WithEnvT m Bool
withOptionSwitch parsed option = withOptionSwitch' parsed (id, id) option

-- | Given an 'Iso a Bool' we can apply the same boolean env merging
-- semantics as we do for 'Bool' in `withOptionsSwitch' to @a@.
withOptionSwitch' :: (Monad m) => a -> (a -> Bool, Bool -> a) -> Config.Option a -> WithEnvT m a
withOptionSwitch' parsed (fwd, bwd) option =
  if fwd parsed
    then pure (bwd True)
    else fmap bwd $ withOptionDefault Nothing (fmap fwd option)

--------------------------------------------------------------------------------

-- | A 'Read' style parser used for consuming Env Vars and building
-- 'ReadM' parsers for 'optparse-applicative'.
class FromEnv a where
  fromEnv :: String -> Either String a

type WithEnv = WithEnvT Identity

-- NOTE: Should we use `Data.Environment.Environment` for context?

-- | The monadic context for querying Env Vars.
newtype WithEnvT m a = WithEnvT {unWithEnvT :: ReaderT [(String, String)] (ExceptT String m) a}
  deriving newtype (Functor, Applicative, Monad, MonadReader [(String, String)], MonadError String, MonadIO)

instance MonadTrans WithEnvT where
  lift = WithEnvT . lift . lift

instance Morph.MFunctor WithEnvT where
  hoist f (WithEnvT m) = WithEnvT $ Morph.hoist (Morph.hoist f) m

-- | Given an environment run a 'WithEnv' action producing either a
-- parse error or an @a@.
runWithEnv :: [(String, String)] -> WithEnv a -> Either String a
runWithEnv env (WithEnvT m) = runIdentity $ runExceptT $ runReaderT m env

-- | Given an environment run a 'WithEnvT' action producing either a
-- parse error or an @a@.
runWithEnvT :: [(String, String)] -> WithEnvT m a -> m (Either String a)
runWithEnvT env (WithEnvT m) = runExceptT $ runReaderT m env

--------------------------------------------------------------------------------

-- Deserialize from seconds, in the usual way
instance FromEnv Time.NominalDiffTime where
  fromEnv s =
    case (readMaybe s :: Maybe Double) of
      Nothing -> Left "could not parse as a Double"
      Just i -> Right $ realToFrac i

instance FromEnv Time.DiffTime where
  fromEnv s =
    case (readMaybe s :: Maybe Seconds) of
      Nothing -> Left "could not parse as a Double"
      Just i -> Right $ seconds i

instance FromEnv String where
  fromEnv = Right

instance FromEnv Warp.HostPreference where
  fromEnv = Right . String.fromString

instance FromEnv Text where
  fromEnv = Right . Text.pack

instance (FromEnv a) => FromEnv (Maybe a) where
  fromEnv = fmap Just . fromEnv

instance FromEnv Auth.AuthHookType where
  fromEnv = \case
    "GET" -> Right Auth.AHTGet
    "POST" -> Right Auth.AHTPost
    _ -> Left "Only expecting GET / POST"

instance FromEnv Int where
  fromEnv s =
    case readMaybe s of
      Nothing -> Left "Expecting Int value"
      Just m -> Right m

instance FromEnv Integer where
  fromEnv s =
    case readMaybe s of
      Nothing -> Left "Expecting Integer value"
      Just m -> Right m

instance FromEnv Auth.AdminSecretHash where
  fromEnv = Right . Auth.hashAdminSecret . Text.pack

instance FromEnv RoleName where
  fromEnv string =
    case mkRoleName (Text.pack string) of
      Nothing -> Left "empty string not allowed"
      Just roleName -> Right roleName

instance FromEnv Bool where
  fromEnv t
    | map Char.toLower t `elem` truthVals = Right True
    | map Char.toLower t `elem` falseVals = Right False
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

instance FromEnv Config.TelemetryStatus where
  fromEnv = fmap (bool Config.TelemetryDisabled Config.TelemetryEnabled) . fromEnv

instance FromEnv Config.AdminInternalErrorsStatus where
  fromEnv = fmap (bool Config.AdminInternalErrorsDisabled Config.AdminInternalErrorsEnabled) . fromEnv

instance FromEnv Config.WsReadCookieStatus where
  fromEnv = fmap (bool Config.WsReadCookieDisabled Config.WsReadCookieEnabled) . fromEnv

instance FromEnv J.Value where
  fromEnv = J.eitherDecode . BLU.fromString

instance FromEnv MetadataDefaults where
  fromEnv = J.eitherDecode . BLU.fromString

instance FromEnv Metadata where
  fromEnv = J.eitherDecode . BLU.fromString

instance FromEnv Options.StringifyNumbers where
  fromEnv = fmap (bool Options.Don'tStringifyNumbers Options.StringifyNumbers) . fromEnv @Bool

instance FromEnv Options.RemoteSchemaPermissions where
  fromEnv = fmap (bool Options.DisableRemoteSchemaPermissions Options.EnableRemoteSchemaPermissions) . fromEnv @Bool

instance FromEnv Options.DangerouslyCollapseBooleans where
  fromEnv = fmap (bool Options.Don'tDangerouslyCollapseBooleans Options.DangerouslyCollapseBooleans) . fromEnv @Bool

instance FromEnv Options.RemoteNullForwardingPolicy where
  fromEnv = fmap (bool Options.RemoteForwardAccurately Options.RemoteOnlyForwardNonNull) . fromEnv @Bool

instance FromEnv Options.InferFunctionPermissions where
  fromEnv = fmap (bool Options.Don'tInferFunctionPermissions Options.InferFunctionPermissions) . fromEnv @Bool

instance FromEnv (Server.Types.MaintenanceMode ()) where
  fromEnv = fmap (bool Server.Types.MaintenanceModeDisabled (Server.Types.MaintenanceModeEnabled ())) . fromEnv @Bool

instance FromEnv Server.Logging.MetadataQueryLoggingMode where
  fromEnv = fmap (bool Server.Logging.MetadataQueryLoggingDisabled Server.Logging.MetadataQueryLoggingEnabled) . fromEnv @Bool

instance FromEnv Query.TxIsolation where
  fromEnv = Utils.readIsoLevel

instance FromEnv Cors.CorsConfig where
  fromEnv = Cors.readCorsDomains

instance FromEnv (HashSet Config.API) where
  fromEnv = fmap HashSet.fromList . traverse readAPI . Text.splitOn "," . Text.pack
    where
      readAPI si = case Text.toUpper $ Text.strip si of
        "METADATA" -> Right Config.METADATA
        "GRAPHQL" -> Right Config.GRAPHQL
        "PGDUMP" -> Right Config.PGDUMP
        "DEVELOPER" -> Right Config.DEVELOPER
        "CONFIG" -> Right Config.CONFIG
        "METRICS" -> Right Config.METRICS
        _ -> Left "Only expecting list of comma separated API types metadata,graphql,pgdump,developer,config,metrics"

instance FromEnv NamingCase where
  fromEnv = NamingCase.parseNamingConventionFromText . Text.pack

instance FromEnv (HashSet Server.Types.ExperimentalFeature) where
  fromEnv = fmap HashSet.fromList . traverse readAPI . Text.splitOn "," . Text.pack
    where
      readAPI si = case Text.toLower $ Text.strip si of
        key | Just (_, ef) <- find ((== key) . fst) experimentalFeatures -> Right ef
        _ ->
          Left
            $ "Only expecting list of comma separated experimental features, options are:"
            ++ intercalate ", " (map (Text.unpack . fst) experimentalFeatures)

      experimentalFeatures :: [(Text, Server.Types.ExperimentalFeature)]
      experimentalFeatures = [(Server.Types.experimentalFeatureKey ef, ef) | ef <- [minBound .. maxBound]]

instance FromEnv Subscription.Options.BatchSize where
  fromEnv s = do
    val <- readEither s
    maybeToEither "batch size should be a non negative integer" $ Subscription.Options.mkBatchSize val

instance FromEnv Subscription.Options.RefetchInterval where
  fromEnv x = do
    val <- fmap (milliseconds . fromInteger) . readEither $ x
    maybeToEither "refetch interval should be a non negative integer" $ Subscription.Options.mkRefetchInterval val

instance FromEnv Milliseconds where
  fromEnv = readEither

instance FromEnv Config.OptionalInterval where
  fromEnv x = do
    i <- fromEnv @(Refined NonNegative Milliseconds) x
    if unrefine i == 0
      then pure $ Config.Skip
      else pure $ Config.Interval i

instance FromEnv Seconds where
  fromEnv = fmap fromInteger . readEither

instance FromEnv Config.WSConnectionInitTimeout where
  fromEnv s = do
    seconds <- fromIntegral @_ @Seconds <$> fromEnv @Int s
    nonNegative <- maybeToEither "WebSocket Connection Timeout must not be negative" $ refineFail seconds
    pure $ Config.WSConnectionInitTimeout nonNegative

instance FromEnv Config.KeepAliveDelay where
  fromEnv =
    fmap Config.KeepAliveDelay . fromEnv @(Refined NonNegative Seconds)

instance FromEnv Auth.JWTConfig where
  fromEnv = readJson

instance FromEnv [Auth.JWTConfig] where
  fromEnv = readJson

instance (Logging.EnabledLogTypes impl) => FromEnv (HashSet (Logging.EngineLogType impl)) where
  fromEnv = fmap HashSet.fromList . Logging.parseEnabledLogTypes

instance FromEnv Logging.LogLevel where
  fromEnv s = case Text.toLower $ Text.strip $ Text.pack s of
    "debug" -> Right Logging.LevelDebug
    "info" -> Right Logging.LevelInfo
    "warn" -> Right Logging.LevelWarn
    "error" -> Right Logging.LevelError
    _ -> Left "Valid log levels: debug, info, warn or error"

instance FromEnv Template.Template where
  fromEnv = Template.parseTemplate . Text.pack

instance (Num a, Ord a, FromEnv a) => FromEnv (Refined NonNegative a) where
  fromEnv s =
    fmap (maybeToEither "Only expecting a non negative numeric") refineFail =<< fromEnv s

instance FromEnv (Refined Positive Int) where
  fromEnv s =
    maybeToEither "Only expecting a positive integer" (refineFail =<< readMaybe s)

instance FromEnv Config.Port where
  fromEnv s =
    maybeToEither "Only expecting a value between 0 and 65535" (Config.mkPort =<< readMaybe s)

instance FromEnv Cache.CacheSize where
  fromEnv = Cache.parseCacheSize

instance FromEnv ExtensionsSchema where
  fromEnv = Right . MonadTx.ExtensionsSchema . Text.pack

instance FromEnv Server.Types.ApolloFederationStatus where
  fromEnv = fmap (bool Server.Types.ApolloFederationDisabled Server.Types.ApolloFederationEnabled) . fromEnv @Bool

instance FromEnv GranularPrometheusMetricsState where
  fromEnv = fmap (bool GranularMetricsOff GranularMetricsOn) . fromEnv @Bool

instance FromEnv Server.Types.CloseWebsocketsOnMetadataChangeStatus where
  fromEnv = fmap (bool Server.Types.CWMCDisabled Server.Types.CWMCEnabled) . fromEnv @Bool

instance FromEnv Server.Types.TriggersErrorLogLevelStatus where
  fromEnv = fmap (bool Server.Types.TriggersErrorLogLevelDisabled Server.Types.TriggersErrorLogLevelEnabled) . fromEnv @Bool

instance FromEnv Server.Types.PersistedQueriesState where
  fromEnv = fmap (bool Server.Types.PersistedQueriesDisabled Server.Types.PersistedQueriesEnabled) . fromEnv @Bool
