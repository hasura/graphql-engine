-- TODO(SOLOMON): Should this be moved into `Data.Environment`?
module Hasura.Server.Init.Env
  ( FromEnv (..),
    WithEnv,
    withEnv,
    withEnvs,
    withEnvBool,
    considerEnv,
    runWithEnv,
  )
where

--------------------------------------------------------------------------------

import Data.Char qualified as C
import Data.String qualified as String
import Data.Text qualified as T
import Data.Time (NominalDiffTime)
import Data.URL.Template (URLTemplate, parseURLTemplate)
import Database.PG.Query qualified as Q
import Hasura.Cache.Bounded qualified as Cache
import Hasura.GraphQL.Execute.Subscription.Options qualified as ES
import Hasura.GraphQL.Schema.NamingCase (NamingCase, parseNamingConventionFromText)
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types.Common (NonNegativeInt, mkNonNegativeInt)
import Hasura.Server.Auth (AdminSecretHash, AuthHookType (..), JWTConfig (..), hashAdminSecret)
import Hasura.Server.Cors (CorsConfig, readCorsDomains)
import Hasura.Server.Init.Config
import Hasura.Server.Types (ExperimentalFeature (..))
import Hasura.Server.Utils (readIsoLevel)
import Hasura.Session (RoleName, mkRoleName)
import Network.Wai.Handler.Warp (HostPreference)

--------------------------------------------------------------------------------

-- | Lookup a key in the application environment then parse the value
-- with a 'FromEnv' instance'
considerEnv :: FromEnv a => String -> WithEnv (Maybe a)
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
considerEnvs :: FromEnv a => [String] -> WithEnv (Maybe a)
considerEnvs envVars = foldl1 (<|>) <$> mapM considerEnv envVars

withEnv :: FromEnv a => Maybe a -> String -> WithEnv (Maybe a)
withEnv mVal envVar =
  maybe (considerEnv envVar) (pure . Just) mVal

-- | Return 'a' if Just or else call 'considerEnv' with a list of env keys k
withEnvs :: FromEnv a => Maybe a -> [String] -> WithEnv (Maybe a)
withEnvs mVal envVars =
  maybe (considerEnvs envVars) (pure . Just) mVal

-- | If @bVal@ is 'True' then return it, else lookup the 'envVar' in
-- the environment and return 'False' if absent.
withEnvBool :: Bool -> String -> WithEnv Bool
withEnvBool bVal envVar =
  if bVal
    then pure bVal
    else do
      mVal <- considerEnv @Bool envVar
      pure $ fromMaybe False mVal

--------------------------------------------------------------------------------

-- | A 'Read' style parser used for consuming envvars and building
-- 'ReadM' parsers for 'optparse-applicative'.
class FromEnv a where
  fromEnv :: String -> Either String a

-- TODO: Convert to a newtype and use `Data.Environment.Environment` for context
type WithEnv a = ReaderT [(String, String)] (ExceptT String Identity) a

-- | Given an environment run a 'WithEnv' action producing either a
-- parse error or an @a@.
runWithEnv :: [(String, String)] -> WithEnv a -> Either String a
runWithEnv env m = runIdentity $ runExceptT $ runReaderT m env

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

instance FromEnv Q.TxIsolation where
  fromEnv = readIsoLevel

instance FromEnv CorsConfig where
  fromEnv = readCorsDomains

instance FromEnv [API] where
  fromEnv = traverse readAPI . T.splitOn "," . T.pack
    where
      readAPI si = case T.toUpper $ T.strip si of
        "METADATA" -> Right METADATA
        "GRAPHQL" -> Right GRAPHQL
        "PGDUMP" -> Right PGDUMP
        "DEVELOPER" -> Right DEVELOPER
        "CONFIG" -> Right CONFIG
        _ -> Left "Only expecting list of comma separated API types metadata,graphql,pgdump,developer,config"

instance FromEnv NamingCase where
  fromEnv = parseNamingConventionFromText . T.pack

instance FromEnv [ExperimentalFeature] where
  fromEnv = traverse readAPI . T.splitOn "," . T.pack
    where
      readAPI si = case T.toLower $ T.strip si of
        "inherited_roles" -> Right EFInheritedRoles
        "streaming_subscriptions" -> Right EFStreamingSubscriptions
        "optimize_permission_filters" -> Right EFOptimizePermissionFilters
        "naming_convention" -> Right EFNamingConventions
        _ ->
          Left $
            "Only expecting list of comma separated experimental features, options are:"
              ++ "inherited_roles, streaming_subscriptions, optimize_permission_filters, naming_convention"

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

instance FromEnv Seconds where
  fromEnv = fmap fromInteger . readEither

instance FromEnv JWTConfig where
  fromEnv = readJson

instance FromEnv [JWTConfig] where
  fromEnv = readJson

instance L.EnabledLogTypes impl => FromEnv [L.EngineLogType impl] where
  fromEnv = L.parseEnabledLogTypes

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
