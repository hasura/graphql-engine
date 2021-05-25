module Hasura.Server.Utils where

import           Hasura.Prelude

import qualified Crypto.Hash                as Crypto
import qualified Data.Aeson                 as J
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base16     as Base16
import qualified Data.ByteString.Lazy       as BL
import qualified Data.CaseInsensitive       as CI
import qualified Data.HashSet               as Set
import qualified Data.List.NonEmpty         as NE
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TI
import qualified Data.UUID                  as UUID
import qualified Data.UUID.V4               as UUID
import qualified Data.Vector                as V
import qualified Database.PG.Query          as Q
import qualified Language.Haskell.TH.Syntax as TH
import qualified Network.HTTP.Client        as HC
import qualified Network.HTTP.Types         as HTTP
import qualified Network.Wreq               as Wreq
import qualified Text.Regex.TDFA            as TDFA
import qualified Text.Regex.TDFA.ReadRegex  as TDFA
import qualified Text.Regex.TDFA.TDFA       as TDFA

import           Control.Lens               ((^..))
import           Data.Aeson
import           Data.Aeson.Internal
import           Data.ByteArray             (convert)
import           Data.Char
import           Data.Text.Extended
import           Data.Time
import           Language.Haskell.TH.Syntax (Q, TExp)
import           System.Environment
import           System.Exit
import           System.Process

import           Hasura.Base.Instances      ()


jsonHeader :: HTTP.Header
jsonHeader = ("Content-Type", "application/json; charset=utf-8")

sqlHeader :: HTTP.Header
sqlHeader = ("Content-Type", "application/sql; charset=utf-8")

htmlHeader :: HTTP.Header
htmlHeader = ("Content-Type", "text/html; charset=utf-8")

gzipHeader :: HTTP.Header
gzipHeader = ("Content-Encoding", "gzip")

userRoleHeader :: IsString a => a
userRoleHeader = "x-hasura-role"

deprecatedAccessKeyHeader :: IsString a => a
deprecatedAccessKeyHeader = "x-hasura-access-key"

adminSecretHeader :: IsString a => a
adminSecretHeader = "x-hasura-admin-secret"

userIdHeader :: IsString a => a
userIdHeader = "x-hasura-user-id"

requestIdHeader :: IsString a => a
requestIdHeader = "x-request-id"

useBackendOnlyPermissionsHeader :: IsString a => a
useBackendOnlyPermissionsHeader = "x-hasura-use-backend-only-permissions"

getRequestHeader :: HTTP.HeaderName -> [HTTP.Header] -> Maybe B.ByteString
getRequestHeader hdrName hdrs = snd <$> mHeader
  where
    mHeader = find (\h -> fst h == hdrName) hdrs

parseStringAsBool :: String -> Either String Bool
parseStringAsBool t
  | map toLower t `elem` truthVals = Right True
  | map toLower t `elem` falseVals = Right False
  | otherwise = Left errMsg
  where
    truthVals = ["true", "t", "yes", "y"]
    falseVals = ["false", "f", "no", "n"]

    errMsg = " Not a valid boolean text. " ++ "True values are "
             ++ show truthVals ++ " and  False values are " ++ show falseVals
             ++ ". All values are case insensitive"

-- Get an env var during compile time
getValFromEnvOrScript :: String -> String -> Q (TExp String)
getValFromEnvOrScript n s = do
  maybeVal <- TH.runIO $ lookupEnv n
  case maybeVal of
    Just val -> [|| val ||]
    Nothing  -> runScript s

-- Run a shell script during compile time
runScript :: FilePath -> Q (TExp String)
runScript fp = do
  TH.addDependentFile fp
  fileContent <- TH.runIO $ TI.readFile fp
  (exitCode, stdOut, stdErr) <- TH.runIO $
    readProcessWithExitCode "/bin/sh" [] $ T.unpack fileContent
  when (exitCode /= ExitSuccess) $ fail $
    "Running shell script " ++ fp ++ " failed with exit code : "
    ++ show exitCode ++ " and with error : " ++ stdErr
  [|| stdOut ||]

-- | Quotes a regex using Template Haskell so syntax errors can be reported at compile-time.
quoteRegex :: TDFA.CompOption -> TDFA.ExecOption -> String -> Q (TExp TDFA.Regex)
quoteRegex compOption execOption regexText = do
  regex <- TDFA.parseRegex regexText `onLeft` (fail . show)
  [|| TDFA.patternToRegex regex compOption execOption ||]

fmapL :: (a -> a') -> Either a b -> Either a' b
fmapL fn (Left e) = Left (fn e)
fmapL _ (Right x) = pure x

generateFingerprint :: IO Text
generateFingerprint = UUID.toText <$> UUID.nextRandom

-- json representation of HTTP exception
httpExceptToJSON :: HC.HttpException -> Value
httpExceptToJSON e = case e of
  HC.HttpExceptionRequest x c ->
      let reqObj = object
            [ "host" .= bsToTxt (HC.host x)
            , "port" .= show (HC.port x)
            , "secure" .= HC.secure x
            , "path" .= bsToTxt (HC.path x)
            , "method" .= bsToTxt (HC.method x)
            , "proxy" .= (showProxy <$> HC.proxy x)
            , "redirectCount" .= show (HC.redirectCount x)
            , "responseTimeout" .= show (HC.responseTimeout x)
            , "requestVersion" .= show (HC.requestVersion x)
            ]
          msg = show c
      in object ["request" .= reqObj, "message" .= msg]
  _        -> toJSON $ show e
  where
    showProxy (HC.Proxy h p) =
      "host: " <> bsToTxt h <> " port: " <> tshow p

-- ignore the following request headers from the client
commonClientHeadersIgnored :: (IsString a) => [a]
commonClientHeadersIgnored =
  [ "Content-Length", "Content-MD5", "User-Agent", "Host"
  , "Origin", "Referer" , "Accept", "Accept-Encoding"
  , "Accept-Language", "Accept-Datetime"
  , "Cache-Control", "Connection", "DNT", "Content-Type"
  ]

commonResponseHeadersIgnored :: (IsString a) => [a]
commonResponseHeadersIgnored =
  [ "Server", "Transfer-Encoding", "Cache-Control"
  , "Access-Control-Allow-Credentials"
  , "Access-Control-Allow-Methods"
  , "Access-Control-Allow-Origin"
  , "Content-Type", "Content-Length"
  ]

isSessionVariable :: Text -> Bool
isSessionVariable = T.isPrefixOf "x-hasura-" . T.toLower

isReqUserId :: Text -> Bool
isReqUserId = (== "req_user_id") . T.toLower

mkClientHeadersForward :: [HTTP.Header] -> [HTTP.Header]
mkClientHeadersForward reqHeaders =
  xForwardedHeaders <> (filterSessionVariables . filterRequestHeaders) reqHeaders
  where
    filterSessionVariables = filter (\(k, _) -> not $ isSessionVariable $ bsToTxt $ CI.original k)
    xForwardedHeaders = flip mapMaybe reqHeaders $ \(hdrName, hdrValue) ->
      case hdrName of
        "Host"       -> Just ("X-Forwarded-Host", hdrValue)
        "User-Agent" -> Just ("X-Forwarded-User-Agent", hdrValue)
        _            -> Nothing

mkSetCookieHeaders :: Wreq.Response a -> HTTP.ResponseHeaders
mkSetCookieHeaders resp =
  map (headerName,) $ resp ^.. Wreq.responseHeader headerName
  where
    headerName = "Set-Cookie"

filterRequestHeaders :: [HTTP.Header] -> [HTTP.Header]
filterRequestHeaders =
  filterHeaders $ Set.fromList commonClientHeadersIgnored

-- ignore the following response headers from remote
filterResponseHeaders :: [HTTP.Header] -> [HTTP.Header]
filterResponseHeaders =
  filterHeaders $ Set.fromList commonResponseHeadersIgnored

filterHeaders :: Set.HashSet HTTP.HeaderName -> [HTTP.Header] -> [HTTP.Header]
filterHeaders list = filter (\(n, _) -> not $ n `Set.member` list)

hyphenate :: String -> String
hyphenate = u . applyFirst toLower
    where u []                 = []
          u (x:xs) | isUpper x = '-' : toLower x : hyphenate xs
                   | otherwise = x : u xs

applyFirst :: (Char -> Char) -> String -> String
applyFirst _ []     = []
applyFirst f [x]    = [f x]
applyFirst f (x:xs) = f x: xs

-- | The version integer
data APIVersion
  = VIVersion1
  | VIVersion2
  deriving (Show, Eq)

instance ToJSON APIVersion where
  toJSON VIVersion1 = toJSON @Int 1
  toJSON VIVersion2 = toJSON @Int 2

instance FromJSON APIVersion where
  parseJSON v = do
    verInt :: Int <- parseJSON v
    case verInt of
      1 -> return VIVersion1
      2 -> return VIVersion2
      i -> fail $ "expected 1 or 2, encountered " ++ show i

englishList :: Text -> NonEmpty Text -> Text
englishList joiner = \case
  one :| []    -> one
  one :| [two] -> one <> " " <> joiner <> " " <> two
  several      ->
    let final :| initials = NE.reverse several
    in commaSeparated (reverse initials) <> ", " <> joiner <> " " <> final

makeReasonMessage :: [a] -> (a -> Text) -> Text
makeReasonMessage errors showError =
  case errors of
    [singleError] -> "because " <> showError singleError
    _ -> "for the following reasons:\n" <> T.unlines
         (map (("  • " <>) . showError) errors)

executeJSONPath :: JSONPath -> Value -> IResult Value
executeJSONPath jsonPath = iparse (valueParser jsonPath)
  where
    valueParser path value = case path of
      []                      -> pure value
      (pathElement:remaining) -> parseWithPathElement pathElement value >>=
                                 ((<?> pathElement) . valueParser remaining)
      where
        parseWithPathElement = \case
                  Key k   -> withObject "Object" (.: k)
                  Index i -> withArray "Array" $
                             maybe (fail "Array index out of range") pure . (V.!? i)

sha1 :: BL.ByteString -> B.ByteString
sha1 = convert @_ @B.ByteString . Crypto.hashlazy @Crypto.SHA1

cryptoHash :: J.ToJSON a => a -> B.ByteString
cryptoHash = Base16.encode . sha1 . J.encode

readIsoLevel :: String -> Either String Q.TxIsolation
readIsoLevel isoS =
  case isoS of
    "read-committed"  -> return Q.ReadCommitted
    "repeatable-read" -> return Q.RepeatableRead
    "serializable"    -> return Q.Serializable
    _                 -> Left "Only expecting read-committed / repeatable-read / serializable"

parseConnLifeTime :: Maybe NominalDiffTime -> Maybe NominalDiffTime
parseConnLifeTime = \case
  Nothing -> Just 600  -- Not set by user; use the default timeout
  Just 0  -> Nothing   -- user wants to disable PG_CONN_LIFETIME
  Just n  -> Just n    -- user specified n seconds lifetime


-- | The environment variables that were moved to metadata. These environment
-- variables are available if a v1 hasura project is run an v2 hasura server.
-- These environment variables are marked as deprecated only when the v1 hasura
-- project is migrated to v2 project.
newtype EnvVarsMovedToMetadata
  = EnvVarsMovedToMetadata { unEnvVarsMovedToMetadata :: [String] }
  deriving (Show)

-- | These env vars are completely deprecated
newtype DeprecatedEnvVars
  = DeprecatedEnvVars { unDeprecatedEnvVars :: [String] }
  deriving (Show)

envVarsMovedToMetadata :: EnvVarsMovedToMetadata
envVarsMovedToMetadata = EnvVarsMovedToMetadata
  [ "HASURA_GRAPHQL_NO_OF_RETRIES"
  , "HASURA_GRAPHQL_PG_CONNECTIONS"
  , "HASURA_GRAPHQL_PG_TIMEOUT"
  , "HASURA_GRAPHQL_PG_CONN_LIFETIME"
  , "HASURA_GRAPHQL_PG_POOL_TIMEOUT"
  , "HASURA_GRAPHQL_USE_PREPARED_STATEMENTS"
  , "HASURA_GRAPHQL_TX_ISOLATION"
  , "HASURA_GRAPHQL_CONNECTIONS_PER_READ_REPLICA"
  ]

deprecatedEnvVars :: DeprecatedEnvVars
deprecatedEnvVars = DeprecatedEnvVars
  [ "HASURA_GRAPHQL_PG_STRIPES"
  , "HASURA_GRAPHQL_QUERY_PLAN_CACHE_SIZE"
  , "HASURA_GRAPHQL_STRIPES_PER_READ_REPLICA"
  ]
