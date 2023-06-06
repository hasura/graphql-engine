{-# LANGUAGE TemplateHaskellQuotes #-}

module Hasura.Server.Utils
  ( APIVersion (..),
    DeprecatedEnvVars (..),
    EnvVarsMovedToMetadata (..),
    adminSecretHeader,
    commonClientHeadersIgnored,
    cryptoHash,
    deprecatedAccessKeyHeader,
    deprecatedEnvVars,
    englishList,
    envVarsMovedToMetadata,
    executeJSONPath,
    filterHeaders,
    fmapL,
    generateFingerprint,
    getRequestHeader,
    gzipHeader,
    httpExceptToJSON,
    isReqUserId,
    isSessionVariable,
    jsonHeader,
    makeReasonMessage,
    mkClientHeadersForward,
    mkSetCookieHeaders,
    parseConnLifeTime,
    parseStringAsBool,
    quoteRegex,
    readIsoLevel,
    redactSensitiveHeader,
    requestIdHeader,
    sqlHeader,
    useBackendOnlyPermissionsHeader,
    userIdHeader,
    userRoleHeader,
    contentLengthHeader,
    sessionVariablePrefix,
  )
where

import Control.Lens ((^..))
import Crypto.Hash qualified as Crypto
import Data.Aeson
import Data.Aeson qualified as J
import Data.Aeson.Types
import Data.ByteArray (convert)
import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.Char
import Data.HashSet qualified as Set
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Extended
import Data.Time
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as V
import Database.PG.Query qualified as PG
import Hasura.Base.Instances ()
import Hasura.Prelude
import Hasura.RQL.Types.Session (isSessionVariable)
import Language.Haskell.TH.Syntax qualified as TH
import Network.HTTP.Client qualified as HC
import Network.HTTP.Types qualified as HTTP
import Network.Wreq qualified as Wreq
import Text.Regex.TDFA qualified as TDFA
import Text.Regex.TDFA.ReadRegex qualified as TDFA
import Text.Regex.TDFA.TDFA qualified as TDFA

jsonHeader :: HTTP.Header
jsonHeader = ("Content-Type", "application/json; charset=utf-8")

sqlHeader :: HTTP.Header
sqlHeader = ("Content-Type", "application/sql; charset=utf-8")

gzipHeader :: HTTP.Header
gzipHeader = ("Content-Encoding", "gzip")

userRoleHeader :: (IsString a) => a
userRoleHeader = "x-hasura-role"

deprecatedAccessKeyHeader :: (IsString a) => a
deprecatedAccessKeyHeader = "x-hasura-access-key"

adminSecretHeader :: (IsString a) => a
adminSecretHeader = "x-hasura-admin-secret"

userIdHeader :: (IsString a) => a
userIdHeader = "x-hasura-user-id"

requestIdHeader :: (IsString a) => a
requestIdHeader = "x-request-id"

contentLengthHeader :: (IsString a) => a
contentLengthHeader = "Content-Length"

useBackendOnlyPermissionsHeader :: (IsString a) => a
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

    errMsg =
      " Not a valid boolean text. "
        ++ "True values are "
        ++ show truthVals
        ++ " and  False values are "
        ++ show falseVals
        ++ ". All values are case insensitive"

{- NOTE: Something like this is not safe in the presence of caching. The only
    way for metaprogramming to depend on some external data and recompile
    properly is via addDependentFile and to include that file in the
    extra-source-files in the cabal file (see: https://github.com/haskell/cabal/issues/4746).
    Leaving this here commented in order to document that fact and also in case
    there's a way forward in the future.

-- Run a shell script during compile time
runScript :: FilePath -> Q (TExp String)
runScript file = do
  fp <- makeRelativeToProject file
  TH.addDependentFile fp
  fileContent <- TH.runIO $ TI.readFile fp
  (exitCode, stdOut, stdErr) <-
    TH.runIO $
      readProcessWithExitCode "/bin/sh" [] $ T.unpack fileContent
  when (exitCode /= ExitSuccess) $
    fail $
      "Running shell script " ++ fp ++ " failed with exit code: "
        ++ show exitCode
        ++ " and with error: "
        ++ stdErr
  [||stdOut||]
-}

-- | Quotes a regex using Template Haskell so syntax errors can be reported at compile-time.
quoteRegex :: TDFA.CompOption -> TDFA.ExecOption -> String -> TH.Code TH.Q TDFA.Regex
quoteRegex compOption execOption regexText =
  (TDFA.parseRegex regexText `onLeft` (fail . show)) `TH.bindCode` \regex ->
    [||TDFA.patternToRegex regex compOption execOption||]

fmapL :: (a -> a') -> Either a b -> Either a' b
fmapL fn (Left e) = Left (fn e)
fmapL _ (Right x) = pure x

generateFingerprint :: IO Text
generateFingerprint = UUID.toText <$> UUID.nextRandom

-- json representation of HTTP exception
httpExceptToJSON :: HC.HttpException -> Value
httpExceptToJSON e = case e of
  HC.HttpExceptionRequest x c ->
    let reqObj =
          object
            [ "host" .= bsToTxt (HC.host x),
              "port" .= show (HC.port x),
              "secure" .= HC.secure x,
              "path" .= bsToTxt (HC.path x),
              "method" .= bsToTxt (HC.method x),
              "proxy" .= (showProxy <$> HC.proxy x),
              "redirectCount" .= show (HC.redirectCount x),
              "responseTimeout" .= show (HC.responseTimeout x),
              "requestVersion" .= show (HC.requestVersion x)
            ]
        msg = show c
     in object ["request" .= reqObj, "message" .= msg]
  _ -> toJSON $ show e
  where
    showProxy (HC.Proxy h p) =
      "host: " <> bsToTxt h <> " port: " <> tshow p

-- ignore the following request headers from the client
commonClientHeadersIgnored :: (IsString a) => [a]
commonClientHeadersIgnored =
  [ "Content-Length",
    "Content-MD5",
    "User-Agent",
    "Host",
    "Origin",
    "Referer",
    "Accept",
    "Accept-Encoding",
    "Accept-Language",
    "Accept-Datetime",
    "Cache-Control",
    "Connection",
    "DNT",
    "Content-Type"
  ]

sessionVariablePrefix :: Text
sessionVariablePrefix = "x-hasura-"

isReqUserId :: Text -> Bool
isReqUserId = (== "req_user_id") . T.toLower

mkClientHeadersForward :: [HTTP.Header] -> [HTTP.Header]
mkClientHeadersForward reqHeaders =
  xForwardedHeaders <> (filterSessionVariables . filterRequestHeaders) reqHeaders
  where
    filterSessionVariables = filter (\(k, _) -> not $ isSessionVariable $ bsToTxt $ CI.original k)
    xForwardedHeaders = flip mapMaybe reqHeaders $ \(hdrName, hdrValue) ->
      case hdrName of
        "Host" -> Just ("X-Forwarded-Host", hdrValue)
        "User-Agent" -> Just ("X-Forwarded-User-Agent", hdrValue)
        "Origin" -> Just ("X-Forwarded-Origin", hdrValue)
        _ -> Nothing

mkSetCookieHeaders :: Wreq.Response a -> HTTP.ResponseHeaders
mkSetCookieHeaders resp =
  map (headerName,) $ resp ^.. Wreq.responseHeader headerName
  where
    headerName = "Set-Cookie"

filterRequestHeaders :: [HTTP.Header] -> [HTTP.Header]
filterRequestHeaders =
  filterHeaders $ Set.fromList commonClientHeadersIgnored

filterHeaders :: Set.HashSet HTTP.HeaderName -> [HTTP.Header] -> [HTTP.Header]
filterHeaders list = filter (\(n, _) -> not $ n `Set.member` list)

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
  one :| [] -> one
  one :| [two] -> one <> " " <> joiner <> " " <> two
  several ->
    let final :| initials = NE.reverse several
     in commaSeparated (reverse initials) <> ", " <> joiner <> " " <> final

makeReasonMessage :: [a] -> (a -> Text) -> Text
makeReasonMessage errors showError =
  case errors of
    [singleError] -> "because " <> showError singleError
    _ ->
      "for the following reasons:\n"
        <> T.unlines
          (map (("  • " <>) . showError) errors)

executeJSONPath :: JSONPath -> Value -> IResult Value
executeJSONPath jsonPath = iparse (valueParser jsonPath)
  where
    valueParser path value = case path of
      [] -> pure value
      (pathElement : remaining) ->
        parseWithPathElement pathElement value
          >>= ((<?> pathElement) . valueParser remaining)
      where
        parseWithPathElement = \case
          Key k -> withObject "Object" (.: k)
          Index i ->
            withArray "Array"
              $ maybe (fail "Array index out of range") pure
              . (V.!? i)

sha1 :: BL.ByteString -> B.ByteString
sha1 = convert @_ @B.ByteString . Crypto.hashlazy @Crypto.SHA1

cryptoHash :: (J.ToJSON a) => a -> B.ByteString
cryptoHash = Base16.encode . sha1 . J.encode

readIsoLevel :: String -> Either String PG.TxIsolation
readIsoLevel isoS =
  case isoS of
    "read-committed" -> return PG.ReadCommitted
    "repeatable-read" -> return PG.RepeatableRead
    "serializable" -> return PG.Serializable
    _ -> Left "Only expecting read-committed / repeatable-read / serializable"

parseConnLifeTime :: Maybe NominalDiffTime -> Maybe NominalDiffTime
parseConnLifeTime = \case
  Nothing -> Just 600 -- Not set by user; use the default timeout
  Just 0 -> Nothing -- user wants to disable PG_CONN_LIFETIME
  Just n -> Just n -- user specified n seconds lifetime

-- | The environment variables that were moved to metadata. These environment
-- variables are available if a v1 hasura project is run an v2 hasura server.
-- These environment variables are marked as deprecated only when the v1 hasura
-- project is migrated to v2 project.
newtype EnvVarsMovedToMetadata = EnvVarsMovedToMetadata {unEnvVarsMovedToMetadata :: [String]}
  deriving (Show)

-- | These env vars are completely deprecated
newtype DeprecatedEnvVars = DeprecatedEnvVars {unDeprecatedEnvVars :: [String]}
  deriving (Show)

envVarsMovedToMetadata :: EnvVarsMovedToMetadata
envVarsMovedToMetadata =
  EnvVarsMovedToMetadata
    [ "HASURA_GRAPHQL_NO_OF_RETRIES",
      "HASURA_GRAPHQL_PG_CONNECTIONS",
      "HASURA_GRAPHQL_PG_TIMEOUT",
      "HASURA_GRAPHQL_PG_CONN_LIFETIME",
      "HASURA_GRAPHQL_PG_POOL_TIMEOUT",
      "HASURA_GRAPHQL_USE_PREPARED_STATEMENTS",
      "HASURA_GRAPHQL_TX_ISOLATION",
      "HASURA_GRAPHQL_CONNECTIONS_PER_READ_REPLICA"
    ]

deprecatedEnvVars :: DeprecatedEnvVars
deprecatedEnvVars =
  DeprecatedEnvVars
    [ "HASURA_GRAPHQL_PG_STRIPES",
      "HASURA_GRAPHQL_QUERY_PLAN_CACHE_SIZE",
      "HASURA_GRAPHQL_STRIPES_PER_READ_REPLICA"
    ]

sensitiveHeaders :: HashSet HTTP.HeaderName
sensitiveHeaders =
  Set.fromList
    [ "Authorization",
      "Cookie"
    ]

redactSensitiveHeader :: HTTP.Header -> HTTP.Header
redactSensitiveHeader (headerName, value) = (headerName, if headerName `elem` sensitiveHeaders then "<REDACTED>" else value)
