{-# LANGUAGE TemplateHaskellQuotes #-}

module Hasura.Server.Utils
  ( APIVersion (..),
    DeprecatedEnvVars (..),
    EnvVarsMovedToMetadata (..),
    cryptoHash,
    deprecatedEnvVars,
    englishList,
    envVarsMovedToMetadata,
    executeJSONPath,
    fmapL,
    generateFingerprint,
    httpExceptToJSON,
    isReqUserId,
    makeReasonMessage,
    parseConnLifeTime,
    quoteRegex,
    readIsoLevel,
    sessionVariablePrefix,
  )
where

import Crypto.Hash qualified as Crypto
import Data.Aeson
import Data.Aeson qualified as J
import Data.Aeson.Types
import Data.ByteArray (convert)
import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BL
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
import Language.Haskell.TH.Syntax qualified as TH
import Network.HTTP.Client qualified as HC
import Text.Regex.TDFA qualified as TDFA
import Text.Regex.TDFA.ReadRegex qualified as TDFA
import Text.Regex.TDFA.TDFA qualified as TDFA

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

sessionVariablePrefix :: Text
sessionVariablePrefix = "x-hasura-"

isReqUserId :: Text -> Bool
isReqUserId = (== "req_user_id") . T.toLower

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
