{-# LANGUAGE TypeApplications #-}
module Hasura.Server.Utils where

import           Data.Aeson
import           Data.Char
import           Data.List                  (find)
import           Data.Time.Clock
import           Language.Haskell.TH.Syntax (Lift)
import           System.Environment
import           System.Exit
import           System.Process

import qualified Data.ByteString            as B
import qualified Data.CaseInsensitive       as CI
import qualified Data.HashSet               as Set
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.IO               as TI
import qualified Data.UUID                  as UUID
import qualified Data.UUID.V4               as UUID
import qualified Language.Haskell.TH.Syntax as TH
import qualified Network.HTTP.Client        as HC
import qualified Network.HTTP.Types         as HTTP
import qualified Text.Regex.TDFA            as TDFA
import qualified Text.Regex.TDFA.ByteString as TDFA

import           Hasura.Prelude

newtype RequestId
  = RequestId { unRequestId :: Text }
  deriving (Show, Eq, ToJSON, FromJSON)

jsonHeader :: (T.Text, T.Text)
jsonHeader = ("Content-Type", "application/json; charset=utf-8")

sqlHeader :: (T.Text, T.Text)
sqlHeader = ("Content-Type", "application/sql; charset=utf-8")

htmlHeader :: (T.Text, T.Text)
htmlHeader = ("Content-Type", "text/html; charset=utf-8")

gzipHeader :: (T.Text, T.Text)
gzipHeader = ("Content-Encoding", "gzip")

brHeader :: (T.Text, T.Text)
brHeader = ("Content-Encoding", "br")

userRoleHeader :: T.Text
userRoleHeader = "x-hasura-role"

deprecatedAccessKeyHeader :: T.Text
deprecatedAccessKeyHeader = "x-hasura-access-key"

adminSecretHeader :: T.Text
adminSecretHeader = "x-hasura-admin-secret"

userIdHeader :: T.Text
userIdHeader = "x-hasura-user-id"

requestIdHeader :: T.Text
requestIdHeader = "x-request-id"

getRequestHeader :: B.ByteString -> [HTTP.Header] -> Maybe B.ByteString
getRequestHeader hdrName hdrs = snd <$> mHeader
  where
    mHeader = find (\h -> fst h == CI.mk hdrName) hdrs

getRequestId :: (MonadIO m) => [HTTP.Header] -> m RequestId
getRequestId headers =
  -- generate a request id for every request if the client has not sent it
  case getRequestHeader (txtToBs requestIdHeader) headers  of
    Nothing    -> RequestId <$> liftIO generateFingerprint
    Just reqId -> return $ RequestId $ bsToTxt reqId

-- Get an env var during compile time
getValFromEnvOrScript :: String -> String -> TH.Q TH.Exp
getValFromEnvOrScript n s = do
  maybeVal <- TH.runIO $ lookupEnv n
  case maybeVal of
    Just val -> TH.lift val
    Nothing  -> runScript s

-- Run a shell script during compile time
runScript :: FilePath -> TH.Q TH.Exp
runScript fp = do
  TH.addDependentFile fp
  fileContent <- TH.runIO $ TI.readFile fp
  (exitCode, stdOut, stdErr) <- TH.runIO $
    readProcessWithExitCode "/bin/sh" [] $ T.unpack fileContent
  when (exitCode /= ExitSuccess) $ fail $
    "Running shell script " ++ fp ++ " failed with exit code : "
    ++ show exitCode ++ " and with error : " ++ stdErr
  TH.lift stdOut

-- find duplicates
duplicates :: Ord a => [a] -> [a]
duplicates = mapMaybe greaterThanOne . group . sort
  where
    greaterThanOne l = bool Nothing (Just $ head l) $ length l > 1

-- regex related
matchRegex :: B.ByteString -> Bool -> T.Text -> Either String Bool
matchRegex regex caseSensitive src =
  fmap (`TDFA.match` TE.encodeUtf8 src) compiledRegexE
  where
    compOpt = TDFA.defaultCompOpt
      { TDFA.caseSensitive = caseSensitive
      , TDFA.multiline = True
      , TDFA.lastStarGreedy = True
      }
    execOption = TDFA.defaultExecOpt {TDFA.captureGroups = False}
    compiledRegexE = TDFA.compile compOpt execOption regex


fmapL :: (a -> a') -> Either a b -> Either a' b
fmapL fn (Left e) = Left (fn e)
fmapL _ (Right x) = pure x

-- diff time to micro seconds
diffTimeToMicro :: NominalDiffTime -> Int
diffTimeToMicro diff =
  floor (realToFrac diff :: Double) * aSecond
  where
    aSecond = 1000 * 1000

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
      "host: " <> bsToTxt h <> " port: " <> T.pack (show p)

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
  deriving (Show, Eq, Lift)

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

makeReasonMessage :: [a] -> (a -> Text) -> Text
makeReasonMessage errors showError =
  case errors of
    [singleError] -> "because " <> showError singleError
    _ -> "for the following reasons:\n" <> T.unlines
         (map (("  • " <>) . showError) errors)
