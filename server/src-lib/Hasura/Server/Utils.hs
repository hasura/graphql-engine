module Hasura.Server.Utils where

import qualified Database.PG.Query.Connection as Q

import           Data.Aeson
import           Data.Char
import           Data.List                    (find)
import           Data.List.Split
import           Data.Time.Clock
import           Network.URI
import           System.Environment
import           System.Exit
import           System.Process

import qualified Data.ByteString              as B
import qualified Data.CaseInsensitive         as CI
import qualified Data.HashSet                 as Set
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import qualified Data.Text.IO                 as TI
import qualified Data.UUID                    as UUID
import qualified Data.UUID.V4                 as UUID
import qualified Language.Haskell.TH.Syntax   as TH
import qualified Network.HTTP.Client          as HC
import qualified Network.HTTP.Types           as HTTP
import qualified Text.Ginger                  as TG
import qualified Text.Regex.TDFA              as TDFA
import qualified Text.Regex.TDFA.ByteString   as TDFA

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

-- Parsing postgres database url
-- from: https://github.com/futurice/postgresql-simple-url/
parseDatabaseUrl :: String -> Maybe String -> Maybe Q.ConnInfo
parseDatabaseUrl databaseUrl opts = parseURI databaseUrl >>= uriToConnectInfo opts

uriToConnectInfo :: Maybe String -> URI -> Maybe Q.ConnInfo
uriToConnectInfo opts uri
  | uriScheme uri /= "postgres:" && uriScheme uri /= "postgresql:" = Nothing
  | otherwise = ($ Q.defaultConnInfo {Q.connOptions = opts}) <$> mkConnectInfo uri

type ConnectInfoChange = Q.ConnInfo -> Q.ConnInfo

mkConnectInfo :: URI -> Maybe ConnectInfoChange
mkConnectInfo uri = case uriPath uri of
                           ('/' : rest) | not (null rest) -> Just $ uriParameters uri
                           _                              -> Nothing

uriParameters :: URI -> ConnectInfoChange
uriParameters uri = (\info -> info { Q.connDatabase = tail $ uriPath uri }) . maybe id uriAuthParameters (uriAuthority uri)

dropLast :: [a] -> [a]
dropLast []     = []
dropLast [_]    = []
dropLast (x:xs) = x : dropLast xs

uriAuthParameters :: URIAuth -> ConnectInfoChange
uriAuthParameters uriAuth = port . host . auth
  where port = case uriPort uriAuth of
                 (':' : p) -> \info -> info { Q.connPort = read p }
                 _         -> id
        host = case uriRegName uriAuth of
                 h  -> \info -> info { Q.connHost = unEscapeString h }
        auth = case splitOn ":" (uriUserInfo uriAuth) of
                 [""]   -> id
                 [u]    -> \info -> info { Q.connUser = unEscapeString $ dropLast u }
                 [u, p] -> \info -> info { Q.connUser = unEscapeString u, Q.connPassword = unEscapeString $ dropLast p }
                 _      -> id

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

-- Ginger Templating
type GingerTmplt = TG.Template TG.SourcePos

parseGingerTmplt :: TG.Source -> Either String GingerTmplt
parseGingerTmplt src = either parseE Right res
  where
    res = runIdentity $ TG.parseGinger' parserOptions src
    parserOptions = TG.mkParserOptions resolver
    resolver = const $ return Nothing
    parseE e = Left $ TG.formatParserError (Just "") e

renderGingerTmplt :: (ToJSON a) => a -> GingerTmplt -> T.Text
renderGingerTmplt v = TG.easyRender (toJSON v)

-- find duplicates
duplicates :: Ord a => [a] -> [a]
duplicates = mapMaybe greaterThanOne . group . sort
  where
    greaterThanOne l = bool Nothing (Just $ head l) $ length l > 1

_1 :: (a, b, c) -> a
_1 (x, _, _) = x

_2 :: (a, b, c) -> b
_2 (_, y, _) = y

_3 :: (a, b, c) -> c
_3 (_, _, z) = z

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
