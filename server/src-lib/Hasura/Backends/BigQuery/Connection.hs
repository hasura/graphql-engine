module Hasura.Backends.BigQuery.Connection
  ( BigQueryProblem,
    resolveConfigurationInput,
    resolveConfigurationInputs,
    resolveConfigurationJson,
    initConnection,
    runBigQuery,
  )
where

import Control.Concurrent.MVar
import Control.Exception
import Control.Retry qualified as Retry
import Crypto.Hash.Algorithms (SHA256 (..))
import Crypto.PubKey.RSA.PKCS15 (signSafer)
import Crypto.PubKey.RSA.Types as Cry (Error)
import Data.Aeson qualified as J
import Data.Aeson.Casing qualified as J
import Data.ByteArray.Encoding qualified as BAE
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as BL
import Data.Environment qualified as Env
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Data.Time.Clock
import Data.Time.Clock.POSIX (getPOSIXTime)
import Hasura.Backends.BigQuery.Source
import Hasura.Backends.MSSQL.Connection qualified as MSSQLConn (getEnv)
import Hasura.Base.Error
import Hasura.Prelude
import Network.HTTP.Client
import Network.HTTP.Simple
import Network.HTTP.Types

newtype Scope = Scope {unScope :: T.Text}
  deriving (Show, Eq, IsString)

data GoogleAccessTokenRequest = GoogleAccessTokenRequest
  { _gatrGrantType :: Text,
    _gatrAssertion :: Text
  }
  deriving (Show, Generic, Eq)

instance J.FromJSON GoogleAccessTokenRequest where
  parseJSON = J.genericParseJSON (J.aesonDrop 5 J.snakeCase) {J.omitNothingFields = False}

instance J.ToJSON GoogleAccessTokenRequest where
  toJSON = J.genericToJSON (J.aesonDrop 5 J.snakeCase) {J.omitNothingFields = False}
  toEncoding = J.genericToEncoding (J.aesonDrop 5 J.snakeCase) {J.omitNothingFields = False}

mkTokenRequest :: Text -> GoogleAccessTokenRequest
mkTokenRequest = GoogleAccessTokenRequest "urn:ietf:params:oauth:grant-type:jwt-bearer"

data TokenProblem
  = BearerTokenDecodeProblem TE.UnicodeException
  | BearerTokenSignsaferProblem Cry.Error
  | TokenFetchProblem JSONException
  | TokenRequestNonOK Status
  deriving (Show, Generic)

tokenProblemMessage :: TokenProblem -> Text
tokenProblemMessage = \case
  BearerTokenDecodeProblem _ -> "Cannot decode bearer token"
  BearerTokenSignsaferProblem _ -> "Cannot sign bearer token"
  TokenFetchProblem _ -> "JSON exception occurred while fetching token"
  TokenRequestNonOK status -> "HTTP request to fetch token failed with status " <> tshow status

data ServiceAccountProblem
  = ServiceAccountFileDecodeProblem String
  deriving (Show)

instance Exception ServiceAccountProblem

resolveConfigurationJson ::
  (QErrM m, J.FromJSON a) =>
  Env.Environment ->
  ConfigurationJSON a -> -- REVIEW: Can this be made polymorphic?
  m (Either String a)
resolveConfigurationJson env = \case
  FromYamlJSON s -> pure . Right $ s
  FromEnvJSON v -> do
    fileContents <- MSSQLConn.getEnv env v
    case J.eitherDecode . BL.fromStrict . TE.encodeUtf8 $ fileContents of
      Left e -> pure . Left $ e
      Right sa -> pure . Right $ sa

resolveConfigurationInput ::
  (QErrM m) =>
  Env.Environment ->
  ConfigurationInput ->
  m Text
resolveConfigurationInput env = \case
  FromYaml s -> pure s
  FromEnv v -> MSSQLConn.getEnv env v

resolveConfigurationInputs ::
  (QErrM m) =>
  Env.Environment ->
  ConfigurationInputs ->
  m [Text]
resolveConfigurationInputs env = \case
  FromYamls a -> pure a
  FromEnvs v -> filter (not . T.null) . T.splitOn "," <$> MSSQLConn.getEnv env v

initConnection :: (MonadIO m) => ServiceAccount -> BigQueryProjectId -> Maybe RetryOptions -> m BigQueryConnection
initConnection _bqServiceAccount _bqProjectId _bqRetryOptions = do
  _bqAccessTokenMVar <- liftIO $ newMVar Nothing -- `runBigQuery` initializes the token
  pure BigQueryConnection {..}

getAccessToken :: (MonadIO m) => ServiceAccount -> m (Either TokenProblem TokenResp)
getAccessToken sa = do
  eJwt <- encodeBearerJWT sa ["https://www.googleapis.com/auth/cloud-platform"]
  case eJwt of
    Left tokenProblem -> pure . Left $ tokenProblem
    Right jwt ->
      case TE.decodeUtf8' jwt of
        Left unicodeEx -> pure . Left . BearerTokenDecodeProblem $ unicodeEx
        Right assertion -> do
          tokenFetchResponse :: Response (Either JSONException TokenResp) <-
            httpJSONEither
              $ setRequestBodyJSON (mkTokenRequest assertion)
              $ parseRequest_ ("POST " <> tokenURL)
          if getResponseStatusCode tokenFetchResponse /= 200
            then pure . Left . TokenRequestNonOK . getResponseStatus $ tokenFetchResponse
            else case getResponseBody tokenFetchResponse of
              Left jsonEx -> pure . Left . TokenFetchProblem $ jsonEx
              Right tr@TokenResp {_trExpiresAt} -> do
                -- We add the current POSIXTime and store the POSIX "moment" at
                -- which this token will expire, so that at the site where
                -- we need to check if a token is nearing expiry, we only
                -- need to compare it with the _then_ "current" POSIXTime.
                expiresAt <- (fromIntegral _trExpiresAt +) <$> liftIO getPOSIXTime
                pure . Right $ tr {_trExpiresAt = truncate expiresAt}
  where
    -- TODO: use jose for jwt encoding
    b64EncodeJ :: (J.ToJSON a) => a -> BS.ByteString
    b64EncodeJ = base64 . BL.toStrict . J.encode
    base64 :: BS.ByteString -> BS.ByteString
    base64 = BAE.convertToBase BAE.Base64URLUnpadded
    tokenURL :: String
    tokenURL = "https://www.googleapis.com/oauth2/v4/token"
    maxTokenLifetime :: Int
    maxTokenLifetime = 3600
    truncateEquals :: B8.ByteString -> B8.ByteString
    truncateEquals bs =
      case B8.unsnoc bs of
        Nothing -> mempty
        Just (bs', x)
          | x == '=' -> bs'
          | otherwise -> bs
    encodeBearerJWT :: (MonadIO m) => ServiceAccount -> [Scope] -> m (Either TokenProblem BS.ByteString)
    encodeBearerJWT ServiceAccount {..} scopes = do
      inp <- mkSigInput . truncate <$> liftIO getPOSIXTime
      signRes <- liftIO $ signSafer (Just SHA256) (unPKey _saPrivateKey) inp
      case signRes of
        Left e -> pure . Left . BearerTokenSignsaferProblem $ e
        Right sig -> pure . Right $ inp <> "." <> truncateEquals (base64 sig)
      where
        mkSigInput :: Int -> BS.ByteString
        mkSigInput n = header <> "." <> payload
          where
            header =
              b64EncodeJ
                $ J.object
                  [ "alg" J..= ("RS256" :: T.Text),
                    "typ" J..= ("JWT" :: T.Text)
                  ]
            payload =
              b64EncodeJ
                $ J.object
                  [ "aud" J..= tokenURL,
                    "scope" J..= T.intercalate " " (map unScope scopes),
                    "iat" J..= n,
                    "exp" J..= (n + maxTokenLifetime),
                    "iss" J..= _saClientEmail
                  ]

-- | Get a usable token. If the token has expired refresh it.
getUsableToken :: (MonadIO m) => BigQueryConnection -> m (Either TokenProblem TokenResp)
getUsableToken BigQueryConnection {_bqServiceAccount, _bqAccessTokenMVar} =
  liftIO
    $ modifyMVar _bqAccessTokenMVar
    $ \mTokenResp -> do
      case mTokenResp of
        Nothing -> do
          refreshedToken <- getAccessToken _bqServiceAccount
          case refreshedToken of
            Left e -> pure (Nothing, Left e)
            Right t -> pure (Just t, Right t)
        Just t@TokenResp {_trAccessToken, _trExpiresAt} -> do
          pt <- liftIO $ getPOSIXTime
          if (pt >= fromIntegral _trExpiresAt - (10 :: NominalDiffTime)) -- when posix-time is greater than expires-at-minus-threshold
            then do
              refreshedToken' <- getAccessToken _bqServiceAccount
              case refreshedToken' of
                Left e -> pure (Just t, Left e)
                Right t' -> pure (Just t', Right t')
            else pure (Just t, Right t)

data BigQueryProblem
  = TokenProblem TokenProblem
  deriving (Show, Generic)

instance J.ToJSON BigQueryProblem where
  toJSON (TokenProblem tokenProblem) =
    J.object ["token_problem" J..= tokenProblemMessage tokenProblem]

runBigQuery ::
  (MonadIO m) =>
  BigQueryConnection ->
  Request ->
  m (Either BigQueryProblem (Response BL.ByteString))
runBigQuery conn req = do
  eToken <- getUsableToken conn
  case eToken of
    Left e -> pure . Left . TokenProblem $ e
    Right TokenResp {_trAccessToken, _trExpiresAt} -> do
      let req' = setRequestHeader "Authorization" ["Bearer " <> (TE.encodeUtf8 . coerce) _trAccessToken] req
      -- TODO: Make this catch the HTTP exceptions
      Right <$> case _bqRetryOptions conn of
        Just opts -> withGoogleApiRetries opts (httpLBS req')
        Nothing -> httpLBS req'

-- | Uses up to specified number retries for Google API requests with the specified base delay, uses full jitter backoff,
-- see https://aws.amazon.com/ru/blogs/architecture/exponential-backoff-and-jitter/
-- HTTP statuses for transient errors were taken from
-- https://github.com/googleapis/python-api-core/blob/34ebdcc251d4f3d7d496e8e0b78847645a06650b/google/api_core/retry.py#L112-L115
withGoogleApiRetries :: (MonadIO m) => RetryOptions -> m (Response body) -> m (Response body)
withGoogleApiRetries RetryOptions {..} action =
  Retry.retrying retryPolicy checkStatus (const action)
  where
    baseDelay = fromInteger . diffTimeToMicroSeconds $ microseconds _retryBaseDelay
    retryPolicy = Retry.fullJitterBackoff baseDelay <> Retry.limitRetries _retryNumRetries
    checkStatus _ resp =
      pure $ responseStatus resp `elem` [tooManyRequests429, internalServerError500, serviceUnavailable503]
