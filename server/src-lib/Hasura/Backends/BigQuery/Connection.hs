{-# LANGUAGE NumericUnderscores #-}

module Hasura.Backends.BigQuery.Connection where


import           Control.Concurrent.MVar
import           Control.Exception
import           Crypto.Hash.Algorithms (SHA256(..))
import           Crypto.PubKey.RSA.PKCS15 (signSafer)
import           Crypto.PubKey.RSA.Types as Cry (Error)
import           Data.Bifunctor (bimap)
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import qualified Data.Aeson as J
import qualified Data.Aeson.Casing as J
import qualified Data.Aeson.TH as J
import qualified Data.Environment as Env
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Time.Clock
import           Hasura.Prelude
import           Hasura.Backends.BigQuery.Source
import qualified Hasura.Backends.MSSQL.Connection as MSSQLConn (getEnv)
import           Hasura.RQL.Types.Error
import           Network.HTTP.Simple
import           Network.HTTP.Types


newtype Scope
  = Scope { unScope :: T.Text }
  deriving (Show, Eq, IsString)


data GoogleAccessTokenRequest = GoogleAccessTokenRequest
  { _gatrGrantType :: !Text
  , _gatrAssertion :: !Text
  } deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 5 J.snakeCase){J.omitNothingFields=False} ''GoogleAccessTokenRequest)

mkTokenRequest :: Text -> GoogleAccessTokenRequest
mkTokenRequest = GoogleAccessTokenRequest "urn:ietf:params:oauth:grant-type:jwt-bearer"


data TokenProblem
  = BearerTokenDecodeProblem TE.UnicodeException
  | BearerTokenSignsaferProblem Cry.Error
  | TokenFetchProblem JSONException
  | TokenRequestNonOK Status
  deriving (Show)
instance Exception TokenProblem

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
  QErrM m =>
  Env.Environment ->
  ConfigurationInput ->
  m Text
resolveConfigurationInput env = \case
  FromYaml s -> pure s
  FromEnv v -> MSSQLConn.getEnv env v


resolveConfigurationInputs ::
  QErrM m =>
  Env.Environment ->
  ConfigurationInputs ->
  m [Text]
resolveConfigurationInputs env = \case
  FromYamls a -> pure a
  FromEnvs v -> filter (not . T.null) . T.splitOn "," <$> MSSQLConn.getEnv env v


getAccessToken :: MonadIO m => ServiceAccount -> m (Either TokenProblem TokenResp)
getAccessToken sa = do
  eJwt <- encodeBearerJWT sa ["https://www.googleapis.com/auth/cloud-platform"]
  case eJwt of
    Left tokenProblem -> pure . Left $ tokenProblem
    Right jwt ->
      case TE.decodeUtf8' jwt of
        Left unicodeEx -> pure . Left . BearerTokenDecodeProblem $ unicodeEx
        Right assertion -> do
          tokenFetchResponse :: Response (Either JSONException TokenResp) <-
            httpJSONEither $
              setRequestBodyJSON (mkTokenRequest assertion) $
              parseRequest_ ("POST " <> tokenURL)
          if getResponseStatusCode tokenFetchResponse /= 200
            then
              pure . Left . TokenRequestNonOK . getResponseStatus $ tokenFetchResponse
            else
              case getResponseBody tokenFetchResponse of
                Left jsonEx -> pure . Left . TokenFetchProblem $ jsonEx
                Right tr@TokenResp{_trExpiresAt} -> do
                  -- We add the current POSIXTime and store the POSIX "moment" at
                  -- which this token will expire, so that at the site where
                  -- we need to check if a token is nearing expiry, we only
                  -- need to compare it with the _then_ "current" POSIXTime.
                  expiresAt <- (fromIntegral _trExpiresAt +) <$> liftIO getPOSIXTime
                  pure . Right $ tr { _trExpiresAt = truncate expiresAt }
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
          | x == '='  -> bs'
          | otherwise -> bs
    encodeBearerJWT :: ( MonadIO m ) => ServiceAccount -> [Scope] -> m (Either TokenProblem BS.ByteString)
    encodeBearerJWT ServiceAccount{..} scopes = do
      inp <- mkSigInput . truncate <$> liftIO getPOSIXTime
      signRes <- liftIO $ signSafer (Just SHA256) (unPKey _saPrivateKey) inp
      case signRes of
        Left e -> pure . Left . BearerTokenSignsaferProblem $ e
        Right sig -> pure . Right $ inp <> "." <> truncateEquals (base64 sig)
      where
        mkSigInput :: Int -> BS.ByteString
        mkSigInput n = header <> "." <> payload
          where
            header = b64EncodeJ $ J.object
              [ "alg" J..= ("RS256" :: T.Text)
              , "typ" J..= ("JWT"   :: T.Text)
              ]
            payload = b64EncodeJ $ J.object [ "aud"   J..= tokenURL , "scope" J..= T.intercalate " " (map unScope scopes)
              , "iat"   J..= n
              , "exp"   J..= (n + maxTokenLifetime)
              , "iss"   J..= _saClientEmail
              ]


getServiceAccount :: MonadIO m => FilePath -> m (Either ServiceAccountProblem ServiceAccount)
getServiceAccount serviceAccountFilePath =
  bimap ServiceAccountFileDecodeProblem id . J.eitherDecode' <$> liftIO (BL.readFile serviceAccountFilePath)


-- | Get a usable token. If the token has expired refresh it.
getUsableToken :: MonadIO m => BigQuerySourceConfig -> m (Either TokenProblem TokenResp)
getUsableToken BigQuerySourceConfig{_scServiceAccount, _scAccessTokenMVar} =
  liftIO $ modifyMVar _scAccessTokenMVar $ \mTokenResp -> do
    case mTokenResp of
      Nothing -> do
        refreshedToken <- getAccessToken _scServiceAccount
        case refreshedToken of
          Left e -> pure (Nothing, Left e)
          Right t -> pure (Just t, Right t)
      Just t@TokenResp{_trAccessToken, _trExpiresAt} -> do
        pt <- liftIO $ getPOSIXTime
        if (pt >= fromIntegral _trExpiresAt - (10 :: NominalDiffTime)) -- when posix-time is greater than expires-at-minus-threshold
          then do
            refreshedToken' <- getAccessToken _scServiceAccount
            case refreshedToken' of
              Left e -> pure (Just t, Left e)
              Right t' -> pure (Just t', Right t')
          else pure (Just t, Right t)


data BigQueryProblem
  = TokenProblem TokenProblem
  deriving (Show)


runBigQuery ::
  (MonadIO m) =>
  BigQuerySourceConfig ->
  Request ->
  m (Either BigQueryProblem (Response BL.ByteString))
runBigQuery sc req = do
  eToken <- getUsableToken sc
  case eToken of
    Left e -> pure . Left . TokenProblem $ e
    Right TokenResp{_trAccessToken, _trExpiresAt} -> do
      let req' = setRequestHeader "Authorization" ["Bearer " <> (TE.encodeUtf8 . coerce) _trAccessToken] req
      -- TODO: Make this catch the HTTP exceptions
      Right <$> httpLBS req'
