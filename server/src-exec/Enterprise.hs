module Main where

import           Control.Exception    (try)
import           Control.Lens
import           Data.Time.Clock      (getCurrentTime)
import           Debug.Trace
import           Hasura.Prelude
import           Hasura.Server.Init

import qualified Crypto.JWT           as Jose
import qualified Data.Aeson           as J
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T
import qualified Network.HTTP.Client  as HTTP
import qualified Network.HTTP.Types   as HTTP
import qualified Network.Wreq         as Wreq
import qualified Options.Applicative  as OptParse
import qualified System.Environment   as Sys

import qualified Hasura.App           as HGE
import qualified Hasura.HTTP          as HGE
import qualified Hasura.RQL.Types     as HGE
import qualified Hasura.Server.App    as HGE
import qualified Hasura.Server.Query  as HGE


newtype EnterpriseKey
  = EnterpriseKey { unEnterpriseKey :: Text }
  deriving (Show, Eq)

-- TODO: refactor OSS HGE to remove this Raw* BS
data EnterpriseOptionsG a
  = EnterpriseOptionsG
  { _eoEnterpriseKey :: !EnterpriseKey
  , _eoHgeOptions    :: !(HGEOptionsG a)
  }

type RawEnterpriseOptions = EnterpriseOptionsG RawServeOptions
type EnterpriseOptions = EnterpriseOptionsG ServeOptions

enterpriseKeyEnv :: (String, String)
enterpriseKeyEnv =
  ( "HASURA_GRAPHQL_ENTERPRISE_KEY"
  , "The enterprise that you have received"
  )

instance FromEnv EnterpriseKey where
  fromEnv = Right . EnterpriseKey . T.pack

parseEnterpriseKey :: OptParse.Parser (Maybe EnterpriseKey)
parseEnterpriseKey =  OptParse.optional $ EnterpriseKey <$>
  OptParse.strOption ( OptParse.long "enterprise-key" <>
                       OptParse.metavar "<THE ENTERPRISE KEY>" <>
                       OptParse.help (snd enterpriseKeyEnv)
                     )

mkEnterpriseOpts :: Maybe EnterpriseKey -> WithEnv EnterpriseKey
mkEnterpriseOpts key = do
  val <- withEnv key (fst enterpriseKeyEnv)
  case val of
    Nothing -> throwError "Enterprise key is required."
    Just v  -> return v

parseArgs :: IO EnterpriseOptions
parseArgs = do
  (mKey, rawHgeOpts) <- OptParse.execParser opts
  env <- Sys.getEnvironment
  let eitherOpts = runWithEnv env $ mkHGEOptions rawHgeOpts
      eEntKey = runWithEnv env $ mkEnterpriseOpts mKey
  key <- either HGE.printErrExit return eEntKey
  either HGE.printErrExit (return . EnterpriseOptionsG key) eitherOpts
  where
    opts = OptParse.info (OptParse.helper <*> entOpts)
           ( OptParse.fullDesc <>
             OptParse.header
             "Hasura GraphQL Engine (enterprise edition): Realtime GraphQL API over Postgres with access control" <>
             OptParse.footerDoc (Just mainCmdFooter)
           )
    entOpts = (,) <$> parseEnterpriseKey <*> hgeOpts
    hgeOpts = HGEOptionsG <$> parseRawConnInfo <*> HGE.parseHGECommand


data UserType
  = UTOwner
  | UTEditor
  | UTViewer
  deriving (Show, Eq)

data UserInfo
  = UserInfo
  { _uiUserType :: !UserType
  } deriving (Show, Eq)

userViewer :: UserInfo
userViewer = UserInfo UTViewer

txtToQErr :: Text -> HGE.QErr
txtToQErr = undefined

getJWKSet :: (MonadIO m, MonadError HGE.QErr m)
          => HTTP.Manager -> Text -> m Jose.JWKSet
getJWKSet manager url = do
  let options = HGE.wreqOptions manager []
  res  <- liftIO $ try $ Wreq.getWith options $ show url
  resp <- either logAndThrowHttp return res
  let status = resp ^. Wreq.responseStatus
      respBody = resp ^. Wreq.responseBody

  when (status ^. Wreq.statusCode /= 200) $ do
    --let urlT = T.pack $ show url
    let errMsg = "non-200 response on fetching JWK from: " <> url
    logAndThrow errMsg

  either error return . J.eitherDecode $ respBody
  where
    logAndThrow e = do
      liftIO . putStrLn . T.unpack $ "FATAL ERROR::: ==> " <> e
      throwError $ txtToQErr e

    logAndThrowHttp :: (MonadIO m, MonadError HGE.QErr m)
                    => HTTP.HttpException -> m a
    logAndThrowHttp err = do
      let errMsg = "error fetching JWK: " <> T.pack (show err)
      logAndThrow errMsg


jwtErrToQErr :: Jose.JWTError -> HGE.QErr
jwtErrToQErr = undefined

resolveUserInfo
  :: (MonadIO m, MonadError Jose.JWTError m)
  => Jose.JWKSet
  -> [HTTP.Header]
  -> m UserInfo
resolveUserInfo jwkSet headers = do
  let collabToken = snd <$> find ((==) "Hasura-Collaborator-Token" . fst) headers
  case collabToken of
    Nothing -> return userViewer
    Just token -> do
      jwt <- Jose.decodeCompact (BL.fromStrict token)
      currentTime <- liftIO getCurrentTime
      claims <- Jose.verifyClaimsAt config jwkSet currentTime jwt
      return $ UserInfo UTViewer
  where
    audCheck aud = aud == "hasura-auth"
    config = Jose.defaultJWTValidationSettings audCheck

authorizeRqlQuery :: UserInfo -> HGE.RQLQuery -> Bool
authorizeRqlQuery user query =
  case _uiUserType user of
    UTOwner  -> True
    UTEditor -> True
    UTViewer -> case query of
      HGE.RQGetInconsistentMetadata _  -> True
      HGE.RQDropInconsistentMetadata _ -> True
      HGE.RQExportMetadata _           -> True
      HGE.RQReloadMetadata _           -> True
      HGE.RQRunSql _                   -> True
      _                                -> False

authorizationMiddleware :: HTTP.Manager -> HGE.RQLQuery -> HGE.Handler ()
authorizationMiddleware manager query = do
  reqHeaders <- asks HGE.hcReqHeaders
  jwkSet <- getJWKSet manager "https://example.com"
  userInfo <- withExceptT jwtErrToQErr $ resolveUserInfo jwkSet reqHeaders
  traceM "inside auth middleware"
  traceM "inside rql query"
  if authorizeRqlQuery userInfo query
  then return ()
  else HGE.throw400 HGE.AccessDenied unauthorized
  where
    unauthorized = "{\"error\": \"uauthorized for this endpoint/query\"}"

main :: IO ()
main = do
  putStrLn "running enterprise edition YO!"
  EnterpriseOptionsG key hgeOpts <- parseArgs
  let HGEOptionsG rci command = hgeOpts
  (httpManager, loggerCtx, instanceId, logger, pgLogger) <- HGE.initialiseCtx
  HGE.handleCommand command rci httpManager loggerCtx instanceId logger
    pgLogger (Just $ authorizationMiddleware httpManager)
