module Main where

import           Hasura.Prelude
import           Hasura.Server.Init

import qualified Data.Aeson          as J
import qualified Data.Text           as T
import qualified Network.Wai         as Wai
import qualified Options.Applicative as OptParse
import qualified System.Environment  as Sys

import qualified App                 as HGE
import qualified Hasura.Server.Query as HGE


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


data UserInfo = UserInfo

authorizeRqlQuery :: UserInfo -> HGE.RQLQuery -> Bool
authorizeRqlQuery user query = True

authorizationMiddleware :: Wai.Middleware
authorizationMiddleware app req sendResp = do
  reqBody <- liftIO $ Wai.strictRequestBody req
  let rqlQuery = J.decode reqBody
      ui = UserInfo
  case rqlQuery of
    Nothing -> runApp
    Just query ->
      if authorizeRqlQuery ui query
      then runApp
      else error "unauthorized"
  where
    -- run the application normally
    runApp = app req sendResp

main :: IO ()
main = do
  putStrLn "running enterprise edition YO!"
  EnterpriseOptionsG key hgeOpts <- parseArgs
  let HGEOptionsG rci command = hgeOpts
  (httpManager, loggerCtx, instanceId, logger, pgLogger) <- HGE.initialiseCtx
  HGE.handleCommand command rci httpManager loggerCtx instanceId logger pgLogger (Just authorizationMiddleware)
