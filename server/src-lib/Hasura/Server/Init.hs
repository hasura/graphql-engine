{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasura.Server.Init where

import qualified Database.PG.Query    as Q

import           Network.URI
import           Options.Applicative
import           System.Exit          (exitFailure)
import           Text.Read            (readMaybe)

import qualified Data.Text            as T

import           Hasura.Prelude
import           Hasura.RQL.DDL.Utils

data InitError
  = InitError !String
  deriving (Show, Eq)

instance Q.FromPGConnErr InitError where
  fromPGConnErr = InitError . show

instance Q.FromPGTxErr InitError where
  fromPGTxErr = InitError . show

type AccessKey = T.Text

initErrExit :: (Show e) => e -> IO a
initErrExit e = print e >> exitFailure

-- clear the hdb_views schema
initStateTx :: Q.Tx ()
initStateTx = Q.unitQ clearHdbViews () False

data RawConnInfo =
  RawConnInfo
  { connHost     :: !(Maybe String)
  , connPort     :: !(Maybe Int)
  , connUser     :: !(Maybe String)
  , connPassword :: !String
  , connUrl      :: !(Maybe String)
  , connDatabase :: !(Maybe String)
  , connOptions  :: !(Maybe String)
  } deriving (Eq, Read, Show)

parseRawConnInfo :: Parser RawConnInfo
parseRawConnInfo =
  RawConnInfo
  <$> optional (strOption ( long "host" <>
                  metavar "HOST" <>
                  help "Postgres server host" ))
  <*> optional (option auto ( long "port" <>
                  short 'p' <>
                  metavar "PORT" <>
                  help "Postgres server port" ))
  <*> optional (strOption ( long "user" <>
                  short 'u' <>
                  metavar "USER" <>
                  help "Database user name" ))
  <*> strOption ( long "password" <>
                  short 'p' <>
                  metavar "PASSWORD" <>
                  value "" <>
                  help "Password of the user" )
  <*> optional (strOption ( long "database-url" <>
                  metavar "DataBase-URL" <>
                  help "Postgres database URL"))
  <*> optional (strOption ( long "dbname" <>
                  short 'd' <>
                  metavar "NAME" <>
                  help "Database name to connect to" ))
  <*> pure Nothing

connInfoErrModifier :: String -> String
connInfoErrModifier s = "Fatal Error : " ++ s

mkConnInfo :: RawConnInfo -> Either String Q.ConnInfo
mkConnInfo (RawConnInfo mHost mPort mUser pass mURL mDB opts) =
  case (mHost, mPort, mUser, mDB, mURL) of
    (Just host, Just port, Just user, Just db, Nothing) ->
      return $ Q.ConnInfo host port user pass db opts
    _ -> throwError "expecting all options for host, port, user and db"
    -- (_, _, _, _, Just dbURL) -> parseURL dbURL mUser pass mPort mDB opts
    -- _ -> throwError
    --   "Invalid options. Expecting database connection params or database-url"

-- parseURL
--   :: String
--   -> Maybe String
--   -> String
--   -> Maybe Int
--   -> Maybe String
--   -> Maybe String
--   -> Either String Q.ConnInfo
-- parseURL urlS mUser password mPort mDB opts =
--   case parseURI urlS of
--     Nothing  -> throwError "database-url is not valid"
--     Just url -> do
--       let uriAuth = uriAuthority url
--           p = uriPath url
--       parseURIAuth p uriAuth
--   where
--     parseURIAuth _ Nothing = throwError "Authorization info not found in database-url"
--     parseURIAuth pt (Just authInfo) = do
--       mDbName <- parsePath pt
--       dbName <- case mDbName of
--         Nothing -> case mDB of
--           Nothing -> throwError
--             "DB name not found in database-url. Expecting --dbname or -d flag"
--           Just d -> return d
--         Just d -> return d

--       (user, pass) <- parseUserInfo $ uriUserInfo authInfo
--       let host = uriRegName authInfo
--       port <- case parsePort $ uriPort authInfo of
--             Just p -> return p
--             Nothing -> case mPort of
--               Nothing -> throwError
--                 "Port not found in datbase-url. Expecting --port or -p flag"
--               Just p  -> return p
--       return $ Q.ConnInfo host port user pass dbName opts

--     parsePort "" = Nothing
--     parsePort s  = readMaybe $ tail s

--     parsePath "" = return Nothing
--     parsePath "/" = return Nothing
--     parsePath s = do
--       let l = T.splitOn "/" $ T.pack s
--       case l of
--         [] -> return Nothing
--         [_] -> return Nothing
--         [_, b] -> return $ Just $ T.unpack b
--         _      -> throwError $
--           "Invalid URL path. Expecting /<db-name> in URL path."
--           ++ " Occured " ++ s

--     parseUserInfo ui =
      -- let userI = init ui
      --     (user, pass) = break (==':') userI
      --     mNewPass = if null pass || (pass == ":")
      --                  then Nothing
      --                  else Just $ tail pass
      -- newUser <- case user of
      --   "" -> case mUser of
      --     Nothing -> throwError
      --       "User not found in database-url. Expecting --user or -u flag"
      --     Just u  -> return u
      --   u -> return u
      -- newPass <- case mNewPass of
      --       Nothing -> return password
      --       Just p  -> return p
      -- return (newUser, newPass)

parseConnInfo :: Parser Q.ConnInfo
parseConnInfo =
  Q.ConnInfo
  <$> strOption ( long "host" <>
                  metavar "HOST" <>
                  help "Postgres server host" )
  <*> option auto ( long "port" <>
                  short 'p' <>
                  metavar "PORT" <>
                  help "Postgres server port" )
  <*> strOption ( long "user" <>
                  short 'u' <>
                  metavar "USER" <>
                  help "Database user name" )
  <*> strOption ( long "password" <>
                  short 'p' <>
                  metavar "PASSWORD" <>
                  value "" <>
                  help "Password of the user" )
  <*> strOption ( long "dbname" <>
                  short 'd' <>
                  metavar "NAME" <>
                  help "Database name to connect to" )
  <*> pure Nothing

readIsoLevel :: String -> Either String Q.TxIsolation
readIsoLevel isoS =
  case isoS of
    "read-comitted" -> return Q.ReadCommitted
    "repeatable-read" -> return Q.RepeatableRead
    "serializable" -> return Q.ReadCommitted
    _ -> Left "Only expecting read-comitted / repeatable-read / serializable"

parseTxIsolation :: Parser Q.TxIsolation
parseTxIsolation =
  option (eitherReader readIsoLevel) ( long "tx-iso" <>
                  short 'i' <>
                  value Q.ReadCommitted <>
                  metavar "TXISO" <>
                  help "transaction isolation. read-committed / repeatable-read / serializable" )

parseRootDir :: Parser (Maybe String)
parseRootDir =
  optional $ strOption ( long "root-dir" <>
               metavar "STATIC-DIR" <>
               help "this static dir is served at / and takes precedence over all routes" )

parseConnParams :: Parser Q.ConnParams
parseConnParams =
  Q.ConnParams
  <$> option auto ( long "stripes" <>
               short 's' <>
               metavar "NO OF STRIPES" <>
               value 1 <>
               help "Number of stripes" )
  <*> option auto ( long "connections" <>
               short 'c' <>
               metavar "NO OF CONNS" <>
               value 50 <>
               help "Number of conns that need to be opened to Postgres" )
  <*> option auto ( long "timeout" <>
               short 'c' <>
               metavar "SECONDS" <>
               value 180 <>
               help "Each connection's idle time before it is closed" )

parseServerPort :: Parser Int
parseServerPort =
  option auto ( long "server-port" <>
           metavar "PORT" <>
           value 8080 <>
           showDefault <>
           help "Port on which graphql-engine should be served")

parseAccessKey :: Parser (Maybe AccessKey)
parseAccessKey = optional $ strOption ( long "access-key" <>
                             metavar "SECRET ACCESS KEY" <>
                             help "Secret access key, required to access this instance"
                           )

data CorsConfig
  = CorsConfig
  { ccDomain   :: !T.Text
  , ccDisabled :: !Bool
  } deriving (Show, Eq)

parseCorsConfig :: Parser CorsConfig
parseCorsConfig =
  CorsConfig
  <$> strOption ( long "cors-domain" <>
                  metavar "CORS DOMAIN" <>
                  value "*" <>
                  showDefault <>
                  help "The domain, including scheme and port, to allow CORS for"
                )
  <*> switch ( long "disable-cors" <>
               help "Disable CORS handling"
             )

parseWebHook :: Parser (Maybe T.Text)
parseWebHook = optional $ strOption ( long "auth-hook" <>
                            metavar "AUTHENTICATION WEB HOOK" <>
                            help "The authentication webhook, required to authenticate requests"
                          )
