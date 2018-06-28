{-# LANGUAGE OverloadedStrings #-}

module Hasura.Server.Utils where

import qualified Database.PG.Query.Connection as Q

import           Data.List.Split
import           Network.URI

import qualified Data.Text                    as T
import           Hasura.Prelude

dropAndSnakeCase :: T.Text -> T.Text
dropAndSnakeCase = T.drop 9 . toSnakeCase . T.toLower

toSnakeCase :: T.Text -> T.Text
toSnakeCase = T.pack . map change . T.unpack
  where
    change '-' = '_'
    change c   = c

isXHasuraTxt :: T.Text -> Bool
isXHasuraTxt = T.isInfixOf "x-hasura-" . T.toLower

userRoleHeader :: T.Text
userRoleHeader = "x-hasura-role"

accessKeyHeader :: T.Text
accessKeyHeader = "x-hasura-access-key"

-- Parsing postgres database url
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
                 h  -> \info -> info { Q.connHost = h }
        auth = case splitOn ":" (uriUserInfo uriAuth) of
                 [""]   -> id
                 [u]    -> \info -> info { Q.connUser = dropLast u }
                 [u, p] -> \info -> info { Q.connUser = u, Q.connPassword = dropLast p }
                 _      -> id
