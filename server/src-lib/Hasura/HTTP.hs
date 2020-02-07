module Hasura.HTTP
  ( wreqOptions
  , HttpException(..)
  , hdrsToText
  , addDefaultHeaders
  ) where

import           Hasura.Prelude

import           Control.Lens          hiding ((.=))
import           Data.CaseInsensitive  (original)
import           Data.Text.Conversions (UTF8 (..), convertText)

import qualified Data.Aeson            as J
import qualified Network.HTTP.Client   as HTTP
import qualified Network.HTTP.Types    as HTTP
import qualified Network.Wreq          as Wreq

import           Hasura.Server.Version (HasVersion, currentVersion)

hdrsToText :: [HTTP.Header] -> [(Text, Text)]
hdrsToText hdrs =
  [ (bsToTxt $ original hdrName, bsToTxt hdrVal)
  | (hdrName, hdrVal) <- hdrs
  ]

wreqOptions :: HasVersion => HTTP.Manager -> [HTTP.Header] -> Wreq.Options
wreqOptions manager hdrs =
  Wreq.defaults
  & Wreq.headers .~  addDefaultHeaders hdrs
  & Wreq.checkResponse ?~ (\_ _ -> return ())
  & Wreq.manager .~ Right manager

-- Adds defaults headers overwriting any existing ones
addDefaultHeaders :: HasVersion => [HTTP.Header] -> [HTTP.Header]
addDefaultHeaders hdrs = defaultHeaders <> rmDefaultHeaders hdrs
  where
    rmDefaultHeaders = filter (not . isDefaultHeader)

isDefaultHeader :: HasVersion => HTTP.Header -> Bool
isDefaultHeader (hdrName, _) = hdrName `elem` (map fst defaultHeaders)

defaultHeaders :: HasVersion => [HTTP.Header]
defaultHeaders = [contentType, userAgent]
  where
    contentType = ("Content-Type", "application/json")
    userAgent   = ( "User-Agent"
                  , "hasura-graphql-engine/" <> unUTF8 (convertText currentVersion)
                  )

newtype HttpException
  = HttpException
  { unHttpException :: HTTP.HttpException }
  deriving (Show)

instance J.ToJSON HttpException where
  toJSON = \case
    (HttpException (HTTP.InvalidUrlException _ e)) ->
      J.object [ "type" J..= ("invalid_url" :: Text)
               , "message" J..= e
               ]
    (HttpException (HTTP.HttpExceptionRequest _ cont)) ->
      J.object [ "type" J..= ("http_exception" :: Text)
               , "message" J..= show cont
               ]
