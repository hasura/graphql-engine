module Hasura.HTTP
  ( wreqOptions
  , HttpException(..)
  , hdrsToText
  ) where

import           Control.Lens          hiding ((.=))
import           Hasura.Prelude

import qualified Data.Aeson            as J
import qualified Data.Text.Encoding    as T
import qualified Network.HTTP.Client   as HTTP
import qualified Network.HTTP.Types    as HTTP
import qualified Network.Wreq          as Wreq

import           Data.CaseInsensitive  (original)
import           Hasura.Server.Version (currentVersion)

hdrsToText :: [HTTP.Header] -> [(Text, Text)]
hdrsToText hdrs =
  [ (bsToTxt $ original hdrName, bsToTxt hdrVal)
  | (hdrName, hdrVal) <- hdrs
  ]

wreqOptions :: HTTP.Manager -> [HTTP.Header] -> Wreq.Options
wreqOptions manager hdrs =
  Wreq.defaults
  & Wreq.headers .~  contentType : userAgent : hdrs
  & Wreq.checkResponse ?~ (\_ _ -> return ())
  & Wreq.manager .~ Right manager
  where
    contentType = ("Content-Type", "application/json")
    userAgent   = ( "User-Agent"
                  , "hasura-graphql-engine/" <> T.encodeUtf8 currentVersion
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
