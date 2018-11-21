{-# LANGUAGE OverloadedStrings #-}

module Hasura.HTTP.Utils where

import           Control.Lens
import           Hasura.Prelude

import qualified Data.Text.Encoding    as T
import qualified Network.HTTP.Client   as HTTP
import qualified Network.HTTP.Types    as HTTP
import qualified Network.Wreq          as Wreq

import           Hasura.Server.Version (currentVersion)


wreqOptions :: HTTP.Manager -> [HTTP.Header] -> Wreq.Options
wreqOptions manager hdrs =
  Wreq.defaults
  & Wreq.headers .~  contentType : userAgent : hdrs
  & Wreq.checkResponse ?~ (\_ _ -> return ())
  & Wreq.manager .~ Right manager
  where
    contentType = ("Content-Type", "application/json")
    userAgent   = ( "User-Agent"
                  , "hasura/graphql-engine-" <> T.encodeUtf8 currentVersion
                  )
