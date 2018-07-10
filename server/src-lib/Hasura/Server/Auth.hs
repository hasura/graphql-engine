{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Hasura.Server.Auth
  ( getUserInfo
  , AuthMode(..)
  ) where

import           Control.Exception        (try)
import           Control.Lens
import           Data.Aeson
import           Data.CaseInsensitive     (CI (..), original)

import qualified Data.ByteString          as B
import qualified Data.HashMap.Strict      as M
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Network.HTTP.Client      as H
import qualified Network.HTTP.Types       as N
import qualified Network.Wreq             as Wreq

import           Hasura.Prelude
import           Hasura.RQL.Types

bsToTxt :: B.ByteString -> T.Text
bsToTxt = TE.decodeUtf8With TE.lenientDecode

data AuthMode
  = AMNoAuth
  | AMAccessKey !T.Text
  | AMAccessKeyAndHook !T.Text !T.Text
  deriving (Show, Eq)

httpToQErr :: H.HttpException -> QErr
httpToQErr e = case e of
  H.InvalidUrlException _ _ -> err500 Unexpected "Invalid Webhook Url"
  H.HttpExceptionRequest _ H.ConnectionTimeout -> err500 Unexpected
    "Webhook : Connection timeout"
  H.HttpExceptionRequest _ H.ResponseTimeout -> err500 Unexpected
    "Webhook : Response timeout"
  _ -> err500 Unexpected "HTTP Exception from Webhook"

userRoleHeader :: T.Text
userRoleHeader = "x-hasura-role"

userInfoFromWebhook
  :: (MonadIO m, MonadError QErr m)
  => H.Manager
  -> T.Text
  -> [N.Header]
  -> m UserInfo
userInfoFromWebhook manager urlT reqHeaders = do
  let options =
        Wreq.defaults
        & Wreq.headers .~ filteredHeaders
        & Wreq.checkResponse ?~ (\_ _ -> return ())
        & Wreq.manager .~ Right manager

  res <- liftIO $ try $ Wreq.getWith options $ T.unpack urlT
  resp <- either (throwError . httpToQErr) return res
  let status = resp ^. Wreq.responseStatus

  validateStatus status
  rawHeaders <- decodeResp $ resp ^. Wreq.responseBody

  let headers = M.fromList [(T.toLower k, v) | (k, v) <- M.toList rawHeaders]

  case M.lookup userRoleHeader headers of
    Nothing -> throw500 "missing x-hasura-role key in webhook response: "
    Just v  -> return $ UserInfo (RoleName v) headers

  where
    filteredHeaders = flip filter reqHeaders $ \(n, _) ->
      n /= "Content-Length" && n /= "User-Agent" && n /= "Host"
      && n /= "Origin" && n /= "Referer"

    validateStatus statusCode
      | statusCode == N.status200 = return ()
      | statusCode == N.status401 =
        throw401 "Authentication hook unauthorized this request"
      | otherwise =
        throw500 "Invalid response from authorization hook"

    decodeResp bs = case eitherDecode bs of
      Left e -> throw500 $
        "Invalid response from authorization hook: " <> T.pack e
      Right a -> return a

accessKeyHeader :: T.Text
accessKeyHeader = "x-hasura-access-key"

getUserInfo
  :: (MonadIO m, MonadError QErr m)
  => [N.Header]
  -> AuthMode
  -> m UserInfo
getUserInfo rawHeaders = \case

  AMNoAuth -> return userInfoFromHeaders

  AMAccessKey accKey ->
    case getHeader accessKeyHeader of
      Just givenAccKey -> userInfoWhenAccessKey accKey givenAccKey
      Nothing -> throw401 "x-hasura-access-key required, but not found"

  AMAccessKeyAndHook accKey hook ->
    maybe
      (userInfoFromWebhook undefined hook rawHeaders)
      (userInfoWhenAccessKey accKey) $
      getHeader accessKeyHeader

  where

    headers =
      M.fromList $ filter (T.isPrefixOf "x-hasura-" . fst) $
      flip map rawHeaders $
      \(hdrName, hdrVal) ->
        (T.toLower $ bsToTxt $ original hdrName, bsToTxt hdrVal)

    getHeader h = M.lookup h headers

    userInfoFromHeaders =
      case M.lookup "x-hasura-role" headers of
        Just v  -> UserInfo (RoleName v) headers
        Nothing -> UserInfo adminRole M.empty

    userInfoWhenAccessKey key reqKey = do
      when (reqKey /= key) $ throw401 "invalid x-hasura-access-key"
      return userInfoFromHeaders
