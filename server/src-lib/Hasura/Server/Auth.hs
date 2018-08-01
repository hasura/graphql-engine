{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
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
import           Hasura.Server.Logging

bsToTxt :: B.ByteString -> T.Text
bsToTxt = TE.decodeUtf8With TE.lenientDecode

data AuthMode
  = AMNoAuth
  | AMAccessKey !T.Text
  | AMAccessKeyAndHook !T.Text !T.Text
  deriving (Show, Eq)

userRoleHeader :: T.Text
userRoleHeader = "x-hasura-role"

userInfoFromWebhook
  :: (MonadIO m, MonadError QErr m)
  => (WebHookLog -> IO ())
  -> H.Manager
  -> T.Text
  -> [N.Header]
  -> m UserInfo
userInfoFromWebhook logger manager urlT reqHeaders = do
  let options =
        Wreq.defaults
        & Wreq.headers .~ filteredHeaders
        & Wreq.checkResponse ?~ (\_ _ -> return ())
        & Wreq.manager .~ Right manager

  res <- liftIO $ try $ Wreq.getWith options $ T.unpack urlT
  resp <- either logAndThrow return res
  let status = resp ^. Wreq.responseStatus

  validateStatus status
  rawHeaders <- decodeResp $ resp ^. Wreq.responseBody

  let headers = M.fromList [(T.toLower k, v) | (k, v) <- M.toList rawHeaders]

  case M.lookup userRoleHeader headers of
    Nothing -> throw500 "missing x-hasura-role key in webhook response: "
    Just v  -> return $ UserInfo (RoleName v) headers

  where
    logAndThrow err = do
      liftIO $ logger $ WebHookLog urlT err
      throw500 "Internal Server Error"

    filteredHeaders = flip filter reqHeaders $ \(n, _) ->
      n `notElem` ["Content-Length", "User-Agent", "Host", "Origin", "Referer"]

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
  => (WebHookLog -> IO ())
  -> H.Manager
  -> [N.Header]
  -> AuthMode
  -> m UserInfo
getUserInfo logger manager rawHeaders = \case

  AMNoAuth -> return userInfoFromHeaders

  AMAccessKey accKey ->
    case getHeader accessKeyHeader of
      Just givenAccKey -> userInfoWhenAccessKey accKey givenAccKey
      Nothing -> throw401 "x-hasura-access-key required, but not found"

  AMAccessKeyAndHook accKey hook ->
    maybe
      (userInfoFromWebhook logger manager hook rawHeaders)
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
