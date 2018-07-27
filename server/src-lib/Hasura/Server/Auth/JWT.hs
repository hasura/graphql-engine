{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Hasura.Server.Auth.JWT
  ( processJwt
  , RawJWT
  , SharedSecret
  ) where

import           Control.Lens
import           Crypto.JWT
import           Data.List                  (find)
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Utils        (userRoleHeader)

import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.CaseInsensitive       as CI
import qualified Data.HashMap.Strict        as Map
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Network.HTTP.Types         as HTTP


type SharedSecret = T.Text
type RawJWT = BL.ByteString

processJwt :: (MonadIO m, MonadError QErr m) => SharedSecret -> HTTP.RequestHeaders -> m UserInfo
processJwt secret headers = do
  let mAuthzHeader = find (\h -> fst h == CI.mk "Authorization") headers
  (_, authzHeader) <- maybe (throw400 InvalidHeaders "cannot find Authorization header") return mAuthzHeader
  let tokenParts = BLC.words $ BL.fromStrict authzHeader
  when (length tokenParts /= 2) $ throw400 InvalidHeaders "malformed Authorization header"
  let jwt = tokenParts !! 1
  eClaims <- liftIO $ runExceptT $ verifyJwt secret jwt
  claims  <- liftJWTError eClaims
  ourMap <- mapM parseJSONToTxt $ claims ^. unregisteredClaims
  let mRole = Map.lookup userRoleHeader ourMap
  role <- maybe (throw400 InvalidHeaders "role info missing") return mRole
  let restUserInfo = Map.filterWithKey (\_ -> T.isPrefixOf "x-hasura-") $ Map.delete userRoleHeader ourMap
  return $ UserInfo (RoleName role) restUserInfo

  where
    liftJWTError :: (MonadError QErr m) => Either JWTError ClaimsSet -> m ClaimsSet
    liftJWTError =
      either (\e -> throw400 InvalidHeaders $ "Invalid JWT: " <> T.pack (show e)) return

    parseJSONToTxt (A.String t) = return t
    parseJSONToTxt _ = throw400 InvalidHeaders "x-hasura-* values cannot be JSON. should be string"


verifyJwt :: SharedSecret -> RawJWT -> ExceptT JWTError IO ClaimsSet
verifyJwt secret rawJWT = do
  let secret' = BL.fromStrict . TE.encodeUtf8 $ secret
  when (BL.length secret' < 32) $ throwError $ JWSError KeySizeTooSmall
  let jwkey = fromOctets secret'  -- turn raw secret into symmetric JWK
  jwt <- decodeCompact rawJWT    -- decode JWT
  verifyClaims config jwkey jwt
  where
    audCheck = const True      -- should be a proper audience check
    config = defaultJWTValidationSettings audCheck
