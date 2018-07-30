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
import           Data.Time.Clock            (getCurrentTime)
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


processJwt
  :: ( MonadIO m
     , MonadError QErr m)
  => SharedSecret
  -> HTTP.RequestHeaders
  -> m UserInfo
processJwt secret headers = do

  -- try to parse JWT token from Authorization header
  let mAuthzHeader = find (\h -> fst h == CI.mk "Authorization") headers
  (_, authzHeader) <- maybe missingAuthzHeader return mAuthzHeader
  let tokenParts = BLC.words $ BL.fromStrict authzHeader
  when (length tokenParts /= 2) malformedAuthzHeader

  -- verify the JWT
  let jwt = tokenParts !! 1
  claims <- liftJWTError invalidJWTError $ verifyJwt secret jwt

  -- filter only x-hasura claims
  let claimsMap = Map.filterWithKey (\k _ -> T.isPrefixOf "x-hasura-" k) $
        claims ^. unregisteredClaims

  -- transform the map of text:aeson-value -> text:text
  metadataWithRole <- decodeJSON $ A.Object claimsMap

  -- throw error if role is not in claims
  let mRole = Map.lookup userRoleHeader metadataWithRole
  role <- maybe missingRoleClaim return mRole

  -- delete the x-hasura-role key from this map
  let metadata = Map.delete userRoleHeader metadataWithRole

  return $ UserInfo (RoleName role) metadata

  where
    liftJWTError :: (MonadError e' m) => (e -> e') -> ExceptT e m a -> m a
    liftJWTError ef action = do
      res <- runExceptT action
      either (throwError . ef) return res

    decodeJSON val = case A.fromJSON val of
      A.Error e   -> throw400 JWTInvalidClaims ("x-hasura-* claims: " <> T.pack e)
      A.Success a -> return a

    invalidJWTError e =
      err400 JWTInvalid $ "Could not verify JWT: " <> T.pack (show e)

    missingRoleClaim =
      throw400 JWTRoleClaimMissing "Your JWT claim should contain x-hasura-role"
    malformedAuthzHeader =
      throw400 InvalidHeaders "Malformed Authorization header"
    missingAuthzHeader =
      throw400 InvalidHeaders "Missing Authorization header in JWT authentication mode"


verifyJwt
  :: ( MonadError JWTError m
     , MonadIO m
     )
  => SharedSecret
  -> RawJWT
  -> m ClaimsSet
verifyJwt secret rawJWT = do
  let secret' = BL.fromStrict . TE.encodeUtf8 $ secret
  -- this will work with HS256 algo, on HS384 and higher there will
  -- JWSInvalidSignature error
  -- https://github.com/frasertweedale/hs-jose/issues/46
  when (BL.length secret' < 32) $ throwError $ JWSError KeySizeTooSmall
  let jwkey = fromOctets secret' -- turn raw secret into symmetric JWK
  jwt <- decodeCompact rawJWT    -- decode JWT
  t <- liftIO getCurrentTime
  verifyClaimsAt config jwkey t jwt
  where
    audCheck = const True        -- should be a proper audience check
    config = defaultJWTValidationSettings audCheck
