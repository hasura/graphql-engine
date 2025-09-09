{-# LANGUAGE UndecidableInstances #-}
-- TODO:
-- In the use of ‘unregisteredClaims’ (imported from Crypto.JWT):
--     Deprecated: "use a sub-type"
{-# OPTIONS_GHC -Wno-deprecations #-}

module Hasura.Server.AuthSpec (spec) where

import Control.Concurrent.Extended (ForkableMonadIO)
import Control.Lens hiding ((.=))
import Control.Monad.Catch (MonadMask)
import Crypto.JOSE.JWK qualified as Jose
import Crypto.JWT qualified as JWT
import Data.Aeson ((.=))
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.Either (isRight)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as Set
import Data.Parser.JSONPath
import Data.Text qualified as T
import Hasura.Authentication.Role (RoleName, adminRoleName, mkRoleName)
import Hasura.Authentication.Session
import Hasura.Authentication.User (UserAdminSecret (..), UserInfo (..), UserRoleBuild (..), mkUserInfo)
import Hasura.Base.Error
import Hasura.GraphQL.Transport.HTTP.Protocol (ReqsText)
import Hasura.Logging (Logger (..))
import Hasura.Prelude
import Hasura.Server.Auth hiding (getUserInfoWithExpTime, processJwt)
import Hasura.Server.Auth.JWT hiding (processJwt)
import Hasura.Server.Utils ()
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types qualified as HTTP
import Test.Hspec

spec :: Spec
spec = do
  getUserInfoWithExpTimeTests
  setupAuthModeTests
  parseClaimsMapTests
  parseCognitoJwksTests
  filterOutUnknownKeyTypesTests

allowedRolesClaimText :: K.Key
allowedRolesClaimText = fromSessionVariable allowedRolesClaim

defaultRoleClaimText :: K.Key
defaultRoleClaimText = fromSessionVariable defaultRoleClaim

-- Unit test the core of our authentication code. This doesn't test the details
-- of resolving roles from JWT or webhook.
-- TODO(swann): does this need to also test passing
getUserInfoWithExpTimeTests :: Spec
getUserInfoWithExpTimeTests = describe "getUserInfo" $ do
  ---- FUNCTION UNDER TEST:
  let gqlUserInfoWithExpTime ::
        J.Object ->
        [HTTP.Header] ->
        AuthMode ->
        Maybe ReqsText ->
        IO (Either Code RoleName)
      gqlUserInfoWithExpTime claims rawHeaders authMode =
        runExceptT
          . withExceptT qeCode -- just look at Code for purposes of tests
          . fmap _uiRole -- just look at RoleName for purposes of tests
          . fmap (view _1) -- disregard Nothing expiration
          . getUserInfoWithExpTime_ userInfoFromAuthHook processJwt () () rawHeaders authMode
        where
          -- mock authorization callbacks:
          userInfoFromAuthHook _ _ _hook _reqHeaders _optionalReqs = do
            (,Nothing,[]) <$> _UserInfo "hook"
            where
              -- we don't care about details here; we'll just check role name in tests:
              _UserInfo nm =
                mkUserInfo
                  (URBFromSessionVariablesFallback $ mkRoleNameE nm)
                  UAdminSecretNotSent
                  (mkSessionVariablesHeaders mempty)
          processAuthZHeader _jwtCtx _authzHeader = pure (parseObjectAsClaims claims, Nothing)
          processJwt = processJwt_ processAuthZHeader tokenIssuer (const JHAuthorization)

  let getUserInfoWithExpTime ::
        J.Object ->
        [HTTP.Header] ->
        AuthMode ->
        IO (Either Code RoleName)
      getUserInfoWithExpTime o claims authMode = gqlUserInfoWithExpTime o claims authMode Nothing

  let ourUnauthRole = mkRoleNameE "an0nymous"

  describe "started without admin secret" $ do
    it "gives admin by default" $ do
      mode <- setupAuthMode'E Nothing Nothing mempty Nothing
      getUserInfoWithExpTime mempty mempty mode
        `shouldReturn` Right adminRoleName
    it "allows any requested role" $ do
      mode <- setupAuthMode'E Nothing Nothing mempty Nothing
      getUserInfoWithExpTime mempty [sessionVariableToHeader userRoleHeader "r00t"] mode
        `shouldReturn` Right (mkRoleNameE "r00t")

  describe "admin secret only" $ do
    describe "unauth role NOT set" $ do
      mode <- runIO $ setupAuthMode'E (Just $ Set.singleton $ hashAdminSecret "secret") Nothing mempty Nothing

      it "accepts when admin secret matches" $ do
        getUserInfoWithExpTime mempty [sessionVariableToHeader adminSecretHeader "secret"] mode
          `shouldReturn` Right adminRoleName
      it "accepts when admin secret matches, honoring role request" $ do
        getUserInfoWithExpTime mempty [sessionVariableToHeader adminSecretHeader "secret", sessionVariableToHeader userRoleHeader "r00t"] mode
          `shouldReturn` Right (mkRoleNameE "r00t")

      it "rejects when doesn't match" $ do
        getUserInfoWithExpTime mempty [sessionVariableToHeader adminSecretHeader "bad secret"] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [sessionVariableToHeader adminSecretHeader ""] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [("blah", "blah"), sessionVariableToHeader adminSecretHeader "blah"] mode
          `shouldReturn` Left AccessDenied
        -- with deprecated header:
        getUserInfoWithExpTime mempty [sessionVariableToHeader deprecatedAccessKeyHeader "bad secret"] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [sessionVariableToHeader deprecatedAccessKeyHeader ""] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [("blah", "blah"), sessionVariableToHeader deprecatedAccessKeyHeader "blah"] mode
          `shouldReturn` Left AccessDenied

      it "rejects when no secret sent, since no fallback unauth role" $ do
        getUserInfoWithExpTime mempty mempty mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [sessionVariableToHeader userRoleHeader "r00t", sessionVariableToHeader userRoleHeader "admin"] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [sessionVariableToHeader userRoleHeader "r00t"] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [("blah", "blah")] mode
          `shouldReturn` Left AccessDenied

    describe "unauth role set" $ do
      mode <-
        runIO
          $ setupAuthMode'E (Just $ Set.singleton $ hashAdminSecret "secret") Nothing mempty (Just ourUnauthRole)
      it "accepts when admin secret matches" $ do
        getUserInfoWithExpTime mempty [sessionVariableToHeader adminSecretHeader "secret"] mode
          `shouldReturn` Right adminRoleName
        getUserInfoWithExpTime mempty [sessionVariableToHeader adminSecretHeader "secret", ("heh", "heh")] mode
          `shouldReturn` Right adminRoleName
      it "accepts when admin secret matches, honoring role request" $ do
        getUserInfoWithExpTime mempty [sessionVariableToHeader adminSecretHeader "secret", sessionVariableToHeader userRoleHeader "r00t"] mode
          `shouldReturn` Right (mkRoleNameE "r00t")

      it "rejects when doesn't match" $ do
        getUserInfoWithExpTime mempty [sessionVariableToHeader adminSecretHeader "bad secret"] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [sessionVariableToHeader adminSecretHeader ""] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [("blah", "blah"), sessionVariableToHeader adminSecretHeader "blah"] mode
          `shouldReturn` Left AccessDenied
        -- with deprecated header:
        getUserInfoWithExpTime mempty [sessionVariableToHeader deprecatedAccessKeyHeader "bad secret"] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [sessionVariableToHeader deprecatedAccessKeyHeader ""] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [("blah", "blah"), sessionVariableToHeader deprecatedAccessKeyHeader "blah"] mode
          `shouldReturn` Left AccessDenied

      it "accepts when no secret sent and unauth role defined" $ do
        getUserInfoWithExpTime mempty mempty mode
          `shouldReturn` Right ourUnauthRole
        getUserInfoWithExpTime mempty [("heh", "heh")] mode
          `shouldReturn` Right ourUnauthRole
        -- FIXME MAYBE (see NOTE (*))
        getUserInfoWithExpTime mempty [sessionVariableToHeader userRoleHeader "r00t"] mode
          `shouldReturn` Right ourUnauthRole

  -- Unauthorized role is not supported for webhook
  describe "webhook" $ do
    mode <-
      runIO
        $ setupAuthMode'E (Just $ Set.singleton $ hashAdminSecret "secret") (Just fakeAuthHook) mempty Nothing

    it "accepts when admin secret matches" $ do
      getUserInfoWithExpTime mempty [sessionVariableToHeader adminSecretHeader "secret"] mode
        `shouldReturn` Right adminRoleName
    it "accepts when admin secret matches, honoring role request" $ do
      getUserInfoWithExpTime mempty [sessionVariableToHeader adminSecretHeader "secret", sessionVariableToHeader userRoleHeader "r00t"] mode
        `shouldReturn` Right (mkRoleNameE "r00t")

    it "rejects when admin secret doesn't match" $ do
      getUserInfoWithExpTime mempty [sessionVariableToHeader adminSecretHeader "bad secret"] mode
        `shouldReturn` Left AccessDenied
      getUserInfoWithExpTime mempty [sessionVariableToHeader adminSecretHeader ""] mode
        `shouldReturn` Left AccessDenied
      getUserInfoWithExpTime mempty [("blah", "blah"), sessionVariableToHeader adminSecretHeader "blah"] mode
        `shouldReturn` Left AccessDenied
      -- with deprecated header:
      getUserInfoWithExpTime mempty [sessionVariableToHeader deprecatedAccessKeyHeader "bad secret"] mode
        `shouldReturn` Left AccessDenied
      getUserInfoWithExpTime mempty [sessionVariableToHeader deprecatedAccessKeyHeader ""] mode
        `shouldReturn` Left AccessDenied
      getUserInfoWithExpTime mempty [("blah", "blah"), sessionVariableToHeader deprecatedAccessKeyHeader "blah"] mode
        `shouldReturn` Left AccessDenied

    it "authenticates with webhook when no admin secret sent" $ do
      getUserInfoWithExpTime mempty mempty mode
        `shouldReturn` Right (mkRoleNameE "hook")
      getUserInfoWithExpTime mempty [("blah", "blah")] mode
        `shouldReturn` Right (mkRoleNameE "hook")
      getUserInfoWithExpTime mempty [sessionVariableToHeader userRoleHeader "hook"] mode
        `shouldReturn` Right (mkRoleNameE "hook")

    -- FIXME MAYBE (see NOTE (*))
    it "ignores requested role, uses webhook role" $ do
      getUserInfoWithExpTime mempty [sessionVariableToHeader userRoleHeader "r00t", sessionVariableToHeader userRoleHeader "admin"] mode
        `shouldReturn` Right (mkRoleNameE "hook")
      getUserInfoWithExpTime mempty [sessionVariableToHeader userRoleHeader "r00t"] mode
        `shouldReturn` Right (mkRoleNameE "hook")

  -- helper for generating mocked up verified JWT token claims, as though returned by 'processAuthZHeader':
  let unObject l = case J.object l of
        J.Object o -> o
        _ -> error "impossible"

  describe "JWT" $ do
    describe "unauth role NOT set" $ do
      mode <-
        runIO
          $ setupAuthMode'E (Just $ Set.singleton $ hashAdminSecret "secret") Nothing [fakeJWTConfig] Nothing

      it "accepts when admin secret matches" $ do
        getUserInfoWithExpTime mempty [sessionVariableToHeader adminSecretHeader "secret"] mode
          `shouldReturn` Right adminRoleName
      it "accepts when admin secret matches, honoring role request" $ do
        getUserInfoWithExpTime mempty [sessionVariableToHeader adminSecretHeader "secret", sessionVariableToHeader userRoleHeader "r00t"] mode
          `shouldReturn` Right (mkRoleNameE "r00t")

      it "rejects when admin secret doesn't match" $ do
        getUserInfoWithExpTime mempty [sessionVariableToHeader adminSecretHeader "bad secret"] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [sessionVariableToHeader adminSecretHeader ""] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [("blah", "blah"), sessionVariableToHeader adminSecretHeader "blah"] mode
          `shouldReturn` Left AccessDenied
        -- with deprecated header:
        getUserInfoWithExpTime mempty [sessionVariableToHeader deprecatedAccessKeyHeader "bad secret"] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [sessionVariableToHeader deprecatedAccessKeyHeader ""] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [("blah", "blah"), sessionVariableToHeader deprecatedAccessKeyHeader "blah"] mode
          `shouldReturn` Left AccessDenied

      it "rejects when admin secret not sent and no 'Authorization' header" $ do
        getUserInfoWithExpTime mempty [("blah", "blah")] mode
          `shouldReturn` Left InvalidHeaders
        getUserInfoWithExpTime mempty mempty mode
          `shouldReturn` Left InvalidHeaders

    describe "unauth role set" $ do
      mode <-
        runIO
          $ setupAuthMode'E
            (Just $ Set.singleton $ hashAdminSecret "secret")
            Nothing
            [fakeJWTConfig]
            (Just ourUnauthRole)

      it "accepts when admin secret matches" $ do
        getUserInfoWithExpTime mempty [sessionVariableToHeader adminSecretHeader "secret"] mode
          `shouldReturn` Right adminRoleName
        getUserInfoWithExpTime mempty [sessionVariableToHeader adminSecretHeader "secret", ("heh", "heh")] mode
          `shouldReturn` Right adminRoleName
      it "accepts when admin secret matches, honoring role request" $ do
        getUserInfoWithExpTime mempty [sessionVariableToHeader adminSecretHeader "secret", sessionVariableToHeader userRoleHeader "r00t"] mode
          `shouldReturn` Right (mkRoleNameE "r00t")

      it "rejects when admin secret doesn't match" $ do
        getUserInfoWithExpTime mempty [sessionVariableToHeader adminSecretHeader "bad secret"] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [sessionVariableToHeader adminSecretHeader ""] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [("blah", "blah"), sessionVariableToHeader adminSecretHeader "blah"] mode
          `shouldReturn` Left AccessDenied
        -- with deprecated header:
        getUserInfoWithExpTime mempty [sessionVariableToHeader deprecatedAccessKeyHeader "bad secret"] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [sessionVariableToHeader deprecatedAccessKeyHeader ""] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [("blah", "blah"), sessionVariableToHeader deprecatedAccessKeyHeader "blah"] mode
          `shouldReturn` Left AccessDenied

      it "authorizes as unauth role when no 'Authorization' header" $ do
        getUserInfoWithExpTime mempty [("blah", "blah")] mode
          `shouldReturn` Right ourUnauthRole
        getUserInfoWithExpTime mempty mempty mode
          `shouldReturn` Right ourUnauthRole

    describe "when Authorization header sent, and no admin secret" $ do
      modeA <-
        runIO
          $ setupAuthMode'E
            (Just $ Set.singleton $ hashAdminSecret "secret")
            Nothing
            [fakeJWTConfig]
            (Just ourUnauthRole)
      modeB <-
        runIO
          $ setupAuthMode'E
            (Just $ Set.singleton $ hashAdminSecret "secret")
            Nothing
            [fakeJWTConfig]
            Nothing

      -- Here the unauth role does not come into play at all, so map same tests over both modes:
      forM_ [(modeA, "with unauth role set"), (modeB, "with unauth role NOT set")]
        $ \(mode, modeMsg) -> describe modeMsg $ do
          it "authorizes successfully with JWT when requested role allowed" $ do
            let claim =
                  unObject
                    [ allowedRolesClaimText .= (["editor", "user", "mod"] :: [Text]),
                      defaultRoleClaimText .= ("user" :: Text)
                    ]
            getUserInfoWithExpTime claim [("Authorization", "Bearer IGNORED"), sessionVariableToHeader userRoleHeader "editor"] mode
              `shouldReturn` Right (mkRoleNameE "editor")
            -- Uses the defaultRoleClaimText:
            getUserInfoWithExpTime claim [("Authorization", "Bearer IGNORED")] mode
              `shouldReturn` Right (mkRoleNameE "user")

          it "rejects when requested role is not allowed" $ do
            let claim =
                  unObject
                    [ allowedRolesClaimText .= (["editor", "user", "mod"] :: [Text]),
                      defaultRoleClaimText .= ("user" :: Text)
                    ]
            getUserInfoWithExpTime claim [("Authorization", "Bearer IGNORED"), sessionVariableToHeader userRoleHeader "r00t"] mode
              `shouldReturn` Left AccessDenied
            getUserInfoWithExpTime claim [("Authorization", "Bearer IGNORED"), sessionVariableToHeader userRoleHeader "admin"] mode
              `shouldReturn` Left AccessDenied

          -- A corner case, but the behavior seems desirable:
          it "always rejects when token has empty allowedRolesClaimText" $ do
            let claim = unObject [allowedRolesClaimText .= (mempty :: [Text]), defaultRoleClaimText .= ("user" :: Text)]
            getUserInfoWithExpTime claim [("Authorization", "Bearer IGNORED"), sessionVariableToHeader userRoleHeader "admin"] mode
              `shouldReturn` Left AccessDenied
            getUserInfoWithExpTime claim [("Authorization", "Bearer IGNORED"), sessionVariableToHeader userRoleHeader "user"] mode
              `shouldReturn` Left AccessDenied
            getUserInfoWithExpTime claim [("Authorization", "Bearer IGNORED")] mode
              `shouldReturn` Left AccessDenied

          it "rejects when token doesn't have proper allowedRolesClaimText and defaultRoleClaimText" $ do
            let claim0 = unObject [allowedRolesClaimText .= (["editor", "user", "mod"] :: [Text])]
                claim1 = unObject [defaultRoleClaimText .= ("user" :: Text)]
                claim2 = unObject mempty
            for_ [claim0, claim1, claim2] $ \claim ->
              getUserInfoWithExpTime claim [("Authorization", "Bearer IGNORED")] mode
                `shouldReturn` Left JWTRoleClaimMissing

-- (*) FIXME NOTE (re above):
--
-- Ideally we should always return AccessDenied if the role we would
-- otherwise have returned does not match the requested role (from the
-- 'userRoleHeader').
--
-- This would harden a bit against bugs, makes the spec simpler, but
-- especially is better UX since in the current behavior the user can't be
-- sure which role their query is operating as (and in the worst case we
-- might e.g. delete rows the user didn't intend)
--
-- But this is a breaking change we need to think a little more about;
-- users might be relying on the behavior, e.g. just hardcoding a dev role
-- into clients.

-- Some very basic unit tests of AuthMode construction and error modes
setupAuthModeTests :: Spec
setupAuthModeTests = describe "setupAuthMode" $ do
  let secret = Set.singleton $ hashAdminSecret "secret"
      unauthRole = mkRoleNameE "anon"

  -- These are all various error cases, except for the AMNoAuth mode:
  it "with no admin secret provided" $ do
    setupAuthMode' Nothing Nothing mempty Nothing
      `shouldReturn` Right AMNoAuth
    -- We insist on an admin secret in order to use webhook or JWT auth:
    setupAuthMode' Nothing Nothing [fakeJWTConfig] Nothing
      `shouldReturn` Left ()
    setupAuthMode' Nothing (Just fakeAuthHook) mempty Nothing
      `shouldReturn` Left ()
    -- ...and we can't have both:
    setupAuthMode' Nothing (Just fakeAuthHook) [fakeJWTConfig] Nothing
      `shouldReturn` Left ()
    -- If the unauthenticated role was set but would otherwise be ignored this
    -- should be an error (for now), since users might expect all access to use
    -- the specified role. This first case would be the real worrying one:
    setupAuthMode' Nothing Nothing mempty (Just unauthRole)
      `shouldReturn` Left ()
    setupAuthMode' Nothing Nothing [fakeJWTConfig] (Just unauthRole)
      `shouldReturn` Left ()
    setupAuthMode' Nothing (Just fakeAuthHook) mempty (Just unauthRole)
      `shouldReturn` Left ()
    setupAuthMode' Nothing (Just fakeAuthHook) [fakeJWTConfig] (Just unauthRole)
      `shouldReturn` Left ()

  it "with admin secret provided" $ do
    setupAuthMode' (Just secret) Nothing mempty Nothing
      `shouldReturn` Right (AMAdminSecret secret Nothing)
    setupAuthMode' (Just secret) Nothing mempty (Just unauthRole)
      `shouldReturn` Right (AMAdminSecret secret $ Just unauthRole)

    setupAuthMode' (Just secret) Nothing [fakeJWTConfig] Nothing >>= \case
      Right (AMAdminSecretAndJWT s _ Nothing) -> do
        s `shouldBe` secret
      _ -> expectationFailure "AMAdminSecretAndJWT"
    setupAuthMode' (Just secret) Nothing [fakeJWTConfig] (Just unauthRole) >>= \case
      Right (AMAdminSecretAndJWT s _ ur) -> do
        s `shouldBe` secret
        ur `shouldBe` Just unauthRole
      _ -> expectationFailure "AMAdminSecretAndJWT"

    setupAuthMode' (Just secret) (Just fakeAuthHook) mempty Nothing
      `shouldReturn` Right (AMAdminSecretAndHook secret fakeAuthHook)
    -- auth hook can't make use of unauthenticated role for now (no good UX):
    setupAuthMode' (Just secret) (Just fakeAuthHook) mempty (Just unauthRole)
      `shouldReturn` Left ()
    -- we can't have both:
    setupAuthMode' (Just secret) (Just fakeAuthHook) [fakeJWTConfig] Nothing
      `shouldReturn` Left ()
    setupAuthMode' (Just secret) (Just fakeAuthHook) [fakeJWTConfig] (Just unauthRole)
      `shouldReturn` Left ()

parseClaimsMapTests :: Spec
parseClaimsMapTests = describe "parseClaimMapTests" $ do
  let parseClaimsMap_ ::
        JWT.ClaimsSet ->
        JWTClaims ->
        IO (Either Code ClaimsMap)
      parseClaimsMap_ claimsSet claims =
        runExceptT $ withExceptT qeCode $ parseClaimsMap claimsSet claims

      unObject l = case J.object l of
        J.Object o -> o
        _ -> error "Impossible!"

      defaultClaimsMap =
        HashMap.fromList
          [ (allowedRolesClaim, J.toJSON (map mkRoleNameE ["user", "editor"])),
            (defaultRoleClaim, J.toJSON (mkRoleNameE "user"))
          ]

  describe "JWT configured with namespace" $ do
    describe "JWT configured with namespace key, the key is a text value which is expected to be at the root of the JWT token" $ do
      it "parses claims map from the JWT token with correct namespace " $ do
        let claimsObj =
              unObject
                $ [ "x-hasura-allowed-roles" .= (["user", "editor"] :: [Text]),
                    "x-hasura-default-role" .= ("user" :: Text)
                  ]
        let obj = unObject $ ["claims_map" .= claimsObj]
            claimsSet = mkClaimsSetWithUnregisteredClaims obj
        parseClaimsMap_ claimsSet (JCNamespace (ClaimNs "claims_map") defaultClaimsFormat)
          `shouldReturn` Right defaultClaimsMap

      it "doesn't parse claims map from the JWT token with wrong namespace " $ do
        let claimsObj =
              unObject
                $ [ "x-hasura-allowed-roles" .= (["user", "editor"] :: [Text]),
                    "x-hasura-default-role" .= ("user" :: Text)
                  ]
        let obj = unObject $ ["claims_map" .= claimsObj]
            claimsSet = mkClaimsSetWithUnregisteredClaims obj
        parseClaimsMap_ claimsSet (JCNamespace (ClaimNs "wrong_claims_map") defaultClaimsFormat)
          `shouldReturn` Left JWTInvalidClaims

    describe "JWT configured with namespace JSON path, JSON path to the claims map" $ do
      it "parse claims map from the JWT token using claims namespace JSON Path" $ do
        let unregisteredClaims =
              unObject
                $ [ "x-hasura-allowed-roles" .= (["user", "editor"] :: [Text]),
                    "x-hasura-default-role" .= ("user" :: Text),
                    "sub" .= ("random" :: Text),
                    "exp" .= (1626420800 :: Int) -- we ignore these non session variables, in the response
                  ]
            claimsSetWithSub =
              (JWT.emptyClaimsSet & JWT.claimSub .~ Just "random") & JWT.unregisteredClaims .~ KM.toMapText unregisteredClaims
        parseClaimsMap_ claimsSetWithSub (JCNamespace (ClaimNsPath (mkJSONPathE "$")) defaultClaimsFormat)
          -- "$" JSON path signifies the claims are to be found in the root of the JWT token
          `shouldReturn` Right defaultClaimsMap

      it "throws error while attempting to parse claims map from the JWT token with a wrong namespace JSON Path" $ do
        let claimsObj =
              unObject
                $ [ "x-hasura-allowed-roles" .= (["user", "editor"] :: [Text]),
                    "x-hasura-default-role" .= ("user" :: Text)
                  ]
            obj = unObject $ ["hasura_claims" .= claimsObj]
            claimsSet = mkClaimsSetWithUnregisteredClaims obj
        parseClaimsMap_ claimsSet (JCNamespace (ClaimNsPath (mkJSONPathE "$.claims")) defaultClaimsFormat)
          `shouldReturn` Left JWTInvalidClaims

  describe "JWT configured with custom JWT claims" $ do
    let rolesObj =
          unObject
            $ [ "allowed" .= (["user", "editor"] :: [Text]),
                "default" .= ("user" :: Text)
              ]
        userId = unObject ["id" .= ("1" :: Text)]
        obj =
          unObject
            $ [ "roles" .= rolesObj,
                "user" .= userId
              ]
        claimsSet = mkClaimsSetWithUnregisteredClaims obj
        userIdClaim = unsafeMkSessionVariable ("x-hasura-user-id" :: CI.CI Text)

    describe "custom claims with JSON paths to the claim location in the JWT token" $ do
      it "parse custom claims values, with correct values" $ do
        let customDefRoleClaim = mkCustomDefaultRoleClaim (Just "$.roles.default") Nothing
            customAllowedRolesClaim = mkCustomAllowedRoleClaim (Just "$.roles.allowed") Nothing
            otherClaims =
              HashMap.fromList
                [(userIdClaim, mkCustomOtherClaim (Just "$.user.id") Nothing)]
            customClaimsMap = JWTCustomClaimsMap customDefRoleClaim customAllowedRolesClaim otherClaims

        parseClaimsMap_ claimsSet (JCMap customClaimsMap)
          `shouldReturn` Right
            ( HashMap.fromList
                [ (allowedRolesClaim, J.toJSON (map mkRoleNameE ["user", "editor"])),
                  (defaultRoleClaim, J.toJSON (mkRoleNameE "user")),
                  (userIdClaim, J.String "1")
                ]
            )

      it "parse custom claims values with session variable mapped to a standard JWT claim (sub)" $ do
        let customDefRoleClaim = mkCustomDefaultRoleClaim (Just "$.roles.default") Nothing
            customAllowedRolesClaim = mkCustomAllowedRoleClaim (Just "$.roles.allowed") Nothing
            otherClaims =
              HashMap.fromList
                [(userIdClaim, mkCustomOtherClaim (Just "$.sub") Nothing)]
            customClaimsMap = JWTCustomClaimsMap customDefRoleClaim customAllowedRolesClaim otherClaims

        parseClaimsMap_ (claimsSet & JWT.claimSub .~ (Just "2")) (JCMap customClaimsMap)
          `shouldReturn` Right
            ( HashMap.fromList
                [ (allowedRolesClaim, J.toJSON (map mkRoleNameE ["user", "editor"])),
                  (defaultRoleClaim, J.toJSON (mkRoleNameE "user")),
                  (userIdClaim, J.String "2")
                ]
            )

      it "throws error when a specified custom claim value is missing" $ do
        let customDefRoleClaim = mkCustomDefaultRoleClaim (Just "$.roles.wrong_default") Nothing -- wrong path provided
            customAllowedRolesClaim = mkCustomAllowedRoleClaim (Just "$.roles.allowed") Nothing
            customClaimsMap = JWTCustomClaimsMap customDefRoleClaim customAllowedRolesClaim mempty
        parseClaimsMap_ claimsSet (JCMap customClaimsMap)
          `shouldReturn` Left JWTRoleClaimMissing

      it "doesn't throw an error when the specified custom claim is missing, but the default value is provided" $ do
        let customDefRoleClaim = mkCustomDefaultRoleClaim (Just "$.roles.wrong_default") (Just "editor")
            customAllowedRolesClaim = mkCustomAllowedRoleClaim (Just "$.roles.allowed") Nothing
            customClaimsMap = JWTCustomClaimsMap customDefRoleClaim customAllowedRolesClaim mempty
        parseClaimsMap_ claimsSet (JCMap customClaimsMap)
          `shouldReturn` Right
            ( HashMap.fromList
                [ (allowedRolesClaim, J.toJSON (map mkRoleNameE ["user", "editor"])),
                  (defaultRoleClaim, J.toJSON (mkRoleNameE "editor"))
                ]
            )

    describe "custom claims with literal values" $ do
      it "uses the literal custom claim value" $ do
        let customDefRoleClaim = mkCustomDefaultRoleClaim Nothing (Just "editor")
            customAllowedRolesClaim = mkCustomAllowedRoleClaim Nothing (Just ["user", "editor"])
            customClaimsMap = JWTCustomClaimsMap customDefRoleClaim customAllowedRolesClaim mempty
        parseClaimsMap_ JWT.emptyClaimsSet (JCMap customClaimsMap)
          `shouldReturn` Right
            ( HashMap.fromList
                [ (allowedRolesClaim, J.toJSON (map mkRoleNameE ["user", "editor"])),
                  (defaultRoleClaim, J.toJSON (mkRoleNameE "editor"))
                ]
            )

mkCustomDefaultRoleClaim :: Maybe Text -> Maybe Text -> JWTCustomClaimsMapDefaultRole
mkCustomDefaultRoleClaim claimPath defVal =
  -- check if claimPath is provided, if not then use the default value
  -- as the literal value by removing the `Maybe` of defVal
  case claimPath of
    Just path -> JWTCustomClaimsMapJSONPath (mkJSONPathE path) $ defRoleName
    Nothing -> JWTCustomClaimsMapStatic $ fromMaybe (mkRoleNameE "user") defRoleName
  where
    defRoleName = mkRoleNameE <$> defVal

mkCustomAllowedRoleClaim :: Maybe Text -> Maybe [Text] -> JWTCustomClaimsMapAllowedRoles
mkCustomAllowedRoleClaim claimPath defVal =
  -- check if claimPath is provided, if not then use the default value
  -- as the literal value by removing the `Maybe` of defVal
  case claimPath of
    Just path -> JWTCustomClaimsMapJSONPath (mkJSONPathE path) $ defAllowedRoles
    Nothing ->
      JWTCustomClaimsMapStatic
        $ fromMaybe (mkRoleNameE <$> ["user", "editor"]) defAllowedRoles
  where
    defAllowedRoles = fmap mkRoleNameE <$> defVal

-- use for claims other than `x-hasura-default-role` and `x-hasura-allowed-roles`
mkCustomOtherClaim :: Maybe Text -> Maybe Text -> JWTCustomClaimsMapValue
mkCustomOtherClaim claimPath defVal =
  -- check if claimPath is provided, if not then use the default value
  -- as the literal value by removing the `Maybe` of defVal
  case claimPath of
    Just path -> JWTCustomClaimsMapJSONPath (mkJSONPathE path) $ defVal
    Nothing -> JWTCustomClaimsMapStatic $ fromMaybe "default claim value" defVal

fakeJWTConfig :: JWTConfig
fakeJWTConfig =
  let jcKeyOrUrl = Left (Jose.fromOctets [])
      jcAudience = Nothing
      jcIssuer = Nothing
      jcClaims = JCNamespace (ClaimNs "") JCFJson
      jcAllowedSkew = Nothing
      jcHeader = Nothing
   in JWTConfig {..}

fakeAuthHook :: AuthHook
fakeAuthHook = AuthHook "http://fake" AHTGet False

mkRoleNameE :: Text -> RoleName
mkRoleNameE = fromMaybe (error "fixme") . mkRoleName

mkJSONPathE :: Text -> J.JSONPath
mkJSONPathE = either (error . T.unpack) id . parseJSONPath

setupAuthMode' ::
  ( ForkableMonadIO m,
    MonadMask m
  ) =>
  Maybe (HashSet AdminSecretHash) ->
  Maybe AuthHook ->
  [JWTConfig] ->
  Maybe RoleName ->
  m (Either () AuthMode)
setupAuthMode' mAdminSecretHash mWebHook jwtSecrets mUnAuthRole = do
  httpManager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  fmap (mapLeft $ const ())
    $ runExceptT
    $ setupAuthMode
      (fromMaybe Set.empty mAdminSecretHash)
      mWebHook
      jwtSecrets
      mUnAuthRole
      (Logger $ void . return)
      httpManager

setupAuthMode'E ::
  ( ForkableMonadIO m,
    MonadMask m
  ) =>
  Maybe (HashSet AdminSecretHash) ->
  Maybe AuthHook ->
  [JWTConfig] ->
  Maybe RoleName ->
  m AuthMode
setupAuthMode'E a b c d =
  either (const $ error "fixme") id
    <$> setupAuthMode' a b c d

mkClaimsSetWithUnregisteredClaims :: J.Object -> JWT.ClaimsSet
mkClaimsSetWithUnregisteredClaims unregisteredClaims =
  JWT.emptyClaimsSet & JWT.unregisteredClaims .~ KM.toMapText unregisteredClaims

-- | Sample cognito JWKS for testing, the first few from:
-- https://cognito-identity.amazonaws.com/.well-known/jwks_uri
cognitoJwksJson :: BL.ByteString
cognitoJwksJson = "{\"keys\":[{\"kty\":\"RSA\",\"e\":\"AQAB\",\"use\":\"sig\",\"kid\":\"ap-northeast-1-8\",\"alg\":\"RS512\",\"n\":\"AIKA9NEvE3TfOWZo2V72bCtDTeJQynYa1xV7wJcqS1A5nplcFTvM1HRDkVWzFf9ofzDlHR2x/iDNrl9GcEJMslX7mMMsVnUU5p64KfBFAk6mfNVtyu2glv0pVfxcQyDvYUIRppz6FHosNEK4/5ad6J/wzfqh21xF5Wg28PsLbMK3SPAQHQ/Bw3fB1+Y+CJL/jyk/0Rbhsl4mVLYsyN/NvohQEAAQV/z1L1v72uLnbz0by8+eaZfqEeAimeLsaa2ampcXJY5bReqme5gmvrtoWCVbzbsjG/ZRBtb8kJ65uB2brH6Zi7Br67l+QQGM2N1fLG9mEo4gc1+gpAXaVHg0wBM=\"},{\"kty\":\"RSA\",\"e\":\"AQAB\",\"use\":\"sig\",\"kid\":\"ap-northeast-1-7\",\"alg\":\"RS512\",\"n\":\"AKBuwnAJ/XUhykMo4UaEdnhtQiNHUdWnF/or3nmvvDXOCbLfNX6XMKa9GQiFAfzCTJAxHvArPt0v6R0sBtrYWw6ioBvV+pZrV1r1nYZ4ZxqZtV+Gc/CnjhNn/8JEzM8oZ1XchZ3/Mh3/bw2/Auyu/3FCn92pHAihTkMtJ55JDmM6AXybqEZBjTCKPzjkTf7A6kusUzCseShpkhCWtL2sJRLk5mmFoDISKeE8/HXNiUuYn0waWtM/qQLWfAjJaLJXhhMHE2cwGCDwIxAIp8uz7iA2BtFdt4aB6aDxS+shVn9nUUetvFGDj4Goyt1qqrwc/ZepAnvTLlFUatBM71FfFE0=\"},{\"kty\":\"RSA\",\"e\":\"AQAB\",\"use\":\"sig\",\"kid\":\"ap-northeast-1-6\",\"alg\":\"RS512\",\"n\":\"AIkxdCCZssUqsUL+hL2n8/Eht3X98DENiGmF+xXPTlBaFzR8kTWxl6F03cwGsE6fuRh5Rb+qrcJCMKWi2tTMt6qGUyNraPCB0aVnn2gqEjMAYgZRkrHBikEwzD9KzZys183vzeoF12VYRft5IDGJeoouxnx0MO3D8q/VCuUacKNs+B9lvcaNYi+V68aw0ZMVUHuOZyb74A/+2JxeJnLeW17gxDF2RP+vp/4/h9zhtMzmPClHJF62bUA4TW14Z+rVtkELXdOTb655Aw3nlzeeSSN8aKlR9+z0qTwO/YWIbMbIbYXyEYfTG28IWIbkyLEpfpFulhK6JshcJ94rjgJb5Vk=\"},{\"kty\":\"RSA\",\"e\":\"AQAB\",\"use\":\"sig\",\"kid\":\"ap-northeast-15\",\"alg\":\"RS512\",\"n\":\"AJw/MmbJh6myaqNmgQ7PTymwwVdVZq27L79cGGXcnlRvkqT3MFJXX10VSbhwXqz3YoVhtNf6C8LKxJSjsEcFDHEwHc5+7vkOi+iOmTX1D1dmehb1exz2D9RuH8VE6Bl6Y429Y3k+9Fgbe6WqtyPr9bIQt+mno0lXOkeyxKuNldqJkbuYdHSgPsUKfd+gdkGyQuB6DZ83i6u5EHKepk1yhjsMngt0+/a0ZODMAEkJz9dM9F61dSgLTLIyC6Kkn/Ok2yuTBryZ/eI8UGIhsqZWt/1Syrwf4kF0Uvjwyzx+iqG+fkce0LMAS4f1UuJOCFSAdbng6TYkcxQRe1RvTcUtxSk=\"}]}"

parseCognitoJwksTests :: Spec
parseCognitoJwksTests = describe "parseCognitoJwks" $ do
  -- make sure canonicalizeJWKJson works as expected
  it "internals should leave unexpectedly shaped json untouched" $ do
    let inputs = ["[]", "{\"keys\":[]}", "{\"foo\":[1,2,3]}"]
    for_ inputs $ \input -> do
      case J.decode input of
        Nothing -> error "fix test inputs"
        Just inputV -> canonicalizeJWKJson inputV `shouldBe` inputV

  it "should successfully parse Cognito JWKs" $ do
    let result = parseJWKSetRobustly cognitoJwksJson
    result `shouldSatisfy` isRight

  it "should be idempotent when encoding and parsing again" $ do
    case parseJWKSetRobustly cognitoJwksJson of
      Left err -> expectationFailure $ "Failed to parse JWKS: " ++ err
      Right jwkSet -> do
        let encodedJwkSet = J.encode jwkSet
        let reparsedResult = parseJWKSetRobustly encodedJwkSet

        case reparsedResult of
          Left err -> expectationFailure $ "Failed to reparse encoded JWKS: " ++ err
          Right reparsedJwkSet -> reparsedJwkSet `shouldBe` jwkSet

-- https://github.com/hasura/graphql-engine/issues/10733#issuecomment-2912141757
unknownUseJson, unknownUseJsonOnlyKnowns, missingUseImplyingSig :: BL.ByteString
unknownUseJson = "{\"keys\":[{\"use\":\"sig\",\"kty\":\"RSA\",\"kid\":\"321693135832881917\",\"alg\":\"RS256\",\"n\":\"AIKA9NEvE3TfOWZo2V72bCtDTeJQynYa1xV7wJcqS1A5nplcFTvM1HRDkVWzFf9ofzDlHR2x/iDNrl9GcEJMslX7mMMsVnUU5p64KfBFAk6mfNVtyu2glv0pVfxcQyDvYUIRppz6FHosNEK4/5ad6J/wzfqh21xF5Wg28PsLbMK3SPAQHQ/Bw3fB1+Y+CJL/jyk/0Rbhsl4mVLYsyN/NvohQEAAQV/z1L1v72uLnbz0by8+eaZfqEeAimeLsaa2ampcXJY5bReqme5gmvrtoWCVbzbsjG/ZRBtb8kJ65uB2brH6Zi7Br67l+QQGM2N1fLG9mEo4gc1+gpAXaVHg0wBM=\",\"e\":\"AQAB\"},{\"use\":\"saml_response_sig\",\"kty\":\"RSA\",\"kid\":\"321336135382996543\",\"n\":\"AIKA9NEvE3TfOWZo2V72bCtDTeJQynYa1xV7wJcqS1A5nplcFTvM1HRDkVWzFf9ofzDlHR2x/iDNrl9GcEJMslX7mMMsVnUU5p64KfBFAk6mfNVtyu2glv0pVfxcQyDvYUIRppz6FHosNEK4/5ad6J/wzfqh21xF5Wg28PsLbMK3SPAQHQ/Bw3fB1+Y+CJL/jyk/0Rbhsl4mVLYsyN/NvohQEAAQV/z1L1v72uLnbz0by8+eaZfqEeAimeLsaa2ampcXJY5bReqme5gmvrtoWCVbzbsjG/ZRBtb8kJ65uB2brH6Zi7Br67l+QQGM2N1fLG9mEo4gc1+gpAXaVHg0wBM=\",\"e\":\"AQAB\"},{\"use\":\"sig\",\"kty\":\"RSA\",\"kid\":\"321838209426266877\",\"alg\":\"RS256\",\"n\":\"AIKA9NEvE3TfOWZo2V72bCtDTeJQynYa1xV7wJcqS1A5nplcFTvM1HRDkVWzFf9ofzDlHR2x/iDNrl9GcEJMslX7mMMsVnUU5p64KfBFAk6mfNVtyu2glv0pVfxcQyDvYUIRppz6FHosNEK4/5ad6J/wzfqh21xF5Wg28PsLbMK3SPAQHQ/Bw3fB1+Y+CJL/jyk/0Rbhsl4mVLYsyN/NvohQEAAQV/z1L1v72uLnbz0by8+eaZfqEeAimeLsaa2ampcXJY5bReqme5gmvrtoWCVbzbsjG/ZRBtb8kJ65uB2brH6Zi7Br67l+QQGM2N1fLG9mEo4gc1+gpAXaVHg0wBM=\",\"e\":\"AQAB\"}]}"
unknownUseJsonOnlyKnowns = "{\"keys\":[{\"use\":\"sig\",\"kty\":\"RSA\",\"kid\":\"321693135832881917\",\"alg\":\"RS256\",\"n\":\"AIKA9NEvE3TfOWZo2V72bCtDTeJQynYa1xV7wJcqS1A5nplcFTvM1HRDkVWzFf9ofzDlHR2x/iDNrl9GcEJMslX7mMMsVnUU5p64KfBFAk6mfNVtyu2glv0pVfxcQyDvYUIRppz6FHosNEK4/5ad6J/wzfqh21xF5Wg28PsLbMK3SPAQHQ/Bw3fB1+Y+CJL/jyk/0Rbhsl4mVLYsyN/NvohQEAAQV/z1L1v72uLnbz0by8+eaZfqEeAimeLsaa2ampcXJY5bReqme5gmvrtoWCVbzbsjG/ZRBtb8kJ65uB2brH6Zi7Br67l+QQGM2N1fLG9mEo4gc1+gpAXaVHg0wBM=\",\"e\":\"AQAB\"},{\"use\":\"sig\",\"kty\":\"RSA\",\"kid\":\"321838209426266877\",\"alg\":\"RS256\",\"n\":\"AIKA9NEvE3TfOWZo2V72bCtDTeJQynYa1xV7wJcqS1A5nplcFTvM1HRDkVWzFf9ofzDlHR2x/iDNrl9GcEJMslX7mMMsVnUU5p64KfBFAk6mfNVtyu2glv0pVfxcQyDvYUIRppz6FHosNEK4/5ad6J/wzfqh21xF5Wg28PsLbMK3SPAQHQ/Bw3fB1+Y+CJL/jyk/0Rbhsl4mVLYsyN/NvohQEAAQV/z1L1v72uLnbz0by8+eaZfqEeAimeLsaa2ampcXJY5bReqme5gmvrtoWCVbzbsjG/ZRBtb8kJ65uB2brH6Zi7Br67l+QQGM2N1fLG9mEo4gc1+gpAXaVHg0wBM=\",\"e\":\"AQAB\"}]}"
missingUseImplyingSig = "{ \"keys\": [{ \"kty\": \"OKP\", \"crv\": \"Ed25519\", \"x\": \"0EHgrcLDwfQEODCoSru5mS6mdbbzlr44xgSXdhFPmBo\" }]}"

filterOutUnknownKeyTypesTests :: Spec
filterOutUnknownKeyTypesTests = describe "filterOutUnknownKeyTypes" $ do
  it "should filter JWKs with unknown 'use' field value" $ do
    let expected = case parseJWKSetRobustly unknownUseJsonOnlyKnowns of
          Right (JWKSet s) -> s
          _ -> error "bad parse in unknownUseJsonOnlyKnowns"
    case parseJWKSetRobustly unknownUseJson of
      Right (JWKSet s) -> do
        length s `shouldBe` 2
        s `shouldBe` expected
      _ -> error "bad parse"

  -- https://github.com/hasura/graphql-engine/issues/10774
  it "should not filter if 'use' is missing" $ do
    case parseJWKSetRobustly missingUseImplyingSig of
      Right (JWKSet s) -> length s `shouldBe` 1
      e -> error $ "bad parse in missingUseImplyingSig: " <> show e
