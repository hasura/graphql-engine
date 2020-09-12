{-# LANGUAGE UndecidableInstances #-}

module Hasura.Server.AuthSpec (spec) where

import           Hasura.Logging
import           Hasura.Prelude
import           Hasura.Server.Version

import           Control.Monad.Trans.Control
import qualified Crypto.JOSE.JWK             as Jose
import           Data.Aeson                  ((.=))
import qualified Data.Aeson                  as J
import           Data.Parser.JSONPath
import qualified Data.HashMap.Strict         as Map
import qualified Network.HTTP.Types          as N

import           Hasura.RQL.Types
import           Hasura.Server.Auth          hiding (getUserInfoWithExpTime, processJwt)
import           Hasura.Server.Auth.JWT      hiding (processJwt)
import           Hasura.Server.Utils
import           Hasura.Session
import qualified Hasura.Tracing              as Tracing
import           Test.Hspec

spec :: Spec
spec = do
  getUserInfoWithExpTimeTests
  setupAuthModeTests
  parseClaimsMapTests

allowedRolesClaimText :: Text
allowedRolesClaimText = sessionVariableToText allowedRolesClaim

defaultRoleClaimText :: Text
defaultRoleClaimText = sessionVariableToText defaultRoleClaim

-- Unit test the core of our authentication code. This doesn't test the details
-- of resolving roles from JWT or webhook.
getUserInfoWithExpTimeTests :: Spec
getUserInfoWithExpTimeTests = describe "getUserInfo" $ do
  ---- FUNCTION UNDER TEST:
  let getUserInfoWithExpTime
        :: J.Object
        -- ^ For JWT, inject the raw claims object as though returned from 'processAuthZHeader'
        -- acting on an 'Authorization' header from the request
        -> [N.Header] -> AuthMode -> IO (Either Code RoleName)
      getUserInfoWithExpTime claims rawHeaders =
        runExceptT
        . withExceptT qeCode -- just look at Code for purposes of tests
        . fmap _uiRole -- just look at RoleName for purposes of tests
        . fmap fst -- disregard Nothing expiration
        . getUserInfoWithExpTime_ userInfoFromAuthHook processJwt () () rawHeaders
        where
          -- mock authorization callbacks:
          userInfoFromAuthHook _ _ _hook _reqHeaders = do
            (, Nothing) <$> _UserInfo "hook"
            where
              -- we don't care about details here; we'll just check role name in tests:
              _UserInfo nm =
                mkUserInfo (URBFromSessionVariablesFallback $ mkRoleNameE nm)
                           UAdminSecretNotSent
                           (mkSessionVariablesHeaders mempty)
          processJwt = processJwt_ $
            -- processAuthZHeader:
            \_jwtCtx _authzHeader -> return (claimsObjToClaimsMap claims , Nothing)
            where
              claimsObjToClaimsMap = Map.fromList . map (first mkSessionVariable) . Map.toList

  let setupAuthMode'E a b c d =
        either (const $ error "fixme") id <$> setupAuthMode' a b c d

  let ourUnauthRole = mkRoleNameE "an0nymous"


  describe "started without admin secret" $ do
    it "gives admin by default" $ do
      mode <- setupAuthMode'E Nothing Nothing Nothing Nothing
      getUserInfoWithExpTime mempty [] mode
        `shouldReturn` Right adminRoleName
    it "allows any requested role" $ do
      mode <- setupAuthMode'E Nothing Nothing Nothing Nothing
      getUserInfoWithExpTime mempty [(userRoleHeader, "r00t")] mode
        `shouldReturn` Right (mkRoleNameE "r00t")


  describe "admin secret only" $ do
    describe "unauth role NOT set" $ do
      mode <- runIO $ setupAuthMode'E (Just $ hashAdminSecret "secret") Nothing Nothing Nothing

      it "accepts when admin secret matches" $ do
        getUserInfoWithExpTime mempty [(adminSecretHeader, "secret")] mode
          `shouldReturn` Right adminRoleName
      it "accepts when admin secret matches, honoring role request" $ do
        getUserInfoWithExpTime mempty [(adminSecretHeader, "secret"), (userRoleHeader, "r00t")] mode
          `shouldReturn` Right (mkRoleNameE "r00t")

      it "rejects when doesn't match" $ do
        getUserInfoWithExpTime mempty [(adminSecretHeader, "bad secret")] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [(adminSecretHeader, "")] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [("blah", "blah"), (adminSecretHeader, "blah")] mode
          `shouldReturn` Left AccessDenied
        -- with deprecated header:
        getUserInfoWithExpTime mempty [(deprecatedAccessKeyHeader, "bad secret")] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [(deprecatedAccessKeyHeader, "")] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [("blah", "blah"), (deprecatedAccessKeyHeader, "blah")] mode
          `shouldReturn` Left AccessDenied

      it "rejects when no secret sent, since no fallback unauth role" $ do
        getUserInfoWithExpTime mempty [] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [(userRoleHeader, "r00t"), (userRoleHeader, "admin")] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [(userRoleHeader, "r00t")] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [("blah", "blah")] mode
          `shouldReturn` Left AccessDenied

    describe "unauth role set" $ do
      mode <- runIO $
        setupAuthMode'E (Just $ hashAdminSecret "secret") Nothing Nothing (Just ourUnauthRole)
      it "accepts when admin secret matches" $ do
        getUserInfoWithExpTime mempty [(adminSecretHeader, "secret")] mode
          `shouldReturn` Right adminRoleName
        getUserInfoWithExpTime mempty [(adminSecretHeader, "secret"), ("heh", "heh")] mode
          `shouldReturn` Right adminRoleName
      it "accepts when admin secret matches, honoring role request" $ do
        getUserInfoWithExpTime mempty [(adminSecretHeader, "secret"), (userRoleHeader, "r00t")] mode
          `shouldReturn` Right (mkRoleNameE "r00t")

      it "rejects when doesn't match" $ do
        getUserInfoWithExpTime mempty [(adminSecretHeader, "bad secret")] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [(adminSecretHeader, "")] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [("blah", "blah"), (adminSecretHeader, "blah")] mode
          `shouldReturn` Left AccessDenied
        -- with deprecated header:
        getUserInfoWithExpTime mempty [(deprecatedAccessKeyHeader, "bad secret")] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [(deprecatedAccessKeyHeader, "")] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [("blah", "blah"), (deprecatedAccessKeyHeader, "blah")] mode
          `shouldReturn` Left AccessDenied

      it "accepts when no secret sent and unauth role defined" $ do
        getUserInfoWithExpTime mempty [] mode
          `shouldReturn` Right ourUnauthRole
        getUserInfoWithExpTime mempty [("heh", "heh")] mode
          `shouldReturn` Right ourUnauthRole
        -- FIXME MAYBE (see NOTE (*))
        getUserInfoWithExpTime mempty [(userRoleHeader, "r00t")] mode
          `shouldReturn` Right ourUnauthRole


  -- Unauthorized role is not supported for webhook
  describe "webhook" $ do
    mode <- runIO $
      setupAuthMode'E (Just $ hashAdminSecret "secret") (Just fakeAuthHook) Nothing Nothing

    it "accepts when admin secret matches" $ do
      getUserInfoWithExpTime mempty [(adminSecretHeader, "secret")] mode
        `shouldReturn` Right adminRoleName
    it "accepts when admin secret matches, honoring role request" $ do
      getUserInfoWithExpTime mempty [(adminSecretHeader, "secret"), (userRoleHeader, "r00t")] mode
        `shouldReturn` Right (mkRoleNameE "r00t")

    it "rejects when admin secret doesn't match" $ do
      getUserInfoWithExpTime mempty [(adminSecretHeader, "bad secret")] mode
        `shouldReturn` Left AccessDenied
      getUserInfoWithExpTime mempty [(adminSecretHeader, "")] mode
        `shouldReturn` Left AccessDenied
      getUserInfoWithExpTime mempty [("blah", "blah"), (adminSecretHeader, "blah")] mode
        `shouldReturn` Left AccessDenied
      -- with deprecated header:
      getUserInfoWithExpTime mempty [(deprecatedAccessKeyHeader, "bad secret")] mode
        `shouldReturn` Left AccessDenied
      getUserInfoWithExpTime mempty [(deprecatedAccessKeyHeader, "")] mode
        `shouldReturn` Left AccessDenied
      getUserInfoWithExpTime mempty [("blah", "blah"), (deprecatedAccessKeyHeader, "blah")] mode
        `shouldReturn` Left AccessDenied

    it "authenticates with webhook when no admin secret sent" $ do
      getUserInfoWithExpTime mempty [] mode
        `shouldReturn` Right (mkRoleNameE "hook")
      getUserInfoWithExpTime mempty [("blah", "blah")] mode
        `shouldReturn` Right (mkRoleNameE "hook")
      getUserInfoWithExpTime mempty [(userRoleHeader, "hook")] mode
        `shouldReturn` Right (mkRoleNameE "hook")

    -- FIXME MAYBE (see NOTE (*))
    it "ignores requested role, uses webhook role" $ do
      getUserInfoWithExpTime mempty [(userRoleHeader, "r00t"), (userRoleHeader, "admin")] mode
        `shouldReturn` Right (mkRoleNameE "hook")
      getUserInfoWithExpTime mempty [(userRoleHeader, "r00t")] mode
        `shouldReturn` Right (mkRoleNameE "hook")


  -- helper for generating mocked up verified JWT token claims, as though returned by 'processAuthZHeader':
  let unObject l = case J.object l of
        J.Object o -> o
        _          -> error "impossible"

  describe "JWT" $ do
    describe "unauth role NOT set" $ do
      mode <- runIO $
        setupAuthMode'E (Just $ hashAdminSecret "secret") Nothing (Just fakeJWTConfig) Nothing

      it "accepts when admin secret matches" $ do
        getUserInfoWithExpTime mempty [(adminSecretHeader, "secret")] mode
          `shouldReturn` Right adminRoleName
      it "accepts when admin secret matches, honoring role request" $ do
        getUserInfoWithExpTime mempty [(adminSecretHeader, "secret"), (userRoleHeader, "r00t")] mode
          `shouldReturn` Right (mkRoleNameE "r00t")

      it "rejects when admin secret doesn't match" $ do
        getUserInfoWithExpTime mempty [(adminSecretHeader, "bad secret")] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [(adminSecretHeader, "")] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [("blah", "blah"), (adminSecretHeader, "blah")] mode
          `shouldReturn` Left AccessDenied
        -- with deprecated header:
        getUserInfoWithExpTime mempty [(deprecatedAccessKeyHeader, "bad secret")] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [(deprecatedAccessKeyHeader, "")] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [("blah", "blah"), (deprecatedAccessKeyHeader, "blah")] mode
          `shouldReturn` Left AccessDenied

      it "rejects when admin secret not sent and no 'Authorization' header" $ do
        getUserInfoWithExpTime mempty [("blah", "blah")] mode
          `shouldReturn` Left InvalidHeaders
        getUserInfoWithExpTime mempty [] mode
          `shouldReturn` Left InvalidHeaders

    describe "unauth role set" $ do
      mode <- runIO $
        setupAuthMode'E (Just $ hashAdminSecret "secret") Nothing
                        (Just fakeJWTConfig) (Just ourUnauthRole)

      it "accepts when admin secret matches" $ do
        getUserInfoWithExpTime mempty [(adminSecretHeader, "secret")] mode
          `shouldReturn` Right adminRoleName
        getUserInfoWithExpTime mempty [(adminSecretHeader, "secret"), ("heh", "heh")] mode
          `shouldReturn` Right adminRoleName
      it "accepts when admin secret matches, honoring role request" $ do
        getUserInfoWithExpTime mempty [(adminSecretHeader, "secret"), (userRoleHeader, "r00t")] mode
          `shouldReturn` Right (mkRoleNameE "r00t")

      it "rejects when admin secret doesn't match" $ do
        getUserInfoWithExpTime mempty [(adminSecretHeader, "bad secret")] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [(adminSecretHeader, "")] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [("blah", "blah"), (adminSecretHeader, "blah")] mode
          `shouldReturn` Left AccessDenied
        -- with deprecated header:
        getUserInfoWithExpTime mempty [(deprecatedAccessKeyHeader, "bad secret")] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [(deprecatedAccessKeyHeader, "")] mode
          `shouldReturn` Left AccessDenied
        getUserInfoWithExpTime mempty [("blah", "blah"), (deprecatedAccessKeyHeader, "blah")] mode
          `shouldReturn` Left AccessDenied

      it "authorizes as unauth role when no 'Authorization' header" $ do
        getUserInfoWithExpTime mempty [("blah", "blah")] mode
          `shouldReturn` Right ourUnauthRole
        getUserInfoWithExpTime mempty [] mode
          `shouldReturn` Right ourUnauthRole

    describe "when Authorization header sent, and no admin secret" $ do
      modeA <- runIO $ setupAuthMode'E (Just $ hashAdminSecret "secret") Nothing
                                       (Just fakeJWTConfig) (Just ourUnauthRole)
      modeB <- runIO $ setupAuthMode'E (Just $ hashAdminSecret "secret") Nothing
                                       (Just fakeJWTConfig) Nothing

      -- Here the unauth role does not come into play at all, so map same tests over both modes:
      forM_ [(modeA, "with unauth role set"), (modeB, "with unauth role NOT set")] $
        \(mode, modeMsg) -> describe modeMsg $ do

          it "authorizes successfully with JWT when requested role allowed" $ do
            let claim = unObject [ allowedRolesClaimText .= (["editor","user", "mod"] :: [Text])
                                , defaultRoleClaimText .= ("user" :: Text)
                                ]
            getUserInfoWithExpTime claim [("Authorization", "IGNORED"), (userRoleHeader, "editor")] mode
              `shouldReturn` Right (mkRoleNameE "editor")
            -- Uses the defaultRoleClaimText:
            getUserInfoWithExpTime claim [("Authorization", "IGNORED")] mode
              `shouldReturn` Right (mkRoleNameE "user")

          it "rejects when requested role is not allowed" $ do
            let claim = unObject [ allowedRolesClaimText .= (["editor","user", "mod"] :: [Text])
                                , defaultRoleClaimText .= ("user" :: Text)
                                ]
            getUserInfoWithExpTime claim [("Authorization", "IGNORED"), (userRoleHeader, "r00t")] mode
              `shouldReturn` Left AccessDenied
            getUserInfoWithExpTime claim [("Authorization", "IGNORED"), (userRoleHeader, "admin")] mode
              `shouldReturn` Left AccessDenied

          -- A corner case, but the behavior seems desirable:
          it "always rejects when token has empty allowedRolesClaimText" $ do
            let claim = unObject [ allowedRolesClaimText .= ([] :: [Text]), defaultRoleClaimText .= ("user" :: Text) ]
            getUserInfoWithExpTime claim [("Authorization", "IGNORED"), (userRoleHeader, "admin")] mode
              `shouldReturn` Left AccessDenied
            getUserInfoWithExpTime claim [("Authorization", "IGNORED"), (userRoleHeader, "user")] mode
              `shouldReturn` Left AccessDenied
            getUserInfoWithExpTime claim [("Authorization", "IGNORED")] mode
              `shouldReturn` Left AccessDenied

          it "rejects when token doesn't have proper allowedRolesClaimText and defaultRoleClaimText" $ do
            let claim0 = unObject [ allowedRolesClaimText .= (["editor","user", "mod"] :: [Text]) ]
                claim1 = unObject [ defaultRoleClaimText .= ("user" :: Text) ]
                claim2 = unObject []
            for_ [claim0, claim1, claim2] $ \claim ->
              getUserInfoWithExpTime claim [("Authorization", "IGNORED")] mode
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
  let secret = hashAdminSecret "secret"
      unauthRole = mkRoleNameE "anon"

  -- These are all various error cases, except for the AMNoAuth mode:
  it "with no admin secret provided" $ do
    setupAuthMode' Nothing       Nothing             Nothing              Nothing
      `shouldReturn` (Right AMNoAuth)
    -- We insist on an admin secret in order to use webhook or JWT auth:
    setupAuthMode' Nothing       Nothing             (Just fakeJWTConfig) Nothing
      `shouldReturn` Left ()
    setupAuthMode' Nothing       (Just fakeAuthHook) Nothing              Nothing
      `shouldReturn` Left ()
    -- ...and we can't have both:
    setupAuthMode' Nothing       (Just fakeAuthHook) (Just fakeJWTConfig) Nothing
      `shouldReturn` Left ()
    -- If the unauthenticated role was set but would otherwise be ignored this
    -- should be an error (for now), since users might expect all access to use
    -- the specified role. This first case would be the real worrying one:
    setupAuthMode' Nothing       Nothing             Nothing              (Just unauthRole)
      `shouldReturn` Left ()
    setupAuthMode' Nothing       Nothing             (Just fakeJWTConfig) (Just unauthRole)
      `shouldReturn` Left ()
    setupAuthMode' Nothing       (Just fakeAuthHook) Nothing              (Just unauthRole)
      `shouldReturn` Left ()
    setupAuthMode' Nothing       (Just fakeAuthHook) (Just fakeJWTConfig) (Just unauthRole)
      `shouldReturn` Left ()

  it "with admin secret provided" $ do
    setupAuthMode' (Just secret) Nothing             Nothing              Nothing
      `shouldReturn` Right (AMAdminSecret secret Nothing)
    setupAuthMode' (Just secret) Nothing             Nothing              (Just unauthRole)
      `shouldReturn` Right (AMAdminSecret secret $ Just unauthRole)

    setupAuthMode' (Just secret) Nothing            (Just fakeJWTConfig) Nothing >>= \case
      Right (AMAdminSecretAndJWT s _ Nothing) -> do
        s `shouldBe` secret
      _ -> expectationFailure "AMAdminSecretAndJWT"
    setupAuthMode' (Just secret) Nothing            (Just fakeJWTConfig) (Just unauthRole) >>= \case
      Right (AMAdminSecretAndJWT s _ ur) -> do
        s `shouldBe` secret
        ur `shouldBe` Just unauthRole
      _ -> expectationFailure "AMAdminSecretAndJWT"

    setupAuthMode' (Just secret) (Just fakeAuthHook) Nothing              Nothing
      `shouldReturn` Right (AMAdminSecretAndHook secret fakeAuthHook)
    -- auth hook can't make use of unauthenticated role for now (no good UX):
    setupAuthMode' (Just secret) (Just fakeAuthHook) Nothing              (Just unauthRole)
      `shouldReturn` Left ()
    -- we can't have both:
    setupAuthMode' (Just secret) (Just fakeAuthHook) (Just fakeJWTConfig) Nothing
      `shouldReturn` Left ()
    setupAuthMode' (Just secret) (Just fakeAuthHook) (Just fakeJWTConfig) (Just unauthRole)
      `shouldReturn` Left ()

parseClaimsMapTests :: Spec
parseClaimsMapTests = describe "parseClaimMapTests" $ do
  let
    parseClaimsMap_
      :: J.Object
      -> JWTClaims
      -> IO (Either Code ClaimsMap)
    parseClaimsMap_ obj claims =
      runExceptT $ withExceptT qeCode $ parseClaimsMap obj claims

    unObject l = case J.object l of
      J.Object o -> o
      _          -> error "Impossible!"

    defaultClaimsMap =
      Map.fromList
       [ (allowedRolesClaim, J.toJSON (map mkRoleNameE ["user","editor"]))
       , (defaultRoleClaim, J.toJSON (mkRoleNameE "user"))]

  describe "JWT configured with namespace" $ do

    describe "JWT configured with namespace key, the key is a text value which is expected to be at the root of the JWT token" $ do
      it "parses claims map from the JWT token with correct namespace " $ do
        let claimsObj = unObject $
                        [ "x-hasura-allowed-roles" .= (["user","editor"] :: [Text])
                        , "x-hasura-default-role"  .= ("user" :: Text)
                        ]
        let obj = (unObject $ ["claims_map" .= claimsObj])
        parseClaimsMap_ obj  (JCNamespace (ClaimNs "claims_map") defaultClaimsFormat)
          `shouldReturn`
          Right defaultClaimsMap

      it "doesn't parse claims map from the JWT token with wrong namespace " $ do
        let claimsObj = unObject $
                        [ "x-hasura-allowed-roles" .= (["user","editor"] :: [Text])
                        , "x-hasura-default-role"  .= ("user" :: Text)
                        ]
        let obj = (unObject $ ["claims_map" .= claimsObj])
        parseClaimsMap_ obj  (JCNamespace (ClaimNs "wrong_claims_map") defaultClaimsFormat)
          `shouldReturn`
          Left JWTInvalidClaims

    describe "JWT configured with namespace JSON path, JSON path to the claims map" $ do
      it "parse claims map from the JWT token using claims namespace JSON Path" $ do
        let obj = unObject $
                        [ "x-hasura-allowed-roles" .= (["user","editor"] :: [Text])
                        , "x-hasura-default-role"  .= ("user" :: Text)
                        , "sub" .= ("random" :: Text)
                        , "exp" .= (1626420800 :: Int)        -- we ignore these non session variables, in the response
                        ]
        parseClaimsMap_ obj  (JCNamespace (ClaimNsPath (mkJSONPathE "$")) defaultClaimsFormat)
        -- "$" JSON path signifies the claims are to be found in the root of the JWT token
          `shouldReturn`
          Right defaultClaimsMap

      it "throws error while attempting to parse claims map from the JWT token with a wrong namespace JSON Path" $ do
        let claimsObj = unObject $
                        [ "x-hasura-allowed-roles" .= (["user","editor"] :: [Text])
                        , "x-hasura-default-role"  .= ("user" :: Text)
                        ]
            obj = unObject $ [ "hasura_claims" .= claimsObj ]
        parseClaimsMap_ obj  (JCNamespace (ClaimNsPath (mkJSONPathE "$.claims")) defaultClaimsFormat)
          `shouldReturn`
          Left JWTInvalidClaims

  describe "JWT configured with custom JWT claims" $ do

    let rolesObj = unObject $
                    [ "allowed" .= (["user","editor"] :: [Text])
                    , "default"  .= ("user" :: Text)
                    ]
        userId = unObject [ "id" .= ("1" :: Text)]
        obj = unObject $ [ "roles" .= rolesObj
                         , "user"  .= userId
                         ]
        userIdClaim = mkSessionVariable "x-hasura-user-id"

    describe "custom claims with JSON paths to the claim location in the JWT token" $ do

      it "parse custom claims values, with correct values" $ do
        let customDefRoleClaim = mkCustomDefaultRoleClaim (Just "$.roles.default") Nothing
            customAllowedRolesClaim = mkCustomAllowedRoleClaim (Just "$.roles.allowed") Nothing
            otherClaims = Map.fromList
                 [(userIdClaim, (mkCustomOtherClaim (Just "$.user.id") Nothing))]
            customClaimsMap = JWTCustomClaimsMap customDefRoleClaim customAllowedRolesClaim otherClaims

        parseClaimsMap_ obj  (JCMap customClaimsMap)
          `shouldReturn`
          Right (Map.fromList
           [ (allowedRolesClaim, J.toJSON (map mkRoleNameE ["user","editor"]))
           , (defaultRoleClaim, J.toJSON (mkRoleNameE "user"))
           , (userIdClaim, J.String "1")
           ])

      it "throws error when a specified custom claim value is missing" $ do

        let customDefRoleClaim = mkCustomDefaultRoleClaim (Just "$.roles.wrong_default") Nothing -- wrong path provided
            customAllowedRolesClaim = mkCustomAllowedRoleClaim (Just "$.roles.allowed") Nothing
            customClaimsMap = JWTCustomClaimsMap customDefRoleClaim customAllowedRolesClaim mempty
        parseClaimsMap_ obj  (JCMap customClaimsMap)
          `shouldReturn`
          Left JWTRoleClaimMissing

      it "doesn't throw an error when the specified custom claim is missing, but the default value is provided" $ do

        let customDefRoleClaim = mkCustomDefaultRoleClaim (Just "$.roles.wrong_default") (Just "editor")
            customAllowedRolesClaim = mkCustomAllowedRoleClaim (Just "$.roles.allowed") Nothing
            customClaimsMap = JWTCustomClaimsMap customDefRoleClaim customAllowedRolesClaim mempty
        parseClaimsMap_ obj  (JCMap customClaimsMap)
          `shouldReturn`
          Right (Map.fromList
           [ (allowedRolesClaim, J.toJSON (map mkRoleNameE ["user","editor"]))
           , (defaultRoleClaim, J.toJSON (mkRoleNameE "editor"))
           ])

    describe "custom claims with literal values" $ do

      it "uses the literal custom claim value" $ do

        let customDefRoleClaim = mkCustomDefaultRoleClaim Nothing (Just "editor")
            customAllowedRolesClaim = mkCustomAllowedRoleClaim Nothing (Just ["user", "editor"])
            customClaimsMap = JWTCustomClaimsMap customDefRoleClaim customAllowedRolesClaim mempty
        parseClaimsMap_ mempty (JCMap customClaimsMap)
          `shouldReturn`
          Right (Map.fromList
           [ (allowedRolesClaim, J.toJSON (map mkRoleNameE ["user","editor"]))
           , (defaultRoleClaim, J.toJSON (mkRoleNameE "editor"))
           ])

mkCustomDefaultRoleClaim :: (Maybe Text) -> (Maybe Text) -> JWTCustomClaimsMapDefaultRole
mkCustomDefaultRoleClaim claimPath defVal =
  -- check if claimPath is provided, if not then use the default value
  -- as the literal value by removing the `Maybe` of defVal
  case claimPath of
    Just path -> JWTCustomClaimsMapJSONPath (mkJSONPathE path) $ defRoleName
    Nothing -> JWTCustomClaimsMapStatic $ maybe (mkRoleNameE "user") id defRoleName
  where
    defRoleName = mkRoleNameE <$> defVal

mkCustomAllowedRoleClaim :: (Maybe Text) -> (Maybe [Text]) -> JWTCustomClaimsMapAllowedRoles
mkCustomAllowedRoleClaim claimPath defVal =
  -- check if claimPath is provided, if not then use the default value
  -- as the literal value by removing the `Maybe` of defVal
  case claimPath of
    Just path -> JWTCustomClaimsMapJSONPath (mkJSONPathE path) $ defAllowedRoles
    Nothing ->
      JWTCustomClaimsMapStatic $
        maybe (fmap mkRoleNameE $ ["user", "editor"]) id defAllowedRoles
  where
    defAllowedRoles = fmap mkRoleNameE <$> defVal

-- use for claims other than `x-hasura-default-role` and `x-hasura-allowed-roles`
mkCustomOtherClaim :: (Maybe Text) -> (Maybe Text) -> JWTCustomClaimsMapValue
mkCustomOtherClaim claimPath defVal =
  -- check if claimPath is provided, if not then use the default value
  -- as the literal value by removing the `Maybe` of defVal
  case claimPath of
    Just path -> JWTCustomClaimsMapJSONPath (mkJSONPathE path) $ defVal
    Nothing -> JWTCustomClaimsMapStatic $ maybe "default claim value" id defVal

fakeJWTConfig :: JWTConfig
fakeJWTConfig =
  let jcKeyOrUrl = Left (Jose.fromOctets [])
      jcAudience = Nothing
      jcIssuer = Nothing
      jcClaims = JCNamespace (ClaimNs "") JCFJson
   in JWTConfig{..}

fakeAuthHook :: AuthHook
fakeAuthHook = AuthHookG "http://fake" AHTGet

mkRoleNameE :: Text -> RoleName
mkRoleNameE = fromMaybe (error "fixme") . mkRoleName

mkJSONPathE :: Text -> JSONPath
mkJSONPathE = either error id . parseJSONPath


newtype NoReporter a = NoReporter { runNoReporter :: IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadBaseControl IO)

instance Tracing.HasReporter NoReporter

setupAuthMode'
  :: Maybe AdminSecretHash
  -> Maybe AuthHook
  -> Maybe JWTConfig
  -> Maybe RoleName
  -> IO (Either () AuthMode)
setupAuthMode'  mAdminSecretHash mWebHook mJwtSecret mUnAuthRole =
  withVersion (VersionDev "fake") $
  -- just throw away the error message for ease of testing:
  fmap (either (const $ Left ()) Right)
    $ runNoReporter
    $ runExceptT
    $ setupAuthMode mAdminSecretHash mWebHook mJwtSecret mUnAuthRole
      -- NOTE: this won't do any http or launch threads if we don't specify JWT URL:
      (error "H.Manager") (Logger $ void . return)
