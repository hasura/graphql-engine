module Hasura.Server.Auth.JWTSpec (spec) where

import Control.Arrow
import Crypto.JOSE.JWK qualified as JoseJWK
import Crypto.JWT qualified as CryptoJWT
import Data.ByteString.UTF8 qualified as BS
import Data.Fixed (Pico)
import Control.Lens ((.~))
import Data.Either (isLeft, isRight)
import Data.IORef (newIORef)
import Data.Word (Word8)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime (..), addUTCTime, defaultTimeLocale, formatTime, fromGregorian, secondsToDiffTime, secondsToNominalDiffTime)
import Hasura.Logging (Hasura, Logger (..))
import Hasura.Prelude
import Hasura.Server.Auth.JWT (JWTClaimCheckConfig (..), JWTClaims (..), JWTClaimsFormat (..), JWTCtx (..), JWTHeader (..), JWTNamespace (..), RawJWT (..), verifyJwt)
import Hasura.Server.Auth.JWT qualified as JWT
import Hasura.Server.Auth.JWT.Logging (JwkFetchError)
import Network.HTTP.Types (Header, ResponseHeaders)
import Test.Hspec

spec :: Spec
spec = do
  audienceValidationTests
  determineJwkExpiryLifetimeTests

-- | Audience validation tests. jose skips its 'audCheck' predicate entirely
-- when 'aud' is absent from a token, so by default a token omitting 'aud' is
-- accepted even when 'audience' is configured. 'CheckInvalidIfMissing' opts
-- into strict validation where absence is treated as a mismatch.
audienceValidationTests :: Spec
audienceValidationTests = describe "JWT audience validation" $ do
  let testJwk = JoseJWK.fromOctets (replicate 32 (0x42 :: Word8))
      mkCtx audCheck = do
        keyRef <- newIORef (JoseJWK.JWKSet [testJwk], Nothing)
        pure
          JWTCtx
            { jcxUrl = Nothing
            , jcxKeyConfig = keyRef
            , jcxAudience = audCheck
            , jcxIssuer = Nothing
            , jcxClaims = JCNamespace (ClaimNs "hasura") JCFJson
            , jcxAllowedSkew = Just (secondsToNominalDiffTime (10 * 365 * 24 * 3600))
            , jcxHeader = JHAuthorization
            }
      signNoAud = do
        let action :: CryptoJWT.JOSE CryptoJWT.Error IO CryptoJWT.SignedJWT
            action = do
              alg <- JoseJWK.bestJWSAlg testJwk
              CryptoJWT.signClaims testJwk (CryptoJWT.newJWSHeader ((), alg))
                (CryptoJWT.emptyClaimsSet & CryptoJWT.claimSub .~ Just "test-user")
        CryptoJWT.runJOSE action >>= either (fail . show) pure

  it "by default accepts a JWT with no aud claim even when audience is configured" $ do
    ctx <- mkCtx (Just JWTClaimCheckConfig { checkValue = CryptoJWT.Audience ["myapp"], invalidIfMissing = False })
    signed <- signNoAud
    result <- runExceptT (verifyJwt ctx (RawJWT (CryptoJWT.encodeCompact signed)))
    result `shouldSatisfy` isRight

  it "rejects a JWT with no aud claim when extra_required_claims includes audience" $ do
    ctx <- mkCtx (Just JWTClaimCheckConfig { checkValue = CryptoJWT.Audience ["myapp"], invalidIfMissing = True })
    signed <- signNoAud
    result <- runExceptT (verifyJwt ctx (RawJWT (CryptoJWT.encodeCompact signed)))
    result `shouldSatisfy` isLeft

determineJwkExpiryLifetimeTests :: Spec
determineJwkExpiryLifetimeTests = describe "determineJwkExpiryLifetime" $ do
  it "no-cache in Cache-Control means an immediate expiry" $ do
    let expires = expiresHeader (addUTCTime (secondsToNominalDiffTime 60) currentTimeForTest)
    let cacheControl = cacheControlHeader "max-age=10, no-cache"
    result <- determineJwkExpiryLifetime' [expires, cacheControl]
    result `shouldBe` (Right . Just $ expectedExpiryTime 0)

  it "must-revalidate without max-age in Cache-Control means an immediate expiry" $ do
    let expires = expiresHeader (addUTCTime (secondsToNominalDiffTime 60) currentTimeForTest)
    let cacheControl = cacheControlHeader "must-revalidate"
    result <- determineJwkExpiryLifetime' [expires, cacheControl]
    result `shouldBe` (Right . Just $ expectedExpiryTime 0)

  it "must-revalidate with max-age in Cache-Control uses max-age for token expiry" $ do
    let expires = expiresHeader (addUTCTime (secondsToNominalDiffTime 60) currentTimeForTest)
    let cacheControl = cacheControlHeader "max-age=10, must-revalidate"
    result <- determineJwkExpiryLifetime' [expires, cacheControl]
    result `shouldBe` (Right . Just $ expectedExpiryTime 10)

  it "no-store in Cache-Control means an immediate expiry" $ do
    let expires = expiresHeader (addUTCTime (secondsToNominalDiffTime 60) currentTimeForTest)
    let cacheControl = cacheControlHeader "max-age=10, no-store"
    result <- determineJwkExpiryLifetime' [expires, cacheControl]
    result `shouldBe` (Right . Just $ expectedExpiryTime 0)

  it "max-age in Cache-Control without no-cache, no-store is used for token expiry" $ do
    let expires = expiresHeader (addUTCTime (secondsToNominalDiffTime 60) currentTimeForTest)
    let cacheControl = cacheControlHeader "public, max-age=10"
    result <- determineJwkExpiryLifetime' [expires, cacheControl]
    result `shouldBe` (Right . Just $ expectedExpiryTime 10)

  it "s-maxage in Cache-Control without no-cache, no-store is used for token expiry" $ do
    let expires = expiresHeader (addUTCTime (secondsToNominalDiffTime 60) currentTimeForTest)
    let cacheControl = cacheControlHeader "public, s-maxage=10"
    result <- determineJwkExpiryLifetime' [expires, cacheControl]
    result `shouldBe` (Right . Just $ expectedExpiryTime 10)

  it "Expires header is used as a fallback if Cache-Control contains nothing indicative" $ do
    let expires = expiresHeader (addUTCTime (secondsToNominalDiffTime 60) currentTimeForTest)
    let cacheControl = cacheControlHeader "public"
    result <- determineJwkExpiryLifetime' [expires, cacheControl]
    result `shouldBe` (Right . Just $ expectedExpiryTime 60)

  it "Expires header is used as a fallback if Cache-Control is missing" $ do
    let expires = expiresHeader (addUTCTime (secondsToNominalDiffTime 60) currentTimeForTest)
    result <- determineJwkExpiryLifetime' [expires]
    result `shouldBe` (Right . Just $ expectedExpiryTime 60)

  it "If no relevant headers, then return Nothing" $ do
    result <- determineJwkExpiryLifetime' [("X-SomeOtherHeader", "Irrelevant")]
    result `shouldBe` (Right Nothing)

  it "If max-age in Cache-Control is corrupt, then return an error" $ do
    let expires = expiresHeader (addUTCTime (secondsToNominalDiffTime 60) currentTimeForTest)
    let cacheControl = cacheControlHeader "max-age=lolbroken"
    result <- determineJwkExpiryLifetime' [expires, cacheControl]
    result `shouldBe` (Left ())

  it "If Expires is corrupt, then return an error" $ do
    let expires = ("Expires", "lolbroken")
    let cacheControl = cacheControlHeader "public"
    result <- determineJwkExpiryLifetime' [expires, cacheControl]
    result `shouldBe` (Left ())

determineJwkExpiryLifetime' :: (MonadIO m) => ResponseHeaders -> m (Either () (Maybe UTCTime))
determineJwkExpiryLifetime' headers =
  discardJwkFetchError <$> runExceptT (JWT.determineJwkExpiryLifetime (pure currentTimeForTest) voidLogger headers)

currentTimeForTest :: UTCTime
currentTimeForTest = UTCTime (fromGregorian 2021 01 21) (secondsToDiffTime 0)

voidLogger :: Logger Hasura
voidLogger = (Logger $ void . return)

cacheControlHeader :: Text -> Header
cacheControlHeader val = ("Cache-Control", encodeUtf8 val)

expiresHeader :: UTCTime -> Header
expiresHeader val = ("Expires", BS.fromString $ formatTime defaultTimeLocale "%a, %d %b %Y %T GMT" val)

-- JwkFetchError is not Eq, so we'll discard it for testing
discardJwkFetchError :: Either JwkFetchError a -> Either () a
discardJwkFetchError = left (const ())

-- get expected expiry timestamp
expectedExpiryTime :: Pico -> UTCTime
expectedExpiryTime secs = addUTCTime (secondsToNominalDiffTime secs) currentTimeForTest
