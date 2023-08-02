module Hasura.Server.Auth.JWTSpec (spec) where

import Control.Arrow
import Data.ByteString.UTF8 qualified as BS
import Data.Fixed (Pico)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime (..), addUTCTime, defaultTimeLocale, formatTime, fromGregorian, secondsToDiffTime, secondsToNominalDiffTime)
import Hasura.Logging (Hasura, Logger (..))
import Hasura.Prelude
import Hasura.Server.Auth.JWT qualified as JWT
import Hasura.Server.Auth.JWT.Logging (JwkFetchError)
import Network.HTTP.Types (Header, ResponseHeaders)
import Test.Hspec

spec :: Spec
spec = do
  determineJwkExpiryLifetimeTests

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
