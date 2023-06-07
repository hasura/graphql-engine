module Network.HTTP.Client.TransformableSpec (spec) where

-- import           Data.Aeson                        (eitherDecodeStrict)

-- import           Test.QuickCheck

import Control.Exception (Exception (displayException))
import Control.Lens
import Data.CaseInsensitive qualified as CI
import Data.Text qualified as T
import Hasura.Prelude
import Network.HTTP.Client.Transformable qualified as Client
import Test.Hspec

spec :: Spec
spec = do
  specMkRequest
  specBodyLens
  specHeadersLens
  specHostLens
  specUrlLens
  specMethodLens
  specPathLens
  specPortLens
  specQueryParamsLens
  specTimeoutLens

specMkRequest :: Spec
specMkRequest = describe "mRequestEither" $ do
  it "Constructs a valid request" $ do
    -- GIVEN
    let url' = "http://localhost:1234/foo/bar?baz=bam"

    -- WHEN
    let result = either (const Nothing) (const $ Just ()) $ Client.mkRequestEither url'

    -- THEN
    result `shouldBe` Just ()

  it "Returns left on bad URL" $ do
    -- GIVEN
    let url' = "http:://localhost:1234/foo/bar?baz=bam"

    -- WHEN
    let result = either (Left . displayException) (const $ Right ()) $ Client.mkRequestEither url'

    -- THEN
    result `shouldBe` Left "InvalidUrlException \"http:://localhost:1234/foo/bar?baz=bam\" \"URL must be absolute\""

-- | Make 'Request' from some URL 'Text', throwing an impure exception when the
-- input is malformed.
--
-- This should only be used in the test suite, where we have known-good URL
-- test fixtures.
unsafeMkRequest :: Text -> Client.Request
unsafeMkRequest urlTxt = case Client.mkRequestEither urlTxt of
  Right req -> req
  Left _err -> error . T.unpack $ "Malformed URL [ " <> urlTxt <> " ]"

specBodyLens :: Spec
specBodyLens = describe "Body Lens" $ do
  -- GIVEN
  let req = unsafeMkRequest "http://localhost:1234/foo/bar?baz=bam"

  it "get body s ≡ a" $ do
    -- THEN
    preview (Client.body . Client._RequestBodyLBS) req `shouldBe` Just mempty

  it "get body . set body b ≡ b" $ do
    -- WHEN
    let req' = set Client.body (Client.RequestBodyLBS "{ \"hello\": \"world\"}") req

    -- THEN
    preview (Client.body . Client._RequestBodyLBS) req' `shouldBe` (Just "{ \"hello\": \"world\"}")

  it "over id ≡ id" $ do
    -- WHEN
    let req' = over Client.body id req

    -- THEN
    preview (Client.body . Client._RequestBodyLBS) req' `shouldBe` Just mempty

  it "over body (const b) ≡ set body b" $ do
    -- WHEN
    let req' = over Client.body (const (Client.RequestBodyLBS "{ \"hello\": \"world\"}")) req

    -- THEN
    preview (Client.body . Client._RequestBodyLBS) req' `shouldBe` (Just "{ \"hello\": \"world\"}")

specHeadersLens :: Spec
specHeadersLens = describe "Headers Lens" $ do
  -- GIVEN
  let req = unsafeMkRequest "http://localhost:1234/foo/bar?baz=bam"

  it "get headers s ≡ a" $ do
    -- THEN
    view Client.headers req `shouldBe` []

  it "get headers . set headers b ≡ b" $ do
    -- WHEN
    let req' = set Client.headers [(CI.mk "Content-Type", "Application/pdf")] req

    -- THEN
    view Client.headers req' `shouldBe` [(CI.mk "Content-Type", "Application/pdf")]

  it "over id ≡ id" $ do
    -- WHEN
    let req' = over Client.headers id req

    -- THEN
    view Client.headers req' `shouldBe` []

  it "over headers (const b) ≡ set headers b" $ do
    -- WHEN
    let req' = over Client.headers (const [(CI.mk "Content-Type", "Application/pdf")]) req

    -- THEN
    view Client.headers req' `shouldBe` [(CI.mk "Content-Type", "Application/pdf")]

specHostLens :: Spec
specHostLens = describe "Host Lens" $ do
  -- GIVEN
  let req = unsafeMkRequest "http://localhost:1234/foo/bar?baz=bam"

  it "get host s ≡ a" $ do
    -- THEN
    view Client.host req `shouldBe` "localhost"

  it "get host . set host b ≡ b" $ do
    -- WHEN
    let req' = set Client.host "www.google.com" req

    -- THEN
    view Client.host req' `shouldBe` "www.google.com"

  it "over id ≡ id" $ do
    -- WHEN
    let req' = over Client.host id req

    -- THEN
    view Client.host req' `shouldBe` "localhost"

  it "over host (const b) ≡ set host b" $ do
    -- WHEN
    let req' = over Client.host (const "www.google.com") req

    -- THEN
    view Client.host req' `shouldBe` "www.google.com"

specUrlLens :: Spec
specUrlLens = describe "url lens" $ do
  -- GIVEN
  let url' = "http://localhost:1234/foo/bar?baz=bam"
      req = unsafeMkRequest url'

  it "get url s ≡ a" $ do
    -- THEN
    view Client.url req `shouldBe` url'
    view Client.path req `shouldBe` "/foo/bar"
    view Client.port req `shouldBe` 1234
    view Client.queryParams req `shouldBe` [("baz", Just "bam")]

  it "get url . set url b ≡ b" $ do
    -- WHEN
    let url'' = "http://www.google.com:8080/foo?bar=baz"
        req' = set Client.url url'' req

    -- THEN
    view Client.url req' `shouldBe` url''

  it "over id ≡ id" $ do
    -- WHEN
    let req' = over Client.url id req

    -- THEN
    view Client.url req' `shouldBe` url'

  it "over url (const b) ≡ set url b" $ do
    -- WHEN
    let url'' = "http://www.google.com:8080/foo?bar=baz"
        req' = over Client.url (const url'') req

    -- THEN
    view Client.url req' `shouldBe` url''

  it "Setting with http:// scheme sets port to 80" $ do
    -- WHEN
    let url'' = "http://www.google.com/foo?bar=baz"
        req' = set Client.url url'' req

    -- THEN
    view Client.url req' `shouldBe` url''
    view Client.port req' `shouldBe` 80

  it "Setting with https:// scheme sets port to 443" $ do
    -- WHEN
    let url'' = "https://www.google.com/foo?bar=baz"
        req' = set Client.url url'' req

    -- THEN
    view Client.url req' `shouldBe` url''
    view Client.port req' `shouldBe` 443

  it "Setting with explicit port sets that port" $ do
    -- WHEN
    let url'' = "https://www.google.com:456/foo?bar=baz"
        req' = set Client.url url'' req

    -- THEN
    view Client.url req' `shouldBe` url''
    view Client.port req' `shouldBe` 456

  it "Setting with no path clears path" $ do
    -- WHEN
    let url'' = "http://www.google.com?bar=baz"
        req' = set Client.url url'' req

    -- THEN
    view Client.url req' `shouldBe` url''
    view Client.path req' `shouldBe` mempty

  it "Setting with no query params clears query params" $ do
    -- WHEN
    let url'' = "http://www.google.com"
        req' = set Client.url url'' req

    -- THEN
    view Client.url req' `shouldBe` url''
    view Client.queryParams req' `shouldBe` mempty

specMethodLens :: Spec
specMethodLens = describe "Method Lens" $ do
  -- GIVEN
  let req = unsafeMkRequest "http://localhost:1234/foo/bar?baz=bam"

  it "get method s ≡ a" $ do
    -- THEN
    view Client.method req `shouldBe` "GET"

  it "get method . set method b ≡ b" $ do
    -- WHEN
    let req' = set Client.method "POST" req

    -- THEN
    view Client.method req' `shouldBe` "POST"

  it "over id ≡ id" $ do
    -- WHEN
    let req' = over Client.method id req

    -- THEN
    view Client.method req' `shouldBe` "GET"

  it "over method (const b) ≡ set method b" $ do
    -- WHEN
    let req' = over Client.method (const "POST") req

    -- THEN
    view Client.method req' `shouldBe` "POST"

specPathLens :: Spec
specPathLens = describe "Path Lens" $ do
  -- GIVEN
  let req = unsafeMkRequest "http://localhost:1234/foo/bar?baz=bam"

  it "get path s ≡ a" $ do
    -- THEN
    view Client.path req `shouldBe` "/foo/bar"

  it "get path . set path b ≡ b" $ do
    -- WHEN
    let req' = set Client.path "/baz/bam" req

    -- THEN
    view Client.path req' `shouldBe` "/baz/bam"

  it "over id ≡ id" $ do
    -- WHEN
    let req' = over Client.path id req

    -- THEN
    view Client.path req' `shouldBe` "/foo/bar"

  it "over path (const b) ≡ set path b" $ do
    -- WHEN
    let req' = over Client.path (const "/baz/bam") req

    -- THEN
    view Client.path req' `shouldBe` "/baz/bam"

specPortLens :: Spec
specPortLens = describe "Port Lens" $ do
  -- GIVEN
  let req = unsafeMkRequest "http://localhost:1234/foo/bar?baz=bam"

  it "get port s ≡ a" $ do
    -- THEN
    view Client.port req `shouldBe` 1234

  it "get port . set port b ≡ b" $ do
    -- WHEN
    let req' = set Client.port 5000 req

    -- THEN
    view Client.port req' `shouldBe` 5000

  it "over id ≡ id" $ do
    -- WHEN
    let req' = over Client.port id req

    -- THEN
    view Client.port req' `shouldBe` 1234

  it "over port (const b) ≡ set port b" $ do
    -- WHEN
    let req' = over Client.port (const 5000) req

    -- THEN
    view Client.port req' `shouldBe` 5000

specQueryParamsLens :: Spec
specQueryParamsLens = describe "QueryParams Lens" $ do
  -- GIVEN
  let req = unsafeMkRequest "http://localhost:1234/foo/bar?baz=bam"

  it "get queryParams s ≡ a" $ do
    -- THEN
    view Client.queryParams req `shouldBe` [("baz", Just "bam")]

  it "get queryParams . set queryParams b ≡ b" $ do
    -- WHEN
    let req' = set Client.queryParams [("key", Just "value"), ("flag", Nothing)] req

    -- THEN
    view Client.queryParams req' `shouldBe` [("key", Just "value"), ("flag", Nothing)]

  it "over id ≡ id" $ do
    -- WHEN
    let req' = over Client.queryParams id req

    -- THEN
    view Client.queryParams req' `shouldBe` [("baz", Just "bam")]

  it "over queryParams (const b) ≡ set queryParams b" $ do
    -- WHEN
    let req' = over Client.queryParams (const [("key", Just "value"), ("flag", Nothing)]) req

    -- THEN
    view Client.queryParams req' `shouldBe` [("key", Just "value"), ("flag", Nothing)]

specTimeoutLens :: Spec
specTimeoutLens = describe "QueryParams Lens" $ do
  -- GIVEN
  let req = unsafeMkRequest "http://localhost:1234/foo/bar?baz=bam"

  it "get timeout s ≡ a" $ do
    -- THEN
    view Client.timeout req `shouldBe` Client.responseTimeoutDefault

  it "get timeout . set timeout b s ≡ b" $ do
    -- WHEN
    let req' = set Client.timeout (Client.responseTimeoutMicro 5000) req

    -- THEN
    view Client.timeout req' `shouldBe` Client.responseTimeoutMicro 5000

  it "over timeout id ≡ id" $ do
    -- WHEN
    let req' = over Client.timeout id req

    -- THEN
    view Client.timeout req' `shouldBe` Client.responseTimeoutDefault

  it "over timeout (const b) ≡ set timeout b" $ do
    -- WHEN
    let req' = over Client.timeout (const (Client.responseTimeoutMicro 5000)) req

    -- THEN
    view Client.timeout req' `shouldBe` Client.responseTimeoutMicro 5000
