{-# LANGUAGE NumericUnderscores #-}

-- | Helper functions for HTTP requests.
module Harness.Http
  ( get_,
    getWithStatus,
    post,
    postValue,
    postValueWithStatus,
    healthCheck,
    healthCheck',
    HealthCheckResult (..),
    Http.RequestHeaders,
  )
where

import Conduit (foldMapC, runConduit, (.|))
import Control.Concurrent.Extended (sleep)
import Control.Exception
import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.String
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import GHC.Stack
import Hasura.Prelude
import Network.HTTP.Client.Conduit qualified as Http.Conduit
import Network.HTTP.Simple qualified as Http
import Network.HTTP.Types qualified as Http
import Text.Pretty.Simple (pShow)

--------------------------------------------------------------------------------
-- API

-- | Performs get, doesn't return the result. Simply throws if there's
-- not a 200 response.
get_ :: (HasCallStack) => String -> IO ()
get_ = getWithStatus [200]

-- | Performs get, doesn't return the result. Simply throws if there's
-- not an expected response status code.
getWithStatus :: (HasCallStack) => [Int] -> String -> IO ()
getWithStatus acceptableStatusCodes url =
  Http.withResponse @_ @IO (fromString url) \response -> do
    let actualStatusCode = Http.getResponseStatusCode response
    unless (actualStatusCode `elem` acceptableStatusCodes) $ do
      body <- runConduit $ Http.getResponseBody response .| foldMapC id
      fail
        $ unlines
          [ "The HTTP response had an unexpected response code.",
            "URL: " <> url,
            "Expected status codes: " <> show acceptableStatusCodes,
            "Actual status code: " <> show actualStatusCode,
            "Body:",
            T.unpack $ T.decodeUtf8 body
          ]

-- | Post the JSON to the given URL, and produces a very descriptive
-- exception on failure.
postValue :: (HasCallStack) => String -> Http.RequestHeaders -> Value -> IO Value
postValue = postValueWithStatus 200

post :: String -> Http.RequestHeaders -> Value -> IO (Http.Response L8.ByteString)
post url headers value = do
  let request =
        fromString url
          & Http.setRequestHeaders headers
          & Http.setRequestMethod Http.methodPost
          & Http.setRequestBodyJSON value
          & Http.setRequestResponseTimeout (Http.Conduit.responseTimeoutMicro 60_000_000)
  response <- Http.httpLbs request
  unless ("Content-Type" `elem` (fst <$> Http.getResponseHeaders response))
    $ error ("Missing Content-Type header in response. Response: " <> TL.unpack (pShow response))
  pure response

-- | Post the JSON to the given URL and expected HTTP response code.
-- Produces a very descriptive exception or failure.
postValueWithStatus :: (HasCallStack) => Int -> String -> Http.RequestHeaders -> Value -> IO Value
postValueWithStatus statusCode url headers value = do
  response <- post url headers value
  let requestBodyString = L8.unpack $ encode value
      responseBodyString = L8.unpack $ Http.getResponseBody response
      responseStatusCode = Http.getResponseStatusCode response
  if responseStatusCode == statusCode
    then
      eitherDecode (Http.getResponseBody response)
        `onLeft` \err ->
          reportError
            [ "In request: " ++ url,
              "With body:",
              requestBodyString,
              "Couldn't decode JSON body:",
              err,
              "Body was:",
              responseBodyString
            ]
    else
      reportError
        [ "Expecting reponse code ",
          show statusCode,
          " but got ",
          show responseStatusCode,
          " from HTTP request: ",
          url,
          "With body:",
          requestBodyString,
          "Response body is:",
          responseBodyString
        ]
  where
    reportError = error . unlines

-- | Wait for a service to become healthy.
healthCheck :: (HasCallStack) => String -> IO ()
healthCheck url = do
  result <- healthCheck' url
  case result of
    Healthy -> return ()
    Unhealthy failures ->
      error
        $ "Health check failed for URL: "
        ++ url
        ++ ", with failures: "
        ++ show failures
        ++ "\nIs graphql-engine starting up without errors outside of this test suite?"

data HealthCheckResult = Healthy | Unhealthy [Http.HttpException]

-- | Wait for a service to become healthy.
healthCheck' :: (HasCallStack) => String -> IO HealthCheckResult
healthCheck' url = loop [] httpHealthCheckAttempts
  where
    loop failures 0 = return $ Unhealthy failures
    loop failures attempts =
      catch
        (getWithStatus [200, 204] url >> return Healthy)
        ( \(failure :: Http.HttpException) -> do
            sleep httpHealthCheckIntervalSeconds
            loop (failure : failures) (attempts - 1)
        )

-- * HTTP health checks

httpHealthCheckAttempts :: Int
httpHealthCheckAttempts = 15

httpHealthCheckIntervalSeconds :: DiffTime
httpHealthCheckIntervalSeconds = 1
