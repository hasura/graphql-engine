-- | Helper functions for HTTP requests.
module Harness.Http
  ( get_,
    getWithStatus,
    post,
    postValue,
    postValueWithStatus,
    healthCheck,
    Http.RequestHeaders,
  )
where

import Control.Concurrent.Extended (sleep)
import Control.Exception
import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.String
import GHC.Stack
import Harness.Constants qualified as Constants
import Hasura.Prelude
import Network.HTTP.Simple qualified as Http
import Network.HTTP.Types qualified as Http

--------------------------------------------------------------------------------
-- API

-- | Performs get, doesn't return the result. Simply throws if there's
-- not a 200 response.
get_ :: HasCallStack => String -> IO ()
get_ url = do
  response <- Http.httpNoBody (fromString url)
  unless (Http.getResponseStatusCode response == 200) $
    error ("Non-200 response code from HTTP request: " ++ url)

-- | Performs get, doesn't return the result. Simply throws if there's
-- not an expected response status code.
getWithStatus :: HasCallStack => [Int] -> String -> IO ()
getWithStatus acceptableStatusCodes url = do
  response <- Http.httpNoBody (fromString url)
  unless (Http.getResponseStatusCode response `elem` acceptableStatusCodes) $
    error ("Unexpected response code from HTTP request: " ++ url ++ ". Expected: " ++ show acceptableStatusCodes)

-- | Post the JSON to the given URL, and produces a very descriptive
-- exception on failure.
postValue :: HasCallStack => String -> Http.RequestHeaders -> Value -> IO Value
postValue = postValueWithStatus 200

post :: String -> Http.RequestHeaders -> Value -> IO (Http.Response L8.ByteString)
post url headers value = do
  let request =
        Http.setRequestHeaders headers $
          Http.setRequestMethod Http.methodPost $
            Http.setRequestBodyJSON value (fromString url)
  Http.httpLbs request

-- | Post the JSON to the given URL and expected HTTP response code.
-- Produces a very descriptive exception or failure.
postValueWithStatus :: HasCallStack => Int -> String -> Http.RequestHeaders -> Value -> IO Value
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
healthCheck :: HasCallStack => String -> IO ()
healthCheck url = loop [] Constants.httpHealthCheckAttempts
  where
    loop failures 0 =
      error
        ( "Health check failed for URL: "
            ++ url
            ++ ", with failures: "
            ++ show failures
        )
    loop failures attempts =
      catch
        (getWithStatus [200, 204] url)
        ( \(failure :: Http.HttpException) -> do
            sleep Constants.httpHealthCheckIntervalSeconds
            loop (failure : failures) (attempts - 1)
        )
