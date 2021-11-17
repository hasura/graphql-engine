-- | Helper functions for HTTP requests.
module Harness.Http
  ( get_,
    postValue_,
    healthCheck,
  )
where

import Control.Concurrent
import Control.Exception
import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.String
import GHC.Stack
import Harness.Constants qualified as Constants
import Network.HTTP.Simple qualified as Http
import Network.HTTP.Types qualified as Http
import Prelude

--------------------------------------------------------------------------------
-- API

-- | Performs get, doesn't return the result. Simply throws if there's
-- not a 200 response.
get_ :: HasCallStack => String -> IO ()
get_ url = do
  response <- Http.httpNoBody (fromString url)
  if Http.getResponseStatusCode response == 200
    then pure ()
    else error ("Non-200 response code from HTTP request: " ++ url)

-- | Post the JSON to the given URL, and produces a very descriptive
-- exception on failure.
postValue_ :: HasCallStack => String -> Value -> IO Value
postValue_ url value = do
  let request =
        Http.setRequestMethod
          Http.methodPost
          (Http.setRequestBodyJSON value (fromString url))
  response <- Http.httpLbs request
  if Http.getResponseStatusCode response == 200
    then case eitherDecode (Http.getResponseBody response) of
      Left err ->
        error
          ( unlines
              [ "In request: " ++ url,
                "With body:",
                L8.unpack (encode value),
                "Couldn't decode JSON body:",
                err,
                "Body was:",
                L8.unpack (Http.getResponseBody response)
              ]
          )
      Right val -> pure val
    else
      error
        ( unlines
            [ "Non-200 response code from HTTP request: ",
              url,
              "With body:",
              L8.unpack (encode value),
              "Response body is:",
              L8.unpack (Http.getResponseBody response)
            ]
        )

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
        (get_ url)
        ( \(failure :: Http.HttpException) -> do
            threadDelay Constants.httpHealthCheckIntervalMicroseconds
            loop (failure : failures) (attempts - 1)
        )
