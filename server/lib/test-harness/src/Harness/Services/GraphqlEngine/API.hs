{-# LANGUAGE QuasiQuotes #-}

-- | This module houses low-level functions and types to help access and work
-- with the hge api.
module Harness.Services.GraphqlEngine.API
  ( -- * Types
    HgeServerInstance (..),
    getHgeServerInstanceUrl,
    PostGraphql (..),

    -- * Api actions
    hgePost,
    hgePostGraphql,
    export_metadata,
    replace_metadata,
  )
where

import Data.Aeson
import Data.Aeson qualified as J
import Data.Has
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Harness.Exceptions
import Harness.Http qualified as Http
import Harness.Logging.Messages
import Harness.Quoter.Yaml (yaml)
import Hasura.Prelude

data HgeServerInstance = HgeServerInstance
  { hgeServerHost :: Text,
    hgeServerPort :: Int,
    hgeAdminSecret :: Text
  }

getHgeServerInstanceUrl :: HgeServerInstance -> Text
getHgeServerInstanceUrl (HgeServerInstance {hgeServerHost, hgeServerPort}) =
  "http://" <> hgeServerHost <> ":" <> tshow hgeServerPort <> "/"

-- | Newtype-wrapper which enables late binding of 'postGraphql' on the test environment.
-- This makes 'TestEnvironment'-based specs more readily compatible with componontised fixtures.
newtype PostGraphql = PostGraphql {getPostGraphql :: J.Value -> IO J.Value}

export_metadata :: (Has Logger env, Has HgeServerInstance env) => env -> IO Value
export_metadata env = do
  hgePost
    env
    200
    "/v1/metadata"
    []
    [yaml|
        type: export_metadata
        args: null
      |]

replace_metadata :: (Has Logger env, Has HgeServerInstance env) => env -> Value -> IO Value
replace_metadata env newMetadata = do
  hgePost
    env
    200
    "/v1/metadata"
    []
    [yaml|
        type: replace_metadata
        args:
          allow_inconsistent_metadata: true
          metadata: *newMetadata
      |]

hgePost ::
  ( Has HgeServerInstance env,
    Has Logger env
  ) =>
  env ->
  Int ->
  Text ->
  Http.RequestHeaders ->
  J.Value ->
  IO J.Value
hgePost env statusCode path headers requestBody = do
  let hgeInstance = getter @HgeServerInstance env
  let hgeUrl = getHgeServerInstanceUrl hgeInstance
  let adminSecret = hgeAdminSecret hgeInstance
  let fullUrl = T.unpack $ hgeUrl <> path
  testLogMessage env $ LogHGERequest path requestBody
  let headersWithAdmin = ("x-hasura-admin-secret", encodeUtf8 adminSecret) : headers
  responseBody <- withFrozenCallStack $ Http.postValueWithStatus statusCode fullUrl headersWithAdmin requestBody
  testLogMessage env $ LogHGEResponse path responseBody
  return responseBody

hgePostGraphql ::
  ( Has HgeServerInstance env,
    Has Logger env
  ) =>
  env ->
  J.Value ->
  IO J.Value
hgePostGraphql env query = do
  hgePost env 200 "/v1/graphql" [] (J.object ["query" J..= query])
