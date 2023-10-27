-- | This module contains a collection of utility functions we use with tracing
-- throughout the codebase, but that are not a core part of the library. If we
-- were to move tracing to a separate library, those functions should be kept
-- here in the core engine code.
module Hasura.Tracing.Utils
  ( traceHTTPRequest,
    attachSourceConfigAttributes,
  )
where

import Control.Lens
import Data.String
import Data.Text.Extended (toTxt)
import Hasura.Prelude
import Hasura.RQL.Types.SourceConfiguration (HasSourceConfiguration (..))
import Hasura.Tracing.Class
import Hasura.Tracing.Context
import Hasura.Tracing.Propagator (HttpPropagator, inject)
import Network.HTTP.Client.Transformable qualified as HTTP

-- | Wrap the execution of an HTTP request in a span in the current
-- trace. Despite its name, this function does not start a new trace, and the
-- span will therefore not be recorded if the surrounding context isn't traced
-- (see 'spanWith').
--
-- Additionally, this function adds metadata regarding the request to the
-- created span, and injects the trace context into the HTTP header.
traceHTTPRequest ::
  (MonadIO m, MonadTrace m) =>
  HttpPropagator ->
  -- | http request that needs to be made
  HTTP.Request ->
  -- | a function that takes the traced request and executes it
  (HTTP.Request -> m a) ->
  m a
traceHTTPRequest propagator req f = do
  let method = bsToTxt (view HTTP.method req)
      uri = view HTTP.url req
  newSpan (method <> " " <> uri) do
    let reqBytes = HTTP.getReqSize req
    attachMetadata [("request_body_bytes", fromString (show reqBytes))]
    headers <- fmap (maybe [] toHeaders) currentContext
    f $ over HTTP.headers (headers <>) req
  where
    toHeaders :: TraceContext -> [HTTP.Header]
    toHeaders context = inject propagator context []

attachSourceConfigAttributes :: forall b m. (HasSourceConfiguration b, MonadTrace m) => SourceConfig b -> m ()
attachSourceConfigAttributes sourceConfig = do
  let backendSourceKind = sourceConfigBackendSourceKind @b sourceConfig
  attachMetadata [("source.kind", toTxt $ backendSourceKind)]
