-- | Taken from wai-websockets package and customized to get IP address on websocket connection
-- http://hackage.haskell.org/package/wai-websockets-3.0.1.2/docs/Network-Wai-Handler-WebSockets.html

-- Potential improvement (TODO): change 'WS.RequestHead' to have IP address or even the
-- 'Wai.Request' object in it

module Network.Wai.Handler.WebSockets.Custom
    ( websocketsOr
    , websocketsApp
    , isWebSocketsReq
    , getRequestHead
    , runWebSockets
    ) where

import           Control.Exception             (bracket, tryJust)
import           Data.ByteString               (ByteString)
import           Network.HTTP.Types            (status500)
import           Prelude

import qualified Data.ByteString.Char8         as BC
import qualified Data.ByteString.Lazy          as BL
import qualified Data.CaseInsensitive          as CI
import qualified Network.Wai.Extended          as Wai
import qualified Network.WebSockets            as WS
import qualified Network.WebSockets.Connection as WS
import qualified Network.WebSockets.Stream     as WS

--------------------------------------------------------------------------------
-- | Returns whether or not the given 'Wai.Request' is a WebSocket request.
isWebSocketsReq :: Wai.Request -> Bool
isWebSocketsReq req =
    fmap CI.mk (lookup "upgrade" $ Wai.requestHeaders req) == Just "websocket"

--------------------------------------------------------------------------------
-- | Upgrade a @websockets@ 'WS.ServerApp' to a @wai@ 'Wai.Application'. Uses
-- the given backup 'Wai.Application' to handle 'Wai.Request's that are not
-- WebSocket requests.
--
-- @
-- websocketsOr opts ws_app backup_app = \\req respond ->
--     __case__ 'websocketsApp' opts ws_app req __of__
--         'Nothing'  -> backup_app req send_response
--         'Just' res -> respond res
-- @
--
-- For example, below is an 'Wai.Application' that sends @"Hello, client!"@ to
-- each connected client.
--
-- @
-- app :: 'Wai.Application'
-- app = 'websocketsOr' 'WS.defaultConnectionOptions' wsApp backupApp
--   __where__
--     wsApp :: 'WS.ServerApp'
--     wsApp pending_conn = do
--         conn <- 'WS.acceptRequest' pending_conn
--         'WS.sendTextData' conn ("Hello, client!" :: 'Data.Text.Text')
--
--     backupApp :: 'Wai.Application'
--     backupApp _ respond = respond $ 'Wai.responseLBS' 'Network.HTTP.Types.status400' [] "Not a WebSocket request"
-- @
websocketsOr :: WS.ConnectionOptions
             -> (Wai.IpAddress -> WS.PendingConnection -> IO ())
             -> Wai.Application
             -> Wai.Application
websocketsOr opts app backup req sendResponse =
    case websocketsApp opts app req of
        Nothing  -> backup req sendResponse
        Just res -> sendResponse res

--------------------------------------------------------------------------------
-- | Handle a single @wai@ 'Wai.Request' with the given @websockets@
-- 'WS.ServerApp'. Returns 'Nothing' if the 'Wai.Request' is not a WebSocket
-- request, 'Just' otherwise.
--
-- Usually, 'websocketsOr' is more convenient.
websocketsApp :: WS.ConnectionOptions
              -> (Wai.IpAddress -> WS.PendingConnection -> IO ())
              -> Wai.Request
              -> Maybe Wai.Response
websocketsApp opts app req
    | isWebSocketsReq req =
        Just $ flip Wai.responseRaw backup $ \src sink ->
            runWebSockets opts req' ipAddress app src sink
    | otherwise = Nothing
  where
    (req', ipAddress) = getRequestHead req
    backup = Wai.responseLBS status500 [("Content-Type", "text/plain")]
                "The web application attempted to send a WebSockets response, but WebSockets are not supported by your WAI handler."

--------------------------------------------------------------------------------
getRequestHead :: Wai.Request -> (WS.RequestHead, Wai.IpAddress)
getRequestHead req = (reqHead, Wai.getSourceFromFallback req)
  where
    reqHead = WS.RequestHead
      (Wai.rawPathInfo req `BC.append` Wai.rawQueryString req)
      (Wai.requestHeaders req)
      (Wai.isSecure req)

--------------------------------------------------------------------------------
-- | Internal function to run the WebSocket io-streams using the conduit library.
runWebSockets :: WS.ConnectionOptions
              -> WS.RequestHead
              -> Wai.IpAddress
              -> (Wai.IpAddress -> WS.PendingConnection -> IO a)
              -> IO ByteString
              -> (ByteString -> IO ())
              -> IO a
runWebSockets opts req ipAddress app src sink = bracket mkStream ensureClose (app ipAddress . pc)
  where
    ensureClose = tryJust onConnectionException . WS.close
    onConnectionException :: WS.ConnectionException -> Maybe ()
    onConnectionException WS.ConnectionClosed = Just ()
    onConnectionException _                   = Nothing
    mkStream =
        WS.makeStream
            (do
                bs <- src
                return $ if BC.null bs then Nothing else Just bs)
            (\mbBl -> case mbBl of
                Nothing -> return ()
                Just bl -> mapM_ sink (BL.toChunks bl))

    pc stream = WS.PendingConnection
        { WS.pendingOptions     = opts
        , WS.pendingRequest     = req
        , WS.pendingOnAccept    = \_ -> return ()
        , WS.pendingStream      = stream
        }
