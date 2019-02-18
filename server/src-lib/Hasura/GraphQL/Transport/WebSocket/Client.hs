module Hasura.GraphQL.Transport.WebSocket.Client
  ( mkGraphqlProxy
  , mkGraphqlClient
  , runGqlClient
  , WebsocketPayload(..)
  )
  where

import           Control.Concurrent                        (forkIO)
import           Network.Socket                            (withSocketsDo)

import           Hasura.Prelude

import qualified Data.Aeson                                as J
import qualified Data.Aeson.Casing                         as J
import qualified Data.Aeson.TH                             as J
import qualified Data.ByteString.Lazy                      as BL
import qualified Network.URI                               as URI
import qualified Network.WebSockets                        as WS

import qualified Hasura.GraphQL.Transport.WebSocket.Server as WS


-- | TODO:
-- | The following ADT is required so that we can parse the incoming websocket
-- | frame, and only pick the payload, for remote schema queries.
-- | Ideally we should use `StartMsg` from Websocket.Protocol, but as
-- | `GraphQLRequest` doesn't have a ToJSON instance we are using our own type to
-- | get only the payload
data WebsocketPayload
  = WebsocketPayload
  { _wpId      :: !Text
  , _wpType    :: !Text
  , _wpPayload :: !J.Value
  } deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''WebsocketPayload)

data WebSocketClientErr
  = WSCEEmptyHostErr
  deriving (Eq)

instance Show WebSocketClientErr where
  show WSCEEmptyHostErr = "empty hostname"

mkGraphqlClient :: WS.WSConn a -> WebsocketPayload -> WS.ClientApp ()
mkGraphqlClient wsconn = mkGraphqlProxy srvrConn
  where srvrConn = WS._wcConnRaw wsconn

-- this function recieves the start msg (WebsocketPayload) when a subscription
-- query is sent
mkGraphqlProxy :: WS.Connection -> WebsocketPayload -> WS.ClientApp ()
mkGraphqlProxy srvrConn payload conn = do
  -- setup initial connection protocol
  setupInitialGraphqlProto
  readRemoteWriteHasura conn srvrConn
  readHasuraWriteRemote srvrConn conn

  where
    -- TODO: add logging
    readHasuraWriteRemote hasuraConn remoteConn = loop
      where
        loop = do
          msg <- WS.receiveData hasuraConn
          sendMsg remoteConn msg >> loop

    -- TODO: add logging
    readRemoteWriteHasura remoteConn hasuraConn =
      -- Fork a thread? that writes WS data to server connection
      void $ forkIO $ forever $ do
        msg <- WS.receiveData remoteConn
        sendMsg hasuraConn msg

    sendMsg :: WS.Connection -> BL.ByteString -> IO ()
    sendMsg = WS.sendTextData

    -- send an init message with the same payload recieved, and then send the
    -- payload as it is (assuming this is the start msg)
    setupInitialGraphqlProto = do
      WS.sendTextData conn $ J.encode $
        J.object [ "type" J..= ("connection_init" :: Text)
                 , "payload" J..= _wpPayload payload
                 ]
      WS.sendTextData conn $ J.encode payload


runGqlClient :: URI.URI -> WS.ClientApp () -> ExceptT WebSocketClientErr IO ()
runGqlClient url client =
  case host of
    Nothing -> throwError WSCEEmptyHostErr
    Just h  -> do
      liftIO $ print $ URI.uriPort <$> uriAuth
      liftIO $ print h
      liftIO $ print port
      liftIO $ print path
      liftIO $ withSocketsDo $ WS.runClient h port path client
  where
    host = (URI.uriUserInfo <$> uriAuth) <> (URI.uriRegName <$> uriAuth)
    port = read $ maybe "80" (drop 1 . URI.uriPort) uriAuth
    path = URI.uriPath url
    uriAuth = URI.uriAuthority url
