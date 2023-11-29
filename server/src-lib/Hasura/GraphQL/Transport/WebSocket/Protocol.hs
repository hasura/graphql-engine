-- | This file contains types for both the websocket protocols (Apollo) and (graphql-ws)
-- | See Apollo: https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md
-- | See graphql-ws: https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md
module Hasura.GraphQL.Transport.WebSocket.Protocol
  ( ClientMsg (CMConnInit, CMConnTerm, CMPing, CMPong, CMStart, CMStop),
    CompletionMsg (CompletionMsg),
    ConnErrMsg (ConnErrMsg, unConnErrMsg),
    ConnParams (_cpHeaders),
    DataMsg (DataMsg),
    ErrorMsg (ErrorMsg),
    OperationId (unOperationId),
    PingPongPayload,
    ServerErrorCode (..),
    ServerMsg (SMComplete, SMConnAck, SMConnErr, SMConnKeepAlive, SMData, SMErr, SMNext, SMPing, SMPong),
    ServerMsgType (..),
    StartMsg (StartMsg),
    StopMsg (StopMsg),
    WSConnInitTimerStatus (Done),
    WSSubProtocol (..),
    encodeServerErrorMsg,
    encodeServerMsg,
    getNewWSTimer,
    getWSTimerState,
    keepAliveMessage,
    showSubProtocol,
    toWSSubProtocol,

    -- * exported for testing
    unsafeMkOperationId,
  )
where

import Control.Concurrent
import Control.Concurrent.Extended (sleep)
import Control.Concurrent.STM
import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as BL
import Data.Text (pack)
import Hasura.EncJSON
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.Prelude

-- NOTE: the `subProtocol` is decided based on the `Sec-WebSocket-Protocol`
-- header on every request sent to the server.
data WSSubProtocol = Apollo | GraphQLWS
  deriving (Eq, Show)

-- NOTE: Please do not change them, as they're used for to identify the type of client
-- on every request that reaches the server. They are unique to each of the protocols.
showSubProtocol :: WSSubProtocol -> String
showSubProtocol subProtocol = case subProtocol of
  -- REF: https://github.com/apollographql/subscriptions-transport-ws/blob/master/src/server.ts#L144
  Apollo -> "graphql-ws"
  -- REF: https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md#communication
  GraphQLWS -> "graphql-transport-ws"

toWSSubProtocol :: String -> WSSubProtocol
toWSSubProtocol str = case str of
  "graphql-transport-ws" -> GraphQLWS
  _ -> Apollo

-- This is set by the client when it connects to the server
newtype OperationId = OperationId {unOperationId :: Text}
  deriving (Show, Eq, J.ToJSON, J.FromJSON, IsString, Hashable)

unsafeMkOperationId :: Text -> OperationId
unsafeMkOperationId = OperationId

data ServerMsgType
  = -- specific to `Apollo` clients
    SMT_GQL_CONNECTION_KEEP_ALIVE
  | SMT_GQL_CONNECTION_ERROR
  | SMT_GQL_DATA
  | -- specific to `graphql-ws` clients
    SMT_GQL_NEXT
  | SMT_GQL_PING
  | SMT_GQL_PONG
  | -- common to clients of both protocols
    SMT_GQL_CONNECTION_ACK
  | SMT_GQL_ERROR
  | SMT_GQL_COMPLETE
  deriving (Eq)

instance Show ServerMsgType where
  show = \case
    -- specific to `Apollo` clients
    SMT_GQL_CONNECTION_KEEP_ALIVE -> "ka"
    SMT_GQL_CONNECTION_ERROR -> "connection_error"
    SMT_GQL_DATA -> "data"
    -- specific to `graphql-ws` clients
    SMT_GQL_NEXT -> "next"
    SMT_GQL_PING -> "ping"
    SMT_GQL_PONG -> "pong"
    -- common to clients of both protocols
    SMT_GQL_CONNECTION_ACK -> "connection_ack"
    SMT_GQL_ERROR -> "error"
    SMT_GQL_COMPLETE -> "complete"

instance J.ToJSON ServerMsgType where
  toJSON = J.toJSON . show

data ConnParams = ConnParams
  {_cpHeaders :: Maybe (HashMap Text Text)}
  deriving stock (Show, Eq, Generic)

instance J.FromJSON ConnParams where
  parseJSON = J.genericParseJSON hasuraJSON

instance J.ToJSON ConnParams where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

data StartMsg = StartMsg
  { _smId :: !OperationId,
    _smPayload :: !GQLReqUnparsed
  }
  deriving (Show, Eq, Generic)

instance J.FromJSON StartMsg where
  parseJSON = J.genericParseJSON hasuraJSON

instance J.ToJSON StartMsg where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

data StopMsg = StopMsg
  { _stId :: OperationId
  }
  deriving (Show, Eq, Generic)

instance J.FromJSON StopMsg where
  parseJSON = J.genericParseJSON hasuraJSON

instance J.ToJSON StopMsg where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

-- Specific to graphql-ws
data PingPongPayload = PingPongPayload
  { _smMessage :: !(Maybe Text) -- NOTE: this is not within the spec, but is specific to our usecase
  }
  deriving stock (Show, Eq, Generic)

instance J.FromJSON PingPongPayload where
  parseJSON = J.genericParseJSON hasuraJSON

instance J.ToJSON PingPongPayload where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

-- Specific to graphql-ws
keepAliveMessage :: PingPongPayload
keepAliveMessage = PingPongPayload . Just . pack $ "keepalive"

-- Specific to graphql-ws
data SubscribeMsg = SubscribeMsg
  { _subId :: !OperationId,
    _subPayload :: !GQLReqUnparsed
  }
  deriving (Show, Eq, Generic)

instance J.FromJSON SubscribeMsg where
  parseJSON = J.genericParseJSON hasuraJSON

instance J.ToJSON SubscribeMsg where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

data ClientMsg
  = CMConnInit !(Maybe ConnParams)
  | CMStart !StartMsg
  | CMStop !StopMsg
  | -- specific to apollo clients
    CMConnTerm
  | -- specific to graphql-ws clients
    CMPing !(Maybe PingPongPayload)
  | CMPong !(Maybe PingPongPayload)
  deriving (Show, Eq)

instance J.FromJSON ClientMsg where
  parseJSON = J.withObject "ClientMessage" $ \obj -> do
    t <- obj J..: "type"
    case (t :: String) of
      "connection_init" -> CMConnInit <$> parsePayload obj
      "start" -> CMStart <$> parseObj obj
      "stop" -> CMStop <$> parseObj obj
      "connection_terminate" -> pure CMConnTerm
      -- graphql-ws specific message types
      "complete" -> CMStop <$> parseObj obj
      "subscribe" -> CMStart <$> parseObj obj
      "ping" -> CMPing <$> parsePayload obj
      "pong" -> CMPong <$> parsePayload obj
      _ -> fail $ "unexpected type for ClientMessage: " <> t
    where
      parseObj o = J.parseJSON (J.Object o)

      parsePayload py = py J..:? "payload"

data DataMsg = DataMsg
  { _dmId :: !OperationId,
    _dmPayload :: !GQResponse
  }

data ErrorMsg = ErrorMsg
  { _emId :: !OperationId,
    _emPayload :: !J.Encoding
  }
  deriving (Show, Eq)

newtype CompletionMsg = CompletionMsg {unCompletionMsg :: OperationId}
  deriving (Show, Eq)

instance J.FromJSON CompletionMsg where
  parseJSON = J.withObject "CompletionMsg" $ \t ->
    CompletionMsg <$> t J..: "id"

instance J.ToJSON CompletionMsg where
  toJSON (CompletionMsg opId) = J.String $ tshow opId

newtype ConnErrMsg = ConnErrMsg {unConnErrMsg :: Text}
  deriving (Show, Eq, J.ToJSON, J.FromJSON, IsString)

data ServerErrorMsg = ServerErrorMsg {unServerErrorMsg :: Text}
  deriving stock (Show, Eq, Generic)

instance J.FromJSON ServerErrorMsg where
  parseJSON = J.genericParseJSON hasuraJSON

instance J.ToJSON ServerErrorMsg where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

data ServerMsg
  = SMConnAck
  | SMConnKeepAlive
  | SMConnErr !ConnErrMsg
  | SMData !DataMsg
  | SMErr !ErrorMsg
  | SMComplete !CompletionMsg
  | -- graphql-ws specific values
    SMNext !DataMsg
  | SMPing !(Maybe PingPongPayload)
  | SMPong !(Maybe PingPongPayload)

-- | This is sent from the server to the client while closing the websocket
--   on encountering an error.
data ServerErrorCode
  = ProtocolError1002
  | GenericError4400 !String
  | Unauthorized4401
  | Forbidden4403
  | ConnectionInitTimeout4408
  | NonUniqueSubscription4409 !OperationId
  | TooManyRequests4429
  deriving stock (Show)

encodeServerErrorMsg :: ServerErrorCode -> BL.ByteString
encodeServerErrorMsg ecode = encJToLBS . encJFromJValue $ case ecode of
  ProtocolError1002 -> packMsg "1002: Protocol Error"
  GenericError4400 msg -> packMsg $ "4400: " <> msg
  Unauthorized4401 -> packMsg "4401: Unauthorized"
  Forbidden4403 -> packMsg "4403: Forbidden"
  ConnectionInitTimeout4408 -> packMsg "4408: Connection initialisation timeout"
  NonUniqueSubscription4409 opId -> packMsg $ "4409: Subscriber for " <> show opId <> " already exists"
  TooManyRequests4429 -> packMsg "4429: Too many requests"
  where
    packMsg = ServerErrorMsg . pack

encodeServerMsg :: ServerMsg -> BL.ByteString
encodeServerMsg msg =
  encJToLBS
    $ encJFromAssocList
    $ case msg of
      SMConnAck ->
        [encTy SMT_GQL_CONNECTION_ACK]
      SMConnKeepAlive ->
        [encTy SMT_GQL_CONNECTION_KEEP_ALIVE]
      SMConnErr connErr ->
        [ encTy SMT_GQL_CONNECTION_ERROR,
          ("payload", encJFromJValue connErr)
        ]
      SMData (DataMsg opId payload) ->
        [ encTy SMT_GQL_DATA,
          ("id", encJFromJValue opId),
          ("payload", encodeGQResp payload)
        ]
      SMErr (ErrorMsg opId payload) ->
        [ encTy SMT_GQL_ERROR,
          ("id", encJFromJValue opId),
          ("payload", encJFromJEncoding payload)
        ]
      SMComplete compMsg ->
        [ encTy SMT_GQL_COMPLETE,
          ("id", encJFromJValue $ unCompletionMsg compMsg)
        ]
      SMPing mPayload ->
        encodePingPongPayload mPayload SMT_GQL_PING
      SMPong mPayload ->
        encodePingPongPayload mPayload SMT_GQL_PONG
      SMNext (DataMsg opId payload) ->
        [ encTy SMT_GQL_NEXT,
          ("id", encJFromJValue opId),
          ("payload", encodeGQResp payload)
        ]
  where
    encTy ty = ("type", encJFromJValue ty)

    encodePingPongPayload mPayload msgType = case mPayload of
      Just payload ->
        [ encTy msgType,
          ("payload", encJFromJValue payload)
        ]
      Nothing -> [encTy msgType]

-- This "timer" is necessary while initialising the connection
-- with the server. Also, this is specific to the GraphQL-WS protocol.
data WSConnInitTimerStatus = Running | Done
  deriving stock (Show, Eq)

type WSConnInitTimer = (TVar WSConnInitTimerStatus, TMVar ())

getWSTimerState :: WSConnInitTimer -> IO WSConnInitTimerStatus
getWSTimerState (timerState, _) = readTVarIO timerState

{-# ANN getNewWSTimer ("HLint: ignore Use withAsync" :: String) #-}
getNewWSTimer :: Seconds -> IO WSConnInitTimer
getNewWSTimer timeout = do
  timerState <- newTVarIO Running
  timer <- newEmptyTMVarIO
  void
    $ forkIO
    $ do
      sleep (seconds timeout)
      atomically $ do
        runTimerState <- readTVar timerState
        case runTimerState of
          Running -> do
            -- time's up, we set status to "Done"
            writeTVar timerState Done
            putTMVar timer ()
          Done -> pure ()
  pure (timerState, timer)
