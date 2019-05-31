module Hasura.GraphQL.Transport.WebSocket.Protocol
  ( OperationId(..)
  , ConnParams(..)
  , StartMsg(..)
  , StopMsg(..)
  , ClientMsg(..)
  , ServerMsg(..)
  , ServerMsgType(..)
  , encodeServerMsg
  , DataMsg(..)
  , ErrorMsg(..)
  , ConnErrMsg(..)
  , CompletionMsg(..)
  ) where

import qualified Data.Aeson                             as J
import qualified Data.Aeson.Casing                      as J
import qualified Data.Aeson.TH                          as J
import qualified Data.ByteString.Lazy                   as BL
import qualified Data.HashMap.Strict                    as Map

import           Hasura.EncJSON
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude

newtype OperationId
  = OperationId { unOperationId :: Text }
  deriving (Show, Eq, J.ToJSON, J.FromJSON, Hashable)

data StartMsg
  = StartMsg
  { _smId      :: !OperationId
  , _smPayload :: !GQLReqUnparsed
  } deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''StartMsg)

newtype StopMsg
  = StopMsg { _stId :: OperationId }
  deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''StopMsg)

data ClientMsg
  = CMConnInit !(Maybe ConnParams)
  | CMStart !StartMsg
  | CMStop !StopMsg
  | CMConnTerm
  deriving (Show, Eq)

newtype ConnParams
  = ConnParams { _cpHeaders :: Maybe (Map.HashMap Text Text) }
  deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''ConnParams)

instance J.FromJSON ClientMsg where
  parseJSON = J.withObject "ClientMessage" $ \obj -> do
    t <- obj J..: "type"
    case t of
      "connection_init" -> CMConnInit <$> obj J..:? "payload"
      "start" -> CMStart <$> J.parseJSON (J.Object obj)
      "stop" -> CMStop <$> J.parseJSON (J.Object obj)
      "connection_terminate" -> return CMConnTerm
      _ -> fail $ "unexpected type for ClientMessage: " <> t

instance J.ToJSON ClientMsg where
  toJSON = \case
    CMConnInit h -> J.object [ "type" J..= ("connection_init" :: Text)
                             , "headers" J..= h
                             ]
    CMStart m    -> J.object [ "type" J..= ("start" :: Text)
                             , "id" J..= _smId m
                             , "payload" J..= _smPayload m
                             ]
    CMStop m     -> J.object [ "type" J..= ("start" :: Text)
                             , "id" J..= _stId m
                             ]
    CMConnTerm   -> J.object [ "type" J..= ("connection_terminate" :: Text) ]


-- server to client messages

data DataMsg
  = DataMsg
  { _dmId      :: !OperationId
  , _dmPayload :: !GraphqlResponse
  } deriving (Show)

data ErrorMsg
  = ErrorMsg
  { _emId      :: !OperationId
  , _emPayload :: !J.Value
  } deriving (Show, Eq)

newtype CompletionMsg
  = CompletionMsg { unCompletionMsg :: OperationId }
  deriving (Show, Eq)

newtype ConnErrMsg
  = ConnErrMsg { unConnErrMsg :: Text }
  deriving (Show, Eq, J.ToJSON, J.FromJSON, IsString)

data ServerMsg
  = SMConnAck
  | SMConnKeepAlive
  | SMConnErr !ConnErrMsg
  | SMData !DataMsg
  | SMErr !ErrorMsg
  | SMComplete !CompletionMsg

data ServerMsgType
  = SMT_GQL_CONNECTION_ACK
  | SMT_GQL_CONNECTION_KEEP_ALIVE
  | SMT_GQL_CONNECTION_ERROR
  | SMT_GQL_DATA
  | SMT_GQL_ERROR
  | SMT_GQL_COMPLETE
  deriving (Eq)

instance Show ServerMsgType where
  show = \case
    SMT_GQL_CONNECTION_ACK        -> "connection_ack"
    SMT_GQL_CONNECTION_KEEP_ALIVE -> "ka"
    SMT_GQL_CONNECTION_ERROR      -> "connection_error"
    SMT_GQL_DATA                  -> "data"
    SMT_GQL_ERROR                 -> "error"
    SMT_GQL_COMPLETE              -> "complete"

instance J.ToJSON ServerMsgType where
  toJSON = J.toJSON . show

instance J.FromJSON ServerMsgType where
  parseJSON = J.withObject "ServerMsgType" $ \obj -> do
    ty <- obj J..: "type"
    case ty of
      "connection_ack"   -> pure SMT_GQL_CONNECTION_ACK
      "ka"               -> pure SMT_GQL_CONNECTION_KEEP_ALIVE
      "connection_error" -> pure SMT_GQL_CONNECTION_ERROR
      "data"             -> pure SMT_GQL_DATA
      "error"            -> pure SMT_GQL_ERROR
      "complete"         -> pure SMT_GQL_COMPLETE
      _                  -> fail $ "unexpected type for ServerMsgType: " <> ty

encodeServerMsg :: ServerMsg -> BL.ByteString
encodeServerMsg msg =
  encJToLBS $ encJFromAssocList $ case msg of

  SMConnAck ->
    [encTy SMT_GQL_CONNECTION_ACK]

  SMConnKeepAlive ->
    [encTy SMT_GQL_CONNECTION_KEEP_ALIVE]

  SMConnErr connErr ->
    [ encTy SMT_GQL_CONNECTION_ERROR
    , ("payload", encJFromJValue connErr)
    ]

  SMData (DataMsg opId payload) ->
    [ encTy SMT_GQL_DATA
    , ("id", encJFromJValue opId)
    , ("payload", encodeGraphqlResponse payload)
    ]

  SMErr (ErrorMsg opId payload) ->
    [ encTy SMT_GQL_ERROR
    , ("id", encJFromJValue opId)
    , ("payload", encJFromJValue payload)
    ]

  SMComplete compMsg ->
    [ encTy SMT_GQL_COMPLETE
    , ("id", encJFromJValue $ unCompletionMsg compMsg)
    ]

  where
    encTy ty = ("type", encJFromJValue ty)
