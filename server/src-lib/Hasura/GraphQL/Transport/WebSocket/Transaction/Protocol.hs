module Hasura.GraphQL.Transport.WebSocket.Transaction.Protocol where

import           Hasura.EncJSON
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude
import           Hasura.Server.Utils

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH

import qualified Hasura.GraphQL.Transport.WebSocket.Server as WS

import qualified Data.ByteString.Lazy                      as BL

data ExecutePayload
  = ExecutePayload
  { _epRequestId :: !(Maybe RequestId)
  , _epQuery     :: !GQLReqUnparsed
  } deriving (Show, Eq)
$(deriveFromJSON (aesonDrop 3 snakeCase) ''ExecutePayload)

data ClientMessage
  = CMExecute !ExecutePayload
  | CMAbort
  | CMCommit
  deriving (Show, Eq)

instance FromJSON ClientMessage where
  parseJSON = withObject "Object" $ \obj -> do
    ty <- obj .: "type"
    case ty of
      "execute" -> CMExecute <$> obj .: "payload"
      "abort"   -> pure CMAbort
      "commit"  -> pure CMCommit
      _         -> fail $ "unexpected type for ClientMessage: " <> ty

data DataMessage
  = DataMessage
  { _dmId      :: !RequestId
  , _dmWsId    :: !WS.WSId
  , _dmPayload :: !EncJSON
  }

data ErrorMessage
  = ErrorMessage
  { _emId      :: !(Maybe RequestId)
  , _emWsId    :: !WS.WSId
  , _emPayload :: !Value
  }

data ServerMessage
  = SMConnErr !Text
  | SMExecMessage !Text
  | SMData !DataMessage
  | SMError !ErrorMessage
  | SMClose !WS.WSId !Text

data ServerMsgType
  = SMT_GQL_TX_CONNECTION_ERROR
  | SMT_GQL_TX_DATA
  | SMT_GQL_TX_ERROR
  | SMT_GQL_TX_CLOSE
  deriving (Eq)

instance Show ServerMsgType where
  show = \case
    SMT_GQL_TX_CONNECTION_ERROR      -> "connection_error"
    SMT_GQL_TX_DATA                  -> "data"
    SMT_GQL_TX_ERROR                 -> "error"
    SMT_GQL_TX_CLOSE                 -> "close"

instance ToJSON ServerMsgType where
  toJSON = toJSON . show

encodeServerMessage :: ServerMessage -> BL.ByteString
encodeServerMessage msg =
  encJToLBS $ encJFromAssocList $ case msg of

  SMConnErr connErr ->
    [ encTy SMT_GQL_TX_CONNECTION_ERROR
    , ("payload", encJFromJValue connErr)
    ]

  SMExecMessage message ->
    [ encTy SMT_GQL_TX_DATA
    , ("payload", encJFromJValue message)
    ]

  SMData (DataMessage reqId wsId payload) ->
    [ encTy SMT_GQL_TX_DATA
    , ("request_id", encJFromJValue reqId)
    , ("id", encJFromJValue wsId)
    , ("payload", payload)
    ]

  SMError (ErrorMessage reqId wsId payload) ->
    [ encTy SMT_GQL_TX_ERROR
    , ("request_id", encJFromJValue reqId)
    , ("id", encJFromJValue wsId)
    , ("payload", encJFromJValue payload)
    ]

  SMClose wsId message ->
    [ encTy SMT_GQL_TX_CLOSE
    , ("id", encJFromJValue wsId)
    , ("payload", encJFromJValue message)
    ]

  where
    encTy ty = ("type", encJFromJValue ty)
