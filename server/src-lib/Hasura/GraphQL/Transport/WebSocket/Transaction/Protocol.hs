module Hasura.GraphQL.Transport.WebSocket.Transaction.Protocol where

import           Hasura.EncJSON
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude
import           Hasura.Server.Init                        (readIsoLevel)
import           Hasura.Server.Utils

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH

import qualified Hasura.GraphQL.Transport.WebSocket.Server as WS

import qualified Data.ByteString.Lazy                      as BL
import qualified Data.HashMap.Strict                       as Map
import qualified Data.Text                                 as T
import qualified Database.PG.Query                         as Q

data ExecutePayload
  = ExecutePayload
  { _epRequestId :: !(Maybe RequestId)
  , _epQuery     :: !GQLReqUnparsed
  } deriving (Show, Eq)
$(deriveFromJSON (aesonDrop 3 snakeCase) ''ExecutePayload)

newtype TxIsolation = TxIsolation Q.TxIsolation
  deriving (Show, Eq)

instance FromJSON TxIsolation where
  parseJSON = withText "String" $ \t ->
    case readIsoLevel (T.unpack t) of
      Left e  -> fail e
      Right v -> pure $ TxIsolation v

data InitPayload
  = InitPayload
  { _ipIsolation :: !TxIsolation
  , _ipHeaders   :: !(Map.HashMap Text Text)
  } deriving (Show, Eq)

instance FromJSON InitPayload where
  parseJSON = withObject "Object" $ \obj ->
    InitPayload <$> obj .:? "isolation" .!= TxIsolation Q.ReadCommitted
                <*> obj .:? "headers" .!= Map.empty

data ClientMessage
  = CMInit !InitPayload
  | CMExecute !ExecutePayload
  | CMAbort
  | CMCommit
  deriving (Show, Eq)

instance FromJSON ClientMessage where
  parseJSON = withObject "Object" $ \obj -> do
    ty <- obj .: "type"
    case ty of
      "init"    -> withPayload CMInit obj
      "execute" -> withPayload CMExecute obj
      "abort"   -> pure CMAbort
      "commit"  -> pure CMCommit
      _         -> fail $ "unexpected type for ClientMessage: " <> ty
    where
      withPayload f o = f <$> o .: "payload"

data DataMessage
  = DataMessage
  { _dmId      :: !RequestId
  , _dmWsId    :: !WS.WSId
  , _dmPayload :: !GraphqlResponse
  }

data ErrorMessage
  = ErrorMessage
  { _emId      :: !(Maybe RequestId)
  , _emWsId    :: !WS.WSId
  , _emPayload :: !Value
  }

data ServerMessage
  = SMConnErr !Text
  | SMInitErr !Text
  | SMInitialised
  | SMExecMessage !Text
  | SMData !DataMessage
  | SMError !ErrorMessage
  | SMClose !WS.WSId !Text

data ServerMsgType
  = SMT_GQL_TX_CONNECTION_ERROR
  | SMT_GQL_TX_INIT_ERROR
  | SMT_GQL_TX_INITIALISED
  | SMT_GQL_TX_DATA
  | SMT_GQL_TX_ERROR
  | SMT_GQL_TX_CLOSE
  deriving (Eq)

instance Show ServerMsgType where
  show = \case
    SMT_GQL_TX_CONNECTION_ERROR -> "connection_error"
    SMT_GQL_TX_INIT_ERROR       -> "init_error"
    SMT_GQL_TX_INITIALISED      -> "initialised"
    SMT_GQL_TX_DATA             -> "data"
    SMT_GQL_TX_ERROR            -> "error"
    SMT_GQL_TX_CLOSE            -> "close"

instance ToJSON ServerMsgType where
  toJSON = toJSON . show

encodeServerMessage :: ServerMessage -> BL.ByteString
encodeServerMessage msg =
  encJToLBS $ encJFromAssocList $ case msg of

  SMConnErr connErr ->
    [ encTy SMT_GQL_TX_CONNECTION_ERROR
    , ("payload", encJFromJValue connErr)
    ]

  SMInitErr initErr ->
    [ encTy SMT_GQL_TX_INIT_ERROR
    , ("payload", encJFromJValue initErr)
    ]

  SMInitialised ->
    [encTy SMT_GQL_TX_INITIALISED]

  SMExecMessage message ->
    [ encTy SMT_GQL_TX_DATA
    , ("payload", encJFromJValue message)
    ]

  SMData (DataMessage reqId wsId payload) ->
    [ encTy SMT_GQL_TX_DATA
    , ("request_id", encJFromJValue reqId)
    , ("id", encJFromJValue wsId)
    , ("payload", encodeGraphqlResponse payload)
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
