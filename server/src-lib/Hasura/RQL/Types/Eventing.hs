-- | This module contains types which are common to event triggers and scheduled triggers.
module Hasura.RQL.Types.Eventing where

import Data.Aeson
import Data.Aeson.TH
import Data.TByteString qualified as TBS
import Data.Text.Extended
import Database.PG.Query qualified as Q
import Database.PG.Query.PTI qualified as PTI
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.DDL.Headers
import PostgreSQL.Binary.Encoding qualified as PE

newtype EventId = EventId {unEventId :: Text}
  deriving (Show, Eq, Ord, Hashable, ToTxt, FromJSON, ToJSON, ToJSONKey, Q.FromCol, Q.ToPrepArg, Generic, NFData, Cacheable)

-- | There are two types of events: EventType (for event triggers) and ScheduledType (for scheduled triggers)
data TriggerTypes = EventType | ScheduledType

data WebhookRequest = WebhookRequest
  { _rqPayload :: Value,
    _rqHeaders :: [HeaderConf],
    _rqVersion :: Text
  }

$(deriveToJSON hasuraJSON {omitNothingFields = True} ''WebhookRequest)

data WebhookResponse = WebhookResponse
  { _wrsBody :: TBS.TByteString,
    _wrsHeaders :: [HeaderConf],
    _wrsStatus :: Int
  }

$(deriveToJSON hasuraJSON {omitNothingFields = True} ''WebhookResponse)

newtype ClientError = ClientError {_ceMessage :: TBS.TByteString}

$(deriveToJSON hasuraJSON {omitNothingFields = True} ''ClientError)

data Response (a :: TriggerTypes)
  = ResponseHTTP WebhookResponse
  | ResponseError ClientError

type InvocationVersion = Text

invocationVersionET :: InvocationVersion
invocationVersionET = "2"

invocationVersionST :: InvocationVersion
invocationVersionST = "1"

instance ToJSON (Response 'EventType) where
  toJSON (ResponseHTTP resp) =
    object
      [ "type" .= String "webhook_response",
        "data" .= toJSON resp,
        "version" .= invocationVersionET
      ]
  toJSON (ResponseError err) =
    object
      [ "type" .= String "client_error",
        "data" .= toJSON err,
        "version" .= invocationVersionET
      ]

instance ToJSON (Response 'ScheduledType) where
  toJSON (ResponseHTTP resp) =
    object
      [ "type" .= String "webhook_response",
        "data" .= toJSON resp,
        "version" .= invocationVersionST
      ]
  toJSON (ResponseError err) =
    object
      [ "type" .= String "client_error",
        "data" .= toJSON err,
        "version" .= invocationVersionST
      ]

data Invocation (a :: TriggerTypes) = Invocation
  { iEventId :: EventId,
    iStatus :: Maybe Int,
    iRequest :: WebhookRequest,
    iResponse :: Response a
  }

-- | PGTextArray is only used for PG array encoding
newtype PGTextArray = PGTextArray {unPGTextArray :: [Text]}
  deriving (Show, Eq)

instance Q.ToPrepArg PGTextArray where
  toPrepVal (PGTextArray l) =
    Q.toPrepValHelper PTI.unknown encoder l
    where
      -- 25 is the OID value of TEXT, https://jdbc.postgresql.org/development/privateapi/constant-values.html
      encoder = PE.array 25 . PE.dimensionArray foldl' (PE.encodingArray . PE.text_strict)
