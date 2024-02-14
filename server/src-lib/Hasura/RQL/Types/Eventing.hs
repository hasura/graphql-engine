-- | This module contains types which are common to event triggers and scheduled triggers.
module Hasura.RQL.Types.Eventing
  ( ClientError (..),
    EventId (..),
    Invocation (..),
    InvocationVersion,
    PGTextArray (..),
    Response (..),
    TriggerTypes (..),
    WebhookRequest (..),
    WebhookResponse (..),
    OpVar (..),
    invocationVersionET,
    invocationVersionST,
  )
where

import Data.Aeson
import Data.SerializableBlob qualified as SB
import Data.Text.Extended
import Database.PG.Query qualified as PG
import Database.PG.Query.PTI qualified as PTI
import Hasura.Prelude
import Hasura.RQL.Types.Headers (HeaderConf)
import PostgreSQL.Binary.Encoding qualified as PE

newtype EventId = EventId {unEventId :: Text}
  deriving (Show, Eq, Ord, Hashable, ToTxt, FromJSON, ToJSON, ToJSONKey, PG.FromCol, PG.ToPrepArg, Generic, NFData)

-- | There are two types of events: EventType (for event triggers) and ScheduledType (for scheduled triggers)
data TriggerTypes = EventType | ScheduledType

data WebhookRequest = WebhookRequest
  { _rqPayload :: Value,
    _rqHeaders :: [HeaderConf],
    _rqVersion :: Text
  }
  deriving stock (Generic)

instance ToJSON WebhookRequest where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}
  toEncoding = genericToEncoding hasuraJSON {omitNothingFields = True}

data WebhookResponse = WebhookResponse
  { _wrsBody :: SB.SerializableBlob,
    _wrsHeaders :: [HeaderConf],
    _wrsStatus :: Int
  }
  deriving stock (Generic)

instance ToJSON WebhookResponse where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}
  toEncoding = genericToEncoding hasuraJSON {omitNothingFields = True}

newtype ClientError = ClientError {_ceMessage :: SB.SerializableBlob}
  deriving stock (Generic)

instance ToJSON ClientError where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}
  toEncoding = genericToEncoding hasuraJSON {omitNothingFields = True}

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

instance PG.ToPrepArg PGTextArray where
  toPrepVal (PGTextArray l) =
    PG.toPrepValHelper PTI.unknown encoder l
    where
      -- 25 is the OID value of TEXT, https://jdbc.postgresql.org/development/privateapi/constant-values.html
      encoder = PE.array 25 . PE.dimensionArray foldl' (PE.encodingArray . PE.text_strict)

-- | Used to construct the payload of Event Trigger
--
-- OLD: Depicts the old database row value for UPDATE/DELETE trigger operations.
--      This is used to construct the 'data.old' field of the event trigger
--      payload. The value of 'data.old' is null in INSERT trigger operation.
--
-- NEW: Depicts the new database row value for INSERT/UPDATE trigger operations.
--      This is used to construct the 'data.new' field of the event trigger
--      payload. The value of 'data.new' is null in DELETE trigger operation.
data OpVar = OLD | NEW deriving (Show)
