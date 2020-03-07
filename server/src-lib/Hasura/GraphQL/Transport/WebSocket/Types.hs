module Hasura.GraphQL.Transport.WebSocket.Types
  ( WSId(..)
  , WSConnData(..)
  , WSConnState(..)
  , ErrRespType(..)
  , WSHeaders(..)
  , WSEvent(..)
  , WSConnInfo(..)
  , OpDetail(..)
  , OperationDetails(..)
  ) where

import qualified Control.Concurrent.STM                      as STM
import qualified Data.Aeson                                  as J
import qualified Data.Aeson.Casing                           as J
import qualified Data.Aeson.TH                               as J
import qualified Data.Time.Clock                             as TC
import qualified Data.UUID                                   as UUID
import qualified Network.HTTP.Types                          as H
import qualified StmContainers.Map                           as STMMap

import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Transport.WebSocket.Protocol
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Utils                         (RequestId)

import qualified Hasura.GraphQL.Execute.LiveQuery            as LQ

newtype WSId
  = WSId { unWSId :: UUID.UUID }
  deriving (Show, Eq, Hashable)

instance J.ToJSON WSId where
  toJSON (WSId uuid) =
    J.toJSON $ UUID.toText uuid

type OperationMap
  = STMMap.Map OperationId (LQ.LiveQueryId, Maybe OperationName)

newtype WSHeaders
  = WSHeaders { unWSHeaders :: [H.Header] }
  deriving (Show, Eq)

data ErrRespType
  = ERTLegacy
  | ERTGraphqlCompliant
  deriving (Show)

data WSConnState
  -- headers from the client for websockets
  = CSNotInitialised !WSHeaders
  | CSInitError Text
  -- headers from the client (in conn params) to forward to the remote schema
  -- and JWT expiry time if any
  | CSInitialised UserInfo (Maybe TC.UTCTime) [H.Header]

data WSConnData
  = WSConnData
  -- the role and headers are set only on connection_init message
  { _wscUser      :: !(STM.TVar WSConnState)
  -- we only care about subscriptions,
  -- the other operations (query/mutations)
  -- are not tracked here
  , _wscOpMap     :: !OperationMap
  , _wscErrRespTy :: !ErrRespType
  }

data OpDetail
  = ODStarted
  | ODProtoErr !Text
  | ODQueryErr !QErr
  | ODCompleted
  | ODStopped
  deriving (Show, Eq)
$(J.deriveToJSON
  J.defaultOptions { J.constructorTagModifier = J.snakeCase . drop 2
                   , J.sumEncoding = J.TaggedObject "type" "detail"
                   }
  ''OpDetail)

data OperationDetails
  = OperationDetails
  { _odOperationId   :: !OperationId
  , _odRequestId     :: !(Maybe RequestId)
  , _odOperationName :: !(Maybe OperationName)
  , _odOperationType :: !OpDetail
  , _odQuery         :: !(Maybe GQLReqUnparsed)
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 3 J.snakeCase) ''OperationDetails)

data WSEvent
  = EAccepted
  | ERejected !QErr
  | EConnErr !ConnErrMsg
  | EOperation !OperationDetails
  | EClosed
  deriving (Show, Eq)
$(J.deriveToJSON
  J.defaultOptions { J.constructorTagModifier = J.snakeCase . drop 1
                   , J.sumEncoding = J.TaggedObject "type" "detail"
                   }
  ''WSEvent)

data WSConnInfo
  = WSConnInfo
  { _wsciWebsocketId :: !WSId
  , _wsciJwtExpiry   :: !(Maybe TC.UTCTime)
  , _wsciMsg         :: !(Maybe Text)
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 5 J.snakeCase) ''WSConnInfo)

