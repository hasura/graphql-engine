module Hasura.GraphQL.Transport.WebSocket.Transaction.Types where

import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Utils

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH

import qualified Hasura.GraphQL.Transport.WebSocket.Server as WS
import qualified Hasura.Logging                            as L

import qualified Control.Concurrent.STM                    as STM
import qualified Database.PG.Query                         as PG

data PGConnCtx
  = PGConnCtx
  { _pccConn      :: !PG.PGConn
  , _pccLocalPool :: !PG.LocalPGPool
  }

data TxStatus
  = TxBegin
  | TxCommit
  | TxAbort
  deriving (Eq)

data WSTxData
  = WSTxData
  { _wtdUserInfo  :: !UserInfo
  , _wtdErrorType :: !WS.ErrRespType
  , _wtdPgConn    :: !PGConnCtx
  , _wtdTxStatus  :: !(STM.TVar TxStatus)
  }

data ExecuteQuery
  = ExecuteQuery
  { _eqRequestId :: !RequestId
  , _eqQuery     :: !GQLReqUnparsed
  }
$(deriveToJSON (aesonDrop 3 snakeCase) ''ExecuteQuery)

data Operation
  = OExecute !ExecuteQuery
  | OCommit
  | OAbort

$(deriveToJSON
  defaultOptions { constructorTagModifier = snakeCase . drop 1
                 , sumEncoding = TaggedObject "type" "detail"
                 }
  ''Operation)

data WSEvent
  = EAccepted
  | ERejected !QErr
  | EConnErr !Text
  | EOperation !Operation
  | EQueryError !QErr
  | EClosed

$(deriveToJSON
  defaultOptions { constructorTagModifier = snakeCase . drop 1
                 , sumEncoding = TaggedObject "type" "detail"
                 }
  ''WSEvent)

data WSLogInfo
  = WSLogInfo
  { _wsliWebsocketId :: !WS.WSId
  , _wsliEvent       :: !WSEvent
  }
$(deriveToJSON (aesonDrop 5 snakeCase) ''WSLogInfo)

data WSLog
  = WSLog
  { _wslLogLevel :: !L.LogLevel
  , _wslInfo     :: !WSLogInfo
  }

instance L.ToEngineLog WSLog L.Hasura where
  toEngineLog (WSLog logLevel wsLog) =
    (logLevel, L.ELTWebsocketTxLog, toJSON wsLog)
