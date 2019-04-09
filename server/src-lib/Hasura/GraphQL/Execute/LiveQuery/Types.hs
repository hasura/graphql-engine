module Hasura.GraphQL.Execute.LiveQuery.Types
  ( LiveQuery(..)
  , OnChange
  , ThreadTM
  , Sinks
  , RespHash
  , mkRespHash
  , RespTV
  ) where

import qualified Control.Concurrent.Async               as A
import qualified Control.Concurrent.STM                 as STM
import qualified Crypto.Hash                            as CH
import qualified Data.Aeson                             as J
import qualified Data.ByteString.Lazy                   as LBS
import qualified StmContainers.Map                      as STMMap

import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude
import           Hasura.RQL.Types

data LiveQuery
  = LiveQuery
  { _lqUser    :: !UserInfo
  , _lqRequest :: !GQLReqUnparsed
  } deriving (Show, Eq, Generic)

instance J.ToJSON LiveQuery where
  toJSON (LiveQuery user req) =
    J.object [ "user" J..= userVars user
             , "request" J..= req
             ]

-- 'k' uniquely identifies a sink
-- in case of websockets, it is (wsId, opId)
type Sinks k = STMMap.Map k OnChange

instance Hashable LiveQuery

type OnChange = GQResp -> IO ()
type ThreadTM = STM.TMVar (A.Async ())

-- a cryptographic hash should ensure that
-- a hash collision is almost improbable
-- Blake2b because it is faster than Sha256
-- With 256 bits, and 86400 * 365 (a subscription open for 365 days)
-- there is ~ 4.294417×10-63 chance of a hash collision.

newtype RespHash
  = RespHash {unRespHash :: CH.Digest CH.Blake2b_256}
  deriving (Show, Eq)

instance J.ToJSON RespHash where
  toJSON = J.toJSON . show . unRespHash

mkRespHash :: LBS.ByteString -> RespHash
mkRespHash = RespHash . CH.hashlazy

type RespTV = STM.TVar (Maybe RespHash)
