module Hasura.GraphQL.Execute.LiveQuery.Types
  ( LiveQuery(..)
  , OnChange
  , ThreadTM
  , Sinks
  ) where

import qualified Control.Concurrent.Async               as A
import qualified Control.Concurrent.STM                 as STM
import qualified StmContainers.Map                      as STMMap

import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude
import           Hasura.RQL.Types

-- 'k' uniquely identifies a sink
-- in case of websockets, it is (wsId, opId)
type Sinks k = STMMap.Map k OnChange

data LiveQuery
  = LiveQuery
  { _lqUser    :: !UserInfo
  , _lqRequest :: !GQLReqUnparsed
  } deriving (Show, Eq, Generic)

instance Hashable LiveQuery

type OnChange = GQResp -> IO ()
type ThreadTM = STM.TMVar (A.Async ())
