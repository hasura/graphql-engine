module Hasura.GraphQL.Execute.LiveQuery.Types
  ( OnChange
  , ThreadTM
  , SinkId
  , newSinkId
  , Sinks

  , RespHash
  , mkRespHash
  , RespTV

  , RefetchInterval
  , refetchIntervalFromMilli
  , refetchIntervalToMicro

  , TMap
  , newTMap
  , resetTMap
  , nullTMap
  , insertTMap
  , deleteTMap
  , lookupTMap
  , toListTMap
  ) where

import           Data.Word                              (Word32)

import qualified Control.Concurrent.Async               as A
import qualified Control.Concurrent.STM                 as STM
import qualified Crypto.Hash                            as CH
import qualified Data.Aeson                             as J
import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.HashMap.Strict                    as Map
import qualified Data.UUID                              as UUID
import qualified Data.UUID.V4                           as UUID

import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude

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

newtype RefetchInterval
  = RefetchInterval {unRefetchInterval :: Word32}
  deriving (Show, Eq, J.ToJSON)

refetchIntervalFromMilli :: Word32 -> RefetchInterval
refetchIntervalFromMilli = RefetchInterval

refetchIntervalToMicro :: RefetchInterval -> Int
refetchIntervalToMicro ri = fromIntegral $ 1000 * unRefetchInterval ri

-- compared to stm.stmmap, this provides a much faster
-- iteration over the elements at the cost of slower
-- concurrent insertions
newtype TMap k v
  = TMap {unTMap :: STM.TVar (Map.HashMap k v)}

newTMap :: STM.STM (TMap k v)
newTMap =
  TMap <$> STM.newTVar Map.empty

resetTMap :: TMap k v -> STM.STM ()
resetTMap =
  flip STM.writeTVar Map.empty . unTMap

nullTMap :: TMap k v -> STM.STM Bool
nullTMap =
  fmap Map.null . STM.readTVar . unTMap

insertTMap :: (Eq k, Hashable k) => v -> k -> TMap k v -> STM.STM ()
insertTMap v k mapTv =
  STM.modifyTVar' (unTMap mapTv) $ Map.insert k v

deleteTMap :: (Eq k, Hashable k) => k -> TMap k v -> STM.STM ()
deleteTMap k mapTv =
  STM.modifyTVar' (unTMap mapTv) $ Map.delete k

lookupTMap :: (Eq k, Hashable k) => k -> TMap k v -> STM.STM (Maybe v)
lookupTMap k =
  fmap (Map.lookup k) . STM.readTVar . unTMap

toListTMap :: TMap k v -> STM.STM [(k, v)]
toListTMap =
  fmap Map.toList . STM.readTVar . unTMap

newtype SinkId
  = SinkId {_unSinkId :: UUID.UUID}
  deriving (Show, Eq, Hashable)

newSinkId :: IO SinkId
newSinkId = SinkId <$> UUID.nextRandom

instance J.ToJSON SinkId where
  toJSON = J.toJSON . show

type Sinks = TMap SinkId OnChange
