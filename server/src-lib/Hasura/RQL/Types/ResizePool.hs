{-# LANGUAGE MagicHash #-}

-- | Types related to resizing a connection pool
--
-- Resize connection pools, on-demand, based on underlying server replicas
--
-- See @'resizeSourcePools' in Hasura.RQL.Types.Backend
module Hasura.RQL.Types.ResizePool
  ( ServerReplicas,
    ResizePoolStrategy (..),
    getServerReplicasInt,
    safeServerReplicas,
    SourceResizePoolSummary (..),
    noPoolsResizedSummary,

    -- * exporting for tests
    unsafeServerReplicas,
    oneServerReplica,
  )
where

import GHC.Exts (Int (I#), Word (W#), int2Word#)
import Hasura.Prelude

-- | Number of server instances. A wrapper over @'Word' type, a non-negative integer
-- with the same size as @'Int'. Useful for resize a connection pool.
newtype ServerReplicas = ServerReplicas {serverReplicaNumber :: Word}
  deriving (Show, Eq)

unsafeServerReplicas :: Word -> ServerReplicas
unsafeServerReplicas = ServerReplicas

oneServerReplica :: ServerReplicas
oneServerReplica = ServerReplicas 1

-- | Safely build @'ServerReplicas' from non-negative and non-zero @'Int' value.
safeServerReplicas :: Int -> Either Text ServerReplicas
safeServerReplicas i@(I# i#)
  | i <= 0 = Left $ "Expecting a non-zero and non-negative integer for ServerReplicas but got " <> tshow i
  | otherwise = Right $ ServerReplicas $ W# (int2Word# i#)

-- | Get server replic count in @'Int'
getServerReplicasInt :: ServerReplicas -> Int
getServerReplicasInt (ServerReplicas replicaNumber) = fromIntegral replicaNumber

-- | A strategy for resizing a pool
data ResizePoolStrategy
  = -- | Never resize the pool
    NeverResizePool
  | -- | Resize the pool by using provided total maximum connections
    ResizePool Int

-- | Summary of a source's pools resize. Predominantly used to log.
data SourceResizePoolSummary = SourceResizePoolSummary
  { _srpsPrimaryResized :: Bool,
    _srpsReadReplicasResized :: Bool,
    _srpsConnectionSet :: [Text]
  }
  deriving (Eq, Show)

noPoolsResizedSummary :: SourceResizePoolSummary
noPoolsResizedSummary =
  SourceResizePoolSummary False False []
