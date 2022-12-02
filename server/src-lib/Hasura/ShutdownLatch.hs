module Hasura.ShutdownLatch
  ( ShutdownLatch,
    newShutdownLatch,
    shutdownGracefully,
    waitForShutdown,
    shuttingDown,
  )
where

import Control.Concurrent.Extended qualified as C
import Hasura.Prelude

-- | A latch for the graceful shutdown of a server process.
newtype ShutdownLatch = ShutdownLatch {unShutdownLatch :: C.MVar ()}

newShutdownLatch :: IO ShutdownLatch
newShutdownLatch = fmap ShutdownLatch C.newEmptyMVar

-- | Block the current thread, waiting on the latch.
waitForShutdown :: ShutdownLatch -> IO ()
waitForShutdown = C.readMVar . unShutdownLatch

-- | Initiate a graceful shutdown of the server associated with the provided
-- latch.
shutdownGracefully :: ShutdownLatch -> IO ()
shutdownGracefully = void . flip C.tryPutMVar () . unShutdownLatch

-- | Returns True if the latch is set for shutdown and vice-versa
shuttingDown :: ShutdownLatch -> IO Bool
shuttingDown latch = not <$> C.isEmptyMVar (unShutdownLatch latch)
