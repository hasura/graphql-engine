{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module:      Data.Pool
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>,
--              Bas van Dijk <v.dijk.bas@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- A high-performance striped pooling abstraction for managing
-- flexibly-sized collections of resources such as database
-- connections.
--
-- \"Striped\" means that a single 'Pool' consists of several
-- sub-pools, each managed independently.  A single stripe is fine for
-- many applications, and probably what you should choose by default.
-- More stripes will lead to reduced contention in high-performance
-- multicore applications, at a trade-off of causing the maximum
-- number of simultaneous resources in use to grow.
module Data.Pool
  ( Pool (idleTime, numStripes),
    getInUseResourceCount,
    LocalPool,
    TimeoutException (..),
    createPool,
    createPool',
    resizePool,
    tryTrimLocalPool,
    tryTrimPool,
    withResource,
    takeResource,
    tryWithResource,
    tryTakeResource,
    destroyResource,
    putResource,
    releaseResource,
    destroyAllResources,
  )
where

import Control.Concurrent
  ( ThreadId,
    forkIOWithUnmask,
    killThread,
    myThreadId,
    threadDelay,
  )
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, mask, mask_, onException)
import qualified Control.Exception as E
import Control.Monad (forM_, forever, join, liftM3, unless, when)
import Control.Monad.Base (liftBase)
import Control.Monad.Trans.Control (MonadBaseControl, control)
import Data.Foldable (foldMap')
import Data.Hashable (hash)
import Data.IORef (IORef, mkWeakIORef, newIORef)
import Data.List (partition)
import Data.Monoid (Sum (..))
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import GHC.Conc.Sync (labelThread)
import qualified GHC.Event as Event

{-# ANN module ("HLint: ignore avoid Control.Concurrent.threadDelay" :: String) #-}

-- | A single resource pool entry.
data Entry a = Entry
  { entry :: a,
    -- | Time of last return.
    lastUse :: UTCTime
  }

-- | A single striped pool.
data LocalPool a = LocalPool
  { -- | Count of open entries (both idle and in use).
    inUse :: TVar Int,
    -- | Idle entries.
    entries :: TVar [Entry a],
    -- | empty value used to attach a finalizer to (internal)
    lfin :: IORef ()
  }
  deriving (Typeable)

data Pool a = Pool
  { -- | Action for creating a new entry to add to the pool.
    create :: IO a,
    -- | Action for destroying an entry that is now done with.
    destroy :: a -> IO (),
    -- | The number of stripes (distinct sub-pools) to maintain.
    -- The smallest acceptable value is 1.
    numStripes :: Int,
    -- | Amount of time for which an unused resource is kept alive.
    -- The smallest acceptable value is 0.5 seconds.
    --
    -- The elapsed time before closing may be a little longer than
    -- requested, as the reaper thread wakes at 1-second intervals.
    idleTime :: NominalDiffTime,
    -- | Maximum number of resources to maintain per stripe.  The
    -- smallest acceptable value is 1.
    --
    -- Requests for resources will block if this limit is reached on a
    -- single stripe, even if other stripes have idle resources
    -- available.
    maxResources :: TVar Int,
    -- | Per-capability resource pools.
    localPools :: V.Vector (LocalPool a),
    -- | empty value used to attach a finalizer to (internal)
    fin :: IORef (),
    -- | Amount of time to wait while attempting to acquire a resource.
    timeout :: Maybe NominalDiffTime
  }
  deriving (Typeable)

instance Show (Pool a) where
  show Pool {..} =
    "Pool {numStripes = "
      ++ show numStripes
      ++ ", "
      ++ "idleTime = "
      ++ show idleTime
      ++ "}"

data TimeoutException = TimeoutException
  deriving (Show)

instance Exception TimeoutException

-- | Create a striped resource pool.
--
-- Although the garbage collector will destroy all idle resources when
-- the pool is garbage collected it's recommended to manually
-- 'destroyAllResources' when you're done with the pool so that the
-- resources are freed up as soon as possible.
createPool ::
  -- | Action that creates a new resource.
  IO a ->
  -- | Action that destroys an existing resource.
  (a -> IO ()) ->
  -- | The number of stripes (distinct sub-pools) to maintain.
  -- The smallest acceptable value is 1.
  Int ->
  -- | Amount of time for which an unused resource is kept open.
  -- The smallest acceptable value is 0.5 seconds.
  --
  -- The elapsed time before destroying a resource may be a little
  -- longer than requested, as the reaper thread wakes at 1-second
  -- intervals.
  NominalDiffTime ->
  -- | Initial maximum number of resources to keep open per stripe.
  -- The smallest acceptable value is 1.
  --
  -- Requests for resources will block if this limit is reached on a
  -- single stripe, even if other stripes have idle resources
  -- available.
  Int ->
  IO (Pool a)
createPool create destroy numStripes idleTime maxResources =
  createPool' create destroy numStripes idleTime maxResources Nothing

-- | Create a striped resource pool with a connection acquisition
-- timeout.
--
-- Although the garbage collector will destroy all idle resources when
-- the pool is garbage collected it's recommended to manually
-- 'destroyAllResources' when you're done with the pool so that the
-- resources are freed up as soon as possible.
createPool' ::
  -- | Action that creates a new resource.
  IO a ->
  -- | Action that destroys an existing resource.
  (a -> IO ()) ->
  -- | The number of stripes (distinct sub-pools) to maintain.
  -- The smallest acceptable value is 1.
  Int ->
  -- | Amount of time for which an unused resource is kept open.
  -- The smallest acceptable value is 0.5 seconds.
  --
  -- The elapsed time before destroying a resource may be a little
  -- longer than requested, as the reaper thread wakes at 1-second
  -- intervals.
  NominalDiffTime ->
  -- | Initial maximum number of resources to keep open per stripe.
  -- The smallest acceptable value is 1.
  --
  -- Requests for resources will block if this limit is reached on a
  -- single stripe, even if other stripes have idle resources
  -- available.
  Int ->
  -- | Amount of time to wait while attempting to acquire a resource.
  Maybe NominalDiffTime ->
  IO (Pool a)
createPool' create destroy numStripes idleTime maxResources' timeout = do
  when (numStripes < 1) $
    modError "pool " $
      "invalid stripe count " ++ show numStripes
  when (idleTime < 0.5) $
    modError "pool " $
      "invalid idle time " ++ show idleTime
  when (maxResources' < 1) $
    modError "pool " $
      "invalid maximum resource count " ++ show maxResources'
  localPools <-
    V.replicateM numStripes $
      liftM3 LocalPool (newTVarIO 0) (newTVarIO []) (newIORef ())
  reaperId <- forkIOLabeledWithUnmask "resource-pool: reaper" $ \unmask ->
    unmask $ reaper destroy idleTime localPools
  maxResources <- newTVarIO maxResources'
  fin <- newIORef ()
  mkWeakIORef fin (killThread reaperId)
    >> V.mapM_ (\lp -> mkWeakIORef (lfin lp) (purgeLocalPool destroy lp)) localPools
  return $ Pool {..}

-- | Adjust the maximum size of each stripe.
--
-- Note: after resizing, the stripes may be holding on to an excess number of
-- resources.  Use 'tryTrimPool' to bring this down.
resizePool :: Pool a -> Int -> IO ()
resizePool Pool {..} maxResources' = do
  when (maxResources' < 1) $
    modError "pool " $
      "invalid maximum resource count " ++ show maxResources'
  atomically $ writeTVar maxResources maxResources'

-- | Attempt to reduce resource allocation below maximum by dropping some unused
-- resources
tryTrimLocalPool :: (a -> IO ()) -> TVar Int -> LocalPool a -> IO ()
tryTrimLocalPool destroy maxResources LocalPool {..} = do
  resources <- atomically $ do
    maxSize <- readTVar maxResources
    curSize <- readTVar inUse
    -- We can distinguish three cases:
    --
    -- 1. `curSize > maxSize`: we have acquired too many resources, and should
    --    try to drop a few. `tryTrimLocalPool` tries to drop any unused
    --    resources, up until `curSize = maxSize`. it won't manage if too many
    --    resources are in use.
    --
    -- 2. `curSize = maxSize`: we're at our resource allocation maximum - no
    --    action required
    --
    -- 3. `curSize < maxSize`: we still have space to grow - no action
    --    required. in this case `desiredDrop` will be negative, and `splitAt
    --    desiredDrop xs = ([], xs)`.
    let desiredDrop = curSize - maxSize
    trimmed <- stateTVar entries (splitAt desiredDrop)
    modifyTVar_ inUse (subtract (length trimmed))
    return (map entry trimmed)
  forM_ resources $ \resource ->
    destroy resource `E.catch` \(_ :: SomeException) -> return ()

-- | Attempt to reduce resource allocation below maximum by dropping some unused
-- resources
tryTrimPool :: Pool a -> IO ()
tryTrimPool Pool {..} =
  V.forM_ localPools $ tryTrimLocalPool destroy maxResources

-- TODO: Propose 'forkIOLabeledWithUnmask' for the base library.

-- | Sparks off a new thread using 'forkIOWithUnmask' to run the given
-- IO computation, but first labels the thread with the given label
-- (using 'labelThread').
--
-- The implementation makes sure that asynchronous exceptions are
-- masked until the given computation is executed. This ensures the
-- thread will always be labeled which guarantees you can always
-- easily find it in the GHC event log.
--
-- Like 'forkIOWithUnmask', the given computation is given a function
-- to unmask asynchronous exceptions. See the documentation of that
-- function for the motivation of this.
--
-- Returns the 'ThreadId' of the newly created thread.
forkIOLabeledWithUnmask ::
  String ->
  ((forall a. IO a -> IO a) -> IO ()) ->
  IO ThreadId
forkIOLabeledWithUnmask label m = mask_ $ forkIOWithUnmask $ \unmask -> do
  tid <- myThreadId
  labelThread tid label
  m unmask

-- | Periodically go through all pools, closing any resources that
-- have been left idle for too long.
reaper :: (a -> IO ()) -> NominalDiffTime -> V.Vector (LocalPool a) -> IO ()
reaper destroy idleTime pools = forever $ do
  threadDelay (1 * 1000000)
  now <- getCurrentTime
  let isStale Entry {..} = now `diffUTCTime` lastUse > idleTime
  V.forM_ pools $ \LocalPool {..} -> do
    resources <- atomically $ do
      (stale, fresh) <- partition isStale <$> readTVar entries
      unless (null stale) $ do
        writeTVar entries fresh
        modifyTVar_ inUse (subtract (length stale))
      return (map entry stale)
    forM_ resources $ \resource -> do
      destroy resource `E.catch` \(_ :: SomeException) -> return ()

-- | Destroy all idle resources of the given 'LocalPool' and remove them from
-- the pool.
purgeLocalPool :: (a -> IO ()) -> LocalPool a -> IO ()
purgeLocalPool destroy LocalPool {..} = do
  resources <- atomically $ do
    idle <- swapTVar entries []
    modifyTVar_ inUse (subtract (length idle))
    return (map entry idle)
  forM_ resources $ \resource ->
    destroy resource `E.catch` \(_ :: SomeException) -> return ()

-- | Temporarily take a resource from a 'Pool', perform an action with
-- it, and return it to the pool afterwards.
--
-- * If the pool has an idle resource available, it is used
--   immediately.
--
-- * Otherwise, if the maximum number of resources has not yet been
--   reached, a new resource is created and used.
--
-- * If the maximum number of resources has been reached, this
--   function blocks until a resource becomes available.
--
-- * If a timeout is set, then this function will only block for
-- 'timeout' seconds before throwing a 'TimeoutException'.
--
-- If the action throws an exception of any type, the resource is
-- destroyed, and not returned to the pool.
--
-- It probably goes without saying that you should never manually
-- destroy a pooled resource, as doing so will almost certainly cause
-- a subsequent user (who expects the resource to be valid) to throw
-- an exception.
withResource ::
  (MonadBaseControl IO m) =>
  Pool a ->
  (a -> m b) ->
  m b
{-# SPECIALIZE withResource :: Pool a -> (a -> IO b) -> IO b #-}
withResource pool act = control $ \runInIO -> mask $ \restore -> do
  (resource, local) <- takeResource pool
  ret <-
    restore (runInIO (act resource))
      `onException` destroyResource pool local resource
  releaseResource pool local resource
  return ret
{-# INLINEABLE withResource #-}

-- | Take a resource from the pool, following the same results as
-- 'withResource'. Note that this function should be used with caution, as
-- improper exception handling can lead to leaked resources.
--
-- * If a timeout is set, then this function will only block for
-- 'timeout' seconds before throwing a 'TimeoutException'.
--
-- This function returns both a resource and the @LocalPool@ it came from so
-- that it may either be destroyed (via 'destroyResource') or returned to the
-- pool (via 'putResource').
takeResource :: Pool a -> IO (a, LocalPool a)
takeResource pool@Pool {..} = do
  timeoutSync <- newTVarIO False
  mgr <- Event.getSystemTimerManager
  timeoutKeyM <- case timeout of
    Nothing -> pure Nothing
    Just seconds -> do
      timeoutKey <-
        Event.registerTimeout mgr (toMicroseconds seconds)
          . atomically
          . writeTVar timeoutSync
          $ True
      pure $ Just timeoutKey
  local@LocalPool {..} <- getLocalPool pool
  resource <- liftBase . join . atomically $ do
    stop <- readTVar timeoutSync
    if stop
      then throwSTM TimeoutException
      else do
        ents <- readTVar entries
        case ents of
          (Entry {..} : es) -> writeTVar entries es >> return (return entry)
          [] -> do
            used <- readTVar inUse
            currentMaximum <- readTVar maxResources
            when (used >= currentMaximum) retry
            writeTVar inUse $! used + 1
            return $
              create `onException` atomically (modifyTVar_ inUse (subtract 1))
  -- Note: We'll miss unregistering the timeout on exception, but it will be
  -- unregistered automatically after the given time has passed.
  forM_ timeoutKeyM (Event.unregisterTimeout mgr)
  return (resource, local)
  where
    toMicroseconds :: NominalDiffTime -> Int
    toMicroseconds = (* 1_000_000) . round . nominalDiffTimeToSeconds
{-# INLINEABLE takeResource #-}

-- | Similar to 'withResource', but only performs the action if a resource could
-- be taken from the pool /without blocking/. Otherwise, 'tryWithResource'
-- returns immediately with 'Nothing' (ie. the action function is /not/ called).
-- Conversely, if a resource can be borrowed from the pool without blocking, the
-- action is performed and it's result is returned, wrapped in a 'Just'.
tryWithResource ::
  forall m a b.
  (MonadBaseControl IO m) =>
  Pool a ->
  (a -> m b) ->
  m (Maybe b)
tryWithResource pool act = control $ \runInIO -> mask $ \restore -> do
  res <- tryTakeResource pool
  case res of
    Just (resource, local) -> do
      ret <-
        restore (runInIO (Just <$> act resource))
          `onException` destroyResource pool local resource
      releaseResource pool local resource
      return ret
    Nothing -> restore . runInIO $ return (Nothing :: Maybe b)
{-# INLINEABLE tryWithResource #-}

-- | A non-blocking version of 'takeResource'. The 'tryTakeResource' function
-- returns immediately, with 'Nothing' if the pool is exhausted, or @'Just' (a,
-- 'LocalPool' a)@ if a resource could be borrowed from the pool successfully.
tryTakeResource :: Pool a -> IO (Maybe (a, LocalPool a))
tryTakeResource pool@Pool {..} = do
  local@LocalPool {..} <- getLocalPool pool
  resource <- liftBase . join . atomically $ do
    ents <- readTVar entries
    case ents of
      (Entry {..} : es) -> writeTVar entries es >> return (return . Just $ entry)
      [] -> do
        used <- readTVar inUse
        currentMaximum <- readTVar maxResources
        if used >= currentMaximum
          then return (return Nothing)
          else do
            writeTVar inUse $! used + 1
            return $
              Just
                <$> create
                  `onException` atomically (modifyTVar_ inUse (subtract 1))
  return $ (,local) <$> resource
{-# INLINEABLE tryTakeResource #-}

-- | Get a (Thread-)'LocalPool'
--
-- Internal, just to not repeat code for 'takeResource' and 'tryTakeResource'
getLocalPool :: Pool a -> IO (LocalPool a)
getLocalPool Pool {..} = do
  i <- liftBase $ (`mod` numStripes) . hash <$> myThreadId
  return $ localPools V.! i
{-# INLINEABLE getLocalPool #-}

-- | Destroy a resource. Note that this will ignore any exceptions in the
-- destroy function.
destroyResource :: Pool a -> LocalPool a -> a -> IO ()
destroyResource Pool {..} LocalPool {..} resource = do
  destroy resource `E.catch` \(_ :: SomeException) -> return ()
  atomically (modifyTVar_ inUse (subtract 1))
{-# INLINEABLE destroyResource #-}

-- | Opposite to 'takeResource'.
--
-- Also takes the 'LocalPool', because if a resource is passed around between
-- threads, we still want to return it to its original stripe.
releaseResource :: Pool a -> LocalPool a -> a -> IO ()
releaseResource pool@Pool {..} local@LocalPool {..} resource = do
  join $ atomically $ do
    currentMaximum <- readTVar maxResources
    currentlyUsed <- readTVar inUse
    if currentlyUsed > currentMaximum
      then -- We already have too many resources; by destroying this one we get
      -- closer to the desired size.
        return $ destroyResource pool local resource
      else -- The pool is not overexhausted; this resource can be used again.
        return $ putResource local resource

-- | Return a resource to the given 'LocalPool'.
putResource :: LocalPool a -> a -> IO ()
putResource LocalPool {..} resource = do
  now <- getCurrentTime
  atomically $ modifyTVar_ entries (Entry resource now :)
{-# INLINEABLE putResource #-}

-- | Destroy all resources in all stripes in the pool. Note that this
-- will ignore any exceptions in the destroy function.
--
-- This function is useful when you detect that all resources in the
-- pool are broken. For example after a database has been restarted
-- all connections opened before the restart will be broken. In that
-- case it's better to close those connections so that 'takeResource'
-- won't take a broken connection from the pool but will open a new
-- connection instead.
--
-- Another use-case for this function is that when you know you are
-- done with the pool you can destroy all idle resources immediately
-- instead of waiting on the garbage collector to destroy them, thus
-- freeing up those resources sooner.
destroyAllResources :: Pool a -> IO ()
destroyAllResources Pool {..} = V.forM_ localPools $ purgeLocalPool destroy

modifyTVar_ :: TVar a -> (a -> a) -> STM ()
modifyTVar_ v f = readTVar v >>= \a -> writeTVar v $! f a

modError :: String -> String -> a
modError func msg =
  error $ "Data.Pool." ++ func ++ ": " ++ msg

getInUseResourceCount :: Pool a -> IO Int
getInUseResourceCount Pool {..} =
  -- We aren't transactionally computing the sum across all local pools to
  -- avoid contention with takeResource/withResource
  getSum <$> foldMap' (atomically . fmap Sum . readTVar . inUse) localPools
