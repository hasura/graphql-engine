{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.PG.Query.Pool
  ( ConnParams (..),
    PGPool,
    pgPoolStats,
    pgPoolMetrics,
    PGPoolStats (..),
    PGPoolMetrics (..),
    getInUseConnections,
    defaultConnParams,
    initPGPool,
    resizePGPool,
    destroyPGPool,
    withConn,
    runTx,
    runTx',
    sql,
    sqlFromFile,
    PGExecErr (..),
    FromPGConnErr (..),
    FromPGTxErr (..),

    -- * Forcing destroying of connections
    PGConnectionStale (..),
  )
where

-------------------------------------------------------------------------------

import Control.Exception.Safe (Exception, Handler (..))
import Control.Exception.Safe qualified as Exc
import Control.Monad (when)
import Control.Monad.Except (MonadError (catchError, throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Control (MonadBaseControl, control)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson (ToJSON (toJSON), Value)
import Data.ByteString qualified as BS
import Data.HashTable.IO qualified as HIO
import Data.IORef (newIORef)
import Data.Pool qualified as RP
import Data.String (fromString)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8')
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import Database.PG.Query.Connection
import Database.PG.Query.Transaction
import Database.PostgreSQL.LibPQ qualified as PQ
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Exp, Q, lift, qAddDependentFile, runIO)
import System.Metrics.Distribution (Distribution)
import System.Metrics.Distribution qualified as EKG.Distribution
import System.Metrics.Prometheus.Histogram (Histogram)
import System.Metrics.Prometheus.Histogram qualified as Histogram
import Prelude

-------------------------------------------------------------------------------

data PGPool = PGPool
  { -- | the underlying connection pool
    _pool :: !(RP.Pool PGConn),
    -- | EKG stats about how we acquire, release, and manage connections
    _stats :: !PGPoolStats,
    -- | Prometheus metrics about how we acquire, release, and manage connections
    _metrics :: !PGPoolMetrics
  }

pgPoolStats :: PGPool -> PGPoolStats
pgPoolStats = _stats

pgPoolMetrics :: PGPool -> PGPoolMetrics
pgPoolMetrics = _metrics

-- | Actual ekg gauges and other metrics are not created here, since those depend on
-- a store and it's much simpler to perform the sampling of the distribution from within graphql-engine.
data PGPoolStats = PGPoolStats
  { -- | time taken to acquire new connections from postgres
    _dbConnAcquireLatency :: !Distribution,
    _poolConnAcquireLatency :: !Distribution
  }

data PGPoolMetrics = PGPoolMetrics
  { -- | time taken to establish and initialise a PostgreSQL connection
    _pgConnAcquireLatencyMetric :: !Histogram,
    -- | time taken to acquire a connection from the pool
    _poolWaitTimeMetric :: !Histogram
  }

getInUseConnections :: PGPool -> IO Int
getInUseConnections = RP.getInUseResourceCount . _pool

data ConnParams = ConnParams
  { cpStripes :: !Int,
    cpConns :: !Int,
    -- | Connections that sit idle for longer than cpIdleTime may be destroyed.
    cpIdleTime :: !Int,
    cpAllowPrepare :: !Bool,
    -- | If passed, 'withExpiringPGconn' will destroy the connection when it is older than lifetime.
    cpMbLifetime :: !(Maybe NominalDiffTime),
    -- | If passed, 'withConnection' will throw a 'TimeoutException' after 'timeout' seconds.
    cpTimeout :: !(Maybe NominalDiffTime),
    -- | Transactions will be cancelled if an asynchronous exception is received.
    cpCancel :: !Bool
  }
  deriving stock (Eq, Show)

defaultConnParams :: ConnParams
defaultConnParams = ConnParams 1 20 60 True Nothing Nothing True

initPGPoolStats :: IO PGPoolStats
initPGPoolStats = do
  _dbConnAcquireLatency <- EKG.Distribution.new
  _poolConnAcquireLatency <- EKG.Distribution.new
  pure PGPoolStats {..}

initPGPoolMetrics :: IO PGPoolMetrics
initPGPoolMetrics = do
  _pgConnAcquireLatencyMetric <- Histogram.new histogramBuckets
  _poolWaitTimeMetric <- Histogram.new histogramBuckets
  pure PGPoolMetrics {..}
  where
    histogramBuckets = [0.000001, 0.0001, 0.01, 0.1, 0.3, 1, 3, 10, 30, 100]

initPGPool ::
  ConnInfo ->
  Value ->
  ConnParams ->
  PGLogger ->
  IO PGPool
initPGPool ci context cp logger = do
  _stats <- initPGPoolStats
  _metrics <- initPGPoolMetrics
  _pool <- RP.createPool' (creator _stats _metrics) destroyer nStripes diffTime nConns nTimeout
  pure PGPool {..}
  where
    nStripes = cpStripes cp
    nConns = cpConns cp
    nTimeout = cpTimeout cp
    retryP = mkPGRetryPolicy $ ciRetries ci
    creator stats metrics = do
      createdAt <- getCurrentTime
      pqConn <- initPQConn ci logger
      connAcquiredAt <- getCurrentTime
      let connAcquiredMicroseconds = realToFrac (1000000 * diffUTCTime connAcquiredAt createdAt)
          connAcquiredSeconds = realToFrac $ diffUTCTime connAcquiredAt createdAt
      EKG.Distribution.add (_dbConnAcquireLatency stats) connAcquiredMicroseconds
      Histogram.observe (_pgConnAcquireLatencyMetric metrics) connAcquiredSeconds
      ctr <- newIORef 0
      table <- HIO.new
      return $ PGConn context pqConn (cpAllowPrepare cp) (cpCancel cp) retryP logger ctr table createdAt (cpMbLifetime cp)
    destroyer = PQ.finish . pgPQConn
    diffTime = fromIntegral $ cpIdleTime cp

resizePGPool ::
  PGPool ->
  Int ->
  IO ()
resizePGPool PGPool {..} size = do
  RP.resizePool _pool size
  RP.tryTrimPool _pool

-- | Release all connections acquired by the pool.
destroyPGPool :: PGPool -> IO ()
destroyPGPool = RP.destroyAllResources . _pool

data PGExecErr
  = PGExecErrConn !PGConnErr
  | PGExecErrTx !PGTxErr
  deriving stock (Eq)

instance ToJSON PGExecErr where
  toJSON (PGExecErrConn pce) = toJSON pce
  toJSON (PGExecErrTx txe) = toJSON txe

instance FromPGTxErr PGExecErr where
  fromPGTxErr = PGExecErrTx

instance FromPGConnErr PGExecErr where
  fromPGConnErr = PGExecErrConn

instance FromPGTxErr PGTxErr where
  fromPGTxErr = id

instance FromPGConnErr PGConnErr where
  fromPGConnErr = id

instance Show PGExecErr where
  show (PGExecErrConn pce) = show pce
  show (PGExecErrTx txe) = show txe

beginTx :: (MonadIO m, FromPGTxErr e) => TxMode -> TxET e m ()
beginTx (i, w) =
  unitQE fromPGTxErr query () True
  where
    query =
      fromText . Text.pack $
        ("BEGIN " <> show i <> " " <> maybe "" show w)

commitTx :: (MonadIO m, FromPGTxErr e) => TxET e m ()
commitTx =
  unitQE fromPGTxErr "COMMIT" () True

abortTx :: (MonadIO m, FromPGTxErr e) => TxET e m ()
abortTx =
  unitQE fromPGTxErr "ABORT" () True

class FromPGTxErr e where
  fromPGTxErr :: PGTxErr -> e

class FromPGConnErr e where
  fromPGConnErr :: PGConnErr -> e

asTransaction ::
  (MonadIO m, FromPGTxErr e) =>
  TxMode ->
  (PGConn -> ExceptT e m a) ->
  PGConn ->
  ExceptT e m a
asTransaction txm f pgConn = do
  -- Begin the transaction. If there is an error, you shouldn't call abort
  execTx pgConn $ beginTx txm
  -- Run the actual transaction and commit. If there is an error, abort
  flip catchError abort $ do
    a <- f pgConn
    execTx pgConn commitTx
    return a
  where
    abort e = do
      execTx pgConn abortTx
      throwError e

-- | Run a command using the postgres pool.
--
-- Catches postgres exceptions and converts them to 'e', including
-- 'TimeoutException's thrown in case the timeout is set and reached.
withConn ::
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadError e m,
    FromPGConnErr e
  ) =>
  PGPool ->
  (PGConn -> m a) ->
  m a
withConn pool f =
  catchConnErr $ withExpiringPGconn pool f

catchConnErr ::
  forall e m a.
  (FromPGConnErr e, MonadError e m, MonadBaseControl IO m) =>
  m a ->
  m a
catchConnErr action =
  control $ \runInIO ->
    runInIO action
      `Exc.catches` [ Handler (runInIO . handlePGConnErr),
                      Handler (runInIO . handleTimeout)
                    ]
  where
    handlePGConnErr :: PGConnErr -> m a
    handlePGConnErr = throwError . fromPGConnErr

    handleTimeout :: RP.TimeoutException -> m a
    handleTimeout RP.TimeoutException =
      throwError (fromPGConnErr $ PGConnErr "connection acquisition timeout expired")

-- | Run a command on the given pool wrapped in a transaction.
runTx ::
  ( MonadIO m,
    MonadBaseControl IO m,
    FromPGTxErr e,
    FromPGConnErr e
  ) =>
  PGPool ->
  TxMode ->
  -- | the command to run
  TxET e m a ->
  ExceptT e m a
runTx pool txm tx = do
  withConn pool $ asTransaction txm $ \connRsrc -> execTx connRsrc tx

-- | Run a command on the given pool without wrapping in a transaction.
runTx' ::
  ( MonadIO m,
    MonadBaseControl IO m,
    FromPGConnErr e
  ) =>
  PGPool ->
  TxET e m a ->
  ExceptT e m a
runTx' pool tx = do
  withConn pool $ \connRsrc -> execTx connRsrc tx

sql :: QuasiQuoter
sql = QuasiQuoter {quoteExp, quotePat, quoteType, quoteDec}
  where
    quotePat _ = error "executableDoc does not support quoting patterns"
    quoteType _ = error "executableDoc does not support quoting types"
    quoteDec _ = error "executableDoc does not support quoting declarations"
    quoteExp s = [|fromString $(lift s)|]

-- | Construct a 'Query' at compile-time from some given file.
--
-- NOTE: This function assumes that the file is UTF-8 encoded.
--
-- Any incompatible character encodings will be rejected at compile-time with
-- a 'TE.UnicodeException' error.
sqlFromFile :: FilePath -> Q Exp
sqlFromFile fp = do
  bytes <- qAddDependentFile fp >> runIO (BS.readFile fp)
  case decodeUtf8' bytes of
    Left err -> Exc.impureThrow $! err
    Right txtContents -> do
      -- NOTE: This is (effectively) the same implementation as the 'Lift'
      -- instance for 'Text' from 'th-lift-instances'.
      let strContents = Text.unpack txtContents
      [|fromText $ Text.pack $(lift strContents)|]

-- | 'RP.withResource' for PGPool but implementing a workaround for #5087,
-- optionally expiring the connection after a configurable amount of time so
-- that memory at least can't accumulate unbounded in long-lived connections.
--
-- See ticket at <https://github.com/hasura/graphql-engine/issues/5087>
-- for discussion of more long-term solutions.
--
-- Note that idle connections that aren't actively expired here will be
-- destroyed per the timeout policy in Data.Pool.
withExpiringPGconn ::
  (MonadBaseControl IO m, MonadIO m) => PGPool -> (PGConn -> m a) -> m a
withExpiringPGconn pool f = do
  -- If the connection was stale, we'll discard it and retry, possibly forcing
  -- creation of new connection:
  old <- liftIO getCurrentTime
  handleLifted (\PGConnectionStale -> withExpiringPGconn pool f) $ do
    RP.withResource (_pool pool) $ \connRsrc@PGConn {..} -> do
      now <- liftIO getCurrentTime
      let microseconds = realToFrac (1000000 * diffUTCTime now old)
          seconds = realToFrac $ diffUTCTime now old
      liftIO (EKG.Distribution.add (_poolConnAcquireLatency (_stats pool)) microseconds)
      liftIO (Histogram.observe (_poolWaitTimeMetric (_metrics pool)) seconds)
      let connectionStale =
            any (\lifetime -> now `diffUTCTime` pgCreatedAt > lifetime) pgMbLifetime
      when connectionStale $ do
        -- Throwing is the only way to signal to resource pool to discard the
        -- connection at this time, so we need to use it for control flow:
        Exc.impureThrow PGConnectionStale
      -- else proceed with callback:
      f connRsrc

-- | Used internally (see 'withExpiringPGconn'), but exported in case we need
-- to allow callback to signal that the connection should be destroyed and we
-- should retry.
data PGConnectionStale = PGConnectionStale
  deriving stock (Show)
  deriving anyclass (Exception)

-- cribbed from lifted-base
handleLifted :: (MonadBaseControl IO m, Exception e) => (e -> m a) -> m a -> m a
handleLifted handler ma = control $ \runInIO ->
  Exc.handle
    (runInIO . handler)
    (runInIO ma)
