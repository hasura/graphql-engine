{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Database.PG.Query.Pool
  ( ConnParams (..),
    PGPool,
    pgPoolStats,
    PGPoolStats (..),
    getInUseConnections,
    withExpiringPGconn,
    defaultConnParams,
    initPGPool,
    resizePGPool,
    destroyPGPool,
    withConn,
    beginTx,
    abortTx,
    commitTx,
    runTx,
    runTx',
    catchConnErr,
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
import Control.Monad.Trans.Except (ExceptT, withExceptT)
import Data.Aeson (ToJSON (toJSON))
import Data.ByteString qualified as BS
import Data.HashTable.IO qualified as HIO
import Data.IORef (newIORef)
import Data.Kind (Constraint, Type)
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
import Prelude

-------------------------------------------------------------------------------

type PGPool :: Type
data PGPool = PGPool
  { -- | the underlying connection pool
    _pool :: !(RP.Pool PGConn),
    -- | EKG stats about how we acquire, release, and manage connections
    _stats :: !PGPoolStats
  }

pgPoolStats :: PGPool -> PGPoolStats
pgPoolStats = _stats

-- | Actual ekg gauges and other metrics are not created here, since those depend on
-- a store and it's much simpler to perform the sampling of the distribution from within graphql-engine.
type PGPoolStats :: Type
data PGPoolStats = PGPoolStats
  { -- | time taken to acquire new connections from postgres
    _dbConnAcquireLatency :: !Distribution,
    _poolConnAcquireLatency :: !Distribution
  }

getInUseConnections :: PGPool -> IO Int
getInUseConnections = RP.getInUseResourceCount . _pool

type ConnParams :: Type
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

initPGPool ::
  ConnInfo ->
  ConnParams ->
  PGLogger ->
  IO PGPool
initPGPool ci cp logger = do
  _stats <- initPGPoolStats
  _pool <- RP.createPool' (creator _stats) destroyer nStripes diffTime nConns nTimeout
  pure PGPool {..}
  where
    nStripes = cpStripes cp
    nConns = cpConns cp
    nTimeout = cpTimeout cp
    retryP = mkPGRetryPolicy $ ciRetries ci
    creator stats = do
      createdAt <- getCurrentTime
      pqConn <- initPQConn ci logger
      connAcquiredAt <- getCurrentTime
      let connAcquiredMicroseconds = realToFrac (1000000 * diffUTCTime connAcquiredAt createdAt)
      EKG.Distribution.add (_dbConnAcquireLatency stats) connAcquiredMicroseconds
      ctr <- newIORef 0
      table <- HIO.new
      return $ PGConn pqConn (cpAllowPrepare cp) (cpCancel cp) retryP logger ctr table createdAt (cpMbLifetime cp)
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

type PGExecErr :: Type
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

instance Show PGExecErr where
  show (PGExecErrConn pce) = show pce
  show (PGExecErrTx txe) = show txe

beginTx :: (MonadIO m) => TxMode -> TxT m ()
beginTx (i, w) =
  unitQ query () True
  where
    query =
      fromText . Text.pack $
        ("BEGIN " <> show i <> " " <> maybe "" show w)

commitTx :: (MonadIO m) => TxT m ()
commitTx =
  unitQ "COMMIT" () True

abortTx :: (MonadIO m) => TxT m ()
abortTx =
  unitQ "ABORT" () True

type FromPGTxErr :: Type -> Constraint
class FromPGTxErr e where
  fromPGTxErr :: PGTxErr -> e

type FromPGConnErr :: Type -> Constraint
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
  withExceptT fromPGTxErr $ execTx pgConn $ beginTx txm
  -- Run the actual transaction and commit. If there is an error, abort
  flip catchError abort $ do
    a <- f pgConn
    withExceptT fromPGTxErr $ execTx pgConn commitTx
    return a
  where
    abort e = do
      withExceptT fromPGTxErr $ execTx pgConn abortTx
      throwError e

-- | Run a command using the postgres pool.
--
-- Catches postgres exceptions and converts them to 'e', including
-- 'TimeoutException's thrown in case the timeout is set and reached.
withConn ::
  ( MonadIO m,
    MonadBaseControl IO m,
    FromPGConnErr e
  ) =>
  PGPool ->
  (PGConn -> ExceptT e m a) ->
  ExceptT e m a
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
      `Exc.catches` [ Handler (runInIO . handler),
                      Handler (runInIO . handleTimeout)
                    ]
  where
    handler = mkConnExHandler action fromPGConnErr

    handleTimeout :: RP.TimeoutException -> m a
    handleTimeout _ =
      throwError (fromPGConnErr $ PGConnErr "connection acquisition timeout expired")

{-# INLINE mkConnExHandler #-}
mkConnExHandler ::
  (MonadError e m) =>
  m a ->
  (PGConnErr -> e) ->
  (PGConnErr -> m a)
mkConnExHandler _ ef = throwError . ef

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
      liftIO (EKG.Distribution.add (_poolConnAcquireLatency (_stats pool)) microseconds)
      let connectionStale =
            maybe False (\lifetime -> now `diffUTCTime` pgCreatedAt > lifetime) pgMbLifetime
      when connectionStale $ do
        -- Throwing is the only way to signal to resource pool to discard the
        -- connection at this time, so we need to use it for control flow:
        Exc.impureThrow PGConnectionStale
      -- else proceed with callback:
      f connRsrc

-- | Used internally (see 'withExpiringPGconn'), but exported in case we need
-- to allow callback to signal that the connection should be destroyed and we
-- should retry.
type PGConnectionStale :: Type
data PGConnectionStale = PGConnectionStale
  deriving stock (Show)
  deriving anyclass (Exception)

-- cribbed from lifted-base
handleLifted :: (MonadBaseControl IO m, Exception e) => (e -> m a) -> m a -> m a
handleLifted handler ma = control $ \runInIO ->
  Exc.handle
    (runInIO . handler)
    (runInIO ma)
