module Database.MSSQL.Transaction
  ( runTx
  , unitQ
  , withQ
  , TxT
  , TxET(..)
  , MSSQLTxError(..)
  , ResultOk(..)
  ) where

import           Hasura.Prelude          (hoistEither)
import           Prelude

import qualified Database.ODBC.SQLServer as ODBC

import           Control.Exception       (try)
import           Control.Monad           (void)
import           Control.Monad.Except    (ExceptT (..), MonadError, catchError, throwError,
                                          withExceptT)
import           Control.Monad.IO.Class  (MonadIO (..))
import           Control.Monad.Morph     (hoist)
import           Control.Monad.Reader    (MonadFix, MonadReader, ReaderT (..))


data MSSQLTxError
  = MSSQLTxError !ODBC.Query !ODBC.ODBCException
  deriving (Eq, Show)

newtype ResultOk = ResultOk [[ODBC.Value]]
  deriving (Eq, Show)

-- | The transaction command to run, parameterised over:
-- e - the exception type
-- m - some Monad
-- a - the successful result type
newtype TxET e m a
  = TxET { txHandler :: ReaderT ODBC.Connection (ExceptT e m) a }
  deriving (Functor, Applicative, Monad, MonadError e, MonadIO, MonadReader ODBC.Connection, MonadFix)

-- | The transaction command to run,
-- returning an MSSQLTxError or the result
type TxT m a = TxET MSSQLTxError m a

beginTx :: MonadIO m => TxT m ()
beginTx =
  unitQ "BEGIN TRANSACTION"

commitTx :: MonadIO m => TxT m ()
commitTx =
  unitQ "COMMIT TRANSACTION"

rollbackTx :: MonadIO m => TxT m ()
rollbackTx =
  unitQ "ROLLBACK TRANSACTION"

unitQ :: MonadIO m => ODBC.Query -> TxT m ()
unitQ = void <$> withQ

withQ :: MonadIO m => ODBC.Query -> TxT m ResultOk
withQ q = TxET $ ReaderT $ \conn ->
  hoist liftIO $ execQuery conn q

execQuery
  :: MonadIO m
  => ODBC.Connection
  -> ODBC.Query
  -> ExceptT MSSQLTxError m ResultOk
execQuery conn query = do
  result :: Either ODBC.ODBCException [[ODBC.Value]] <- liftIO $ try $ ODBC.query conn query
  withExceptT (MSSQLTxError query) $ hoistEither $ ResultOk <$> result

-- | Run a command on the given connection wrapped in a transaction.
runTx :: MonadIO m
      => TxT m ResultOk
      -> ODBC.Connection
      -> ExceptT MSSQLTxError m ResultOk
runTx tx =
  asTransaction (\connRsrc -> execTx connRsrc tx)

{-# INLINE execTx #-}
execTx :: ODBC.Connection -> TxET e m a -> ExceptT e m a
execTx conn tx = runReaderT (txHandler tx) conn

asTransaction :: MonadIO m
              => (ODBC.Connection -> ExceptT MSSQLTxError m ResultOk)
              -> ODBC.Connection
              -> ExceptT MSSQLTxError m ResultOk
asTransaction f conn = do
  -- Begin the transaction. If there is an err, do not rollback
  _ <- execTx conn beginTx
  -- Run the transaction and commit. If there is an err, rollback
  flip catchError rollback $ do
    result <- f conn
    _ <- execTx conn commitTx
    return result
  where
    rollback err = do
      _ <- execTx conn rollbackTx
      throwError err
