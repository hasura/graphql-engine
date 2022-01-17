module Database.MSSQL.Transaction
  ( runTx,
    runTxE,
    unitQuery,
    unitQueryE,
    singleRowQuery,
    singleRowQueryE,
    multiRowQuery,
    multiRowQueryE,
    rawQuery,
    rawQueryE,
    buildGenericQueryTxE,
    TxT,
    TxET (..),
    MSSQLTxError (..),
    withTxET,
  )
where

import Control.Exception (try)
import Control.Monad.Except
  ( ExceptT (..),
    MonadError,
    catchError,
    throwError,
    withExceptT,
  )
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Morph (MFunctor (hoist), MonadTrans (..))
import Control.Monad.Reader (MonadFix, MonadReader, ReaderT (..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Database.MSSQL.Pool
import Database.ODBC.SQLServer (FromRow)
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Prelude (Text, hoistEither, liftEither, mapLeft)
import Prelude

data MSSQLTxError
  = MSSQLQueryError !ODBC.Query !ODBC.ODBCException
  | MSSQLConnError !ODBC.ODBCException
  | MSSQLInternal !Text
  deriving (Eq, Show)

-- | A successful result from a query is a list of rows where each row contains list of column values
newtype MSSQLResult = MSSQLResult [[ODBC.Value]]
  deriving (Eq, Show)

-- | The transaction command to run, parameterised over:
-- e - the exception type
-- m - some Monad
-- a - the successful result type
newtype TxET e m a = TxET {txHandler :: ReaderT ODBC.Connection (ExceptT e m) a}
  deriving (Functor, Applicative, Monad, MonadError e, MonadIO, MonadReader ODBC.Connection, MonadFix)

instance MonadTrans (TxET e) where
  lift = TxET . lift . lift

-- | The transaction command to run,
-- returning an MSSQLTxError or the result
type TxT m a = TxET MSSQLTxError m a

beginTx :: MonadIO m => TxT m ()
beginTx =
  unitQuery "BEGIN TRANSACTION"

commitTx :: MonadIO m => TxT m ()
commitTx = do
  transactionState <- getTransactionState
  case transactionState of
    TSActive -> unitQuery "COMMIT TRANSACTION"
    TSUncommittable -> throwError $ MSSQLInternal "Transaction is uncommittable"
    TSNoActive -> throwError $ MSSQLInternal "No active transaction exist; cannot commit"

rollbackTx :: MonadIO m => TxT m ()
rollbackTx = do
  transactionState <- getTransactionState
  let rollback = unitQuery "ROLLBACK TRANSACTION"
  case transactionState of
    TSActive -> rollback
    TSUncommittable ->
      -- We can only do a full rollback of an uncommittable transaction
      rollback
    TSNoActive ->
      -- Few query exceptions result in an auto-rollback of the transaction.
      -- For eg. Creating a table with already existing table name (See https://github.com/hasura/graphql-engine-mono/issues/3046)
      -- In such cases, we shouldn't rollback the transaction again.
      pure ()

-- | Useful for building transactions which returns no data
--
-- insertId :: TxT m ()
-- insertId = unitQuery "INSERT INTO some_table VALUES (1, \"hello\")"
unitQuery :: (MonadIO m) => ODBC.Query -> TxT m ()
unitQuery = unitQueryE id

-- | Similar to @'unitQuery' but with an error modifier
unitQueryE ::
  (MonadIO m) =>
  -- | Error modifier
  (MSSQLTxError -> e) ->
  -- | Query to run
  ODBC.Query ->
  TxET e m ()
unitQueryE ef = rawQueryE ef emptyResult
  where
    emptyResult :: MSSQLResult -> Either String ()
    emptyResult (MSSQLResult []) = Right ()
    emptyResult (MSSQLResult _) = Left "expecting no data for ()"

-- | Useful for building query transactions which returns only one row.
--
-- returnOne :: TxT m Int
-- returnOne = singleRowQuery "SELECT 1"
singleRowQuery :: (MonadIO m, FromRow a) => ODBC.Query -> TxT m a
singleRowQuery = singleRowQueryE id

-- | Similar to @'multiRowQuery' but with an error modifier
singleRowQueryE ::
  forall m a e.
  (MonadIO m, FromRow a) =>
  -- | Error modifier
  (MSSQLTxError -> e) ->
  -- | Query to run
  ODBC.Query ->
  TxET e m a
singleRowQueryE ef = rawQueryE ef singleRowResult
  where
    singleRowResult :: MSSQLResult -> Either String a
    singleRowResult (MSSQLResult [row]) = ODBC.fromRow row
    singleRowResult (MSSQLResult _) = Left "expecting single row"

-- | The transaction state of the current connection
data TransactionState
  = -- | Has an active transaction.
    TSActive
  | -- | Has no active transaction.
    TSNoActive
  | -- | An error occurred that caused the transaction to be uncommittable. We cannot commit or
    -- rollback to a savepoint; we can only do a full rollback of the transaction.
    TSUncommittable

-- | Get the @'TransactionState' of current connection
-- For more details, refer to https://docs.microsoft.com/en-us/sql/t-sql/functions/xact-state-transact-sql?view=sql-server-ver15
getTransactionState :: (MonadIO m) => TxT m TransactionState
getTransactionState = do
  let query = "SELECT XACT_STATE()"
  xactState :: Int <- singleRowQuery query
  case xactState of
    1 -> pure TSActive
    0 -> pure TSNoActive
    -1 -> pure TSUncommittable
    _ -> throwError $ MSSQLQueryError query $ ODBC.DataRetrievalError "Unexpected value for XACT_STATE"

-- | Useful for building query transactions which returns multiple rows.
--
-- selectIds :: TxT m [Int]
-- selectIds = multiRowQuery "SELECT id FROM author"
multiRowQuery :: (MonadIO m, FromRow a) => ODBC.Query -> TxT m [a]
multiRowQuery = multiRowQueryE id

-- | Similar to @'multiRowQuery' but with an error modifier
multiRowQueryE ::
  forall m a e.
  (MonadIO m, FromRow a) =>
  -- | Error modifier
  (MSSQLTxError -> e) ->
  -- | Query to run
  ODBC.Query ->
  TxET e m [a]
multiRowQueryE ef = rawQueryE ef multiRowResult
  where
    multiRowResult :: MSSQLResult -> Either String [a]
    multiRowResult (MSSQLResult rows) = mapM ODBC.fromRow rows

-- | Build a raw query transaction which on successful execution returns @'MSSQLResult'
rawQuery :: (MonadIO m) => ODBC.Query -> TxT m MSSQLResult
rawQuery = rawQueryE id pure

-- | Similar to @'rawQuery' but with error modifier and @'MSSQLResult' modifier
rawQueryE ::
  (MonadIO m) =>
  -- | Error modifier
  (MSSQLTxError -> e) ->
  -- | Result modifier with a failure
  (MSSQLResult -> Either String a) ->
  -- | Query to run
  ODBC.Query ->
  TxET e m a
rawQueryE ef rf q = do
  rows <- buildGenericQueryTxE ef q id ODBC.query
  liftEither $ mapLeft (ef . MSSQLQueryError q . ODBC.DataRetrievalError) $ rf (MSSQLResult rows)

-- | Build a generic transaction out of an IO action
buildGenericQueryTxE ::
  (MonadIO m) =>
  -- | Exception modifier
  (MSSQLTxError -> e) ->
  -- | The Query
  query ->
  -- | Query to ODBC.Query converter
  (query -> ODBC.Query) ->
  -- | IO action
  (ODBC.Connection -> query -> IO a) ->
  TxET e m a
buildGenericQueryTxE errorF query convertQ runQuery = TxET $
  ReaderT $ \conn -> withExceptT errorF $ execQuery query convertQ (runQuery conn)

execQuery ::
  forall m a query.
  (MonadIO m) =>
  query ->
  (query -> ODBC.Query) ->
  (query -> IO a) ->
  ExceptT MSSQLTxError m a
execQuery query toODBCQuery runQuery = do
  result :: Either ODBC.ODBCException a <- liftIO $ try $ runQuery query
  withExceptT (MSSQLQueryError $ toODBCQuery query) $ hoistEither result

-- | Run a command on the given connection wrapped in a transaction.
runTx ::
  (MonadIO m, MonadBaseControl IO m) =>
  TxT m a ->
  MSSQLPool ->
  ExceptT MSSQLTxError m a
runTx = runTxE id

-- | Run a command on the given connection wrapped in a transaction.
runTxE ::
  (MonadIO m, MonadBaseControl IO m) =>
  (MSSQLTxError -> e) ->
  TxET e m a ->
  MSSQLPool ->
  ExceptT e m a
runTxE ef tx pool = do
  withMSSQLPool pool (asTransaction ef (`execTx` tx))
    >>= hoistEither . mapLeft (ef . MSSQLConnError)

-- withConn pool $ asTransaction txm $ \connRsrc -> execTx connRsrc tx

{-# INLINE execTx #-}
execTx :: ODBC.Connection -> TxET e m a -> ExceptT e m a
execTx conn tx = runReaderT (txHandler tx) conn

asTransaction ::
  MonadIO m =>
  (MSSQLTxError -> e) ->
  (ODBC.Connection -> ExceptT e m a) ->
  ODBC.Connection ->
  ExceptT e m a
asTransaction ef f conn = do
  -- Begin the transaction. If there is an err, do not rollback
  withExceptT ef $ execTx conn beginTx
  -- Run the transaction and commit. If there is an err, rollback
  flip catchError rollback $ do
    result <- f conn
    withExceptT ef $ execTx conn commitTx
    pure result
  where
    rollback err = do
      withExceptT ef $ execTx conn rollbackTx
      throwError err

withTxET :: Monad m => (e1 -> e2) -> TxET e1 m a -> TxET e2 m a
withTxET f (TxET m) = TxET $ hoist (withExceptT f) m
