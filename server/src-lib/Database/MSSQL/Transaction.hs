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
    buildGenericTxE,
    TxT,
    TxET (..),
    MSSQLTxError (..),
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
import Database.ODBC.SQLServer (FromRow)
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Prelude (hoistEither, liftEither, mapLeft)
import Prelude

data MSSQLTxError
  = MSSQLTxError !ODBC.Query !ODBC.ODBCException
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
commitTx =
  unitQuery "COMMIT TRANSACTION"

rollbackTx :: MonadIO m => TxT m ()
rollbackTx =
  unitQuery "ROLLBACK TRANSACTION"

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
rawQueryE ef rf q = TxET $
  ReaderT $ \conn ->
    hoist liftIO $
      withExceptT ef $
        execQuery conn q
          >>= liftEither . mapLeft (MSSQLTxError q . ODBC.DataRetrievalError) . rf

-- | Build a generic transaction out of an IO action
buildGenericTxE ::
  (MonadIO m) =>
  -- | Exception modifier
  (ODBC.ODBCException -> e) ->
  -- | IO action
  (ODBC.Connection -> IO a) ->
  TxET e m a
buildGenericTxE ef f = TxET $
  ReaderT $ \conn -> do
    result <- liftIO $ try $ f conn
    withExceptT ef $ hoistEither result

execQuery ::
  (MonadIO m) =>
  ODBC.Connection ->
  ODBC.Query ->
  ExceptT MSSQLTxError m MSSQLResult
execQuery conn query = do
  result :: Either ODBC.ODBCException [[ODBC.Value]] <- liftIO $ try $ ODBC.query conn query
  withExceptT (MSSQLTxError query) $ hoistEither $ MSSQLResult <$> result

-- | Run a command on the given connection wrapped in a transaction.
runTx ::
  MonadIO m =>
  TxT m a ->
  ODBC.Connection ->
  ExceptT MSSQLTxError m a
runTx = runTxE id

-- | Run a command on the given connection wrapped in a transaction.
runTxE ::
  MonadIO m =>
  (MSSQLTxError -> e) ->
  TxET e m a ->
  ODBC.Connection ->
  ExceptT e m a
runTxE ef tx =
  asTransaction ef (`execTx` tx)

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
