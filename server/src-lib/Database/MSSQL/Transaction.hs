{-# LANGUAGE UndecidableInstances #-}

module Database.MSSQL.Transaction
  ( TxET (..),
    MSSQLTxError (..),
    TxIsolation (..),
    TxT,
    TxE,
    runTx,
    runTxE,
    unitQuery,
    unitQueryE,
    singleRowQuery,
    singleRowQueryE,
    multiRowQuery,
    multiRowQueryE,
    forJsonQueryE,
    buildGenericQueryTxE,
    withTxET,
  )
where

import Autodocodec (HasCodec (codec), bimapCodec, textCodec, (<?>))
import Autodocodec.Aeson qualified as AC
import Control.Exception (try)
import Control.Monad.Morph (MFunctor (hoist))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson qualified as J
import Data.Text qualified as T
import Database.MSSQL.Pool
import Database.ODBC.SQLServer (FromRow)
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Prelude

-- | The transaction command to run, parameterised over:
-- e - the exception type (usually 'MSSQLTxError')
-- m - some Monad, (usually some 'MonadIO')
-- a - the successful result type
newtype TxET e m a = TxET
  { txHandler :: ReaderT ODBC.Connection (ExceptT e m) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError e,
      MonadIO,
      MonadReader ODBC.Connection,
      MonadFix
    )

instance MFunctor (TxET e) where
  hoist f = TxET . hoist (hoist f) . txHandler

instance MonadTrans (TxET e) where
  lift = TxET . lift . lift

deriving via (ReaderT ODBC.Connection (ExceptT e m)) instance (MonadBase IO m) => MonadBase IO (TxET e m)

deriving via (ReaderT ODBC.Connection (ExceptT e m)) instance (MonadBaseControl IO m) => MonadBaseControl IO (TxET e m)

-- | Error type generally used in 'TxET'.
data MSSQLTxError
  = MSSQLQueryError !ODBC.Query !ODBC.ODBCException
  | MSSQLConnError !ODBC.ODBCException
  | MSSQLInternal !Text
  deriving (Eq, Show)

type TxE e a = TxET e IO a

-- | The transaction command to run, returning an MSSQLTxError or the result.
type TxT m a = TxET MSSQLTxError m a

-- | Run a command on the given connection wrapped in a transaction.
--
-- See 'runTxE' if you need to map the error type as well.
runTx ::
  (MonadIO m, MonadBaseControl IO m) =>
  TxIsolation ->
  TxT m a ->
  MSSQLPool ->
  ExceptT MSSQLTxError m a
runTx = runTxE id

-- | Run a command on the given connection wrapped in a transaction.
runTxE ::
  (MonadIO m, MonadBaseControl IO m) =>
  (MSSQLTxError -> e) ->
  TxIsolation ->
  TxET e m a ->
  MSSQLPool ->
  ExceptT e m a
runTxE ef txIsolation tx pool = do
  withMSSQLPool pool (asTransaction ef txIsolation (`execTx` tx))
    >>= hoistEither
    . mapLeft (ef . MSSQLConnError)

-- | Useful for building transactions which return no data.
--
-- @
-- insertId :: TxT m ()
-- insertId = unitQuery "INSERT INTO some_table VALUES (1, \"hello\")"
-- @
--
-- See 'unitQueryE' if you need to map the error type as well.
unitQuery :: (MonadIO m) => ODBC.Query -> TxT m ()
unitQuery = unitQueryE id

-- | Useful for building transactions which return no data.
unitQueryE :: (MonadIO m) => (MSSQLTxError -> e) -> ODBC.Query -> TxET e m ()
unitQueryE ef = rawQueryE ef emptyResult
  where
    emptyResult :: MSSQLResult -> Either String ()
    emptyResult (MSSQLResult []) = Right ()
    emptyResult (MSSQLResult _) = Left "expecting no data for ()"

-- | Useful for building query transactions which return a single one row.
--
-- @
-- returnOne :: TxT m Int
-- returnOne = singleRowQuery "SELECT 1"
-- @
--
-- See 'singleRowQueryE' if you need to map the error type as well.
singleRowQuery :: forall a m. (MonadIO m, FromRow a) => ODBC.Query -> TxT m a
singleRowQuery = singleRowQueryE id

-- | Useful for building query transactions which return a single one row.
singleRowQueryE ::
  forall m a e.
  (MonadIO m, FromRow a) =>
  (MSSQLTxError -> e) ->
  ODBC.Query ->
  TxET e m a
singleRowQueryE ef = rawQueryE ef singleRowResult
  where
    singleRowResult :: MSSQLResult -> Either String a
    singleRowResult (MSSQLResult [row]) = ODBC.fromRow row
    singleRowResult (MSSQLResult _) = Left "expecting single row"

-- | MSSQL splits up results that have a @SELECT .. FOR JSON@ at the top-level
-- into multiple rows with a single column, see
-- https://docs.microsoft.com/en-us/sql/relational-databases/json/format-query-results-as-json-with-for-json-sql-server?view=sql-server-ver15#output-of-the-for-json-clause
--
-- This function simply concatenates each single-column row into one long 'Text' string.
forJsonQueryE ::
  forall m e.
  (MonadIO m) =>
  (MSSQLTxError -> e) ->
  ODBC.Query ->
  TxET e m Text
forJsonQueryE ef = rawQueryE ef concatRowResult
  where
    concatRowResult :: MSSQLResult -> Either String Text
    concatRowResult (MSSQLResult []) = pure mempty
    concatRowResult (MSSQLResult rows@(r1 : _)) | length r1 == 1 = mconcat <$> mapM ODBC.fromRow rows
    concatRowResult (MSSQLResult (r1 : _)) = Left $ "forJsonQueryE: Expected single-column results, but got " <> show (length r1) <> " columns"

-- | Useful for building query transactions which return multiple rows.
--
-- @
-- selectIds :: TxT m [Int]
-- selectIds = multiRowQuery "SELECT id FROM author"
-- @
--
-- See 'multiRowQueryE' if you need to map the error type as well.
multiRowQuery :: forall a m. (MonadIO m, FromRow a) => ODBC.Query -> TxT m [a]
multiRowQuery = multiRowQueryE id

-- | Useful for building query transactions which return multiple rows.
multiRowQueryE ::
  forall m a e.
  (MonadIO m, FromRow a) =>
  (MSSQLTxError -> e) ->
  ODBC.Query ->
  TxET e m [a]
multiRowQueryE ef = rawQueryE ef multiRowResult
  where
    multiRowResult :: MSSQLResult -> Either String [a]
    multiRowResult (MSSQLResult rows) = traverse ODBC.fromRow rows

-- | Build a generic transaction out of an IO action.
buildGenericQueryTxE ::
  (MonadIO m) =>
  -- | map 'MSSQLTxError' to some other type
  (MSSQLTxError -> e) ->
  -- | query to run
  query ->
  -- | how to map a query to a 'ODBC.Query'
  (query -> ODBC.Query) ->
  -- | run the query on a provided 'ODBC.Connection'
  (ODBC.Connection -> query -> IO a) ->
  TxET e m a
buildGenericQueryTxE errorF query convertQ runQuery =
  TxET $ ReaderT $ withExceptT errorF . execQuery query convertQ . runQuery

-- | Map the error type for a 'TxET'.
withTxET :: (Monad m) => (e1 -> e2) -> TxET e1 m a -> TxET e2 m a
withTxET f (TxET m) = TxET $ hoist (withExceptT f) m

-- | A successful result from a query is a list of rows where each row contains
-- list of column values
newtype MSSQLResult = MSSQLResult [[ODBC.Value]]
  deriving (Eq, Show)

-- | Packs a query, along with result and error converters into a 'TxET'.
--
-- Used by 'unitQueryE', 'singleRowQueryE', and 'multiRowQueryE'.
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
  liftEither
    $ mapLeft (ef . MSSQLQueryError q . ODBC.DataRetrievalError)
    $ rf (MSSQLResult rows)

-- | Combinator for abstracting over the query type and ensuring we catch exceptions.
--
-- Used by 'buildGenericQueryTxE'.
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

-- | Run a 'TxET' with the given connection.
--
-- Used by 'runTxE' and 'asTransaction'.
execTx :: ODBC.Connection -> TxET e m a -> ExceptT e m a
execTx conn tx = runReaderT (txHandler tx) conn
{-# INLINE execTx #-}

-- | The transaction state of the current connection
data TransactionState
  = -- | Has an active transaction.
    TSActive
  | -- | Has no active transaction.
    TSNoActive
  | -- | An error occurred that caused the transaction to be uncommittable.
    -- We cannot commit or rollback to a savepoint; we can only do a full
    -- rollback of the transaction.
    TSUncommittable

-- | <https://learn.microsoft.com/en-us/sql/t-sql/statements/set-transaction-isolation-level-transact-sql>
data TxIsolation
  = ReadUncommitted
  | ReadCommitted
  | RepeatableRead
  | Snapshot
  | Serializable
  deriving (Eq, Generic)

instance Show TxIsolation where
  show = \case
    ReadUncommitted -> "READ UNCOMMITTED"
    ReadCommitted -> "READ COMMITTED"
    RepeatableRead -> "REPEATABLE READ"
    Snapshot -> "SNAPSHOT"
    Serializable -> "SERIALIZABLE"

instance Hashable TxIsolation

instance NFData TxIsolation

instance HasCodec TxIsolation where
  codec =
    bimapCodec
      decode
      encode
      textCodec
      <?> "Isolation level"
    where
      decode :: Text -> Either String TxIsolation
      decode = \case
        "read-uncommitted" -> Right ReadUncommitted
        "read-committed" -> Right ReadCommitted
        "repeatable-read" -> Right RepeatableRead
        "snapshot" -> Right Snapshot
        "serializable" -> Right Serializable
        _ ->
          Left
            $ T.unpack
            $ "Unexpected options for isolation_level. Expected "
            <> "'read-uncommited' | 'read-committed' | 'repeatable-read' | 'snapshot' | 'serializable'"
      encode :: TxIsolation -> Text
      encode = \case
        ReadUncommitted -> "read-uncommitted"
        ReadCommitted -> "read-committed"
        RepeatableRead -> "repeatable-read"
        Snapshot -> "snapshot"
        Serializable -> "serializable"

instance J.ToJSON TxIsolation where
  toJSON = AC.toJSONViaCodec
  toEncoding = AC.toEncodingViaCodec

instance J.FromJSON TxIsolation where
  parseJSON = AC.parseJSONViaCodec

-- | Wraps an action in a transaction. Rolls back on errors.
asTransaction ::
  forall e a m.
  (MonadIO m) =>
  (MSSQLTxError -> e) ->
  TxIsolation ->
  (ODBC.Connection -> ExceptT e m a) ->
  ODBC.Connection ->
  ExceptT e m a
asTransaction ef txIsolation action conn = do
  -- Begin the transaction. If there is an error, do not rollback.
  withExceptT ef $ execTx conn $ setTxIsoLevelTx txIsolation >> beginTx
  -- Run the transaction and commit. If there is an error, rollback.
  flip catchError rollbackAndThrow do
    result <- action conn
    -- After running the transaction, set the transaction isolation level
    -- to the default isolation level i.e. Read Committed
    withExceptT ef $ execTx conn $ commitTx >> setTxIsoLevelTx ReadCommitted
    pure result
  where
    -- Rollback and throw error.
    rollbackAndThrow :: e -> ExceptT e m b
    rollbackAndThrow err = do
      withExceptT ef $ execTx conn rollbackTx
      throwError err

beginTx :: (MonadIO m) => TxT m ()
beginTx = unitQuery "BEGIN TRANSACTION"

setTxIsoLevelTx :: (MonadIO m) => TxIsolation -> TxT m ()
setTxIsoLevelTx txIso =
  unitQuery $ ODBC.rawUnescapedText $ "SET TRANSACTION ISOLATION LEVEL " <> tshow txIso <> ";"

commitTx :: (MonadIO m) => TxT m ()
commitTx =
  getTransactionState >>= \case
    TSActive ->
      unitQuery "COMMIT TRANSACTION"
    TSUncommittable ->
      throwError $ MSSQLInternal "Transaction is uncommittable"
    TSNoActive ->
      throwError $ MSSQLInternal "No active transaction exist; cannot commit"

rollbackTx :: (MonadIO m) => TxT m ()
rollbackTx =
  let rollback = unitQuery "ROLLBACK TRANSACTION"
   in getTransactionState >>= \case
        TSActive -> rollback
        TSUncommittable -> rollback
        TSNoActive ->
          -- Some query exceptions result in an auto-rollback of the transaction.
          -- For eg. Creating a table with already existing table name (See https://github.com/hasura/graphql-engine-mono/issues/3046)
          -- In such cases, we shouldn't rollback the transaction again.
          pure ()

-- | Get the @'TransactionState' of current connection
-- For more details, refer to https://docs.microsoft.com/en-us/sql/t-sql/functions/xact-state-transact-sql?view=sql-server-ver15
getTransactionState :: (MonadIO m) => TxT m TransactionState
getTransactionState =
  let query = "SELECT XACT_STATE()"
   in singleRowQuery @Int query
        >>= \case
          1 -> pure TSActive
          0 -> pure TSNoActive
          -1 -> pure TSUncommittable
          _ ->
            throwError
              $ MSSQLQueryError query
              $ ODBC.DataRetrievalError "Unexpected value for XACT_STATE"
