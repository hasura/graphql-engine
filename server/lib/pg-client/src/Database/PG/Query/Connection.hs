{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Database.PG.Query.Connection
  ( initPQConn,
    defaultConnInfo,
    ConnInfo (..),
    ConnDetails (..),
    extractConnOptions,
    extractHost,
    ConnOptions (..),
    pgConnString,
    PGQuery (..),
    PGRetryPolicy,
    mkPGRetryPolicy,
    PGLogEvent (..),
    PGLogger,
    PGConn (..),
    resetPGConn,
    PGConnErr (..),
    readConnErr,
    ResultOk (..),
    getPQRes,
    Template,
    mkTemplate,
    PrepArg,
    prepare,
    execMulti,
    execQuery,
    lenientDecodeUtf8,
    PGErrInternal (..),
    PGStmtErrDetail (..),
    describePrepared,
    PreparedDescription (..),
  )
where

-------------------------------------------------------------------------------

import Control.Concurrent.Interrupt (interruptOnAsyncException)
import Control.Exception.Safe (Exception, SomeException (..), catch, throwIO)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, withExceptT)
import Control.Retry (RetryPolicyM)
import Control.Retry qualified as Retry
import Data.Aeson (ToJSON (toJSON), Value (String), genericToJSON, object, (.=))
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.Aeson.TH (mkToJSON)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 (unpack)
import Data.Foldable (for_)
import Data.HashTable.IO qualified as HIO
import Data.Hashable (Hashable (hashWithSalt))
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Monoid (getLast)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (NominalDiffTime, UTCTime)
import Data.Word (Word16, Word32)
import Database.PostgreSQL.LibPQ qualified as PQ
import Database.PostgreSQL.Simple.Options qualified as Options
import GHC.Generics (Generic)
import Prelude

{-# ANN module ("HLint: ignore Use tshow" :: String) #-}

{-# ANN module ("HLint: ignore Use onLeft" :: String) #-}

-------------------------------------------------------------------------------

data ConnOptions = ConnOptions
  { connHost :: !String,
    connPort :: !Int,
    connUser :: !String,
    connPassword :: !String,
    connDatabase :: !String,
    connOptions :: !(Maybe String)
  }
  deriving stock (Eq, Read, Show)

-- | The data needed to establish a postgres connection, isomorphic  to a
-- postgres connection string, with the recent addition that this can be
-- dynamic: effectively an IO action That returns a connection string (the use
-- case we're trying to support is databases with frequently-rotated secrets)
data ConnDetails
  = CDDatabaseURI !ByteString
  | CDOptions !ConnOptions
  | -- | A database URI meant to be read dynamically at connection initialization
    -- time, by reading the URI from the file
    CDDynamicDatabaseURI !FilePath
  deriving stock (Eq, Read, Show)

-- | strips string, performs no validation
readDynamicURIFile :: FilePath -> IO Text
readDynamicURIFile path = do
  uriDirty <-
    readFileUtf8 path `catch` \(SomeException e) ->
      throwIO $
        PGConnErr $
          "Error reading connection string dynamically from "
            <> Text.pack path
            <> ": "
            <> Text.pack (show e)
  pure $ Text.strip uriDirty
  where
    -- Text.readFile but explicit, ignoring locale:
    readFileUtf8 = fmap decodeUtf8 . BS.readFile

-- | If we connect with a 'CDDatabaseURI', we may still be able to create a
-- 'ConnOptions' object from the URI.
extractConnOptions :: ConnDetails -> Maybe ConnOptions
extractConnOptions = \case
  CDDynamicDatabaseURI _ -> Nothing -- TODO MAYBE: to support this we'd need to be in IO
  CDOptions options -> Just options
  CDDatabaseURI uri -> do
    options <- case Options.parseConnectionString (unpack uri) of
      Right options -> Just options
      Left _ -> Nothing

    getLast do
      connHost <- Options.host options
      connPort <- Options.port options
      connUser <- Options.user options
      connPassword <- Options.password options
      connDatabase <- Options.dbname options

      let connOptions :: Maybe String
          connOptions = getLast (Options.options options)

      pure ConnOptions {..}

-- | Attempt to extract a host name from a 'ConnDetails'. Note that this cannot
-- just reuse 'extractConnOptions' as a URI may specify a host while not
-- specifying a port, for example.
--
-- NOTE: this is in @IO@ due to @CDDynamicDatabaseURI@
extractHost :: ConnDetails -> IO (Maybe String)
extractHost = \case
  CDDynamicDatabaseURI path -> parseURI . Text.unpack <$> readDynamicURIFile path
  CDOptions options -> pure $ Just (connHost options)
  CDDatabaseURI uri -> pure $ parseURI (unpack uri)
  where
    parseURI uri = getLast do
      case Options.parseConnectionString uri of
        Right options -> Options.host options
        Left _ -> mempty

data ConnInfo = ConnInfo
  { ciRetries :: !Int,
    ciDetails :: !ConnDetails
  }
  deriving stock (Eq, Read, Show)

newtype PGConnErr = PGConnErr {getConnErr :: Text}
  deriving stock (Eq, Show)
  deriving newtype (ToJSON)
  deriving anyclass (Exception)

newtype PGExecStatus = PGExecStatus PQ.ExecStatus
  deriving stock (Eq, Show)

instance ToJSON PGExecStatus where
  toJSON (PGExecStatus pqStatus) =
    $(mkToJSON (aesonDrop 0 snakeCase) ''PQ.ExecStatus) pqStatus

type PGRetryPolicyM m = RetryPolicyM m

type PGRetryPolicy = PGRetryPolicyM (ExceptT PGErrInternal IO)

newtype PGLogEvent = PLERetryMsg Value
  deriving stock (Eq, Show)

type PGLogger = PGLogEvent -> IO ()

type PGError = Either PGErrInternal PGConnErr

type PGExec a = ExceptT PGError IO a

throwPGIntErr ::
  (MonadError PGError m) => PGErrInternal -> m a
throwPGIntErr = throwError . Left

throwPGConnErr ::
  (MonadError PGError m) => PGConnErr -> m a
throwPGConnErr = throwError . Right

readConnErr :: PQ.Connection -> IO Text
readConnErr conn = do
  m <- PQ.errorMessage conn
  return $ maybe "(empty connection error message)" lenientDecodeUtf8 m

pgRetrying ::
  (MonadIO m) =>
  Maybe String ->
  IO () ->
  PGRetryPolicyM m ->
  PGLogger ->
  m (Either PGConnErr a) ->
  m a
pgRetrying host resetFn retryP logger action = do
  eRes <- Retry.retrying retryP shouldRetry $ const action
  either (liftIO . throwIO) return eRes
  where
    shouldRetry rs =
      either (const $ onError rs) (const $ return False)

    onError rs = do
      let retryIterNo = Retry.rsIterNumber rs
      liftIO $ do
        logger $
          PLERetryMsg $
            object
              [ "message" .= String "Postgres connection failed",
                "retry_attempt" .= retryIterNo,
                "host" .= fromMaybe "Unknown or invalid host" host
              ]
        resetFn
      return True

-- |
-- Establish and initialize a conn.
initPQConn ::
  ConnInfo ->
  PGLogger ->
  IO PQ.Connection
initPQConn ci logger = do
  host <- extractHost (ciDetails ci)
  -- Retry if postgres connection error occurs
  pgRetrying host resetFn retryP logger $ do
    -- Initialise the connection
    conn <- PQ.connectdb =<< (pgConnString $ ciDetails ci)

    -- Check the status of the connection
    s <- liftIO $ PQ.status conn
    let connOk = s == PQ.ConnectionOk
    bool (whenConnNotOk conn) (whenConnOk conn) connOk
  where
    resetFn = return ()
    retryP = mkPGRetryPolicy $ ciRetries ci

    whenConnNotOk conn = Left . PGConnErr <$> readConnErr conn

    whenConnOk conn = do
      -- Check the server version
      v <- PQ.serverVersion conn
      let serVerOk = v >= 80200
      bool (whenSerVerNotOk v) (whenSerVerOk conn) serVerOk

    whenSerVerNotOk v =
      throwIO $
        PGConnErr $
          "Unsupported postgres version: " <> fromString (show v)

    whenSerVerOk conn = do
      -- Set some parameters and check the response
      mRes <-
        PQ.exec conn $
          mconcat $
            [ "SET client_encoding = 'UTF8';",
              "SET client_min_messages TO WARNING;"
            ]
      case mRes of
        Just res -> do
          st <- PQ.resultStatus res
          case st of
            PQ.CommandOk -> return $ Right conn
            _ ->
              return $ Left $ PGConnErr "unexpected status after setting params"
        Nothing ->
          return $ Left $ PGConnErr "unexpected result after setting params"

defaultConnInfo :: ConnInfo
defaultConnInfo = ConnInfo 0 details
  where
    details =
      CDOptions
        ConnOptions
          { connHost = "127.0.0.1",
            connPort = 5432,
            connUser = "postgres",
            connPassword = "",
            connDatabase = "",
            connOptions = Nothing
          }

-- | NOTE: in @IO@ due to @CDDynamicDatabaseURI@. Connection string might be invalid
pgConnString :: ConnDetails -> IO ByteString
pgConnString (CDDynamicDatabaseURI path) = encodeUtf8 <$> readDynamicURIFile path
pgConnString (CDDatabaseURI uri) = pure uri
pgConnString (CDOptions opts) = pure $ fromString connstr
  where
    connstr =
      str "host=" connHost $
        num "port=" connPort $
          str "user=" connUser $
            str "password=" connPassword $
              str "dbname=" connDatabase $
                mStr "options=" connOptions []

    str name field
      | null value = id
      | otherwise = showString name . quote value . space
      where
        value = field opts

    mStr name field
      | null value = id
      | otherwise = showString name . quote value . space
      where
        value = fromMaybe "" (field opts)

    num name field
      | value <= 0 = id
      | otherwise = showString name . shows value . space
      where
        value = field opts

    quote s rest = '\'' : foldr delta ('\'' : rest) s
      where
        delta c cs = case c of
          '\\' -> '\\' : '\\' : cs
          '\'' -> '\\' : '\'' : cs
          _ -> c : cs

    space [] = []
    space xs = ' ' : xs

data PGStmtErrDetail = PGStmtErrDetail
  { edExecStatus :: !PGExecStatus,
    edStatusCode :: !(Maybe Text),
    edMessage :: !(Maybe Text),
    edDescription :: !(Maybe Text),
    edHint :: !(Maybe Text)
  }
  deriving stock (Eq, Generic, Show)

instance ToJSON PGStmtErrDetail where
  toJSON = genericToJSON $ aesonDrop 2 snakeCase

data ResultOk
  = ResultOkEmpty !PQ.Result
  | ResultOkData !PQ.Result
  deriving stock (Eq, Show)

{-# INLINE getPQRes #-}
getPQRes :: ResultOk -> PQ.Result
getPQRes (ResultOkEmpty res) = res
getPQRes (ResultOkData res) = res

{-# INLINE lenientDecodeUtf8 #-}
lenientDecodeUtf8 :: ByteString -> Text
lenientDecodeUtf8 = decodeUtf8With lenientDecode

retryOnConnErr ::
  PGConn ->
  PGExec a ->
  ExceptT PGErrInternal IO a
retryOnConnErr pgConn action = do
  host <- lift $ fmap (fmap unpack) (PQ.host (pgPQConn pgConn))

  pgRetrying host resetFn retryP logger $ do
    resE <- lift $ runExceptT action
    case resE of
      Right r -> return $ Right r
      Left (Left pgIntErr) -> throwError pgIntErr
      Left (Right pgConnErr) -> return $ Left pgConnErr
  where
    resetFn = resetPGConn pgConn
    PGConn _ _ _ _ retryP logger _ _ _ _ = pgConn

checkResult ::
  PQ.Connection ->
  Maybe PQ.Result ->
  PGExec ResultOk
checkResult conn mRes =
  case mRes of
    Nothing -> do
      -- This is a fatal error.
      msg <- liftIO $ readConnErr conn
      let whenConnOk =
            throwPGIntErr $
              PGIUnexpected $
                "Fatal error (perhaps an OOM): " <> msg
      isConnOk >>= bool (whenConnNotOk msg) whenConnOk
    Just res -> do
      st <- lift $ PQ.resultStatus res
      -- validate the result status with the given function
      case st of
        PQ.CommandOk -> return $ ResultOkEmpty res
        PQ.TuplesOk -> return $ ResultOkData res
        -- Any of these indicate error
        PQ.BadResponse -> withFullErr res st
        PQ.NonfatalError -> withFullErr res st
        PQ.FatalError -> whenFatalErr res st
        PQ.EmptyQuery -> withFullErr res st
        -- Not error, but unexpected status like copy in or copy out
        _ ->
          throwPGIntErr $
            PGIUnexpected $
              "Unexpected execStatus: " <> Text.pack (show st)
  where
    isConnOk = do
      connSt <- lift $ PQ.status conn
      return $ connSt == PQ.ConnectionOk

    whenConnNotOk msg = throwPGConnErr $ PGConnErr msg

    whenFatalErr res st = do
      msg <- liftIO $ readConnErr conn
      isConnOk >>= bool (whenConnNotOk msg) (withFullErr res st)

    errField res = lift . PQ.resultErrorField res
    withFullErr res st = do
      code <- fmap lenientDecodeUtf8 <$> errField res PQ.DiagSqlstate
      msg <- fmap lenientDecodeUtf8 <$> errField res PQ.DiagMessagePrimary
      desc <- fmap lenientDecodeUtf8 <$> errField res PQ.DiagMessageDetail
      hint <- fmap lenientDecodeUtf8 <$> errField res PQ.DiagMessageHint
      throwPGIntErr $
        PGIStatement $
          PGStmtErrDetail (PGExecStatus st) code msg desc hint

{-# INLINE assertResCmd #-}
assertResCmd ::
  PQ.Connection ->
  Maybe PQ.Result ->
  PGExec ()
assertResCmd conn mRes = do
  resOk <- checkResult conn mRes
  checkResOk resOk
  where
    checkResOk (ResultOkEmpty _) = return ()
    checkResOk (ResultOkData _) =
      throwPGIntErr $
        PGIUnexpected "cmd expected; tuples found"

newtype PGCancelErr = PGCancelErr Text
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

cancelPG :: PQ.Cancel -> IO ()
cancelPG c = do
  PQ.cancel c >>= \case
    Left err -> throwIO $ PGCancelErr $ lenientDecodeUtf8 err
    Right () -> pure ()

-- | Modify an action on a libpq connection so that asynchronous
-- exceptions cause a cancel message to be sent on the connection
-- before handling the exception as usual.
-- The intent is to unblock the FFI call via the database server.
--
-- Note that due to handling the exception as usual (instead of
-- just swallowing it and cancelling the request), we don't wait
-- for the server response. Typically, the transaction will be left
-- open, and the exception will cause the connection to be destroyed.
-- The caller can't tell whether a transaction was committed.
cancelOnAsync :: PQ.Connection -> IO a -> PGExec a
cancelOnAsync conn action = do
  lift (PQ.getCancel conn) >>= \case
    -- We can't get a cancel handle when the connection has become invalid [1].
    -- How can a connection suddenly become invalid? A connection is established
    -- when new connections are added to the pool. But the cancel handle is
    -- setup when the connection is acquired through the pool, which may be much
    -- later than when the connection is actually setup. So it may be e.g.
    -- several minutes or hours old already. So we don't setup the cancellation,
    -- because the query is going to fail anyway on this invalid connection. If
    -- we throw an error here, it becomes confusing for the user. An appropriate
    -- way to handle it would be to throw an invalid connection error here. But
    -- we don't have any details on why the connection is invalid. If we let
    -- postgres throw the error, it will have the actual error message/reason
    -- for the invalid connection. Hence, it makes sense to not setup the
    -- cancellation here and try to run the query, which will fail with proper
    -- error message from Postgres. [1]:
    -- https://hackage.haskell.org/package/postgresql-libpq-0.9.4.3/docs/Database-PostgreSQL-LibPQ.html#v:getCancel
    Nothing -> lift action
    Just c -> do
      lift (interruptOnAsyncException (cancelPG c) action)
        `catch` (\(PGCancelErr msg) -> throwPGIntErr $ PGIUnexpected $ "error cancelling query: " <> msg)

mkPGRetryPolicy ::
  (MonadIO m) =>
  -- | number of retries
  Int ->
  PGRetryPolicyM m
mkPGRetryPolicy numRetries =
  Retry.limitRetriesByDelay limitDelay $
    Retry.exponentialBackoff baseDelay <> Retry.limitRetries numRetries
  where
    -- limitDelay effectively clamps numRetries to <= 6
    limitDelay = 6 * 1000 * 1000 -- 6 seconds
    baseDelay = 100 * 1000 -- 0.1 second

data PGConn = PGConn
  { -- | Debugging context.
    pgContext :: !Value,
    pgPQConn :: !PQ.Connection,
    pgAllowPrepare :: !Bool,
    -- | Cancel command execution when interrupted by any asynchronous exception.
    --   On receiving an asynchronous exception, a cancel message is sent to
    --   the server to interrupt the blocking FFI call, then exception processing
    --   is resumed as usual (i.e., the exception isn't swallowed).
    pgCancel :: !Bool,
    pgRetryPolicy :: !PGRetryPolicy,
    pgLogger :: !PGLogger,
    pgCounter :: !(IORef Word16),
    pgTable :: !RKLookupTable,
    pgCreatedAt :: !UTCTime,
    -- | If passed, 'withExpiringPGconn' will destroy the connection when it is older than lifetime.
    pgMbLifetime :: !(Maybe NominalDiffTime)
  }

resetPGConn :: PGConn -> IO ()
resetPGConn (PGConn _ conn _ _ _ _ ctr ht _ _) = do
  -- Reset LibPQ connection
  PQ.reset conn
  -- Set counter to 0
  writeIORef ctr 0
  -- Flush all items in hash table
  keys <- map fst <$> HIO.toList ht
  for_ keys $ HIO.delete ht

type RKLookupTable = HIO.BasicHashTable LocalKey RemoteKey

-- |
-- Local statement key.
data LocalKey
  = LocalKey !Template ![Word32]
  deriving stock (Eq, Show)

{-# INLINE localKey #-}
localKey :: Template -> [PQ.Oid] -> LocalKey
localKey t ol =
  LocalKey t (map oidMapper ol)
  where
    oidMapper (PQ.Oid x) = fromIntegral x

newtype Template = Template ByteString
  deriving stock (Eq, Show)
  deriving newtype (Hashable)

{-# INLINE mkTemplate #-}
mkTemplate :: Text -> Template
mkTemplate = Template . encodeUtf8

instance Hashable LocalKey where
  hashWithSalt salt (LocalKey template _) =
    hashWithSalt salt template

-- |
-- Remote statement key.
type RemoteKey = ByteString

prepare ::
  PGConn ->
  Template ->
  [PQ.Oid] ->
  PGExec RemoteKey
prepare (PGConn _ conn _ _ _ _ counter table _ _) tpl@(Template tplBytes) tl = do
  let lk = localKey tpl tl
  rkm <- lift $ HIO.lookup table lk
  case rkm of
    -- Already prepared
    (Just rk) -> return rk
    -- Not found
    Nothing -> do
      w <- lift $ readIORef counter
      -- Create a new unique remote key
      let rk = fromString $ show w
      -- prepare the statement
      mRes <- lift $ PQ.prepare conn rk tplBytes $ Just tl
      assertResCmd conn mRes
      lift $ do
        -- Insert into table
        HIO.insert table lk rk
        -- Increment the counter
        writeIORef counter (succ w)
      return rk

type PrepArg = (PQ.Oid, Maybe (ByteString, PQ.Format))

data PGQuery a = PGQuery
  { pgqTemplate :: !Template,
    pgqArgs :: [PrepArg],
    pgqPreparable :: Bool,
    pgqConv :: ResultOk -> ExceptT Text IO a
  }

data PGErrInternal
  = PGIUnexpected !Text
  | PGIStatement !PGStmtErrDetail
  deriving stock (Eq)

instance ToJSON PGErrInternal where
  toJSON (PGIUnexpected msg) = toJSON msg
  toJSON (PGIStatement errDetail) = toJSON errDetail

execQuery ::
  PGConn ->
  PGQuery a ->
  ExceptT PGErrInternal IO a
execQuery pgConn pgQuery = do
  resOk <-
    retryOnConnErr pgConn $
      bool withoutPrepare withPrepare $
        allowPrepare && preparable
  withExceptT PGIUnexpected $ convF resOk
  where
    PGConn _ conn allowPrepare cancelable _ _ _ _ _ _ = pgConn
    PGQuery tpl@(Template tplBytes) params preparable convF = pgQuery
    run = bool lift (cancelOnAsync conn) cancelable
    withoutPrepare = do
      let prependToTuple2 a (b, c) = (a, b, c)
          params' = map (\(ty, v) -> prependToTuple2 ty <$> v) params
      mRes <- run $ PQ.execParams conn tplBytes params' PQ.Binary
      checkResult conn mRes
    withPrepare = do
      let (tl, vl) = unzip params
      rk <- prepare pgConn tpl tl
      mRes <- run $ PQ.execPrepared conn rk vl PQ.Binary
      checkResult conn mRes

execMulti ::
  PGConn ->
  Template ->
  (ResultOk -> ExceptT Text IO a) ->
  ExceptT PGErrInternal IO a
execMulti pgConn (Template t) convF = do
  resOk <- retryOnConnErr pgConn $ do
    mRes <-
      bool lift (cancelOnAsync conn) cancelable $
        PQ.exec conn t
    checkResult conn mRes
  withExceptT PGIUnexpected $ convF resOk
  where
    PGConn _ conn _ cancelable _ _ _ _ _ _ = pgConn

-- | Extract the description of a prepared statement.
describePrepared ::
  PGConn ->
  ByteString ->
  ExceptT PGErrInternal IO (PreparedDescription PQ.Oid)
describePrepared pgConn name = do
  resOk <- retryOnConnErr pgConn $ do
    mRes <-
      bool lift (cancelOnAsync (pgPQConn pgConn)) (pgCancel pgConn) $
        PQ.describePrepared (pgPQConn pgConn) name
    checkResult (pgPQConn pgConn) mRes

  let res = getPQRes resOk
  lift $ do
    numberOfParams <- PQ.nparams res
    numberOfFields <- PQ.nfields res
    PreparedDescription
      <$> traverse (PQ.paramtype res) [0 .. (numberOfParams - 1)]
      <*> traverse
        ( \i ->
            (,)
              <$> PQ.fname res i
              <*> PQ.ftype res i
        )
        [0 .. (numberOfFields - 1)]

-- | The description of a prepared statement.
--   See "PQdescribePrepared" in <https://www.postgresql.org/docs/current/libpq-exec.html> for more information.
data PreparedDescription typ = PreparedDescription
  { -- | input parameters
    pd_paramtype :: [typ],
    -- | output columns
    pd_fname_ftype :: [(Maybe ByteString, typ)]
  }
  deriving stock (Eq, Show)
