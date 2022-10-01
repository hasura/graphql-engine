{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

{-# HLINT ignore "Use tshow" #-}
{-# HLINT ignore "Use onLeft" #-}

module Database.PG.Query.Connection
  ( initPQConn,
    defaultConnInfo,
    ConnInfo (..),
    ConnDetails (..),
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
    execCmd,
    execQuery,
    lenientDecodeUtf8,
    PGErrInternal (..),
    PGStmtErrDetail (..),
  )
where

-------------------------------------------------------------------------------

import Control.Concurrent.Interrupt (interruptOnAsyncException)
import Control.Exception.Safe (Exception, catch, throwIO)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, withExceptT)
import Control.Retry (RetryPolicyM)
import Control.Retry qualified as Retry
import Data.Aeson (ToJSON (toJSON), genericToJSON)
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.Aeson.TH (mkToJSON)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (for_)
import Data.HashTable.IO qualified as HIO
import Data.Hashable (Hashable (hashWithSalt))
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (NominalDiffTime, UTCTime)
import Data.Word (Word16, Word32)
import Database.PostgreSQL.LibPQ qualified as PQ
import GHC.Generics (Generic)
import Prelude

-------------------------------------------------------------------------------

type ConnOptions :: Type
data ConnOptions = ConnOptions
  { connHost :: !String,
    connPort :: !Int,
    connUser :: !String,
    connPassword :: !String,
    connDatabase :: !String,
    connOptions :: !(Maybe String)
  }
  deriving stock (Eq, Read, Show)

type ConnDetails :: Type
data ConnDetails
  = CDDatabaseURI !ByteString
  | CDOptions !ConnOptions
  deriving stock (Eq, Read, Show)

type ConnInfo :: Type
data ConnInfo = ConnInfo
  { ciRetries :: !Int,
    ciDetails :: !ConnDetails
  }
  deriving stock (Eq, Read, Show)

type PGConnErr :: Type
newtype PGConnErr = PGConnErr {getConnErr :: Text}
  deriving stock (Eq, Show)
  deriving newtype (ToJSON)
  deriving anyclass (Exception)

type PGExecStatus :: Type
newtype PGExecStatus = PGExecStatus PQ.ExecStatus
  deriving stock (Eq, Show)

instance ToJSON PGExecStatus where
  toJSON (PGExecStatus pqStatus) =
    $(mkToJSON (aesonDrop 0 snakeCase) ''PQ.ExecStatus) pqStatus

type PGRetryPolicyM :: (Type -> Type) -> Type
type PGRetryPolicyM m = RetryPolicyM m

type PGRetryPolicy :: Type
type PGRetryPolicy = PGRetryPolicyM (ExceptT PGErrInternal IO)

type PGLogEvent :: Type
newtype PGLogEvent = PLERetryMsg Text
  deriving stock (Eq, Show)

type PGLogger :: Type
type PGLogger = PGLogEvent -> IO ()

type PGError :: Type
type PGError = Either PGErrInternal PGConnErr

type PGExec :: Type -> Type
type PGExec a = ExceptT PGError IO a

throwPGIntErr ::
  MonadError PGError m => PGErrInternal -> m a
throwPGIntErr = throwError . Left

throwPGConnErr ::
  MonadError PGError m => PGConnErr -> m a
throwPGConnErr = throwError . Right

readConnErr :: PQ.Connection -> IO Text
readConnErr conn = do
  m <- PQ.errorMessage conn
  return $ maybe "(empty connection error message)" lenientDecodeUtf8 m

pgRetrying ::
  (MonadIO m) =>
  IO () ->
  PGRetryPolicyM m ->
  PGLogger ->
  m (Either PGConnErr a) ->
  m a
pgRetrying resetFn retryP logger action = do
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
            "postgres connection failed, retrying("
              <> Text.pack (show retryIterNo)
              <> ")."
        resetFn
      return True

-- |
-- Establish and initialize a conn.
initPQConn ::
  ConnInfo ->
  PGLogger ->
  IO PQ.Connection
initPQConn ci logger =
  -- Retry if postgres connection error occurs
  pgRetrying resetFn retryP logger $ do
    -- Initialise the connection
    conn <- PQ.connectdb (pgConnString $ ciDetails ci)

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
        PGConnErr $ "Unsupported postgres version: " <> fromString (show v)

    whenSerVerOk conn = do
      -- Set some parameters and check the response
      mRes <-
        PQ.exec conn $
          LBS.toStrict . BSB.toLazyByteString . mconcat $
            [ BSB.string7 "SET client_encoding = 'UTF8';",
              BSB.string7 "SET client_min_messages TO WARNING;"
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

pgConnString :: ConnDetails -> ByteString
pgConnString (CDDatabaseURI uri) = uri
pgConnString (CDOptions opts) = fromString connstr
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

type PGStmtErrDetail :: Type
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

type ResultOk :: Type
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
retryOnConnErr pgConn action =
  pgRetrying resetFn retryP logger $ do
    resE <- lift $ runExceptT action
    case resE of
      Right r -> return $ Right r
      Left (Left pgIntErr) -> throwError pgIntErr
      Left (Right pgConnErr) -> return $ Left pgConnErr
  where
    resetFn = resetPGConn pgConn
    PGConn _ _ _ retryP logger _ _ _ _ = pgConn

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
              PGIUnexpected $ "Fatal error (perhaps an OOM): " <> msg
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

type PGCancelErr :: Type
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
  MonadIO m =>
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

type PGConn :: Type
data PGConn = PGConn
  { pgPQConn :: !PQ.Connection,
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
resetPGConn (PGConn conn _ _ _ _ ctr ht _ _) = do
  -- Reset LibPQ connection
  PQ.reset conn
  -- Set counter to 0
  writeIORef ctr 0
  -- Flush all items in hash table
  keys <- map fst <$> HIO.toList ht
  for_ keys $ HIO.delete ht

type RKLookupTable :: Type
type RKLookupTable = HIO.BasicHashTable LocalKey RemoteKey

-- |
-- Local statement key.
type LocalKey :: Type
data LocalKey
  = LocalKey !Template ![Word32]
  deriving stock (Eq, Show)

{-# INLINE localKey #-}
localKey :: Template -> [PQ.Oid] -> LocalKey
localKey t ol =
  LocalKey t (map oidMapper ol)
  where
    oidMapper (PQ.Oid x) = fromIntegral x

type Template :: Type
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
type RemoteKey :: Type
type RemoteKey = ByteString

prepare ::
  PGConn ->
  Template ->
  [PQ.Oid] ->
  PGExec RemoteKey
prepare (PGConn conn _ _ _ _ counter table _ _) tpl@(Template tplBytes) tl = do
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

type PrepArg :: Type
type PrepArg = (PQ.Oid, Maybe (ByteString, PQ.Format))

type PGQuery :: Type -> Type
data PGQuery a = PGQuery
  { pgqTemplate :: !Template,
    pgqArgs :: [PrepArg],
    pgqPreparable :: Bool,
    pgqConv :: ResultOk -> ExceptT Text IO a
  }

type PGErrInternal :: Type
data PGErrInternal
  = PGIUnexpected !Text
  | PGIStatement !PGStmtErrDetail
  deriving stock (Eq)

instance ToJSON PGErrInternal where
  toJSON (PGIUnexpected msg) = toJSON msg
  toJSON (PGIStatement errDetail) = toJSON errDetail

{-# INLINE execQuery #-}
execQuery ::
  PGConn ->
  PGQuery a ->
  ExceptT PGErrInternal IO a
execQuery pgConn pgQuery = do
  resOk <-
    retryOnConnErr pgConn $
      bool withoutPrepare withPrepare $ allowPrepare && preparable
  withExceptT PGIUnexpected $ convF resOk
  where
    PGConn conn allowPrepare cancelable _ _ _ _ _ _ = pgConn
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

{-# INLINE execMulti #-}
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
    PGConn conn _ cancelable _ _ _ _ _ _ = pgConn

{-# INLINE execCmd #-}
execCmd ::
  PGConn ->
  Template ->
  ExceptT PGErrInternal IO ()
execCmd pgConn (Template t) =
  retryOnConnErr pgConn $ do
    mRes <-
      bool lift (cancelOnAsync conn) cancelable $
        PQ.execParams conn t [] PQ.Binary
    assertResCmd conn mRes
  where
    PGConn conn _ cancelable _ _ _ _ _ _ = pgConn
