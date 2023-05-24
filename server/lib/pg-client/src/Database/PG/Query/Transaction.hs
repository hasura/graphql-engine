{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.PG.Query.Transaction
  ( TxIsolation (..),
    TxAccess (..),
    TxMode,
    PGTxErr (..),
    getPGStmtErr,
    TxET (..),
    TxE,
    TxT,
    withQE,
    rawQE,
    unitQE,
    multiQE,
    discardQE,
    serverVersion,
    execTx,
    Query,
    fromText,
    fromBuilder,
    getQueryText,
    describePreparedStatement,
    PreparedDescription (..),
    transformerJoinTxET,
  )
where

-------------------------------------------------------------------------------

import Control.Monad.Base (MonadBase)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Morph (MFunctor (..), MonadTrans (..))
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except (ExceptT, withExceptT)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Aeson (ToJSON (toJSON), object, (.=))
import Data.Aeson.Text (encodeToLazyText)
import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.String (IsString)
import Data.Text (Text)
import Database.PG.Query.Class
import Database.PG.Query.Connection
import Database.PostgreSQL.LibPQ qualified as PQ
import Language.Haskell.TH.Syntax (Lift)
import Text.Builder qualified as TB
import Prelude

-------------------------------------------------------------------------------

data TxIsolation
  = ReadCommitted
  | RepeatableRead
  | Serializable
  deriving stock (Eq, Lift)

instance Show TxIsolation where
  show ReadCommitted = "ISOLATION LEVEL READ COMMITTED"
  show RepeatableRead = "ISOLATION LEVEL REPEATABLE READ"
  show Serializable = "ISOLATION LEVEL SERIALIZABLE"

data TxAccess
  = ReadWrite
  | ReadOnly
  deriving stock (Eq, Lift)

instance Show TxAccess where
  show ReadWrite = "READ WRITE"
  show ReadOnly = "READ ONLY"

type TxMode = (TxIsolation, Maybe TxAccess)

newtype TxET e m a = TxET
  { txHandler :: ReaderT PGConn (ExceptT e m) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadError e,
      MonadIO,
      MonadReader PGConn,
      MonadFix
    )

transformerJoinTxET :: (Monad m) => TxET e (TxET e m) a -> TxET e m a
transformerJoinTxET x =
  TxET $ ReaderT $ \pgConn -> do
    result <- runReaderT (txHandler $ runExceptT (runReaderT (txHandler x) pgConn)) pgConn
    case result of
      Left err -> throwError err
      Right r -> pure r

{- HLINT ignore "Use onLeft" -}

instance MonadTrans (TxET e) where
  lift = TxET . lift . lift

instance MFunctor (TxET e) where
  hoist f = TxET . hoist (hoist f) . txHandler

deriving via (ReaderT PGConn (ExceptT e m)) instance (MonadBase IO m) => MonadBase IO (TxET e m)

deriving via (ReaderT PGConn (ExceptT e m)) instance (MonadBaseControl IO m) => MonadBaseControl IO (TxET e m)

type TxE e a = TxET e IO a

type TxT m a = TxET PGTxErr m a

data PGTxErr = PGTxErr
  { pgteStatement :: !Text,
    pgteArguments :: ![PrepArg],
    pgteIsPrepared :: !Bool,
    pgteError :: !PGErrInternal
  }
  -- PGCustomErr !T.Text
  deriving stock (Eq)

getPGStmtErr :: PGTxErr -> Maybe PGStmtErrDetail
getPGStmtErr (PGTxErr _ _ _ ei) = case ei of
  PGIStatement e -> return e
  PGIUnexpected _ -> Nothing

instance ToJSON PGTxErr where
  toJSON (PGTxErr stmt args isPrep qe) =
    object
      [ "statement" .= stmt,
        "arguments" .= map show args,
        "prepared" .= isPrep,
        "error" .= qe
      ]

instance Show PGTxErr where
  show = show . encodeToLazyText

execTx :: PGConn -> TxET e m a -> ExceptT e m a
execTx conn tx = runReaderT (txHandler tx) conn

newtype Query = Query
  { getQueryText :: Text
  }
  deriving stock (Eq, Show)
  deriving newtype (Hashable, IsString, ToJSON)

fromText :: Text -> Query
fromText = Query

fromBuilder :: TB.Builder -> Query
fromBuilder = Query . TB.run

withQE ::
  (MonadIO m, FromRes a, ToPrepArgs r) =>
  (PGTxErr -> e) ->
  Query ->
  r ->
  Bool ->
  TxET e m a
withQE ef q r = rawQE ef q args
  where
    args = toPrepArgs r

rawQE ::
  (MonadIO m, FromRes a) =>
  (PGTxErr -> e) ->
  Query ->
  [PrepArg] ->
  Bool ->
  TxET e m a
rawQE ef q args prep = TxET $
  ReaderT $ \pgConn ->
    withExceptT (ef . txErrF) $
      hoist liftIO $
        execQuery pgConn $
          PGQuery (mkTemplate stmt) args prep fromRes
  where
    txErrF = PGTxErr stmt args prep
    stmt = getQueryText q

multiQE ::
  (MonadIO m, FromRes a) =>
  (PGTxErr -> e) ->
  Query ->
  TxET e m a
multiQE ef q = TxET $
  ReaderT $ \pgConn ->
    withExceptT (ef . txErrF) $
      hoist liftIO $
        execMulti pgConn (mkTemplate stmt) fromRes
  where
    txErrF = PGTxErr stmt [] False
    stmt = getQueryText q

unitQE ::
  (MonadIO m, ToPrepArgs r) =>
  (PGTxErr -> e) ->
  Query ->
  r ->
  Bool ->
  TxET e m ()
unitQE = withQE

discardQE ::
  (MonadIO m, ToPrepArgs r) =>
  (PGTxErr -> e) ->
  Query ->
  r ->
  Bool ->
  TxET e m ()
discardQE ef t r p = do
  Discard () <- withQE ef t r p
  return ()

-- | Extract the description of a prepared statement.
describePreparedStatement ::
  (MonadIO m) =>
  (PGTxErr -> e) ->
  ByteString ->
  TxET e m (PreparedDescription PQ.Oid)
describePreparedStatement ef name = TxET $
  ReaderT $ \pgConn ->
    withExceptT (ef . PGTxErr mempty [] False) $
      hoist liftIO $
        describePrepared pgConn name

serverVersion ::
  (MonadIO m) => TxET e m Int
serverVersion = do
  conn <- asks pgPQConn
  liftIO $ PQ.serverVersion conn
