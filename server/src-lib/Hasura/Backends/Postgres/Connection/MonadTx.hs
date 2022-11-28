{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Postgres Connection MonadTx
--
-- This module contains 'MonadTx' and related combinators.
--
-- 'MonadTx', a class which abstracts the 'QErr' in 'Q.TxE' via 'MonadError'.
--
-- The combinators are used for running, tracing, or otherwise perform database
-- related tasks. Please consult the individual documentation for more
-- information.
module Hasura.Backends.Postgres.Connection.MonadTx
  ( MonadTx (..),
    runTx,
    runTxWithCtx,
    runQueryTx,
    withUserInfo,
    withTraceContext,
    setHeadersTx,
    setTraceContextInTx,
    sessionInfoJsonExp,
    checkDbConnection,
    doesSchemaExist,
    doesTableExist,
    enablePgcryptoExtension,
    dropHdbCatalogSchema,
    ExtensionsSchema (..),
  )
where

import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Control.Monad.Validate
import Data.Aeson
import Data.Aeson.Extended
import Data.Time.Clock.Compat ()
import Database.PG.Query qualified as PG
import Database.PG.Query.Connection qualified as PG
import Hasura.Backends.Postgres.Execute.Types as ET
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Base.Error
import Hasura.Base.Instances ()
import Hasura.Prelude
import Hasura.SQL.Types
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Test.QuickCheck.Instances.Semigroup ()
import Test.QuickCheck.Instances.Time ()

class (MonadError QErr m) => MonadTx m where
  liftTx :: PG.TxE QErr a -> m a

instance (MonadTx m) => MonadTx (StateT s m) where
  liftTx = lift . liftTx

instance (MonadTx m) => MonadTx (ReaderT s m) where
  liftTx = lift . liftTx

instance (Monoid w, MonadTx m) => MonadTx (WriterT w m) where
  liftTx = lift . liftTx

instance (MonadTx m) => MonadTx (ValidateT e m) where
  liftTx = lift . liftTx

instance (MonadTx m) => MonadTx (Tracing.TraceT m) where
  liftTx = lift . liftTx

instance (MonadIO m) => MonadTx (PG.TxET QErr m) where
  liftTx = hoist liftIO

-- | Executes the given query in a transaction of the specified
-- mode, within the provided PGExecCtx.
runTx ::
  ( MonadIO m,
    MonadBaseControl IO m
  ) =>
  PGExecCtx ->
  PG.TxAccess ->
  PG.TxET QErr m a ->
  ExceptT QErr m a
runTx pgExecCtx = \case
  PG.ReadOnly -> _pecRunReadOnly pgExecCtx
  PG.ReadWrite -> _pecRunReadWrite pgExecCtx

runTxWithCtx ::
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadError QErr m,
    Tracing.MonadTrace m,
    UserInfoM m
  ) =>
  PGExecCtx ->
  PG.TxAccess ->
  PG.TxET QErr m a ->
  m a
runTxWithCtx pgExecCtx txAccess tx = do
  traceCtx <- Tracing.currentContext
  userInfo <- askUserInfo
  liftEitherM $
    runExceptT $
      runTx pgExecCtx txAccess $
        withTraceContext traceCtx $
          withUserInfo userInfo tx

-- | This runs the given set of statements (Tx) without wrapping them in BEGIN
-- and COMMIT. This should only be used for running a single statement query!
runQueryTx ::
  ( MonadIO m,
    MonadError QErr m
  ) =>
  PGExecCtx ->
  PG.TxET QErr IO a ->
  m a
runQueryTx pgExecCtx tx =
  liftEither =<< liftIO (runExceptT $ _pecRunReadNoTx pgExecCtx tx)

setHeadersTx :: (MonadIO m) => SessionVariables -> PG.TxET QErr m ()
setHeadersTx session = do
  PG.unitQE defaultTxErrorHandler setSess () False
  where
    setSess =
      PG.fromText $
        "SET LOCAL \"hasura.user\" = " <> toSQLTxt (sessionInfoJsonExp session)

sessionInfoJsonExp :: SessionVariables -> S.SQLExp
sessionInfoJsonExp = S.SELit . encodeToStrictText

withUserInfo :: (MonadIO m) => UserInfo -> PG.TxET QErr m a -> PG.TxET QErr m a
withUserInfo uInfo tx = setHeadersTx (_uiSession uInfo) >> tx

setTraceContextInTx :: (MonadIO m) => Tracing.TraceContext -> PG.TxET QErr m ()
setTraceContextInTx traceCtx = PG.unitQE defaultTxErrorHandler sql () False
  where
    sql =
      PG.fromText $
        "SET LOCAL \"hasura.tracecontext\" = "
          <> toSQLTxt (S.SELit . encodeToStrictText . Tracing.injectEventContext $ traceCtx)

-- | Inject the trace context as a transaction-local variable,
-- so that it can be picked up by any triggers (including event triggers).
withTraceContext ::
  (MonadIO m) =>
  Tracing.TraceContext ->
  PG.TxET QErr m a ->
  PG.TxET QErr m a
withTraceContext ctx tx = setTraceContextInTx ctx >> tx

deriving instance Tracing.MonadTrace m => Tracing.MonadTrace (PG.TxET e m)

checkDbConnection :: MonadTx m => m ()
checkDbConnection = do
  PG.Discard () <- liftTx $ PG.withQE defaultTxErrorHandler [PG.sql| SELECT 1; |] () False
  pure ()

doesSchemaExist :: MonadTx m => SchemaName -> m Bool
doesSchemaExist schemaName =
  liftTx $
    (runIdentity . PG.getRow)
      <$> PG.withQE
        defaultTxErrorHandler
        [PG.sql|
    SELECT EXISTS
    ( SELECT 1 FROM information_schema.schemata
      WHERE schema_name = $1
    ) |]
        (Identity schemaName)
        False

doesTableExist :: MonadTx m => SchemaName -> TableName -> m Bool
doesTableExist schemaName tableName =
  liftTx $
    (runIdentity . PG.getRow)
      <$> PG.withQE
        defaultTxErrorHandler
        [PG.sql|
    SELECT EXISTS
    ( SELECT 1 FROM pg_tables
      WHERE schemaname = $1 AND tablename = $2
    ) |]
        (schemaName, tableName)
        False

isExtensionAvailable :: MonadTx m => Text -> m Bool
isExtensionAvailable extensionName =
  liftTx $
    (runIdentity . PG.getRow)
      <$> PG.withQE
        defaultTxErrorHandler
        [PG.sql|
    SELECT EXISTS
    ( SELECT 1 FROM pg_catalog.pg_available_extensions
      WHERE name = $1
    ) |]
        (Identity extensionName)
        False

enablePgcryptoExtension :: forall m. MonadTx m => ExtensionsSchema -> m ()
enablePgcryptoExtension (ExtensionsSchema extensionsSchema) = do
  pgcryptoAvailable <- isExtensionAvailable "pgcrypto"
  if pgcryptoAvailable
    then createPgcryptoExtension
    else
      throw400 Unexpected $
        "pgcrypto extension is required, but could not find the extension in the "
          <> "PostgreSQL server. Please make sure this extension is available."
  where
    createPgcryptoExtension :: m ()
    createPgcryptoExtension =
      liftTx $
        PG.unitQE
          needsPGCryptoError
          (PG.fromText $ "CREATE EXTENSION IF NOT EXISTS pgcrypto SCHEMA " <> extensionsSchema)
          ()
          False
      where
        needsPGCryptoError e@(PG.PGTxErr _ _ _ err) =
          case err of
            PG.PGIUnexpected _ -> requiredError e
            PG.PGIStatement pgErr -> case PG.edStatusCode pgErr of
              Just "42501" -> err500 PostgresError permissionsMessage
              Just "P0001" -> requiredError (addHintForExtensionError pgErr)
              _ -> requiredError e
          where
            addHintForExtensionError pgErrDetail =
              e
                { PG.pgteError =
                    PG.PGIStatement $
                      PG.PGStmtErrDetail
                        { PG.edExecStatus = PG.edExecStatus pgErrDetail,
                          PG.edStatusCode = PG.edStatusCode pgErrDetail,
                          PG.edMessage =
                            liftA2
                              (<>)
                              (PG.edMessage pgErrDetail)
                              (Just ". Hint: You can set \"extensions_schema\" to provide the schema to install the extensions. Refer to the documentation here: https://hasura.io/docs/latest/deployment/postgres-requirements/#pgcrypto-in-pg-search-path"),
                          PG.edDescription = PG.edDescription pgErrDetail,
                          PG.edHint = PG.edHint pgErrDetail
                        }
                }
            requiredError pgTxErr =
              (err500 PostgresError requiredMessage) {qeInternal = Just $ ExtraInternal $ toJSON pgTxErr}
            requiredMessage =
              "pgcrypto extension is required, but it could not be created;"
                <> " encountered unknown postgres error"
            permissionsMessage =
              "pgcrypto extension is required, but the current user doesn’t have permission to"
                <> " create it. Please grant superuser permission, or setup the initial schema via"
                <> " https://hasura.io/docs/latest/graphql/core/deployment/postgres-permissions.html"

dropHdbCatalogSchema :: (MonadTx m) => m ()
dropHdbCatalogSchema =
  liftTx $
    PG.catchE defaultTxErrorHandler $
      -- This is where
      -- 1. Metadata storage:- Metadata and its stateful information stored
      -- 2. Postgres source:- Table event trigger related stuff & insert permission check function stored
      PG.unitQ "DROP SCHEMA IF EXISTS hdb_catalog CASCADE" () False
