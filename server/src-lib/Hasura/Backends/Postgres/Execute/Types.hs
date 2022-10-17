-- | Postgres Execute Types
--
-- Execution context and source configuration for Postgres databases.
-- Provides support for things such as read-only transactions and read replicas.
module Hasura.Backends.Postgres.Execute.Types
  ( PGExecCtx (..),
    mkPGExecCtx,
    mkTxErrorHandler,
    defaultTxErrorHandler,
    dmlTxErrorHandler,
    resizePostgresPool,

    -- * Execution in a Postgres Source
    PGSourceConfig (..),
    runPgSourceReadTx,
    runPgSourceWriteTx,
  )
where

import Control.Lens
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Data.Aeson.Extended qualified as J
import Database.PG.Query qualified as PG
import Database.PG.Query.Connection qualified as PG
import Hasura.Backends.Postgres.SQL.Error
import Hasura.Base.Error
import Hasura.Incremental (Cacheable (..))
import Hasura.Prelude
import Hasura.SQL.Types (ExtensionsSchema)
import Hasura.Server.Types (ResizePoolStrategy (..), ServerReplicas, getServerReplicasInt)

-- See Note [Existentially Quantified Types]
type RunTx =
  forall m a. (MonadIO m, MonadBaseControl IO m) => PG.TxET QErr m a -> ExceptT QErr m a

data PGExecCtx = PGExecCtx
  { -- | Run a PG.ReadOnly transaction
    _pecRunReadOnly :: RunTx,
    -- | Run a read only statement without an explicit transaction block
    _pecRunReadNoTx :: RunTx,
    -- | Run a PG.ReadWrite transaction
    _pecRunReadWrite :: RunTx,
    -- | Destroys connection pools
    _pecDestroyConn :: (IO ()),
    -- | Resize pools based on number of server instances
    _pecResizePools :: ServerReplicas -> IO ()
  }

-- | Creates a Postgres execution context for a single Postgres master pool
mkPGExecCtx :: PG.TxIsolation -> PG.PGPool -> ResizePoolStrategy -> PGExecCtx
mkPGExecCtx isoLevel pool resizeStrategy =
  PGExecCtx
    { _pecRunReadOnly = (PG.runTx pool (isoLevel, Just PG.ReadOnly)),
      _pecRunReadNoTx = (PG.runTx' pool),
      _pecRunReadWrite = (PG.runTx pool (isoLevel, Just PG.ReadWrite)),
      _pecDestroyConn = PG.destroyPGPool pool,
      _pecResizePools =
        case resizeStrategy of
          NeverResizePool -> const $ pure ()
          ResizePool maxConnections -> resizePostgresPool pool maxConnections
    }

-- | Resize Postgres pool by setting the number of connections equal to
-- allowed maximum connections across all server instances divided by
-- number of instances
resizePostgresPool :: PG.PGPool -> Int -> ServerReplicas -> IO ()
resizePostgresPool pool maxConnections serverReplicas =
  PG.resizePGPool pool (maxConnections `div` getServerReplicasInt serverReplicas)

defaultTxErrorHandler :: PG.PGTxErr -> QErr
defaultTxErrorHandler = mkTxErrorHandler $ \case
  PGTransactionRollback _ -> True
  _ -> False

-- | Constructs a transaction error handler tailored for the needs of RQL's DML.
dmlTxErrorHandler :: PG.PGTxErr -> QErr
dmlTxErrorHandler = mkTxErrorHandler $ \case
  PGIntegrityConstraintViolation _ -> True
  PGDataException _ -> True
  PGSyntaxErrorOrAccessRuleViolation (Just (PGErrorSpecific code)) ->
    code
      `elem` [ PGUndefinedObject,
               PGInvalidColumnReference
             ]
  _ -> False

-- | Constructs a transaction error handler given a predicate that determines which errors are
-- expected and should be reported to the user. All other errors are considered internal errors.
mkTxErrorHandler :: (PGErrorType -> Bool) -> PG.PGTxErr -> QErr
mkTxErrorHandler isExpectedError txe = fromMaybe unexpectedError expectedError
  where
    unexpectedError = (internalError "database query error") {qeInternal = Just $ ExtraInternal $ J.toJSON txe}
    expectedError =
      uncurry err400 <$> do
        errorDetail <- PG.getPGStmtErr txe
        message <- PG.edMessage errorDetail
        errorType <- pgErrorType errorDetail
        guard $ isExpectedError errorType
        pure $ case errorType of
          PGIntegrityConstraintViolation code ->
            let cv = (ConstraintViolation,)
                customMessage =
                  (code ^? _Just . _PGErrorSpecific) <&> \case
                    PGRestrictViolation -> cv "Can not delete or update due to data being referred. "
                    PGNotNullViolation -> cv "Not-NULL violation. "
                    PGForeignKeyViolation -> cv "Foreign key violation. "
                    PGUniqueViolation -> cv "Uniqueness violation. "
                    PGCheckViolation -> (PermissionError, "Check constraint violation. ")
                    PGExclusionViolation -> cv "Exclusion violation. "
             in maybe (ConstraintViolation, message) (fmap (<> message)) customMessage
          PGDataException code -> case code of
            Just (PGErrorSpecific PGInvalidEscapeSequence) -> (BadRequest, message)
            _ -> (DataException, message)
          PGSyntaxErrorOrAccessRuleViolation code -> (ConstraintError,) $ case code of
            Just (PGErrorSpecific PGInvalidColumnReference) ->
              "there is no unique or exclusion constraint on target column(s)"
            _ -> message
          PGTransactionRollback code -> (ConcurrentUpdate,) $ case code of
            Just (PGErrorSpecific PGSerializationFailure) ->
              "serialization failure due to concurrent update"
            _ -> message

data PGSourceConfig = PGSourceConfig
  { _pscExecCtx :: PGExecCtx,
    _pscConnInfo :: PG.ConnInfo,
    _pscReadReplicaConnInfos :: Maybe (NonEmpty PG.ConnInfo),
    _pscPostDropHook :: IO (),
    _pscExtensionsSchema :: ExtensionsSchema
  }
  deriving (Generic)

instance Eq PGSourceConfig where
  lconf == rconf =
    (_pscConnInfo lconf, _pscReadReplicaConnInfos lconf, _pscExtensionsSchema lconf)
      == (_pscConnInfo rconf, _pscReadReplicaConnInfos rconf, _pscExtensionsSchema rconf)

instance Cacheable PGSourceConfig where
  unchanged _ = (==)

instance J.ToJSON PGSourceConfig where
  toJSON = J.toJSON . show . _pscConnInfo

runPgSourceReadTx ::
  (MonadIO m, MonadBaseControl IO m) =>
  PGSourceConfig ->
  PG.TxET QErr m a ->
  m (Either QErr a)
runPgSourceReadTx psc =
  runExceptT . _pecRunReadNoTx (_pscExecCtx psc)

runPgSourceWriteTx ::
  (MonadIO m, MonadBaseControl IO m) =>
  PGSourceConfig ->
  PG.TxET QErr m a ->
  m (Either QErr a)
runPgSourceWriteTx psc =
  runExceptT . _pecRunReadWrite (_pscExecCtx psc)
