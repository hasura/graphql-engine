-- A module for postgres execution related types

module Hasura.Backends.Postgres.Execute.Types
  ( PGExecCtx(..)
  , mkPGExecCtx
  , checkDbConnection
  , defaultTxErrorHandler
  , mkTxErrorHandler

  -- * Execution in a Postgres Source
  , PGSourceConfig(..)
  , runPgSourceReadTx
  , runPgSourceWriteTx
  ) where

import           Hasura.Prelude

import qualified Data.Aeson.Extended                as J
import qualified Database.PG.Query                  as Q
import qualified Database.PG.Query.Connection       as Q

import           Control.Lens
import           Control.Monad.Trans.Control        (MonadBaseControl (..))
import           Data.Either                        (isRight)

import           Hasura.Backends.Postgres.SQL.Error
import           Hasura.Incremental                 (Cacheable (..))
import           Hasura.RQL.Types.Error

type RunTx =
  forall m a. (MonadIO m, MonadBaseControl IO m) => Q.TxET QErr m a -> ExceptT QErr m a

data PGExecCtx
  = PGExecCtx
  { _pecRunReadOnly  :: RunTx
  -- ^ Run a Q.ReadOnly transaction
  , _pecRunReadNoTx  :: RunTx
  -- ^ Run a read only statement without an explicit transaction block
  , _pecRunReadWrite :: RunTx
  -- ^ Run a Q.ReadWrite transaction
  , _pecCheckHealth  :: (IO Bool)
  -- ^ Checks the health of this execution context
  , _pecDestroyConn  :: (IO ())
  -- ^ Destroys connection pools
  }

-- | Creates a Postgres execution context for a single Postgres master pool
mkPGExecCtx :: Q.TxIsolation -> Q.PGPool -> PGExecCtx
mkPGExecCtx isoLevel pool =
  PGExecCtx
  { _pecRunReadOnly       = (Q.runTx pool (isoLevel, Just Q.ReadOnly))
  , _pecRunReadNoTx       = (Q.runTx' pool)
  , _pecRunReadWrite      = (Q.runTx pool (isoLevel, Just Q.ReadWrite))
  , _pecCheckHealth       = checkDbConnection pool
  , _pecDestroyConn       = Q.destroyPGPool pool
  }

checkDbConnection :: MonadIO m => Q.PGPool -> m Bool
checkDbConnection pool = do
  e <- liftIO $ runExceptT $ Q.runTx' pool select1Query
  pure $ isRight e
  where
    select1Query :: Q.TxE QErr Int
    select1Query =
      runIdentity . Q.getRow <$>
      Q.withQE defaultTxErrorHandler [Q.sql| SELECT 1 |] () False

defaultTxErrorHandler :: Q.PGTxErr -> QErr
defaultTxErrorHandler = mkTxErrorHandler (const False)

-- | Constructs a transaction error handler given a predicate that determines which errors are
-- expected and should be reported to the user. All other errors are considered internal errors.
mkTxErrorHandler :: (PGErrorType -> Bool) -> Q.PGTxErr -> QErr
mkTxErrorHandler isExpectedError txe = fromMaybe unexpectedError expectedError
  where
    unexpectedError = (internalError "database query error") { qeInternal = Just $ J.toJSON txe }
    expectedError = uncurry err400 <$> do
      errorDetail <- Q.getPGStmtErr txe
      message <- Q.edMessage errorDetail
      errorType <- pgErrorType errorDetail
      guard $ isExpectedError errorType
      pure $ case errorType of
        PGIntegrityConstraintViolation code ->
          let cv = (ConstraintViolation,)
              customMessage = (code ^? _Just._PGErrorSpecific) <&> \case
                PGRestrictViolation   -> cv "Can not delete or update due to data being referred. "
                PGNotNullViolation    -> cv "Not-NULL violation. "
                PGForeignKeyViolation -> cv "Foreign key violation. "
                PGUniqueViolation     -> cv "Uniqueness violation. "
                PGCheckViolation      -> (PermissionError, "Check constraint violation. ")
                PGExclusionViolation  -> cv "Exclusion violation. "
          in maybe (ConstraintViolation, message) (fmap (<> message)) customMessage

        PGDataException code -> case code of
          Just (PGErrorSpecific PGInvalidEscapeSequence) -> (BadRequest, message)
          _                                              -> (DataException, message)

        PGSyntaxErrorOrAccessRuleViolation code -> (ConstraintError,) $ case code of
          Just (PGErrorSpecific PGInvalidColumnReference) ->
            "there is no unique or exclusion constraint on target column(s)"
          _ -> message

data PGSourceConfig
  = PGSourceConfig
  { _pscExecCtx              :: !PGExecCtx
  , _pscConnInfo             :: !Q.ConnInfo
  , _pscReadReplicaConnInfos :: !(Maybe (NonEmpty Q.ConnInfo))
  } deriving (Generic)

instance Eq PGSourceConfig where
  lconf == rconf =
    (_pscConnInfo lconf, _pscReadReplicaConnInfos lconf)
    == (_pscConnInfo rconf, _pscReadReplicaConnInfos rconf)

instance Cacheable PGSourceConfig where
  unchanged _ = (==)

instance J.ToJSON PGSourceConfig where
  toJSON = J.toJSON . show . _pscConnInfo

runPgSourceReadTx
  :: (MonadIO m, MonadBaseControl IO m)
  => PGSourceConfig -> Q.TxET QErr m a -> m (Either QErr a)
runPgSourceReadTx psc =
  runExceptT . _pecRunReadNoTx (_pscExecCtx psc)

runPgSourceWriteTx
  :: (MonadIO m, MonadBaseControl IO m)
  => PGSourceConfig -> Q.TxET QErr m a -> m (Either QErr a)
runPgSourceWriteTx psc =
  runExceptT . _pecRunReadWrite (_pscExecCtx psc)
