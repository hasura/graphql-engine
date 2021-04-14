{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

-- A module for postgres execution related types and operations

module Hasura.Backends.Postgres.Connection
  ( MonadTx(..)
  , LazyTxT
  , LazyTx

  , runLazyTx
  , runQueryTx
  , withUserInfo
  , withTraceContext
  , sessionInfoJsonExp

  , RespTx
  , LazyRespTx
  , lazyTxToQTx

  , doesSchemaExist
  , doesTableExist
  , enablePgcryptoExtension
  , dropHdbCatalogSchema

  , PostgresPoolSettings(..)
  , PostgresSourceConnInfo(..)
  , PostgresConnConfiguration(..)
  , DefaultPostgresPoolSettings(..)
  , getDefaultPGPoolSettingIfNotExists
  , defaultPostgresPoolSettings
  , setPostgresPoolSettings
  , pccConnectionInfo
  , pccReadReplicas
  , psciDatabaseUrl
  , psciPoolSettings
  , psciUsePreparedStatements
  , module ET
  ) where

import           Hasura.Prelude

import qualified Database.PG.Query                      as Q
import qualified Database.PG.Query.Connection           as Q

import           Control.Lens                           (makeLenses)
import           Control.Monad.Morph                    (hoist)
import           Control.Monad.Trans.Control            (MonadBaseControl (..))
import           Control.Monad.Unique
import           Control.Monad.Validate
import           Data.Aeson
import           Data.Aeson.Extended
import           Data.Aeson.TH
import           Network.HTTP.Client.Extended           (HasHttpManagerM (..))

import qualified Hasura.Backends.Postgres.SQL.DML       as S
import qualified Hasura.Tracing                         as Tracing

import           Hasura.Backends.Postgres.Execute.Types as ET
import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.EncJSON
import           Hasura.Incremental                     (Cacheable (..))
import           Hasura.RQL.Types.Common                (UrlConf)
import           Hasura.RQL.Types.Error
import           Hasura.SQL.Types
import           Hasura.Session

class (MonadError QErr m) => MonadTx m where
  liftTx :: Q.TxE QErr a -> m a

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

-- | Like 'Q.TxE', but defers acquiring a Postgres connection until the first
-- execution of 'liftTx'.  If no call to 'liftTx' is ever reached (i.e. a
-- successful result is returned or an error is raised before ever executing a
-- query), no connection is ever acquired.
--
-- This is useful for certain code paths that only conditionally need database
-- access. For example, although most queries will eventually hit Postgres,
-- introspection queries or queries that exclusively use remote schemas never
-- will; using 'LazyTxT e m' keeps those branches from unnecessarily allocating a
-- connection.
data LazyTxT e m a
  = LTErr !e
  | LTNoTx !a
  | LTTx !(Q.TxET e m a)
  deriving (Show, Functor)

-- orphan:
instance Show (Q.TxET e m a) where
  show = const "(error \"TxE\")"

lazyTxToQTx :: (Monad m) => LazyTxT e m a -> Q.TxET e m a
lazyTxToQTx = \case
  LTErr e  -> throwError e
  LTNoTx r -> return r
  LTTx tx  -> tx

runLazyTx
  :: ( MonadIO m
     , MonadBaseControl IO m
     )
  => PGExecCtx
  -> Q.TxAccess
  -> LazyTxT QErr m a -> ExceptT QErr m a
runLazyTx pgExecCtx txAccess = \case
  LTErr e  -> throwError e
  LTNoTx a -> return a
  LTTx tx  ->
    case txAccess of
      Q.ReadOnly  -> _pecRunReadOnly pgExecCtx tx
      Q.ReadWrite -> _pecRunReadWrite pgExecCtx tx

-- | This runs the given set of statements (Tx) without wrapping them in BEGIN
-- and COMMIT. This should only be used for running a single statement query!
runQueryTx
  :: (MonadIO m, MonadError QErr m) => PGExecCtx -> LazyTx QErr a -> m a
runQueryTx pgExecCtx = \case
  LTErr e  -> throwError e
  LTNoTx a -> return a
  LTTx tx  -> liftEither =<< liftIO (runExceptT $ _pecRunReadNoTx pgExecCtx tx)

type RespTx = Q.TxE QErr EncJSON
type LazyTx e a = LazyTxT e IO a
type LazyRespTx = LazyTx QErr EncJSON

setHeadersTx :: (MonadIO m) => SessionVariables -> Q.TxET QErr m ()
setHeadersTx session = do
  Q.unitQE defaultTxErrorHandler setSess () False
  where
    setSess = Q.fromText $
      "SET LOCAL \"hasura.user\" = " <> toSQLTxt (sessionInfoJsonExp session)

sessionInfoJsonExp :: SessionVariables -> S.SQLExp
sessionInfoJsonExp = S.SELit . encodeToStrictText

withUserInfo :: (MonadIO m) => UserInfo -> LazyTxT QErr m a -> LazyTxT QErr m a
withUserInfo uInfo = \case
  LTErr e  -> LTErr e
  LTNoTx a -> LTNoTx a
  LTTx tx  ->
    let vars = _uiSession uInfo
    in LTTx $ setHeadersTx vars >> tx

-- | Inject the trace context as a transaction-local variable,
-- so that it can be picked up by any triggers (including event triggers).
withTraceContext
  :: (MonadIO m)
  => Tracing.TraceContext
  -> LazyTxT QErr m a
  -> LazyTxT QErr m a
withTraceContext ctx = \case
  LTErr e  -> LTErr e
  LTNoTx a -> LTNoTx a
  LTTx tx  ->
    let sql = Q.fromText $
          "SET LOCAL \"hasura.tracecontext\" = " <>
            toSQLTxt (S.SELit . encodeToStrictText . Tracing.injectEventContext $ ctx)
        setTraceContext =
          Q.unitQE defaultTxErrorHandler sql () False
     in LTTx $ setTraceContext >> tx

instance (Monad m) => Applicative (LazyTxT e m) where
  pure = LTNoTx

  LTErr e   <*> _        = LTErr e
  LTNoTx f  <*> r        = fmap f r
  LTTx _    <*> LTErr e  = LTErr e
  LTTx txf  <*> LTNoTx a = LTTx $ txf <*> pure a
  LTTx txf  <*> LTTx tx  = LTTx $ txf <*> tx

instance (Monad m) => Monad (LazyTxT e m) where
  LTErr e >>= _  = LTErr e
  LTNoTx a >>= f = f a
  LTTx txa >>= f =
    LTTx $ txa >>= lazyTxToQTx . f

instance (Monad m) => MonadError e (LazyTxT e m) where
  throwError = LTErr
  LTErr e  `catchError` f = f e
  LTNoTx a `catchError` _ = LTNoTx a
  LTTx txe `catchError` f =
    LTTx $ txe `catchError` (lazyTxToQTx . f)

instance MonadTrans (LazyTxT e) where
  lift = LTTx . lift

instance (Tracing.MonadTrace m) => Tracing.MonadTrace (LazyTxT e m) where
  trace t = \case
    LTTx (Q.TxET tx) -> LTTx $ Q.TxET $ Tracing.trace t tx
    v                -> v
  currentContext  = lift Tracing.currentContext
  currentReporter = lift Tracing.currentReporter
  attachMetadata  = lift . Tracing.attachMetadata

instance UserInfoM m => UserInfoM (LazyTxT e m) where
  askUserInfo = lift askUserInfo

instance HasHttpManagerM m => HasHttpManagerM (LazyTxT e m) where
  askHttpManager = lift askHttpManager

instance (MonadIO m) => MonadTx (LazyTxT QErr m) where
  liftTx = LTTx . (hoist liftIO)

instance (MonadIO m) => MonadTx (Q.TxET QErr m) where
  liftTx = hoist liftIO

instance (MonadIO m) => MonadIO (LazyTxT e m) where
  liftIO = LTTx . liftIO

instance (MonadIO m) => MonadBase IO (LazyTxT e m) where
  liftBase = liftIO

instance (MonadIO m, MonadBaseControl IO m) => MonadBaseControl IO (LazyTxT e m) where
  type StM (LazyTxT e m) a = StM (Q.TxET e m) a
  liftBaseWith f = LTTx $ liftBaseWith \run -> f (run . lazyTxToQTx)
  restoreM = LTTx . restoreM

instance (MonadIO m) => MonadUnique (LazyTxT e m) where
  newUnique = liftIO newUnique

doesSchemaExist :: MonadTx m => SchemaName -> m Bool
doesSchemaExist schemaName =
  liftTx $ (runIdentity . Q.getRow) <$> Q.withQE defaultTxErrorHandler [Q.sql|
    SELECT EXISTS
    ( SELECT 1 FROM information_schema.schemata
      WHERE schema_name = $1
    ) |] (Identity schemaName) False

doesTableExist :: MonadTx m => SchemaName -> TableName -> m Bool
doesTableExist schemaName tableName =
  liftTx $ (runIdentity . Q.getRow) <$> Q.withQE defaultTxErrorHandler [Q.sql|
    SELECT EXISTS
    ( SELECT 1 FROM pg_tables
      WHERE schemaname = $1 AND tablename = $2
    ) |] (schemaName, tableName) False

isExtensionAvailable :: MonadTx m => Text -> m Bool
isExtensionAvailable extensionName =
  liftTx $ (runIdentity . Q.getRow) <$> Q.withQE defaultTxErrorHandler [Q.sql|
    SELECT EXISTS
    ( SELECT 1 FROM pg_catalog.pg_available_extensions
      WHERE name = $1
    ) |] (Identity extensionName) False

enablePgcryptoExtension :: forall m. MonadTx m => m ()
enablePgcryptoExtension = do
  pgcryptoAvailable <- isExtensionAvailable "pgcrypto"
  if pgcryptoAvailable then createPgcryptoExtension
    else throw400 Unexpected $
      "pgcrypto extension is required, but could not find the extension in the "
      <> "PostgreSQL server. Please make sure this extension is available."
  where
    createPgcryptoExtension :: m ()
    createPgcryptoExtension =
      liftTx $ Q.unitQE needsPGCryptoError
      "CREATE EXTENSION IF NOT EXISTS pgcrypto SCHEMA public" () False
      where
        needsPGCryptoError e@(Q.PGTxErr _ _ _ err) =
          case err of
            Q.PGIUnexpected _ -> requiredError
            Q.PGIStatement pgErr -> case Q.edStatusCode pgErr of
              Just "42501" -> err500 PostgresError permissionsMessage
              _            -> requiredError
          where
            requiredError =
              (err500 PostgresError requiredMessage) { qeInternal = Just $ toJSON e }
            requiredMessage =
              "pgcrypto extension is required, but it could not be created;"
              <> " encountered unknown postgres error"
            permissionsMessage =
              "pgcrypto extension is required, but the current user doesn’t have permission to"
              <> " create it. Please grant superuser permission, or setup the initial schema via"
              <> " https://hasura.io/docs/latest/graphql/core/deployment/postgres-permissions.html"

dropHdbCatalogSchema :: (MonadTx m) => m ()
dropHdbCatalogSchema = liftTx $ Q.catchE defaultTxErrorHandler $
  -- This is where
  -- 1. Metadata storage:- Metadata and its stateful information stored
  -- 2. Postgres source:- Table event trigger related stuff & insert permission check function stored
  Q.unitQ "DROP SCHEMA IF EXISTS hdb_catalog CASCADE" () False

data PostgresPoolSettings
  = PostgresPoolSettings
  { _ppsMaxConnections :: !(Maybe Int)
  , _ppsIdleTimeout    :: !(Maybe Int)
  , _ppsRetries        :: !(Maybe Int)
  } deriving (Show, Eq, Generic)
instance Cacheable PostgresPoolSettings
instance Hashable PostgresPoolSettings
instance NFData PostgresPoolSettings
$(deriveToJSON hasuraJSON{omitNothingFields = True} ''PostgresPoolSettings)

instance FromJSON PostgresPoolSettings where
  parseJSON = withObject "Object" $ \o ->
    PostgresPoolSettings
      <$> o .:? "max_connections"
      <*> o .:? "idle_timeout"
      <*> o .:? "retries"

instance Arbitrary PostgresPoolSettings where
  arbitrary = genericArbitrary

data DefaultPostgresPoolSettings =
  DefaultPostgresPoolSettings
  { _dppsMaxConnections :: !Int
  , _dppsIdleTimeout    :: !Int
  , _dppsRetries        :: !Int
  } deriving (Show, Eq)

defaultPostgresPoolSettings :: DefaultPostgresPoolSettings
defaultPostgresPoolSettings =
  DefaultPostgresPoolSettings
  { _dppsMaxConnections = 50
  , _dppsIdleTimeout    = 180
  , _dppsRetries        = 1
  }

-- Use this when you want to set only few of the PG Pool settings.
-- The values which are not set will use the default values.
setPostgresPoolSettings :: PostgresPoolSettings
setPostgresPoolSettings =
  PostgresPoolSettings
  { _ppsMaxConnections = (Just $ _dppsMaxConnections defaultPostgresPoolSettings)
  , _ppsIdleTimeout    = (Just $ _dppsIdleTimeout defaultPostgresPoolSettings)
  , _ppsRetries        = (Just $ _dppsRetries defaultPostgresPoolSettings)
  }

-- PG Pool Settings are not given by the user, set defaults
getDefaultPGPoolSettingIfNotExists :: Maybe PostgresPoolSettings -> DefaultPostgresPoolSettings -> (Int, Int, Int)
getDefaultPGPoolSettingIfNotExists connSettings defaultPgPoolSettings =
  case connSettings of
    -- Atleast one of the postgres pool settings is set, then set default values to other settings
    Just connSettings' -> (maxConnections connSettings', idleTimeout connSettings', retries connSettings')
     -- No PG Pool settings provided by user, set default values for all
    Nothing -> (defMaxConnections, defIdleTimeout, defRetries)

  where
    defMaxConnections = _dppsMaxConnections defaultPgPoolSettings
    defIdleTimeout = _dppsIdleTimeout defaultPgPoolSettings
    defRetries = _dppsRetries defaultPgPoolSettings

    maxConnections = fromMaybe defMaxConnections . _ppsMaxConnections
    idleTimeout = fromMaybe defIdleTimeout . _ppsIdleTimeout
    retries = fromMaybe defRetries . _ppsRetries

data PostgresSourceConnInfo
  = PostgresSourceConnInfo
  { _psciDatabaseUrl           :: !UrlConf
  , _psciPoolSettings          :: !(Maybe PostgresPoolSettings)
  , _psciUsePreparedStatements :: !Bool
  } deriving (Show, Eq, Generic)
instance Cacheable PostgresSourceConnInfo
instance Hashable PostgresSourceConnInfo
instance NFData PostgresSourceConnInfo
$(deriveToJSON hasuraJSON{omitNothingFields = True} ''PostgresSourceConnInfo)
$(makeLenses ''PostgresSourceConnInfo)


instance FromJSON PostgresSourceConnInfo where
  parseJSON = withObject "Object" $ \o ->
    PostgresSourceConnInfo
      <$> o .: "database_url"
      <*> o .:? "pool_settings"
      <*> o .:? "use_prepared_statements" .!= False -- By default preparing statements is OFF for postgres source

instance Arbitrary PostgresSourceConnInfo where
  arbitrary = genericArbitrary

instance Arbitrary (NonEmpty PostgresSourceConnInfo) where
  arbitrary = genericArbitrary

data PostgresConnConfiguration
  = PostgresConnConfiguration
  { _pccConnectionInfo :: !PostgresSourceConnInfo
  , _pccReadReplicas   :: !(Maybe (NonEmpty PostgresSourceConnInfo))
  } deriving (Show, Eq, Generic)
instance Cacheable PostgresConnConfiguration
instance Hashable PostgresConnConfiguration
instance NFData PostgresConnConfiguration
$(deriveJSON hasuraJSON{omitNothingFields = True} ''PostgresConnConfiguration)
$(makeLenses ''PostgresConnConfiguration)

instance Arbitrary PostgresConnConfiguration where
  arbitrary = genericArbitrary
