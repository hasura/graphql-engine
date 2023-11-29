{-# LANGUAGE QuasiQuotes #-}

-- | Postgres Execute Types
--
-- Execution context and source configuration for Postgres databases.
-- Provides support for things such as read-only transactions and read replicas.
module Hasura.Backends.Postgres.Execute.Types
  ( PGExecCtx (..),
    PGExecFrom (..),
    PGExecCtxInfo (..),
    PGExecTxType (..),
    mkPGExecCtx,
    mkTxErrorHandler,
    defaultTxErrorHandler,
    dmlTxErrorHandler,
    resizePostgresPool,
    PostgresResolvedConnectionTemplate (..),

    -- * Execution in a Postgres Source
    PGSourceConfig (..),
    ConnectionTemplateConfig (..),
    connectionTemplateConfigResolver,
    ConnectionTemplateResolver (..),
    runPgSourceReadTx,
    runPgSourceWriteTx,
    applyConnectionTemplateResolverNonAdmin,
    pgResolveConnectionTemplate,
    resolvePostgresConnectionTemplate,
    sourceConfigNumReadReplicas,
    sourceConfigConnectonTemplateEnabled,
    getPGColValues,
  )
where

import Control.Lens
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Data.Aeson.Extended qualified as J
import Data.CaseInsensitive qualified as CI
import Data.Has
import Data.HashMap.Internal.Strict qualified as Map
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Text.Extended (toTxt)
import Database.PG.Query qualified as PG
import Database.PG.Query.Class ()
import Database.PG.Query.Connection qualified as PG
import Hasura.Backends.Postgres.Connection.Settings (ConnectionTemplate (..), PostgresConnectionSetMemberName)
import Hasura.Backends.Postgres.Execute.ConnectionTemplate
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Error
import Hasura.Backends.Postgres.SQL.Types (Identifier (..), PGCol, PGScalarType, QualifiedTable, getPGColTxt)
import Hasura.Base.Error
import Hasura.EncJSON (EncJSON, encJFromJValue)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp.RemoteRelationshipPredicate
import Hasura.RQL.Types.Common (SourceName)
import Hasura.RQL.Types.ResizePool
import Hasura.RQL.Types.Roles (adminRoleName)
import Hasura.RQL.Types.Session (SessionVariables (..))
import Hasura.SQL.Types (ExtensionsSchema, toSQL)
import Hasura.Session (UserInfo (_uiRole, _uiSession), getSessionVariableValue, maybeRoleFromSessionVariables)
import Kriti.Error qualified as Kriti
import Network.HTTP.Types qualified as HTTP

-- See Note [Existentially Quantified Types]
type RunTx =
  forall m a. (MonadIO m, MonadBaseControl IO m) => PG.TxET QErr m a -> ExceptT QErr m a

data PGExecCtx = PGExecCtx
  { -- | Run a PG transaction using the information provided by PGExecCtxInfo
    _pecRunTx :: PGExecCtxInfo -> RunTx,
    -- | Destroy connection pools
    _pecDestroyConnections :: IO (),
    -- | Resize pools based on number of server instances and return the summary
    _pecResizePools :: ServerReplicas -> IO SourceResizePoolSummary
  }

-- | Holds the information required to exceute a PG transaction
data PGExecCtxInfo = PGExecCtxInfo
  { -- | The tranasction mode for executing the transaction
    _peciTxType :: PGExecTxType,
    -- | The level from where the PG transaction is being executed from
    _peciFrom :: PGExecFrom
  }

-- | The tranasction mode (isolation level, transaction access) for executing the
--   transaction
data PGExecTxType
  = -- | a transaction without an explicit tranasction block
    NoTxRead
  | -- | a transaction block with custom transaction access and isolation level.
    --  Choose defaultIsolationLevel defined in 'SourceConnConfiguration' if
    --  "Nothing" is provided for isolation level.
    Tx PG.TxAccess (Maybe PG.TxIsolation)

-- | The level from where the transaction is being run
data PGExecFrom
  = -- | transaction initated via a GraphQLRequest
    GraphQLQuery (Maybe PostgresResolvedConnectionTemplate)
  | -- | transaction initiated during run_sql
    RunSQLQuery
  | -- | custom transaction Hasura runs on the database. This is usually used in
    -- event_trigger and actions
    InternalRawQuery
  | -- | transactions initiated via other API's other than 'run_sql' in  /v1/query or
    -- /v2/query
    LegacyRQLQuery

-- | Creates a Postgres execution context for a single Postgres master pool
mkPGExecCtx :: PG.TxIsolation -> PG.PGPool -> ResizePoolStrategy -> PGExecCtx
mkPGExecCtx defaultIsoLevel pool resizeStrategy =
  PGExecCtx
    { _pecDestroyConnections =
        -- \| Destroys connection pools
        PG.destroyPGPool pool,
      _pecResizePools = \serverReplicas ->
        -- \| Resize pools based on number of server instances
        case resizeStrategy of
          NeverResizePool -> pure noPoolsResizedSummary
          ResizePool maxConnections -> resizePostgresPool' maxConnections serverReplicas,
      _pecRunTx = \case
        -- \| Run a read only statement without an explicit transaction block
        (PGExecCtxInfo NoTxRead _) -> PG.runTx' pool
        -- \| Run a transaction
        (PGExecCtxInfo (Tx txAccess (Just isolationLevel)) _) -> PG.runTx pool (isolationLevel, Just txAccess)
        (PGExecCtxInfo (Tx txAccess Nothing) _) -> PG.runTx pool (defaultIsoLevel, Just txAccess)
    }
  where
    resizePostgresPool' maxConnections serverReplicas = do
      -- Resize the pool
      resizePostgresPool pool maxConnections serverReplicas
      -- Return the summary. Only the primary pool is resized
      pure
        $ SourceResizePoolSummary
          { _srpsPrimaryResized = True,
            _srpsReadReplicasResized = False,
            _srpsConnectionSet = []
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

data ConnectionTemplateConfig
  = -- | Connection templates are disabled for Hasura CE
    ConnTemplate_NotApplicable
  | ConnTemplate_NotConfigured
  | ConnTemplate_Resolver ConnectionTemplateResolver

connectionTemplateConfigResolver :: ConnectionTemplateConfig -> Maybe ConnectionTemplateResolver
connectionTemplateConfigResolver = \case
  ConnTemplate_NotApplicable -> Nothing
  ConnTemplate_NotConfigured -> Nothing
  ConnTemplate_Resolver resolver -> Just resolver

-- | A hook to resolve connection template
newtype ConnectionTemplateResolver = ConnectionTemplateResolver
  { -- | Runs the connection template resolver.
    _runResolver ::
      forall m.
      (MonadError QErr m) =>
      SessionVariables ->
      [HTTP.Header] ->
      Maybe QueryContext ->
      m PostgresResolvedConnectionTemplate
  }

data PGSourceConfig = PGSourceConfig
  { _pscExecCtx :: PGExecCtx,
    _pscConnInfo :: PG.ConnInfo,
    _pscReadReplicaConnInfos :: Maybe (NonEmpty PG.ConnInfo),
    _pscPostDropHook :: IO (),
    _pscExtensionsSchema :: ExtensionsSchema,
    _pscConnectionSet :: HashMap PostgresConnectionSetMemberName PG.ConnInfo,
    _pscConnectionTemplateConfig :: ConnectionTemplateConfig
  }
  deriving (Generic)

instance Show PGSourceConfig where
  show _ = "(PGSourceConfig <details>)"

instance Eq PGSourceConfig where
  lconf == rconf =
    (_pscConnInfo lconf, _pscReadReplicaConnInfos lconf, _pscExtensionsSchema lconf, _pscConnectionSet lconf)
      == (_pscConnInfo rconf, _pscReadReplicaConnInfos rconf, _pscExtensionsSchema rconf, _pscConnectionSet rconf)

instance J.ToJSON PGSourceConfig where
  toJSON = J.toJSON . show . _pscConnInfo

instance Has () PGSourceConfig where
  hasLens = united

runPgSourceReadTx ::
  (MonadIO m, MonadBaseControl IO m) =>
  PGSourceConfig ->
  PG.TxET QErr m a ->
  m (Either QErr a)
runPgSourceReadTx psc = do
  let pgRunTx = _pecRunTx (_pscExecCtx psc)
  runExceptT . pgRunTx (PGExecCtxInfo NoTxRead InternalRawQuery)

runPgSourceWriteTx ::
  (MonadIO m, MonadBaseControl IO m) =>
  PGSourceConfig ->
  PGExecFrom ->
  PG.TxET QErr m a ->
  m (Either QErr a)
runPgSourceWriteTx psc pgExecFrom = do
  let pgRunTx = _pecRunTx (_pscExecCtx psc)
  runExceptT . pgRunTx (PGExecCtxInfo (Tx PG.ReadWrite Nothing) pgExecFrom)

-- | Resolve connection templates only for non-admin roles
applyConnectionTemplateResolverNonAdmin ::
  (MonadError QErr m) =>
  Maybe ConnectionTemplateResolver ->
  UserInfo ->
  [HTTP.Header] ->
  Maybe QueryContext ->
  m (Maybe PostgresResolvedConnectionTemplate)
applyConnectionTemplateResolverNonAdmin connectionTemplateResolver userInfo requestHeaders queryContext =
  if _uiRole userInfo == adminRoleName
    then pure Nothing
    else applyConnectionTemplateResolver connectionTemplateResolver (_uiSession userInfo) requestHeaders queryContext

-- | Execute @'ConnectionTemplateResolver' with required parameters
applyConnectionTemplateResolver ::
  (MonadError QErr m) =>
  Maybe ConnectionTemplateResolver ->
  SessionVariables ->
  [HTTP.Header] ->
  Maybe QueryContext ->
  m (Maybe PostgresResolvedConnectionTemplate)
applyConnectionTemplateResolver connectionTemplateResolver sessionVariables requestHeaders queryContext =
  for connectionTemplateResolver $ \resolver ->
    _runResolver resolver sessionVariables requestHeaders queryContext

pgResolveConnectionTemplate :: (MonadError QErr m) => PGSourceConfig -> RequestContext -> Maybe ConnectionTemplate -> m EncJSON
pgResolveConnectionTemplate sourceConfig (RequestContext (RequestContextHeaders headersMap) sessionVariables queryContext) connectionTemplateMaybe = do
  connectionTemplateResolver <-
    case connectionTemplateMaybe of
      Nothing ->
        case _pscConnectionTemplateConfig sourceConfig of
          ConnTemplate_NotApplicable -> connectionTemplateNotApplicableError
          ConnTemplate_NotConfigured ->
            throw400 TemplateResolutionFailed "Connection template not defined for the source"
          ConnTemplate_Resolver resolver ->
            pure resolver
      Just connectionTemplate ->
        case _pscConnectionTemplateConfig sourceConfig of
          -- connection template is an enterprise edition only feature. `ConnTemplate_NotApplicable` error is thrown
          -- when community edition engine is used to test the connection template
          ConnTemplate_NotApplicable -> connectionTemplateNotApplicableError
          _ -> pure $ ConnectionTemplateResolver $ \sessionVariables' reqHeaders queryContext' ->
            resolvePostgresConnectionTemplate connectionTemplate (Map.keys (_pscConnectionSet sourceConfig)) sessionVariables' reqHeaders queryContext'
  let headers = map (\(hName, hVal) -> (CI.mk (txtToBs hName), txtToBs hVal)) $ Map.toList headersMap
  case maybeRoleFromSessionVariables sessionVariables of
    Nothing -> throw400 InvalidParams "No `x-hasura-role` found in session variables. Please try again with non-admin 'x-hasura-role' in the session context."
    Just roleName ->
      when (roleName == adminRoleName) $ throw400 InvalidParams "Only requests made with a non-admin context can resolve the connection template. Please try again with non-admin 'x-hasura-role' in the session context."
  resolvedTemplate <- _runResolver connectionTemplateResolver sessionVariables headers queryContext
  pure . encJFromJValue $ J.object ["result" J..= resolvedTemplate]
  where
    connectionTemplateNotApplicableError = throw400 NotSupported "Connection templating feature is enterprise edition only"

resolvePostgresConnectionTemplate ::
  (MonadError QErr m) =>
  ConnectionTemplate ->
  [PostgresConnectionSetMemberName] ->
  SessionVariables ->
  [HTTP.Header] ->
  Maybe QueryContext ->
  m (PostgresResolvedConnectionTemplate)
resolvePostgresConnectionTemplate (ConnectionTemplate _templateSrc connectionTemplate) connectionSetMembers sessionVariables reqHeaders queryContext = do
  let requestContext = makeRequestContext queryContext reqHeaders sessionVariables
      connectionTemplateCtx = makeConnectionTemplateContext requestContext connectionSetMembers

  case runKritiEval connectionTemplateCtx connectionTemplate of
    Left err ->
      let serializedErr = Kriti.serialize err
       in throw400WithDetail TemplateResolutionFailed ("Connection template evaluation failed: " <> Kriti._message serializedErr) (J.toJSON $ serializedErr)
    Right val -> runAesonParser (J.parseJSON @PostgresResolvedConnectionTemplate) val

sourceConfigNumReadReplicas :: PGSourceConfig -> Int
sourceConfigNumReadReplicas =
  maybe 0 List.NonEmpty.length . _pscReadReplicaConnInfos

sourceConfigConnectonTemplateEnabled :: PGSourceConfig -> Bool
sourceConfigConnectonTemplateEnabled pgSourceConfig =
  case _pscConnectionTemplateConfig pgSourceConfig of
    ConnTemplate_NotApplicable -> False
    ConnTemplate_NotConfigured -> False
    ConnTemplate_Resolver _ -> True

getPGColValues ::
  (MonadIO m, MonadError QErr m) =>
  SessionVariables ->
  SourceName ->
  PGSourceConfig ->
  QualifiedTable ->
  (PGScalarType, PGCol) ->
  (PGCol, [RemoteRelSupportedOp RemoteRelSessionVariableORLiteralValue]) ->
  m [Text]
getPGColValues sessionVariables _sourceName sourceConfig table col (field, val) = do
  queryTx <- mkGetPGColValuesQuery sessionVariables table col field val
  res <- liftIO $ runPgSourceReadTx sourceConfig queryTx
  onLeft ((fmap runIdentity) <$> res) throwError

mkGetPGColValuesQuery :: (PG.FromCol a, MonadError QErr m) => SessionVariables -> QualifiedTable -> (PGScalarType, PGCol) -> PGCol -> [RemoteRelSupportedOp RemoteRelSessionVariableORLiteralValue] -> m (PG.TxET QErr IO [(Identity a)])
mkGetPGColValuesQuery sessionVariables table (_colType, col) fieldName boolExps = do
  let columnName = getPGColTxt col
  boolExp <- traverse getBoolExp boolExps
  let whereExpFrag = S.WhereFrag $ S.BEBin S.AndOp (S.BELit True) (foldl' (S.BEBin S.AndOp) (S.BELit True) boolExp)
  pure
    $ PG.withQE
      defaultTxErrorHandler
      ( PG.fromBuilder
          ( toSQL
              ( S.mkSelect
                  { S.selExtr = [S.Extractor (S.SETyAnn (S.SEIdentifier (Identifier columnName)) (S.textTypeAnn)) Nothing],
                    S.selFrom = Just $ S.mkSimpleFromExp table,
                    S.selWhere = Just whereExpFrag
                  }
              )
          )
      )
      ()
      True
  where
    getSessionVariableValueM :: (MonadError QErr m) => RemoteRelSessionVariableORLiteralValue -> m Text
    getSessionVariableValueM (RemoteRelSessionVariable sessionVariable) = onNothing (getSessionVariableValue sessionVariable sessionVariables) (throw400 NotFound $ "Session variable " <> toTxt sessionVariable <> " not found")
    getSessionVariableValueM (RemoteRelLiteralValue value) = pure value

    getBoolExp :: (MonadError QErr m) => RemoteRelSupportedOp RemoteRelSessionVariableORLiteralValue -> m S.BoolExp
    getBoolExp = \case
      RemoteRelEqOp sessionVariableOrLiteralValue -> do
        actualValue <- getSessionVariableValueM sessionVariableOrLiteralValue
        pure $ S.BECompare S.SEQ (S.SEIdentifier (Identifier fieldNameText)) (S.SELit actualValue)
      RemoteRelNeqOp sessionVariableOrLiteralValue -> do
        actualValue <- getSessionVariableValueM sessionVariableOrLiteralValue
        pure $ S.BECompare S.SNE (S.SEIdentifier (Identifier fieldNameText)) (S.SELit actualValue)
      RemoteRelGtOp sessionVariableOrLiteralValue -> do
        actualValue <- getSessionVariableValueM sessionVariableOrLiteralValue
        pure $ S.BECompare S.SGT (S.SEIdentifier (Identifier fieldNameText)) (S.SELit actualValue)
      RemoteRelLtOp sessionVariableOrLiteralValue -> do
        actualValue <- getSessionVariableValueM sessionVariableOrLiteralValue
        pure $ S.BECompare S.SLT (S.SEIdentifier (Identifier fieldNameText)) (S.SELit actualValue)
      RemoteRelGteOp sessionVariableOrLiteralValue -> do
        actualValue <- getSessionVariableValueM sessionVariableOrLiteralValue
        pure $ S.BECompare S.SGTE (S.SEIdentifier (Identifier fieldNameText)) (S.SELit actualValue)
      RemoteRelLteOp sessionVariableOrLiteralValue -> do
        actualValue <- getSessionVariableValueM sessionVariableOrLiteralValue
        pure $ S.BECompare S.SLTE (S.SEIdentifier (Identifier fieldNameText)) (S.SELit actualValue)
      RemoteRelInOp lst -> do
        actualValues <- traverse getSessionVariableValueM lst
        pure $ S.BEIN (S.SEIdentifier (Identifier fieldNameText)) (map S.SELit actualValues)
      RemoteRelNinOp lst -> do
        actualValues <- traverse getSessionVariableValueM lst
        pure $ S.BENot $ S.BEIN (S.SEIdentifier (Identifier fieldNameText)) (map S.SELit actualValues)
      RemoteRelLikeOp sessionVariableOrLiteralValue -> do
        actualValue <- getSessionVariableValueM sessionVariableOrLiteralValue
        pure $ S.BECompare S.SLIKE (S.SEIdentifier (Identifier fieldNameText)) (S.SELit actualValue)
      RemoteRelNlikeOp sessionVariableOrLiteralValue -> do
        actualValue <- getSessionVariableValueM sessionVariableOrLiteralValue
        pure $ S.BECompare S.SNLIKE (S.SEIdentifier (Identifier fieldNameText)) (S.SELit actualValue)
      RemoteRelIsNullOp True -> pure $ S.BENull (S.SEIdentifier (Identifier fieldNameText))
      RemoteRelIsNullOp False -> pure $ S.BENotNull (S.SEIdentifier (Identifier fieldNameText))
      where
        fieldNameText = getPGColTxt fieldName
