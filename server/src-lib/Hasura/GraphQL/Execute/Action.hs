{-# LANGUAGE QuasiQuotes #-}

module Hasura.GraphQL.Execute.Action
  ( fetchActionLogResponses,
    runActionExecution,
    asyncActionsProcessor,
    resolveActionExecution,
    resolveActionMutationAsync,
    resolveAsyncActionQuery,
    insertActionTx,
    fetchUndeliveredActionEventsTx,
    setActionStatusTx,
    fetchActionResponseTx,
    clearActionDataTx,
    setProcessingActionLogsToPendingTx,
    LockedActionIdArray (..),
    module Types,
  )
where

import Control.Concurrent.Async.Lifted.Safe qualified as LA
import Control.Concurrent.Extended (Forever (..), sleep)
import Control.Concurrent.STM qualified as STM
import Control.Exception (try)
import Control.Lens
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Lens
import Data.Aeson.Ordered qualified as AO
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.Environment qualified as Env
import Data.Has
import Data.HashMap.Strict qualified as HashMap
import Data.SerializableBlob qualified as SB
import Data.Set (Set)
import Data.Text.Extended
import Data.Text.NonEmpty
import Database.PG.Query qualified as PG
import Hasura.App.State
import Hasura.Backends.Postgres.Connection.MonadTx
import Hasura.Backends.Postgres.Execute.Prepare
import Hasura.Backends.Postgres.Execute.Types
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.SQL.Value (PGScalarValue (..))
import Hasura.Backends.Postgres.Translate.Select qualified as RS
import Hasura.Backends.Postgres.Translate.Select.Internal.Helpers (selectToSelectWithM, toQuery)
import Hasura.Backends.Postgres.Types.Function qualified as TF
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Eventing.Common
import Hasura.Function.Cache
import Hasura.GraphQL.Execute.Action.Types as Types
import Hasura.GraphQL.Parser.Name qualified as GName
import Hasura.GraphQL.Transport.HTTP.Protocol as GH
import Hasura.HTTP
import Hasura.Logging qualified as L
import Hasura.Metadata.Class
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.DDL.Headers (makeHeadersFromConf, toHeadersConf)
import Hasura.RQL.DDL.Webhook.Transform
import Hasura.RQL.IR.Action qualified as IR
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Select qualified as RS
import Hasura.RQL.IR.Value qualified as IR
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.CustomTypes
import Hasura.RQL.Types.Eventing
import Hasura.RQL.Types.Headers (HeaderConf)
import Hasura.RQL.Types.OpenTelemetry (getOtelTracesPropagator)
import Hasura.RQL.Types.Roles (adminRoleName)
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.RQL.Types.SchemaCache
import Hasura.Server.Init.Config (OptionalInterval (..), ResponseInternalErrorsConfig (..), shouldIncludeInternal)
import Hasura.Server.Prometheus (PrometheusMetrics (..))
import Hasura.Server.Utils
  ( mkClientHeadersForward,
    mkSetCookieHeaders,
  )
import Hasura.Session (SessionVariables, UserInfo, _uiRole, _uiSession)
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Client.Transformable qualified as HTTP
import Network.Wreq qualified as Wreq
import Refined (unrefine)
import System.Metrics.Prometheus.Counter as Prometheus.Counter

fetchActionLogResponses ::
  (MonadError QErr m, MonadMetadataStorage m, Foldable t) =>
  t ActionId ->
  m (ActionLogResponseMap, Bool)
fetchActionLogResponses actionIds = do
  responses <- for (toList actionIds) $ \actionId ->
    (actionId,)
      <$> liftEitherM (fetchActionResponse actionId)
  -- An action is said to be completed/processed iff response is captured from webhook or
  -- in case any exception occured in calling webhook.
  let isActionComplete ActionLogResponse {..} =
        isJust _alrResponsePayload || isJust _alrErrors
  pure (HashMap.fromList responses, all (isActionComplete . snd) responses)

runActionExecution ::
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadError QErr m,
    Tracing.MonadTrace m,
    MonadMetadataStorage m
  ) =>
  UserInfo ->
  ActionExecutionPlan ->
  m (DiffTime, (EncJSON, Maybe HTTP.ResponseHeaders))
runActionExecution userInfo aep =
  withElapsedTime $ case aep of
    AEPSync e -> second Just <$> unActionExecution e
    AEPAsyncQuery (AsyncActionQueryExecutionPlan actionId execution) -> do
      actionLogResponse <- liftEitherM $ fetchActionResponse actionId
      (,Nothing) <$> case execution of
        AAQENoRelationships f -> liftEither $ f actionLogResponse
        AAQEOnSourceDB srcConfig (AsyncActionQuerySourceExecution _ jsonAggSelect f) -> do
          let selectAST = f actionLogResponse
          selectResolved <- traverse (prepareWithoutPlan userInfo) selectAST
          selectWithQuery <- selectToSelectWithM $ RS.mkSQLSelect userInfo jsonAggSelect selectResolved
          let querySQL = toQuery selectWithQuery
          liftEitherM $ runExceptT $ _pecRunTx (_pscExecCtx srcConfig) (PGExecCtxInfo (Tx PG.ReadOnly Nothing) InternalRawQuery) $ liftTx $ asSingleRowJsonResp querySQL []
    AEPAsyncMutation actionId -> pure $ (,Nothing) $ encJFromJValue $ actionIdToText actionId

-- | This function is generally used on the result of 'selectQuerySQL',
-- 'selectAggregateQuerySQL' or 'connectionSelectSQL' to run said query and get
-- back the resulting JSON.
asSingleRowJsonResp ::
  PG.Query ->
  [PG.PrepArg] ->
  PG.TxE QErr EncJSON
asSingleRowJsonResp query args =
  runIdentity
    . PG.getRow
    <$> PG.rawQE dmlTxErrorHandler query args True

-- | Synchronously execute webhook handler and resolve response to action "output"
resolveActionExecution ::
  HTTP.Manager ->
  Env.Environment ->
  L.Logger L.Hasura ->
  Tracing.HttpPropagator ->
  PrometheusMetrics ->
  IR.AnnActionExecution Void ->
  ActionExecContext ->
  Maybe GQLQueryText ->
  ActionExecution
resolveActionExecution httpManager env logger tracesPropagator prometheusMetrics IR.AnnActionExecution {..} ActionExecContext {..} gqlQueryText =
  ActionExecution $ first (encJFromOrderedValue . makeActionResponseNoRelations _aaeFields _aaeOutputType _aaeOutputFields True) <$> runWebhook
  where
    handlerPayload = ActionWebhookPayload (ActionContext _aaeName) _aecSessionVariables _aaePayload gqlQueryText

    runWebhook ::
      (MonadIO m, MonadError QErr m, Tracing.MonadTrace m) =>
      m (ActionWebhookResponse, HTTP.ResponseHeaders)
    runWebhook =
      -- TODO: do we need to add the logger as a reader? can't we just give it as an argument?
      flip runReaderT logger
        $ callWebhook
          env
          httpManager
          tracesPropagator
          prometheusMetrics
          _aaeOutputType
          _aaeOutputFields
          _aecHeaders
          _aaeHeaders
          _aaeForwardClientHeaders
          _aaeWebhook
          handlerPayload
          _aaeTimeOut
          _aaeRequestTransform
          _aaeResponseTransform

throwUnexpected :: (MonadError QErr m) => Text -> m ()
throwUnexpected = throw400 Unexpected

-- Webhook response object should conform to action output fields
validateResponseObject :: (MonadError QErr m) => KM.KeyMap J.Value -> IR.ActionOutputFields -> m ()
validateResponseObject obj outputField = do
  -- Note: Fields not specified in the output are ignored
  void
    $ flip HashMap.traverseWithKey outputField
    $ \fieldName fieldTy ->
      -- When field is non-nullable, it has to present in the response with no null value
      unless (G.isNullable fieldTy) $ case KM.lookup (K.fromText $ G.unName fieldName) obj of
        Nothing ->
          throwUnexpected
            $ "field "
            <> fieldName
            <<> " expected in webhook response, but not found"
        Just v ->
          when (v == J.Null)
            $ throwUnexpected
            $ "expecting not null value for field "
            <>> fieldName

-- Validates the webhook response against the output type
validateResponse :: (MonadError QErr m) => J.Value -> GraphQLType -> IR.ActionOutputFields -> m ()
validateResponse webhookResponse' outputType outputF =
  unless (isCustomScalar (unGraphQLType outputType) outputF) do
    case (webhookResponse', outputType) of
      (J.Null, _) -> do
        unless (isNullableType outputType) $ throwUnexpected "got null for the action webhook response"
      (J.Number _, (GraphQLType (G.TypeNamed _ name))) -> do
        unless (name == GName._Int || name == GName._Float)
          $ throwUnexpected
          $ "got scalar String for the action webhook response, expecting "
          <> G.unName name
      (J.Bool _, (GraphQLType (G.TypeNamed _ name))) -> do
        unless (name == GName._Boolean)
          $ throwUnexpected
          $ "got scalar Boolean for the action webhook response, expecting "
          <> G.unName name
      (J.String _, (GraphQLType (G.TypeNamed _ name))) -> do
        unless (name == GName._String || name == GName._ID)
          $ throwUnexpected
          $ "got scalar String for the action webhook response, expecting "
          <> G.unName name
      (J.Array _, (GraphQLType (G.TypeNamed _ name))) -> throwUnexpected $ "got array for the action webhook response, expecting " <> G.unName name
      (J.Array objs, (GraphQLType (G.TypeList _ outputType''))) -> do
        traverse_ (\o -> validateResponse o (GraphQLType outputType'') outputF) objs
      ((J.Object obj), (GraphQLType (G.TypeNamed _ name))) -> do
        when (isInBuiltScalar (G.unName name))
          $ throwUnexpected
          $ "got object for the action webhook response, expecting "
          <> G.unName name
        validateResponseObject obj outputF
      (_, (GraphQLType (G.TypeList _ _))) ->
        throwUnexpected $ "expecting array for the action webhook response"

-- | Build action response from the Webhook JSON response when there are no relationships defined
makeActionResponseNoRelations :: IR.ActionFields -> GraphQLType -> IR.ActionOutputFields -> Bool -> ActionWebhookResponse -> AO.Value
makeActionResponseNoRelations annFields outputType outputF shouldCheckOutputField webhookResponse =
  let mkResponseObject :: IR.ActionFields -> KM.KeyMap J.Value -> AO.Value
      mkResponseObject fields obj =
        AO.object
          $ flip mapMaybe fields
          $ \(fieldName, annField) ->
            let fieldText = getFieldNameTxt fieldName
             in (fieldText,) <$> case annField of
                  IR.ACFExpression t -> Just $ AO.String t
                  IR.ACFScalar fname -> Just $ maybe AO.Null AO.toOrdered (KM.lookup (K.fromText $ G.unName fname) obj)
                  IR.ACFNestedObject _ nestedFields -> do
                    let mkValue :: J.Value -> Maybe AO.Value
                        mkValue = \case
                          J.Object o -> Just $ mkResponseObject nestedFields o
                          J.Array a -> Just $ AO.array $ mapMaybe mkValue $ toList a
                          J.Null -> Just AO.Null
                          _ -> Nothing
                    KM.lookup (K.fromText fieldText) obj >>= mkValue
      mkResponseArray :: J.Value -> AO.Value
      mkResponseArray = \case
        (J.Object o) -> mkResponseObject annFields o
        x -> AO.toOrdered x
   in -- NOTE (Sam): This case would still not allow for aliased fields to be
      -- a part of the response. Also, seeing that none of the other `annField`
      -- types would be caught in the example, I've chosen to leave it as it is.
      -- NOTE: (Pranshi) Bool here is applied to specify if we want to check ActionOutputFields
      -- (in async actions, we have object types, which need to be validated
      -- and ActionOutputField information is not present in `resolveAsyncActionQuery` -
      -- so added a boolean which will make sure that the response is validated)
      if gTypeContains isCustomScalar (unGraphQLType outputType) outputF && shouldCheckOutputField
        then AO.toOrdered $ J.toJSON webhookResponse
        else case webhookResponse of
          J.Array objs -> AO.array $ toList $ fmap mkResponseArray objs
          J.Object obj ->
            mkResponseObject annFields obj
          J.Null -> AO.Null
          _ -> AO.toOrdered $ J.toJSON webhookResponse

gTypeContains :: (G.GType -> IR.ActionOutputFields -> Bool) -> G.GType -> IR.ActionOutputFields -> Bool
gTypeContains fun gType aof = case gType of
  t@(G.TypeNamed _ _) -> fun t aof
  (G.TypeList _ expectedType) -> gTypeContains fun expectedType aof

isCustomScalar :: G.GType -> IR.ActionOutputFields -> Bool
isCustomScalar (G.TypeNamed _ name) outputF = isJust (lookup (G.unName name) pgScalarTranslations) || (HashMap.null outputF && (not (isInBuiltScalar (G.unName name))))
isCustomScalar (G.TypeList _ _) _ = False

{- Note: [Async action architecture]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In async actions, acquiring the action result is deferred. The async action mutation is made to
initiate the action which returns an UUID. The UUID is used to query/subsribe for actions response.

On mutation, the server makes an action log record in the metadata storage with request headers
and input arguments. The `asyncActionsProcessor` background thread processes the async actions by executing
the webhook handler and writing back the response payload or errors if any in the metadata storage.

When an async action query/subscription is made, the server fetches the relavent data from the
metadata storage. See Note [Resolving async action query] below.
-}

-- | Resolve asynchronous action mutation which returns only the action uuid
resolveActionMutationAsync ::
  (MonadMetadataStorage m, MonadError QErr m) =>
  IR.AnnActionMutationAsync ->
  [HTTP.Header] ->
  SessionVariables ->
  m ActionId
resolveActionMutationAsync annAction reqHeaders sessionVariables =
  liftEitherM $ insertAction actionName sessionVariables reqHeaders inputArgs
  where
    IR.AnnActionMutationAsync actionName _ inputArgs = annAction

{- Note: [Resolving async action query]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Resolving async action query happens in two steps;

1. Fetching webhook response from the metadata storage:
  In this step, using @'fetchActionResponse' method of @'MonadMetadataStorage' type class, we fetch
  the webhook response of any async action mutation (See Note [Async action architecture] for more)

2. Generating client response:
  Generation of appropriate client response happens in two ways based on the availibility of
  relationships on action output object type.

    a. With relationships:- If there are any relationships defined to tables in a source,
       we need to join the rows from the tables. So, we'll generate a SELECT SQL statement from
       a table built virtually from action webhook response fetched in the Step 1 and joining it
       with appropriate tables. Then, we execute this SQL in the source database.

         SELECT .... FROM pg_catalog.jsonb_to_recordset('action webhook response')
         JOIN .... (SELECT ... FROM <source relationship table>)

    b. Without relationships:- In this case, we'll build the response purely in Haskell code from
       the action webhook response fetched in the Step 1.
-}

-- TODO: Add tracing here? Avoided now because currently the function is pure

-- | See Note: [Resolving async action query]
resolveAsyncActionQuery ::
  UserInfo ->
  IR.AnnActionAsyncQuery ('Postgres 'Vanilla) Void ->
  ResponseInternalErrorsConfig ->
  AsyncActionQueryExecution (IR.UnpreparedValue ('Postgres 'Vanilla))
resolveAsyncActionQuery userInfo annAction responseErrorsConfig =
  case actionSource of
    IR.ASINoSource -> AAQENoRelationships \actionLogResponse -> runExcept do
      let ActionLogResponse {..} = actionLogResponse
      resolvedFields <- for asyncFields $ \(fieldName, fld) -> do
        let fieldText = getFieldNameTxt fieldName
        (fieldText,) <$> case fld of
          IR.AsyncTypename t -> pure $ AO.String t
          IR.AsyncOutput annFields ->
            fromMaybe AO.Null <$> forM
              _alrResponsePayload
              \response -> makeActionResponseNoRelations annFields outputType HashMap.empty False <$> decodeValue response
          IR.AsyncId -> pure $ AO.String $ actionIdToText actionId
          IR.AsyncCreatedAt -> pure $ AO.toOrdered $ J.toJSON _alrCreatedAt
          IR.AsyncErrors -> pure $ AO.toOrdered $ J.toJSON $ mkQErrFromErrorValue _alrErrors
      pure $ encJFromOrderedValue $ AO.object resolvedFields
    IR.ASISource sourceName sourceConfig ->
      let jsonAggSelect = mkJsonAggSelect outputType
       in AAQEOnSourceDB sourceConfig
            $ AsyncActionQuerySourceExecution sourceName jsonAggSelect
            $ \actionLogResponse ->
              let annotatedFields =
                    asyncFields <&> second \case
                      IR.AsyncTypename t -> RS.AFExpression t
                      IR.AsyncOutput annFields ->
                        RS.AFComputedField () (ComputedFieldName [nonEmptyTextQQ|__action_computed_field|])
                          $ RS.CFSTable jsonAggSelect
                          $ processOutputSelectionSet TF.AEActionResponsePayload outputType definitionList annFields stringifyNumerics
                      IR.AsyncId -> mkAnnFldFromPGCol idColumn
                      IR.AsyncCreatedAt -> mkAnnFldFromPGCol createdAtColumn
                      IR.AsyncErrors ->
                        if (shouldIncludeInternal (_uiRole userInfo) responseErrorsConfig)
                          then RS.mkAnnColumnField (fst errorsColumn) (ColumnScalar (snd errorsColumn)) NoRedaction Nothing
                          else
                            RS.mkAnnColumnField
                              (fst errorsColumn)
                              (ColumnScalar (snd errorsColumn))
                              NoRedaction
                              ( Just
                                  $ S.ColumnOp
                                    { _colOp = S.jsonbDeleteOp,
                                      _colExp = S.SELit "internal"
                                    }
                              )

                  jsonbToRecordSet = QualifiedObject "pg_catalog" $ FunctionName "jsonb_to_recordset"
                  actionLogInput =
                    IR.UVParameter IR.FreshVar
                      $ ColumnValue (ColumnScalar PGJSONB)
                      $ PGValJSONB
                      $ PG.JSONB
                      $ J.toJSON [actionLogResponse]
                  functionArgs = FunctionArgsExp [TF.AEInput actionLogInput] mempty
                  tableFromExp =
                    RS.FromFunction jsonbToRecordSet functionArgs
                      $ Just
                        [idColumn, createdAtColumn, responsePayloadColumn, errorsColumn, sessionVarsColumn]
                  tableArguments =
                    RS.noSelectArgs
                      { RS._saWhere = Just tableBoolExpression
                      }
                  tablePermissions = RS.TablePerm annBoolExpTrue Nothing
               in RS.AnnSelectG annotatedFields tableFromExp tablePermissions tableArguments stringifyNumerics Nothing
  where
    mkQErrFromErrorValue :: Maybe J.Value -> QErr
    mkQErrFromErrorValue actionLogResponseError =
      let internal = ExtraInternal <$> (actionLogResponseError >>= (^? key "internal"))
          internal' = if shouldIncludeInternal (_uiRole userInfo) responseErrorsConfig then internal else Nothing
          errorMessageText = fromMaybe "internal: error in parsing the action log" $ actionLogResponseError >>= (^? key "error" . _String)
          codeMaybe = actionLogResponseError >>= (^? key "code" . _String)
          code = maybe Unexpected ActionWebhookCode codeMaybe
       in QErr [] HTTP.status500 errorMessageText code internal'
    IR.AnnActionAsyncQuery _ actionId outputType asyncFields definitionList stringifyNumerics _ actionSource = annAction

    idColumn = (unsafePGCol "id", PGUUID)
    responsePayloadColumn = (unsafePGCol TF.actionResponsePayloadColumn, PGJSONB)
    createdAtColumn = (unsafePGCol "created_at", PGTimeStampTZ)
    errorsColumn = (unsafePGCol "errors", PGJSONB)
    sessionVarsColumn = (unsafePGCol "session_variables", PGJSONB)

    mkAnnFldFromPGCol (column', columnType) =
      RS.mkAnnColumnField column' (ColumnScalar columnType) NoRedaction Nothing

    tableBoolExpression =
      let actionIdColumnInfo =
            ColumnInfo
              { ciColumn = fst idColumn,
                ciName = Name._id,
                ciPosition = 0,
                ciType = ColumnScalar (snd idColumn),
                ciIsNullable = False,
                ciDescription = Nothing,
                ciMutability = ColumnMutability False False
              }
          actionIdColumnEq = BoolField $ AVColumn actionIdColumnInfo NoRedaction [AEQ NonNullableComparison $ IR.UVLiteral $ S.SELit $ actionIdToText actionId]
          -- TODO: avoid using ColumnInfo
          sessionVarsColumnInfo =
            ColumnInfo
              { ciColumn = unsafePGCol "session_variables",
                ciName = Name._session_variables,
                ciPosition = 0,
                ciType = ColumnScalar (snd sessionVarsColumn),
                ciIsNullable = True,
                ciDescription = Nothing,
                ciMutability = ColumnMutability False False
              }
          sessionVarValue =
            IR.UVParameter IR.FreshVar
              $ ColumnValue (ColumnScalar PGJSONB)
              $ PGValJSONB
              $ PG.JSONB
              $ J.toJSON
              $ _uiSession userInfo
          sessionVarsColumnEq = BoolField $ AVColumn sessionVarsColumnInfo NoRedaction [AEQ NonNullableComparison sessionVarValue]
       in -- For non-admin roles, accessing an async action's response should be allowed only for the user
          -- who initiated the action through mutation. The action's response is accessible for a query/subscription
          -- only when it's session variables are equal to that of action's.
          if (adminRoleName == (_uiRole userInfo))
            then actionIdColumnEq
            else BoolAnd [actionIdColumnEq, sessionVarsColumnEq]

-- | Process async actions from hdb_catalog.hdb_action_log table. This functions is executed in a background thread.
-- See Note [Async action architecture] above
asyncActionsProcessor ::
  forall m.
  ( HasAppEnv m,
    MonadIO m,
    MonadBaseControl IO m,
    LA.Forall (LA.Pure m),
    MonadMetadataStorage m,
    Tracing.MonadTrace m
  ) =>
  IO Env.Environment ->
  L.Logger L.Hasura ->
  IO SchemaCache ->
  IO OptionalInterval ->
  STM.TVar (Set LockedActionEventId) ->
  Maybe GH.GQLQueryText ->
  Int ->
  m (Forever m)
asyncActionsProcessor getEnvHook logger getSCFromRef' getFetchInterval lockedActionEvents gqlQueryText fetchBatchSize =
  return
    $ Forever ()
    $ const
    $ do
      fetchInterval <- liftIO getFetchInterval
      case fetchInterval of
        -- async actions processor thread is a polling thread, so we sleep
        -- for a second in case the fetch interval is not provided and try to
        -- get it in the next iteration. If the fetch interval is available,
        -- we check for async actions to process.
        Skip -> liftIO $ sleep $ seconds 1
        Interval sleepTime -> do
          schemaCache <- liftIO getSCFromRef'
          let actionCache = scActions schemaCache
              tracesPropagator = getOtelTracesPropagator $ scOpenTelemetryConfig schemaCache
              asyncActions =
                HashMap.filter ((== ActionMutation ActionAsynchronous) . (^. aiDefinition . adType)) actionCache
          unless (HashMap.null asyncActions) $ do
            -- fetch undelivered action events only when there's at least
            -- one async action present in the schema cache
            asyncInvocationsE <- fetchUndeliveredActionEvents fetchBatchSize
            asyncInvocations <- liftIO $ onLeft asyncInvocationsE mempty
            -- save the actions that are currently fetched from the DB to
            -- be processed in a TVar (Set LockedActionEventId) and when
            -- the action is processed we remove it from the set. This set
            -- is maintained because on shutdown of the graphql-engine, we
            -- would like to wait for a certain time (see `--graceful-shutdown-time`)
            -- during which to complete all the in-flight actions. So, when this
            -- locked action events set TVar is empty, it will mean that there are
            -- no events that are in the 'processing' state
            saveLockedEvents (map (EventId . actionIdToText . _aliId) asyncInvocations) lockedActionEvents
            LA.mapConcurrently_ (callHandler actionCache tracesPropagator) asyncInvocations
          liftIO $ sleep $ milliseconds (unrefine sleepTime)
  where
    callHandler :: ActionCache -> Tracing.HttpPropagator -> ActionLogItem -> m ()
    callHandler actionCache tracesPropagator actionLogItem =
      Tracing.newTrace Tracing.sampleAlways "async actions processor" do
        let ActionLogItem
              actionId
              actionName
              reqHeaders
              sessionVariables
              inputPayload = actionLogItem
        case HashMap.lookup actionName actionCache of
          Nothing -> return ()
          Just actionInfo -> do
            let definition = _aiDefinition actionInfo
                outputFields = IR.getActionOutputFields $ snd $ _aiOutputType actionInfo
                webhookUrl = _adHandler definition
                forwardClientHeaders = _adForwardClientHeaders definition
                confHeaders = _adHeaders definition
                timeout = _adTimeout definition
                outputType = _adOutputType definition
                actionContext = ActionContext actionName
                metadataRequestTransform = _adRequestTransform definition
                metadataResponseTransform = _adResponseTransform definition
            eitherRes <- do
              env <- liftIO getEnvHook
              AppEnv {..} <- askAppEnv
              runExceptT
                $ flip runReaderT logger
                $ callWebhook
                  env
                  appEnvManager
                  tracesPropagator
                  appEnvPrometheusMetrics
                  outputType
                  outputFields
                  reqHeaders
                  confHeaders
                  forwardClientHeaders
                  webhookUrl
                  (ActionWebhookPayload actionContext sessionVariables inputPayload gqlQueryText)
                  timeout
                  metadataRequestTransform
                  metadataResponseTransform
            resE <-
              setActionStatus actionId $ case eitherRes of
                Left e -> AASError e
                Right (responsePayload, _) -> AASCompleted $ J.toJSON responsePayload
            removeEventFromLockedEvents (EventId (actionIdToText actionId)) lockedActionEvents
            liftIO $ onLeft resE mempty

callWebhook ::
  forall m r.
  ( MonadIO m,
    MonadError QErr m,
    Tracing.MonadTrace m,
    MonadReader r m,
    Has (L.Logger L.Hasura) r
  ) =>
  Env.Environment ->
  HTTP.Manager ->
  Tracing.HttpPropagator ->
  PrometheusMetrics ->
  GraphQLType ->
  IR.ActionOutputFields ->
  [HTTP.Header] ->
  [HeaderConf] ->
  Bool ->
  EnvRecord ResolvedWebhook ->
  ActionWebhookPayload ->
  Timeout ->
  Maybe RequestTransform ->
  Maybe MetadataResponseTransform ->
  m (ActionWebhookResponse, HTTP.ResponseHeaders)
callWebhook
  env
  manager
  tracesPropagator
  prometheusMetrics
  outputType
  outputFields
  reqHeaders
  confHeaders
  forwardClientHeaders
  resolvedWebhook
  actionWebhookPayload
  timeoutSeconds
  metadataRequestTransform
  metadataResponseTransform = do
    resolvedConfHeaders <- makeHeadersFromConf env confHeaders
    let clientHeaders = if forwardClientHeaders then mkClientHeadersForward reqHeaders else mempty
        -- Using HashMap to avoid duplicate headers between configuration headers
        -- and client headers where configuration headers are preferred
        hdrs = (HashMap.toList . HashMap.fromList) (resolvedConfHeaders <> defaultHeaders <> clientHeaders)
        postPayload = J.toJSON actionWebhookPayload
        requestBody = J.encode postPayload
        requestBodySize = BL.length requestBody
        responseTimeout = HTTP.responseTimeoutMicro $ (unTimeout timeoutSeconds) * 1000000
        (EnvRecord webhookEnvName resolvedWebhookValue) = resolvedWebhook
        webhookUrl = unResolvedWebhook resolvedWebhookValue
        sessionVars = Just $ _awpSessionVariables actionWebhookPayload

    initReq <- liftIO $ HTTP.mkRequestThrow webhookUrl

    let req =
          initReq
            & set HTTP.method "POST"
            & set HTTP.headers hdrs
            & set HTTP.body (HTTP.RequestBodyLBS requestBody)
            & set HTTP.timeout responseTimeout

    (transformedReq, transformedReqSize, reqTransformCtx) <- case metadataRequestTransform of
      Nothing -> pure (Nothing, Nothing, Nothing)
      Just RequestTransform {..} ->
        let reqTransformCtx = fmap mkRequestContext $ mkReqTransformCtx webhookUrl sessionVars templateEngine
         in case applyRequestTransform reqTransformCtx requestFields req of
              Left err -> do
                -- Log The Transformation Error
                logger :: L.Logger L.Hasura <- asks getter
                L.unLoggerTracing logger $ L.UnstructuredLog L.LevelError (SB.fromLBS $ J.encode err)

                -- Throw an exception with the Transformation Error
                throw500WithDetail "Request Transformation Failed" $ J.toJSON err
              Right transformedReq ->
                let transformedPayloadSize = HTTP.getReqSize transformedReq
                 in pure (Just transformedReq, Just transformedPayloadSize, Just reqTransformCtx)

    let actualReq = fromMaybe req transformedReq
        actualSize = fromMaybe requestBodySize transformedReqSize

    httpResponse <-
      Tracing.traceHTTPRequest tracesPropagator actualReq $ \request ->
        liftIO . try $ HTTP.httpLbs request manager

    let requestInfo = ActionRequestInfo webhookEnvName postPayload (confHeaders <> toHeadersConf clientHeaders) transformedReq

    case httpResponse of
      Left e ->
        throw500WithDetail "http exception when calling webhook"
          $ J.toJSON
          $ ActionInternalError (getHttpExceptionJson (ShowErrorInfo True) $ HttpException e) requestInfo Nothing
      Right responseWreq -> do
        -- TODO(SOLOMON): Remove 'wreq'
        let responseBody = responseWreq ^. Wreq.responseBody
            responseBodySize = BL.length responseBody
            actionName = _acName $ _awpAction actionWebhookPayload
            responseStatus = responseWreq ^. Wreq.responseStatus
            mkResponseInfo respBody =
              ActionResponseInfo (HTTP.statusCode responseStatus) respBody
                $ toHeadersConf
                $ responseWreq
                ^. Wreq.responseHeaders

        transformedResponseBody <- case metadataResponseTransform of
          Nothing -> pure responseBody
          Just metadataResponseTransform' ->
            let responseTransform = mkResponseTransform metadataResponseTransform'
                engine = respTransformTemplateEngine responseTransform
                responseTransformCtx = buildRespTransformCtx (reqTransformCtx <*> Just actualReq) sessionVars engine (HTTP.responseBody responseWreq) (HTTP.statusCode responseStatus)
             in applyResponseTransform responseTransform responseTransformCtx `onLeft` \err -> do
                  -- Log The Response Transformation Error
                  logger :: L.Logger L.Hasura <- asks getter
                  L.unLoggerTracing logger $ L.UnstructuredLog L.LevelError (SB.fromLBS $ J.encode err)

                  -- Throw an exception with the Transformation Error
                  throw500WithDetail "Response Transformation Failed" $ J.toJSON err

        -- log the request and response to/from the action handler
        liftIO $ do
          Prometheus.Counter.add
            (pmActionBytesSent prometheusMetrics)
            actualSize
          Prometheus.Counter.add
            (pmActionBytesReceived prometheusMetrics)
            responseBodySize
        logger :: (L.Logger L.Hasura) <- asks getter
        L.unLoggerTracing logger $ ActionHandlerLog req transformedReq requestBodySize transformedReqSize responseBodySize actionName

        case J.eitherDecode transformedResponseBody of
          Left e -> do
            let responseInfo = mkResponseInfo $ J.String $ bsToTxt $ BL.toStrict responseBody
            throw500WithDetail "not a valid json response from webhook"
              $ J.toJSON
              $ ActionInternalError (J.toJSON $ "invalid json: " <> e) requestInfo
              $ Just responseInfo
          Right responseValue -> do
            let responseInfo = mkResponseInfo responseValue
                addInternalToErr e =
                  let actionInternalError =
                        J.toJSON
                          $ ActionInternalError (J.String "unexpected response") requestInfo
                          $ Just responseInfo
                   in e {qeInternal = Just $ ExtraInternal actionInternalError}

            if
              | HTTP.statusIsSuccessful responseStatus -> do
                  modifyQErr addInternalToErr $ do
                    webhookResponse <- decodeValue responseValue
                    validateResponse responseValue outputType outputFields
                    pure (webhookResponse, mkSetCookieHeaders responseWreq)
              | HTTP.statusIsClientError responseStatus -> do
                  ActionWebhookErrorResponse message maybeCode maybeExtensions <-
                    modifyQErr addInternalToErr $ decodeValue responseValue
                  let code = maybe Unexpected ActionWebhookCode maybeCode
                      qErr = QErr [] responseStatus message code (ExtraExtensions <$> maybeExtensions)
                  throwError qErr
              | otherwise -> do
                  let err =
                        J.toJSON
                          $ "expecting 2xx or 4xx status code, but found "
                          ++ show (HTTP.statusCode responseStatus)
                  throw500WithDetail "internal error"
                    $ J.toJSON
                    $ ActionInternalError err requestInfo
                    $ Just responseInfo

processOutputSelectionSet ::
  TF.ArgumentExp v ->
  GraphQLType ->
  [(PGCol, PGScalarType)] ->
  IR.ActionFields ->
  Options.StringifyNumbers ->
  RS.AnnSimpleSelectG ('Postgres 'Vanilla) Void v
processOutputSelectionSet tableRowInput actionOutputType definitionList actionFields strfyNum =
  RS.AnnSelectG annotatedFields selectFrom RS.noTablePermissions RS.noSelectArgs strfyNum Nothing
  where
    annotatedFields = fmap actionFieldToAnnField <$> actionFields
    jsonbToPostgresRecordFunction =
      QualifiedObject "pg_catalog"
        $ FunctionName
        $ if isListType actionOutputType
          then "jsonb_to_recordset" -- Multirow array response
          else "jsonb_to_record" -- Single object response
    functionArgs = FunctionArgsExp [tableRowInput] mempty
    selectFrom = RS.FromFunction jsonbToPostgresRecordFunction functionArgs $ Just definitionList

actionFieldToAnnField :: IR.ActionFieldG Void -> RS.AnnFieldG ('Postgres 'Vanilla) Void v
actionFieldToAnnField = \case
  IR.ACFScalar asf -> RS.mkAnnColumnField (unsafePGCol $ toTxt asf) (ColumnScalar PGJSON) NoRedaction Nothing
  IR.ACFExpression txt -> RS.AFExpression txt
  IR.ACFNestedObject fieldName _ -> RS.mkAnnColumnField (unsafePGCol $ toTxt fieldName) (ColumnScalar PGJSON) NoRedaction Nothing

mkJsonAggSelect :: GraphQLType -> JsonAggSelect
mkJsonAggSelect =
  bool JASSingleObject JASMultipleRows . isListType

insertActionTx ::
  ActionName ->
  SessionVariables ->
  [HTTP.Header] ->
  J.Value ->
  PG.TxE QErr ActionId
insertActionTx actionName sessionVariables httpHeaders inputArgsPayload =
  runIdentity
    . PG.getRow
    <$> PG.withQE
      defaultTxErrorHandler
      [PG.sql|
    INSERT INTO
        "hdb_catalog"."hdb_action_log"
        ("action_name", "session_variables", "request_headers", "input_payload", "status")
    VALUES
        ($1, $2, $3, $4, $5)
    RETURNING "id"
   |]
      ( actionName,
        PG.ViaJSON sessionVariables,
        PG.ViaJSON $ toHeadersMap httpHeaders,
        PG.ViaJSON inputArgsPayload,
        "created" :: Text
      )
      False
  where
    toHeadersMap = HashMap.fromList . map ((bsToTxt . CI.original) *** bsToTxt)

fetchUndeliveredActionEventsTx :: Int -> PG.TxE QErr [ActionLogItem]
fetchUndeliveredActionEventsTx fetchBatchSize =
  map mapEvent
    <$> PG.withQE
      defaultTxErrorHandler
      [PG.sql|
    update hdb_catalog.hdb_action_log set status = 'processing'
    where
      id in (
        select id from hdb_catalog.hdb_action_log
        where status = 'created'
        for update skip locked limit $1
      )
    returning
      id, action_name, request_headers::json, session_variables::json, input_payload::json
  |]
      (Identity batchSize)
      False
  where
    mapEvent
      ( actionId,
        actionName,
        PG.ViaJSON headersMap,
        PG.ViaJSON sessionVariables,
        PG.ViaJSON inputPayload
        ) =
        ActionLogItem actionId actionName (fromHeadersMap headersMap) sessionVariables inputPayload

    fromHeadersMap = map ((CI.mk . txtToBs) *** txtToBs) . HashMap.toList

    batchSize = fromIntegral fetchBatchSize :: Word64

setActionStatusTx :: ActionId -> AsyncActionStatus -> PG.TxE QErr ()
setActionStatusTx actionId = \case
  AASCompleted responsePayload ->
    PG.unitQE
      defaultTxErrorHandler
      [PG.sql|
      update hdb_catalog.hdb_action_log
      set response_payload = $1, status = 'completed'
      where id = $2
    |]
      (PG.ViaJSON responsePayload, actionId)
      False
  AASError qerr ->
    PG.unitQE
      defaultTxErrorHandler
      [PG.sql|
      update hdb_catalog.hdb_action_log
      set errors = $1, status = 'error'
      where id = $2
    |]
      (PG.ViaJSON qerr, actionId)
      False

fetchActionResponseTx :: ActionId -> PG.TxE QErr ActionLogResponse
fetchActionResponseTx actionId = do
  (ca, rp, errs, PG.ViaJSON sessVars) <-
    PG.getRow
      <$> PG.withQE
        defaultTxErrorHandler
        [PG.sql|
     SELECT created_at, response_payload::json, errors::json, session_variables::json
       FROM hdb_catalog.hdb_action_log
      WHERE id = $1
    |]
        (Identity actionId)
        True
  pure $ ActionLogResponse actionId ca (PG.getViaJSON <$> rp) (PG.getViaJSON <$> errs) sessVars

clearActionDataTx :: ActionName -> PG.TxE QErr ()
clearActionDataTx actionName =
  PG.unitQE
    defaultTxErrorHandler
    [PG.sql|
      DELETE FROM hdb_catalog.hdb_action_log
        WHERE action_name = $1
      |]
    (Identity actionName)
    True

setProcessingActionLogsToPendingTx :: LockedActionIdArray -> PG.TxE QErr ()
setProcessingActionLogsToPendingTx lockedActions =
  PG.unitQE
    defaultTxErrorHandler
    [PG.sql|
    UPDATE hdb_catalog.hdb_action_log
    SET status = 'created'
    WHERE status = 'processing' AND id = ANY($1::uuid[])
  |]
    (Identity lockedActions)
    False
