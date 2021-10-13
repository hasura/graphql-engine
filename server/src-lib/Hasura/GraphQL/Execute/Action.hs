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
import Data.Aeson.Ordered qualified as AO
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.Environment qualified as Env
import Data.Has
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.IntMap qualified as IntMap
import Data.Set (Set)
import Data.TByteString qualified as TBS
import Data.Text.Extended
import Data.Text.NonEmpty
import Database.PG.Query qualified as Q
import Hasura.Backends.Postgres.Execute.Prepare
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.SQL.Value (PGScalarValue (..))
import Hasura.Backends.Postgres.Translate.Column (toTxtValue)
import Hasura.Backends.Postgres.Translate.Select (asSingleRowJsonResp)
import Hasura.Backends.Postgres.Translate.Select qualified as RS
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Eventing.Common
import Hasura.GraphQL.Execute.Action.Types as Types
import Hasura.GraphQL.Parser
import Hasura.GraphQL.Transport.HTTP.Protocol as GH
import Hasura.HTTP
import Hasura.Logging qualified as L
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.RQL.DDL.Headers
import Hasura.RQL.DDL.RequestTransform
import Hasura.RQL.DDL.Schema.Cache
import Hasura.RQL.IR.Select qualified as RS
import Hasura.RQL.Types
import Hasura.SQL.Types
import Hasura.Server.Utils
  ( mkClientHeadersForward,
    mkSetCookieHeaders,
  )
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Client.Transformable qualified as HTTP
import Network.Wreq qualified as Wreq

fetchActionLogResponses ::
  (MonadError QErr m, MonadMetadataStorage (MetadataStorageT m), Foldable t) =>
  t ActionId ->
  m (ActionLogResponseMap, Bool)
fetchActionLogResponses actionIds = do
  responses <- for (toList actionIds) $ \actionId ->
    (actionId,)
      <$> liftEitherM (runMetadataStorageT $ fetchActionResponse actionId)
  -- An action is said to be completed/processed iff response is captured from webhook or
  -- in case any exception occured in calling webhook.
  let isActionComplete ActionLogResponse {..} =
        isJust _alrResponsePayload || isJust _alrErrors
  pure (Map.fromList responses, all (isActionComplete . snd) responses)

runActionExecution ::
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadError QErr m,
    Tracing.MonadTrace m,
    MonadMetadataStorage (MetadataStorageT m)
  ) =>
  UserInfo ->
  ActionExecutionPlan ->
  m (DiffTime, (EncJSON, Maybe HTTP.ResponseHeaders))
runActionExecution userInfo aep =
  withElapsedTime $ case aep of
    AEPSync e -> second Just <$> unActionExecution e
    AEPAsyncQuery (AsyncActionQueryExecutionPlan actionId execution) -> do
      actionLogResponse <- liftEitherM $ runMetadataStorageT $ fetchActionResponse actionId
      (,Nothing) <$> case execution of
        AAQENoRelationships f -> liftEither $ f actionLogResponse
        AAQEOnSourceDB srcConfig (AsyncActionQuerySourceExecution _ jsonAggSelect f) -> do
          let selectAST = f actionLogResponse
          selectResolved <- traverse (prepareWithoutPlan userInfo) selectAST
          let querySQL = Q.fromBuilder $ toSQL $ RS.mkSQLSelect jsonAggSelect selectResolved
          liftEitherM $ runExceptT $ runTx (_pscExecCtx srcConfig) Q.ReadOnly $ liftTx $ asSingleRowJsonResp querySQL []
    AEPAsyncMutation actionId -> pure $ (,Nothing) $ encJFromJValue $ actionIdToText actionId

-- | Synchronously execute webhook handler and resolve response to action "output"
resolveActionExecution ::
  Env.Environment ->
  L.Logger L.Hasura ->
  UserInfo ->
  AnnActionExecution ('Postgres 'Vanilla) (Const Void) (UnpreparedValue ('Postgres 'Vanilla)) ->
  ActionExecContext ->
  Maybe GQLQueryText ->
  ActionExecution
resolveActionExecution env logger userInfo annAction execContext gqlQueryText =
  case actionSource of
    -- Build client response
    ASINoSource -> ActionExecution $ first (encJFromOrderedValue . makeActionResponseNoRelations annFields) <$> runWebhook
    ASISource _ sourceConfig -> ActionExecution do
      (webhookRes, respHeaders) <- runWebhook
      let webhookResponseExpression =
            RS.AEInput $
              UVLiteral $
                toTxtValue $ ColumnValue (ColumnScalar PGJSONB) $ PGValJSONB $ Q.JSONB $ J.toJSON webhookRes
          selectAstUnresolved =
            processOutputSelectionSet
              webhookResponseExpression
              outputType
              definitionList
              annFields
              stringifyNum
      (astResolved, finalPlanningSt) <- flip runStateT initPlanningSt $ traverse (prepareWithPlan userInfo) selectAstUnresolved
      let prepArgs = fmap fst $ IntMap.elems $ withUserVars (_uiSession userInfo) $ _psPrepped finalPlanningSt
      (,respHeaders) <$> executeActionInDb sourceConfig astResolved prepArgs
  where
    AnnActionExecution
      actionName
      outputType
      annFields
      inputPayload
      outputFields
      definitionList
      resolvedWebhook
      confHeaders
      forwardClientHeaders
      stringifyNum
      timeout
      actionSource
      metadataTransform = annAction
    ActionExecContext manager reqHeaders sessionVariables = execContext
    actionContext = ActionContext actionName
    handlerPayload = ActionWebhookPayload actionContext sessionVariables inputPayload gqlQueryText

    executeActionInDb ::
      (MonadError QErr m, MonadIO m, MonadBaseControl IO m) =>
      SourceConfig ('Postgres 'Vanilla) ->
      RS.AnnSimpleSelect ('Postgres 'Vanilla) ->
      [Q.PrepArg] ->
      m EncJSON
    executeActionInDb sourceConfig astResolved prepArgs = do
      let jsonAggType = mkJsonAggSelect outputType
      liftEitherM $
        runExceptT $
          runTx (_pscExecCtx sourceConfig) Q.ReadOnly $
            liftTx $ asSingleRowJsonResp (Q.fromBuilder $ toSQL $ RS.mkSQLSelect jsonAggType astResolved) prepArgs

    runWebhook ::
      (MonadIO m, MonadError QErr m, Tracing.MonadTrace m) =>
      m (ActionWebhookResponse, HTTP.ResponseHeaders)
    runWebhook =
      flip runReaderT logger $
        callWebhook
          env
          manager
          outputType
          outputFields
          reqHeaders
          confHeaders
          forwardClientHeaders
          resolvedWebhook
          handlerPayload
          timeout
          metadataTransform

-- | Build action response from the Webhook JSON response when there are no relationships defined
makeActionResponseNoRelations :: RS.AnnFieldsG ('Postgres 'Vanilla) r v -> ActionWebhookResponse -> AO.Value
makeActionResponseNoRelations annFields webhookResponse =
  let mkResponseObject obj =
        AO.object $
          flip mapMaybe annFields $ \(fieldName, annField) ->
            let fieldText = getFieldNameTxt fieldName
             in (fieldText,) <$> case annField of
                  RS.AFExpression t -> Just $ AO.String t
                  RS.AFColumn c -> AO.toOrdered <$> Map.lookup (G.unsafeMkName . getPGColTxt $ RS._acfColumn c) obj
                  -- NOTE (Phil): Coercing the name like this using unsafeMkName (instead
                  -- of using something like pgiName is safe (here) because of how we
                  -- construct ColumnInfo objects in mkPGColumnInfo. Really, we should get
                  -- rid of ColumnInfo altogether, but right now action responses are
                  -- represented as if they were rows in some temp PG table, which is why
                  -- they appear here.
                  _ -> AO.toOrdered <$> Map.lookup fieldText (mapKeys G.unName obj)
   in -- NOTE (Sam): This case would still not allow for aliased fields to be
      -- a part of the response. Also, seeing that none of the other `annField`
      -- types would be caught in the example, I've chosen to leave it as it is.
      case webhookResponse of
        AWRArray objs -> AO.array $ map mkResponseObject objs
        AWRObject obj -> mkResponseObject obj
        AWRNull -> AO.Null

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
  (MonadMetadataStorage m) =>
  AnnActionMutationAsync ->
  [HTTP.Header] ->
  SessionVariables ->
  m ActionId
resolveActionMutationAsync annAction reqHeaders sessionVariables =
  insertAction actionName sessionVariables reqHeaders inputArgs
  where
    AnnActionMutationAsync actionName _ inputArgs = annAction

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
  AnnActionAsyncQuery ('Postgres 'Vanilla) (Const Void) (UnpreparedValue ('Postgres 'Vanilla)) ->
  AsyncActionQueryExecution (UnpreparedValue ('Postgres 'Vanilla))
resolveAsyncActionQuery userInfo annAction =
  case actionSource of
    ASINoSource -> AAQENoRelationships \actionLogResponse -> runExcept do
      let ActionLogResponse {..} = actionLogResponse
      resolvedFields <- for asyncFields $ \(fieldName, fld) -> do
        let fieldText = getFieldNameTxt fieldName
        (fieldText,) <$> case fld of
          AsyncTypename t -> pure $ AO.String t
          AsyncOutput annFields ->
            fromMaybe AO.Null <$> forM
              _alrResponsePayload
              \response -> makeActionResponseNoRelations annFields <$> decodeValue response
          AsyncId -> pure $ AO.String $ actionIdToText actionId
          AsyncCreatedAt -> pure $ AO.toOrdered $ J.toJSON _alrCreatedAt
          AsyncErrors -> pure $ AO.toOrdered $ J.toJSON _alrErrors
      pure $ encJFromOrderedValue $ AO.object resolvedFields
    ASISource sourceName sourceConfig ->
      let jsonAggSelect = mkJsonAggSelect outputType
       in AAQEOnSourceDB sourceConfig $
            AsyncActionQuerySourceExecution sourceName jsonAggSelect $ \actionLogResponse ->
              let annotatedFields =
                    asyncFields <&> second \case
                      AsyncTypename t -> RS.AFExpression t
                      AsyncOutput annFields ->
                        let inputTableArgument = RS.AETableRow $ Just $ Identifier "response_payload"
                         in RS.AFComputedField () (ComputedFieldName $$(nonEmptyText "__action_computed_field")) $
                              RS.CFSTable jsonAggSelect $
                                processOutputSelectionSet inputTableArgument outputType definitionList annFields stringifyNumerics
                      AsyncId -> mkAnnFldFromPGCol idColumn
                      AsyncCreatedAt -> mkAnnFldFromPGCol createdAtColumn
                      AsyncErrors -> mkAnnFldFromPGCol errorsColumn

                  jsonbToRecordSet = QualifiedObject "pg_catalog" $ FunctionName "jsonb_to_recordset"
                  actionLogInput = UVParameter Nothing $ ColumnValue (ColumnScalar PGJSONB) $ PGValJSONB $ Q.JSONB $ J.toJSON [actionLogResponse]
                  functionArgs = RS.FunctionArgsExp [RS.AEInput actionLogInput] mempty
                  tableFromExp =
                    RS.FromFunction jsonbToRecordSet functionArgs $
                      Just
                        [idColumn, createdAtColumn, responsePayloadColumn, errorsColumn, sessionVarsColumn]
                  tableArguments =
                    RS.noSelectArgs
                      { RS._saWhere = Just tableBoolExpression
                      }
                  tablePermissions = RS.TablePerm annBoolExpTrue Nothing
               in RS.AnnSelectG annotatedFields tableFromExp tablePermissions tableArguments stringifyNumerics
  where
    AnnActionAsyncQuery _ actionId outputType asyncFields definitionList stringifyNumerics _ actionSource = annAction

    idColumn = (unsafePGCol "id", PGUUID)
    responsePayloadColumn = (unsafePGCol "response_payload", PGJSONB)
    createdAtColumn = (unsafePGCol "created_at", PGTimeStampTZ)
    errorsColumn = (unsafePGCol "errors", PGJSONB)
    sessionVarsColumn = (unsafePGCol "session_variables", PGJSONB)

    mkAnnFldFromPGCol (column', columnType) =
      RS.mkAnnColumnField column' (ColumnScalar columnType) Nothing Nothing

    -- TODO (from master):- Avoid using ColumnInfo
    mkPGColumnInfo (column', columnType) =
      ColumnInfo column' (G.unsafeMkName $ getPGColTxt column') 0 (ColumnScalar columnType) True Nothing

    tableBoolExpression =
      let actionIdColumnInfo =
            ColumnInfo
              (unsafePGCol "id")
              $$(G.litName "id")
              0
              (ColumnScalar PGUUID)
              False
              Nothing
          actionIdColumnEq = BoolFld $ AVColumn actionIdColumnInfo [AEQ True $ UVLiteral $ S.SELit $ actionIdToText actionId]
          sessionVarsColumnInfo = mkPGColumnInfo sessionVarsColumn
          sessionVarValue =
            UVParameter Nothing $
              ColumnValue (ColumnScalar PGJSONB) $
                PGValJSONB $ Q.JSONB $ J.toJSON $ _uiSession userInfo
          sessionVarsColumnEq = BoolFld $ AVColumn sessionVarsColumnInfo [AEQ True sessionVarValue]
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
  ( MonadIO m,
    MonadBaseControl IO m,
    LA.Forall (LA.Pure m),
    Tracing.HasReporter m,
    MonadMetadataStorage (MetadataStorageT m)
  ) =>
  Env.Environment ->
  L.Logger L.Hasura ->
  IORef (RebuildableSchemaCache, SchemaCacheVer) ->
  STM.TVar (Set LockedActionEventId) ->
  HTTP.Manager ->
  Milliseconds ->
  Maybe GH.GQLQueryText ->
  m (Forever m)
asyncActionsProcessor env logger cacheRef lockedActionEvents httpManager sleepTime gqlQueryText =
  return $
    Forever () $
      const $ do
        actionCache <- scActions . lastBuiltSchemaCache . fst <$> liftIO (readIORef cacheRef)
        let asyncActions =
              Map.filter ((== ActionMutation ActionAsynchronous) . (^. aiDefinition . adType)) actionCache
        unless (Map.null asyncActions) $ do
          -- fetch undelivered action events only when there's at least
          -- one async action present in the schema cache
          asyncInvocationsE <- runMetadataStorageT fetchUndeliveredActionEvents
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
          LA.mapConcurrently_ (callHandler actionCache) asyncInvocations
        liftIO $ sleep $ milliseconds sleepTime
  where
    callHandler :: ActionCache -> ActionLogItem -> m ()
    callHandler actionCache actionLogItem = Tracing.runTraceT "async actions processor" do
      let ActionLogItem
            actionId
            actionName
            reqHeaders
            sessionVariables
            inputPayload = actionLogItem
      case Map.lookup actionName actionCache of
        Nothing -> return ()
        Just actionInfo -> do
          let definition = _aiDefinition actionInfo
              outputFields = getActionOutputFields $ snd $ _aiOutputObject actionInfo
              webhookUrl = _adHandler definition
              forwardClientHeaders = _adForwardClientHeaders definition
              confHeaders = _adHeaders definition
              timeout = _adTimeout definition
              outputType = _adOutputType definition
              actionContext = ActionContext actionName
              metadataTransform = _aiRequestTransform actionInfo
          eitherRes <-
            runExceptT $
              flip runReaderT logger $
                callWebhook
                  env
                  httpManager
                  outputType
                  outputFields
                  reqHeaders
                  confHeaders
                  forwardClientHeaders
                  webhookUrl
                  (ActionWebhookPayload actionContext sessionVariables inputPayload gqlQueryText)
                  timeout
                  metadataTransform
          resE <- runMetadataStorageT $
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
  GraphQLType ->
  ActionOutputFields ->
  [HTTP.Header] ->
  [HeaderConf] ->
  Bool ->
  ResolvedWebhook ->
  ActionWebhookPayload ->
  Timeout ->
  Maybe MetadataTransform ->
  m (ActionWebhookResponse, HTTP.ResponseHeaders)
callWebhook
  env
  manager
  outputType
  outputFields
  reqHeaders
  confHeaders
  forwardClientHeaders
  resolvedWebhook
  actionWebhookPayload
  timeoutSeconds
  metadataTransform = do
    resolvedConfHeaders <- makeHeadersFromConf env confHeaders
    let clientHeaders = if forwardClientHeaders then mkClientHeadersForward reqHeaders else mempty
        -- Using HashMap to avoid duplicate headers between configuration headers
        -- and client headers where configuration headers are preferred
        hdrs = ("Content-Type", "application/json") : (Map.toList . Map.fromList) (resolvedConfHeaders <> clientHeaders)
        postPayload = J.toJSON actionWebhookPayload
        requestBody = J.encode postPayload
        requestBodySize = BL.length requestBody
        responseTimeout = HTTP.responseTimeoutMicro $ (unTimeout timeoutSeconds) * 1000000
        url = unResolvedWebhook resolvedWebhook
        sessionVars = Just $ _awpSessionVariables actionWebhookPayload

    initReq <- liftIO $ HTTP.mkRequestThrow url

    let req =
          initReq
            & set HTTP.method "POST"
            & set HTTP.headers hdrs
            & set HTTP.body (Just requestBody)
            & set HTTP.timeout responseTimeout

    (transformedReq, transformedReqSize) <- case metadataTransform of
      Nothing -> pure (Nothing, Nothing)
      Just transform' ->
        case applyRequestTransform url (mkRequestTransform transform') req sessionVars of
          Left err -> do
            -- Log The Transformation Error
            logger :: L.Logger L.Hasura <- asks getter
            L.unLogger logger $ L.UnstructuredLog L.LevelError (TBS.fromLBS $ J.encode err)

            -- Throw an exception with the Transformation Error
            throw500WithDetail "Request Transformation Failed" $ J.toJSON err
          Right transformedReq ->
            let transformedPayloadSize = HTTP.getReqSize transformedReq
             in pure (Just transformedReq, Just transformedPayloadSize)

    let actualReq = fromMaybe req transformedReq

    httpResponse <-
      Tracing.tracedHttpRequest actualReq $ \request ->
        liftIO . try $ HTTP.performRequest request manager

    let requestInfo = ActionRequestInfo url postPayload (confHeaders <> toHeadersConf clientHeaders) transformedReq

    case httpResponse of
      Left e ->
        throw500WithDetail "http exception when calling webhook" $
          J.toJSON $ ActionInternalError (J.toJSON $ HttpException e) requestInfo Nothing
      Right responseWreq -> do
        let responseBody = responseWreq ^. Wreq.responseBody
            responseBodySize = BL.length responseBody
            actionName = _acName $ _awpAction actionWebhookPayload
            responseStatus = responseWreq ^. Wreq.responseStatus
            mkResponseInfo respBody =
              ActionResponseInfo (HTTP.statusCode responseStatus) respBody $
                toHeadersConf $ responseWreq ^. Wreq.responseHeaders

        -- log the request and response to/from the action handler
        logger :: (L.Logger L.Hasura) <- asks getter
        L.unLogger logger $ ActionHandlerLog req transformedReq requestBodySize transformedReqSize responseBodySize actionName

        case J.eitherDecode responseBody of
          Left e -> do
            let responseInfo = mkResponseInfo $ J.String $ bsToTxt $ BL.toStrict responseBody
            throw500WithDetail "not a valid json response from webhook" $
              J.toJSON $
                ActionInternalError (J.toJSON $ "invalid json: " <> e) requestInfo $ Just responseInfo
          Right responseValue -> do
            let responseInfo = mkResponseInfo responseValue
                addInternalToErr e =
                  let actionInternalError =
                        J.toJSON $
                          ActionInternalError (J.String "unexpected response") requestInfo $ Just responseInfo
                   in e {qeInternal = Just $ ExtraInternal actionInternalError}

            if
                | HTTP.statusIsSuccessful responseStatus -> do
                  let expectingArray = isListType outputType
                      expectingNull = isNullableType outputType
                  modifyQErr addInternalToErr $ do
                    webhookResponse <- decodeValue responseValue
                    case webhookResponse of
                      AWRNull -> do
                        unless expectingNull $
                          if expectingArray
                            then throwUnexpected "expecting array for action webhook response but got null"
                            else throwUnexpected "expecting object for action webhook response but got null"
                        validateResponseObject Map.empty
                      AWRArray objs -> do
                        unless expectingArray $
                          throwUnexpected "expecting object for action webhook response but got array"
                        mapM_ validateResponseObject objs
                      AWRObject obj -> do
                        when expectingArray $
                          throwUnexpected "expecting array for action webhook response but got object"
                        validateResponseObject obj
                    pure (webhookResponse, mkSetCookieHeaders responseWreq)
                | HTTP.statusIsClientError responseStatus -> do
                  ActionWebhookErrorResponse message maybeCode maybeExtensions <-
                    modifyQErr addInternalToErr $ decodeValue responseValue
                  let code = maybe Unexpected ActionWebhookCode maybeCode
                      qErr = QErr [] responseStatus message code (ExtraExtensions <$> maybeExtensions)
                  throwError qErr
                | otherwise -> do
                  let err =
                        J.toJSON $
                          "expecting 2xx or 4xx status code, but found "
                            ++ show (HTTP.statusCode responseStatus)
                  throw500WithDetail "internal error" $
                    J.toJSON $
                      ActionInternalError err requestInfo $ Just responseInfo
    where
      throwUnexpected = throw400 Unexpected

      -- Webhook response object should conform to action output fields
      validateResponseObject obj = do
        -- Note: Fields not specified in the output are ignored

        void $
          flip Map.traverseWithKey outputFields $ \fieldName fieldTy ->
            -- When field is non-nullable, it has to present in the response with no null value
            unless (G.isNullable fieldTy) $ case Map.lookup fieldName obj of
              Nothing ->
                throwUnexpected $
                  "field " <> fieldName <<> " expected in webhook response, but not found"
              Just v ->
                when (v == J.Null) $
                  throwUnexpected $
                    "expecting not null value for field " <>> fieldName

processOutputSelectionSet ::
  RS.ArgumentExp ('Postgres 'Vanilla) v ->
  GraphQLType ->
  [(PGCol, PGScalarType)] ->
  RS.AnnFieldsG ('Postgres 'Vanilla) r v ->
  Bool ->
  RS.AnnSimpleSelectG ('Postgres 'Vanilla) r v
processOutputSelectionSet tableRowInput actionOutputType definitionList annotatedFields =
  RS.AnnSelectG annotatedFields selectFrom RS.noTablePermissions RS.noSelectArgs
  where
    jsonbToPostgresRecordFunction =
      QualifiedObject "pg_catalog" $
        FunctionName $
          if isListType actionOutputType
            then "jsonb_to_recordset" -- Multirow array response
            else "jsonb_to_record" -- Single object response
    functionArgs = RS.FunctionArgsExp [tableRowInput] mempty
    selectFrom = RS.FromFunction jsonbToPostgresRecordFunction functionArgs $ Just definitionList

mkJsonAggSelect :: GraphQLType -> JsonAggSelect
mkJsonAggSelect =
  bool JASSingleObject JASMultipleRows . isListType

insertActionTx ::
  ActionName ->
  SessionVariables ->
  [HTTP.Header] ->
  J.Value ->
  Q.TxE QErr ActionId
insertActionTx actionName sessionVariables httpHeaders inputArgsPayload =
  runIdentity . Q.getRow
    <$> Q.withQE
      defaultTxErrorHandler
      [Q.sql|
    INSERT INTO
        "hdb_catalog"."hdb_action_log"
        ("action_name", "session_variables", "request_headers", "input_payload", "status")
    VALUES
        ($1, $2, $3, $4, $5)
    RETURNING "id"
   |]
      ( actionName,
        Q.AltJ sessionVariables,
        Q.AltJ $ toHeadersMap httpHeaders,
        Q.AltJ inputArgsPayload,
        "created" :: Text
      )
      False
  where
    toHeadersMap = Map.fromList . map ((bsToTxt . CI.original) *** bsToTxt)

fetchUndeliveredActionEventsTx :: Q.TxE QErr [ActionLogItem]
fetchUndeliveredActionEventsTx =
  map mapEvent
    <$> Q.listQE
      defaultTxErrorHandler
      [Q.sql|
    update hdb_catalog.hdb_action_log set status = 'processing'
    where
      id in (
        select id from hdb_catalog.hdb_action_log
        where status = 'created'
        for update skip locked limit 10
      )
    returning
      id, action_name, request_headers::json, session_variables::json, input_payload::json
  |]
      ()
      False
  where
    mapEvent
      ( actionId,
        actionName,
        Q.AltJ headersMap,
        Q.AltJ sessionVariables,
        Q.AltJ inputPayload
        ) =
        ActionLogItem actionId actionName (fromHeadersMap headersMap) sessionVariables inputPayload

    fromHeadersMap = map ((CI.mk . txtToBs) *** txtToBs) . Map.toList

setActionStatusTx :: ActionId -> AsyncActionStatus -> Q.TxE QErr ()
setActionStatusTx actionId = \case
  AASCompleted responsePayload ->
    Q.unitQE
      defaultTxErrorHandler
      [Q.sql|
      update hdb_catalog.hdb_action_log
      set response_payload = $1, status = 'completed'
      where id = $2
    |]
      (Q.AltJ responsePayload, actionId)
      False
  AASError qerr ->
    Q.unitQE
      defaultTxErrorHandler
      [Q.sql|
      update hdb_catalog.hdb_action_log
      set errors = $1, status = 'error'
      where id = $2
    |]
      (Q.AltJ qerr, actionId)
      False

fetchActionResponseTx :: ActionId -> Q.TxE QErr ActionLogResponse
fetchActionResponseTx actionId = do
  (ca, rp, errs, Q.AltJ sessVars) <-
    Q.getRow
      <$> Q.withQE
        defaultTxErrorHandler
        [Q.sql|
     SELECT created_at, response_payload::json, errors::json, session_variables::json
       FROM hdb_catalog.hdb_action_log
      WHERE id = $1
    |]
        (Identity actionId)
        True
  pure $ ActionLogResponse actionId ca (Q.getAltJ <$> rp) (Q.getAltJ <$> errs) sessVars

clearActionDataTx :: ActionName -> Q.TxE QErr ()
clearActionDataTx actionName =
  Q.unitQE
    defaultTxErrorHandler
    [Q.sql|
      DELETE FROM hdb_catalog.hdb_action_log
        WHERE action_name = $1
      |]
    (Identity actionName)
    True

setProcessingActionLogsToPendingTx :: LockedActionIdArray -> Q.TxE QErr ()
setProcessingActionLogsToPendingTx lockedActions =
  Q.unitQE
    defaultTxErrorHandler
    [Q.sql|
    UPDATE hdb_catalog.hdb_action_log
    SET status = 'created'
    WHERE status = 'processing' AND id = ANY($1::uuid[])
  |]
    (Identity lockedActions)
    False
