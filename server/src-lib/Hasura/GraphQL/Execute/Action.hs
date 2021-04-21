module Hasura.GraphQL.Execute.Action
  ( fetchActionLogResponses
  , runActionExecution
  , asyncActionsProcessor
  , resolveActionExecution
  , resolveActionMutationAsync
  , resolveAsyncActionQuery
  , insertActionTx
  , fetchUndeliveredActionEventsTx
  , setActionStatusTx
  , fetchActionResponseTx
  , clearActionDataTx
  , module Types
  ) where

import           Hasura.Prelude

import qualified Control.Concurrent.Async.Lifted.Safe        as LA
import qualified Data.Aeson                                  as J
import qualified Data.Aeson.Ordered                          as AO
import qualified Data.ByteString.Lazy                        as BL
import qualified Data.CaseInsensitive                        as CI
import qualified Data.Environment                            as Env
import qualified Data.HashMap.Strict                         as Map
import qualified Data.HashSet                                as Set
import qualified Data.IntMap                                 as IntMap
import qualified Data.Text                                   as T
import qualified Database.PG.Query                           as Q
import qualified Language.GraphQL.Draft.Syntax               as G
import qualified Network.HTTP.Client                         as HTTP
import qualified Network.HTTP.Types                          as HTTP
import qualified Network.Wreq                                as Wreq

import           Control.Concurrent.Extended                 (sleep)
import           Control.Exception                           (try)
import           Control.Lens
import           Control.Monad.Trans.Control                 (MonadBaseControl)
import           Data.Has
import           Data.IORef
import           Data.Text.Extended

import qualified Hasura.Backends.Postgres.Execute.RemoteJoin as RJ
import qualified Hasura.Backends.Postgres.SQL.DML            as S
import qualified Hasura.Backends.Postgres.Translate.Select   as RS
import qualified Hasura.GraphQL.Execute.RemoteJoin           as RJ
import qualified Hasura.Logging                              as L
import qualified Hasura.RQL.IR.Select                        as RS
import qualified Hasura.Tracing                              as Tracing

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Backends.Postgres.SQL.Value          (PGScalarValue (..))
import           Hasura.Backends.Postgres.Translate.Column   (toTxtValue)
import           Hasura.Backends.Postgres.Translate.Select   (asSingleRowJsonResp)
import           Hasura.EncJSON
import           Hasura.GraphQL.Execute.Action.Types         as Types
import           Hasura.GraphQL.Execute.Prepare
import           Hasura.GraphQL.Parser
import           Hasura.HTTP
import           Hasura.Metadata.Class
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.DDL.Schema.Cache
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.Server.Utils                         (mkClientHeadersForward,
                                                              mkSetCookieHeaders)
import           Hasura.Server.Version                       (HasVersion)
import           Hasura.Session

fetchActionLogResponses
  :: (MonadError QErr m, MonadMetadataStorage (MetadataStorageT m), Foldable t)
  => t ActionId -> m (ActionLogResponseMap, Bool)
fetchActionLogResponses actionIds = do
  responses <- for (toList actionIds) $ \actionId -> (actionId,)
                 <$> liftEitherM (runMetadataStorageT $ fetchActionResponse actionId)
  -- An action is said to be completed/processed iff response is captured from webhook or
  -- in case any exception occured in calling webhook.
  let isActionComplete ActionLogResponse{..} =
        isJust _alrResponsePayload || isJust _alrErrors
  pure (Map.fromList responses, all (isActionComplete . snd) responses)

runActionExecution
  :: ( MonadIO m, MonadBaseControl IO m
     , MonadError QErr m, Tracing.MonadTrace m
     , MonadMetadataStorage (MetadataStorageT m)
     )
  => ActionExecutionPlan -> m (DiffTime, (EncJSON, Maybe HTTP.ResponseHeaders))
runActionExecution aep =
  withElapsedTime $ case aep of
    AEPSync e -> second Just <$> unActionExecution e
    AEPAsyncQuery (AsyncActionQueryExecutionPlan actionId execution) -> do
      actionLogResponse <- liftEitherM $ runMetadataStorageT $ fetchActionResponse actionId
      (,Nothing) <$> case execution of
        AAQENoRelationships f -> liftEither $ f actionLogResponse
        AAQEOnSourceDB srcConfig (AsyncActionQuerySourceExecution _ jsonAggSelect f) -> do
          let selectAST = f actionLogResponse
          (selectResolved, _) <- flip runStateT Set.empty $ RS.traverseAnnSimpleSelect prepareWithoutPlan selectAST
          let querySQL = Q.fromBuilder $ toSQL $ RS.mkSQLSelect jsonAggSelect selectResolved
          liftEitherM $ runExceptT $ runLazyTx (_pscExecCtx srcConfig) Q.ReadOnly $ liftTx $ asSingleRowJsonResp querySQL []
    AEPAsyncMutation actionId -> pure $ (,Nothing) $ encJFromJValue $ actionIdToText actionId

-- | Synchronously execute webhook handler and resolve response to action "output"
resolveActionExecution
  :: (HasVersion)
  => Env.Environment
  -> L.Logger L.Hasura
  -> UserInfo
  -> AnnActionExecution 'Postgres (UnpreparedValue 'Postgres)
  -> ActionExecContext
  -> ActionExecution
resolveActionExecution env logger userInfo annAction execContext =
  case actionSource of
    -- Build client response
    ASINoSource -> ActionExecution $ first (AO.toEncJSON . makeActionResponseNoRelations annFields) <$> runWebhook
    ASISource _ sourceConfig -> ActionExecution do
      (webhookRes, respHeaders) <- runWebhook
      let webhookResponseExpression = RS.AEInput $ UVLiteral $
            toTxtValue $ ColumnValue (ColumnScalar PGJSONB) $ PGValJSONB $ Q.JSONB $ J.toJSON webhookRes
          selectAstUnresolved = processOutputSelectionSet webhookResponseExpression
                                outputType definitionList annFields stringifyNum
      (astResolved, finalPlanningSt) <- flip runStateT initPlanningSt $ RS.traverseAnnSimpleSelect prepareWithPlan selectAstUnresolved
      let prepArgs = fmap fst $ IntMap.elems $ withUserVars (_uiSession userInfo) $ _psPrepped finalPlanningSt
      (,respHeaders) <$> executeActionInDb sourceConfig astResolved prepArgs
  where
    AnnActionExecution actionName outputType annFields inputPayload
      outputFields definitionList resolvedWebhook confHeaders
      forwardClientHeaders stringifyNum timeout actionSource = annAction
    ActionExecContext manager reqHeaders sessionVariables = execContext
    actionContext = ActionContext actionName
    handlerPayload = ActionWebhookPayload actionContext sessionVariables inputPayload


    executeActionInDb :: (MonadError QErr m, MonadIO m, MonadBaseControl IO m, Tracing.MonadTrace m)
                      => SourceConfig 'Postgres -> RS.AnnSimpleSel 'Postgres -> [Q.PrepArg] -> m EncJSON
    executeActionInDb sourceConfig astResolved prepArgs = do
      let (astResolvedWithoutRemoteJoins, maybeRemoteJoins) = RJ.getRemoteJoinsSelect astResolved
          jsonAggType = mkJsonAggSelect outputType
      liftEitherM $ runExceptT $ runLazyTx (_pscExecCtx sourceConfig) Q.ReadOnly $
        case maybeRemoteJoins of
          Just remoteJoins ->
            let query = Q.fromBuilder $ toSQL $
                        RS.mkSQLSelect jsonAggType astResolvedWithoutRemoteJoins
            in RJ.executeQueryWithRemoteJoins env manager reqHeaders userInfo query prepArgs remoteJoins
          Nothing ->
            liftTx $ asSingleRowJsonResp (Q.fromBuilder $ toSQL $ RS.mkSQLSelect jsonAggType astResolved) prepArgs

    runWebhook :: (HasVersion, MonadIO m, MonadError QErr m, Tracing.MonadTrace m)
               => m (ActionWebhookResponse, HTTP.ResponseHeaders)
    runWebhook = flip runReaderT logger $
      callWebhook env manager outputType outputFields reqHeaders confHeaders
        forwardClientHeaders resolvedWebhook handlerPayload timeout

-- | Build action response from the Webhook JSON response when there are no relationships defined
makeActionResponseNoRelations :: RS.AnnFieldsG b v -> ActionWebhookResponse -> AO.Value
makeActionResponseNoRelations annFields webhookResponse =
  let mkResponseObject obj =
        AO.object $ flip mapMaybe annFields $ \(fieldName, annField) ->
          let fieldText = getFieldNameTxt fieldName
          in (fieldText,) <$> case annField of
            RS.AFExpression t -> Just $ AO.String t
            _                 -> AO.toOrdered <$> Map.lookup fieldText (mapKeys G.unName obj)
  in case webhookResponse of
    AWRArray objs -> AO.array $ map mkResponseObject objs
    AWRObject obj -> mkResponseObject obj

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
resolveActionMutationAsync
  :: (MonadMetadataStorage m)
  => AnnActionMutationAsync
  -> [HTTP.Header]
  -> SessionVariables
  -> m ActionId
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
resolveAsyncActionQuery
  :: UserInfo
  -> AnnActionAsyncQuery 'Postgres (UnpreparedValue 'Postgres)
  -> AsyncActionQueryExecution (UnpreparedValue 'Postgres)
resolveAsyncActionQuery userInfo annAction =
  case actionSource of
    ASINoSource -> AAQENoRelationships \actionLogResponse -> runExcept do
      let ActionLogResponse{..} = actionLogResponse
      resolvedFields <- for asyncFields $ \(fieldName, fld) -> do
        let fieldText = getFieldNameTxt fieldName
        (fieldText,) <$> case fld of
          AsyncTypename t       -> pure $ AO.String t
          AsyncOutput annFields ->
            fromMaybe AO.Null <$> forM _alrResponsePayload
            \response -> makeActionResponseNoRelations annFields <$> decodeValue response
          AsyncId               -> pure $ AO.String $ actionIdToText actionId
          AsyncCreatedAt        -> pure $ AO.toOrdered $ J.toJSON _alrCreatedAt
          AsyncErrors           -> pure $ AO.toOrdered $ J.toJSON _alrErrors
      pure $ AO.toEncJSON $ AO.object resolvedFields

    ASISource sourceName sourceConfig ->
      let jsonAggSelect = mkJsonAggSelect outputType
      in AAQEOnSourceDB sourceConfig $
        AsyncActionQuerySourceExecution sourceName jsonAggSelect $ \actionLogResponse ->
        let annotatedFields = asyncFields <&> second \case
              AsyncTypename t -> RS.AFExpression t
              AsyncOutput annFields ->
                let inputTableArgument = RS.AETableRow $ Just $ Identifier "response_payload"
                in RS.AFComputedField () $ RS.CFSTable jsonAggSelect $
                   processOutputSelectionSet inputTableArgument outputType
                   definitionList annFields stringifyNumerics

              AsyncId        -> mkAnnFldFromPGCol idColumn
              AsyncCreatedAt -> mkAnnFldFromPGCol createdAtColumn
              AsyncErrors    -> mkAnnFldFromPGCol errorsColumn

            jsonbToRecordSet = QualifiedObject "pg_catalog" $ FunctionName "jsonb_to_recordset"
            actionLogInput = UVParameter Nothing $ ColumnValue (ColumnScalar PGJSONB) $ PGValJSONB $ Q.JSONB $ J.toJSON [actionLogResponse]
            functionArgs = RS.FunctionArgsExp [RS.AEInput actionLogInput] mempty
            tableFromExp = RS.FromFunction jsonbToRecordSet functionArgs $ Just
                           [idColumn, createdAtColumn, responsePayloadColumn, errorsColumn, sessionVarsColumn]
            tableArguments = RS.noSelectArgs
                             { RS._saWhere = Just tableBoolExpression}
            tablePermissions = RS.TablePerm annBoolExpTrue Nothing

        in RS.AnnSelectG annotatedFields tableFromExp tablePermissions tableArguments stringifyNumerics
  where
    AnnActionAsyncQuery _ actionId outputType asyncFields definitionList stringifyNumerics _ actionSource = annAction

    idColumn = (unsafePGCol "id", PGUUID)
    responsePayloadColumn = (unsafePGCol "response_payload", PGJSONB)
    createdAtColumn = (unsafePGCol "created_at", PGTimeStampTZ)
    errorsColumn = (unsafePGCol "errors", PGJSONB)
    sessionVarsColumn = (unsafePGCol "session_variables", PGJSONB)

    -- TODO (from master):- Avoid using ColumnInfo
    mkAnnFldFromPGCol columnInfoArgs =
      RS.mkAnnColumnField (mkPGColumnInfo columnInfoArgs) Nothing Nothing

    mkPGColumnInfo (column', columnType) =
      ColumnInfo column' (G.unsafeMkName $ getPGColTxt column') 0 (ColumnScalar columnType) True Nothing

    tableBoolExpression =
      let actionIdColumnInfo = ColumnInfo (unsafePGCol "id") $$(G.litName "id")
                               0 (ColumnScalar PGUUID) False Nothing
          actionIdColumnEq = BoolFld $ AVCol actionIdColumnInfo [AEQ True $ UVLiteral $ S.SELit $ actionIdToText actionId]
          sessionVarsColumnInfo = mkPGColumnInfo sessionVarsColumn
          sessionVarValue = UVParameter Nothing $ ColumnValue (ColumnScalar PGJSONB) $
                            PGValJSONB $ Q.JSONB $ J.toJSON $ _uiSession userInfo
          sessionVarsColumnEq = BoolFld $ AVCol sessionVarsColumnInfo [AEQ True sessionVarValue]

      -- For non-admin roles, accessing an async action's response should be allowed only for the user
      -- who initiated the action through mutation. The action's response is accessible for a query/subscription
      -- only when it's session variables are equal to that of action's.
      in if (adminRoleName == (_uiRole userInfo))  then actionIdColumnEq
         else BoolAnd [actionIdColumnEq, sessionVarsColumnEq]


-- | Process async actions from hdb_catalog.hdb_action_log table. This functions is executed in a background thread.
-- See Note [Async action architecture] above
asyncActionsProcessor
  :: forall m void
   . ( HasVersion
     , MonadIO m
     , MonadBaseControl IO m
     , LA.Forall (LA.Pure m)
     , Tracing.HasReporter m
     , MonadMetadataStorage (MetadataStorageT m)
     )
  => Env.Environment
  -> L.Logger L.Hasura
  -> IORef (RebuildableSchemaCache, SchemaCacheVer)
  -> HTTP.Manager
  -> Milliseconds
  -> m void
asyncActionsProcessor env logger cacheRef httpManager sleepTime = forever $ do
  actionCache <- scActions . lastBuiltSchemaCache . fst <$> liftIO (readIORef cacheRef)
  let asyncActions = Map.filter ((== ActionMutation ActionAsynchronous) . (^. aiDefinition.adType)) actionCache
  if (Map.null asyncActions)
  then return ()
  else do
    -- fetch undelivered action events only when there's at least
    -- one async action present in the schema cache
    asyncInvocationsE <- runMetadataStorageT fetchUndeliveredActionEvents
    asyncInvocations <- liftIO $ onLeft asyncInvocationsE mempty
    LA.mapConcurrently_ (callHandler actionCache) asyncInvocations
  liftIO $ sleep $ milliseconds sleepTime
  where
    callHandler :: ActionCache -> ActionLogItem -> m ()
    callHandler actionCache actionLogItem = Tracing.runTraceT "async actions processor" do
      let ActionLogItem actionId actionName reqHeaders
            sessionVariables inputPayload = actionLogItem
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
          eitherRes <- runExceptT $ flip runReaderT logger $
                       callWebhook env httpManager outputType outputFields reqHeaders confHeaders
                         forwardClientHeaders webhookUrl
                         (ActionWebhookPayload actionContext sessionVariables inputPayload)
                         timeout
          resE <- runMetadataStorageT $ setActionStatus actionId $ case eitherRes of
              Left e                     -> AASError e
              Right (responsePayload, _) -> AASCompleted $ J.toJSON responsePayload

          liftIO $ onLeft resE mempty

callWebhook
  :: forall m r.
  ( HasVersion
  , MonadIO m
  , MonadError QErr m
  , Tracing.MonadTrace m
  , MonadReader r m
  , Has (L.Logger L.Hasura) r
  )
  => Env.Environment
  -> HTTP.Manager
  -> GraphQLType
  -> ActionOutputFields
  -> [HTTP.Header]
  -> [HeaderConf]
  -> Bool
  -> ResolvedWebhook
  -> ActionWebhookPayload
  -> Timeout
  -> m (ActionWebhookResponse, HTTP.ResponseHeaders)
callWebhook env manager outputType outputFields reqHeaders confHeaders
            forwardClientHeaders resolvedWebhook actionWebhookPayload timeoutSeconds = do
  resolvedConfHeaders <- makeHeadersFromConf env confHeaders
  let clientHeaders = if forwardClientHeaders then mkClientHeadersForward reqHeaders else []
      contentType = ("Content-Type", "application/json")
      -- Using HashMap to avoid duplicate headers between configuration headers
      -- and client headers where configuration headers are preferred
      hdrs = contentType : (Map.toList . Map.fromList) (resolvedConfHeaders <> clientHeaders)
      postPayload = J.toJSON actionWebhookPayload
      requestBody = J.encode postPayload
      requestBodySize = BL.length requestBody
      url = unResolvedWebhook resolvedWebhook
      responseTimeout = HTTP.responseTimeoutMicro $ (unTimeout timeoutSeconds) * 1000000
  httpResponse <- do
    initReq <- liftIO $ HTTP.parseRequest (T.unpack url)
    let req = initReq { HTTP.method          = "POST"
                      , HTTP.requestHeaders  = addDefaultHeaders hdrs
                      , HTTP.requestBody     = HTTP.RequestBodyLBS requestBody
                      , HTTP.responseTimeout = responseTimeout
                      }
    Tracing.tracedHttpRequest req \req' ->
      liftIO . try $ HTTP.httpLbs req' manager
  let requestInfo = ActionRequestInfo url postPayload $
                     confHeaders <> toHeadersConf clientHeaders
  case httpResponse of
    Left e ->
      throw500WithDetail "http exception when calling webhook" $
      J.toJSON $ ActionInternalError (J.toJSON $ HttpException e) requestInfo Nothing

    Right responseWreq -> do
      let responseBody = responseWreq ^. Wreq.responseBody
          responseBodySize = BL.length responseBody
          responseStatus = responseWreq ^. Wreq.responseStatus
          mkResponseInfo respBody =
            ActionResponseInfo (HTTP.statusCode responseStatus) respBody $
            toHeadersConf $ responseWreq ^. Wreq.responseHeaders

      -- log the request and response to/from the action handler
      logger :: (L.Logger L.Hasura) <- asks getter
      L.unLogger logger $ ActionHandlerLog requestBodySize responseBodySize

      case J.eitherDecode responseBody of
        Left e -> do
          let responseInfo = mkResponseInfo $ J.String $ bsToTxt $ BL.toStrict responseBody
          throw500WithDetail "not a valid json response from webhook" $ J.toJSON $
            ActionInternalError (J.toJSON $ "invalid json: " <> e) requestInfo $ Just responseInfo

        Right responseValue -> do
          let responseInfo = mkResponseInfo responseValue
              addInternalToErr e =
                let actionInternalError = J.toJSON $
                      ActionInternalError (J.String "unexpected response") requestInfo $ Just responseInfo
                in e{qeInternal = Just actionInternalError}

          if | HTTP.statusIsSuccessful responseStatus  -> do
                 let expectingArray = isListType outputType
                 modifyQErr addInternalToErr $ do
                   webhookResponse <- decodeValue responseValue
                   case webhookResponse of
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
                 ActionWebhookErrorResponse message maybeCode <-
                   modifyQErr addInternalToErr $ decodeValue responseValue
                 let code = maybe Unexpected ActionWebhookCode maybeCode
                     qErr = QErr [] responseStatus message code Nothing
                 throwError qErr

             | otherwise -> do
                 let err = J.toJSON $ "expecting 2xx or 4xx status code, but found "
                           ++ show (HTTP.statusCode responseStatus)
                 throw500WithDetail "internal error" $ J.toJSON $
                   ActionInternalError err requestInfo $ Just responseInfo
    where
      throwUnexpected = throw400 Unexpected

      -- Webhook response object should conform to action output fields
      validateResponseObject obj = do
        -- Fields not specified in the output type shouldn't be present in the response
        let extraFields = filter (not . flip Map.member outputFields) $ Map.keys obj
        unless (null extraFields) $ throwUnexpected $
          "unexpected fields in webhook response: " <> commaSeparated extraFields

        void $ flip Map.traverseWithKey outputFields $ \fieldName fieldTy ->
          -- When field is non-nullable, it has to present in the response with no null value
          unless (G.isNullable fieldTy) $ case Map.lookup fieldName obj of
            Nothing -> throwUnexpected $
                       "field " <> fieldName <<> " expected in webhook response, but not found"
            Just v -> when (v == J.Null) $ throwUnexpected $
                      "expecting not null value for field " <>> fieldName

processOutputSelectionSet
  :: RS.ArgumentExp 'Postgres v
  -> GraphQLType
  -> [(PGCol, PGScalarType)]
  -> RS.AnnFieldsG 'Postgres v
  -> Bool
  -> RS.AnnSimpleSelG 'Postgres v
processOutputSelectionSet tableRowInput actionOutputType definitionList annotatedFields =
  RS.AnnSelectG annotatedFields selectFrom RS.noTablePermissions RS.noSelectArgs
  where
    jsonbToPostgresRecordFunction =
      QualifiedObject "pg_catalog" $ FunctionName $
      if isListType actionOutputType then
        "jsonb_to_recordset" -- Multirow array response
      else "jsonb_to_record" -- Single object response

    functionArgs = RS.FunctionArgsExp [tableRowInput] mempty
    selectFrom = RS.FromFunction jsonbToPostgresRecordFunction functionArgs $ Just definitionList

mkJsonAggSelect :: GraphQLType -> JsonAggSelect
mkJsonAggSelect =
  bool JASSingleObject JASMultipleRows . isListType

insertActionTx
  :: ActionName -> SessionVariables -> [HTTP.Header] -> J.Value
  -> Q.TxE QErr ActionId
insertActionTx actionName sessionVariables httpHeaders inputArgsPayload =
  runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler [Q.sql|
    INSERT INTO
        "hdb_catalog"."hdb_action_log"
        ("action_name", "session_variables", "request_headers", "input_payload", "status")
    VALUES
        ($1, $2, $3, $4, $5)
    RETURNING "id"
   |]
    ( actionName
    , Q.AltJ sessionVariables
    , Q.AltJ $ toHeadersMap httpHeaders
    , Q.AltJ inputArgsPayload
    , "created"::Text
    ) False
  where
    toHeadersMap = Map.fromList . map ((bsToTxt . CI.original) *** bsToTxt)

fetchUndeliveredActionEventsTx :: Q.TxE QErr [ActionLogItem]
fetchUndeliveredActionEventsTx =
  map mapEvent <$> Q.listQE defaultTxErrorHandler [Q.sql|
    update hdb_catalog.hdb_action_log set status = 'processing'
    where
      id in (
        select id from hdb_catalog.hdb_action_log
        where status = 'created'
        for update skip locked limit 10
      )
    returning
      id, action_name, request_headers::json, session_variables::json, input_payload::json
  |] () False
 where
   mapEvent (actionId, actionName, Q.AltJ headersMap,
             Q.AltJ sessionVariables, Q.AltJ inputPayload) =
     ActionLogItem actionId actionName (fromHeadersMap headersMap) sessionVariables inputPayload

   fromHeadersMap = map ((CI.mk . txtToBs) *** txtToBs) . Map.toList

setActionStatusTx :: ActionId -> AsyncActionStatus -> Q.TxE QErr ()
setActionStatusTx actionId = \case
  AASCompleted responsePayload ->
    Q.unitQE defaultTxErrorHandler [Q.sql|
      update hdb_catalog.hdb_action_log
      set response_payload = $1, status = 'completed'
      where id = $2
    |] (Q.AltJ responsePayload, actionId) False

  AASError qerr                ->
    Q.unitQE defaultTxErrorHandler [Q.sql|
      update hdb_catalog.hdb_action_log
      set errors = $1, status = 'error'
      where id = $2
    |] (Q.AltJ qerr, actionId) False

fetchActionResponseTx :: ActionId -> Q.TxE QErr ActionLogResponse
fetchActionResponseTx actionId = do
  (ca, rp, errs, Q.AltJ sessVars) <-
    Q.getRow <$> Q.withQE defaultTxErrorHandler [Q.sql|
     SELECT created_at, response_payload::json, errors::json, session_variables::json
       FROM hdb_catalog.hdb_action_log
      WHERE id = $1
    |] (Identity actionId) True
  pure $ ActionLogResponse actionId ca (Q.getAltJ <$> rp) (Q.getAltJ <$> errs) sessVars

clearActionDataTx :: ActionName -> Q.TxE QErr ()
clearActionDataTx actionName =
  Q.unitQE defaultTxErrorHandler [Q.sql|
      DELETE FROM hdb_catalog.hdb_action_log
        WHERE action_name = $1
      |] (Identity actionName) True
