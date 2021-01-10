module Hasura.GraphQL.Execute.Action
  ( ActionExecution(..)
  , runActionExecution
  , ActionExecutionPlan(..)
  , ActionExecuteResult(..)
  , asyncActionsProcessor
  , resolveActionExecution
  , resolveActionMutationAsync
  , resolveAsyncActionQuery
  , insertActionTx
  , fetchUndeliveredActionEventsTx
  , setActionStatusTx
  , fetchActionResponseTx
  , clearActionDataTx
  ) where

import           Hasura.Prelude

import qualified Control.Concurrent.Async.Lifted.Safe        as LA
import qualified Data.Aeson                                  as J
import qualified Data.Aeson.Casing                           as J
import qualified Data.Aeson.Ordered                          as AO
import qualified Data.Aeson.TH                               as J
import qualified Data.ByteString.Lazy                        as BL
import qualified Data.CaseInsensitive                        as CI
import qualified Data.Environment                            as Env
import qualified Data.HashMap.Strict                         as Map
import qualified Data.HashSet                                as Set
import qualified Data.Text                                   as T
import qualified Database.PG.Query                           as Q
import qualified Language.GraphQL.Draft.Syntax               as G
import qualified Network.HTTP.Client                         as HTTP
import qualified Network.HTTP.Types                          as HTTP
import qualified Network.Wreq                                as Wreq

import           Control.Concurrent                          (threadDelay)
import           Control.Exception                           (try)
import           Control.Lens
import           Control.Monad.Trans.Control                 (MonadBaseControl)
import           Data.Has
import           Data.Int                                    (Int64)
import           Data.IORef
import           Data.Text.Extended

import qualified Hasura.Backends.Postgres.Execute.RemoteJoin as RJ
import qualified Hasura.Backends.Postgres.SQL.DML            as S
import qualified Hasura.Backends.Postgres.Translate.Select   as RS
import qualified Hasura.Logging                              as L
import qualified Hasura.RQL.IR.Select                        as RS
import qualified Hasura.Tracing                              as Tracing

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Backends.Postgres.SQL.Value          (PGScalarValue (..))
import           Hasura.Backends.Postgres.Translate.Column   (toTxtValue)
import           Hasura.Backends.Postgres.Translate.Select   (asSingleRowJsonResp)
import           Hasura.EncJSON
import           Hasura.GraphQL.Execute.Prepare
import           Hasura.GraphQL.Parser
import           Hasura.GraphQL.Utils                        (showNames)
import           Hasura.HTTP
import           Hasura.Metadata.Class
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.DDL.Schema.Cache
import           Hasura.RQL.Types
import           Hasura.Server.Utils                         (mkClientHeadersForward,
                                                              mkSetCookieHeaders)
import           Hasura.Server.Version                       (HasVersion)
import           Hasura.Session
import           Hasura.SQL.Types


newtype ActionExecution =
  ActionExecution {
    unActionExecution
      :: forall m
       . (MonadIO m, MonadBaseControl IO m, MonadError QErr m, Tracing.MonadTrace m) => m EncJSON
  }

-- A plan to execute any action
data ActionExecutionPlan
  = AEPSync !ActionExecution
  | AEPAsyncQuery !ActionId !(ActionLogResponse -> ActionExecution)
  | AEPAsyncMutation !EncJSON

runActionExecution
  :: ( MonadIO m, MonadBaseControl IO m
     , MonadError QErr m, Tracing.MonadTrace m
     , MonadMetadataStorage (MetadataStorageT m)
     )
  => ActionExecutionPlan -> m (DiffTime, EncJSON)
runActionExecution aep = do
  (time, resp) <- withElapsedTime $ case aep of
    AEPSync e -> unActionExecution e
    AEPAsyncQuery actionId f -> do
      actionLogResponse <- liftEitherM $ runMetadataStorageT $ fetchActionResponse actionId
      unActionExecution $ f actionLogResponse
    AEPAsyncMutation m -> pure m
  pure (time, resp)

newtype ActionContext
  = ActionContext {_acName :: ActionName}
  deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''ActionContext)

data ActionWebhookPayload
  = ActionWebhookPayload
  { _awpAction           :: !ActionContext
  , _awpSessionVariables :: !SessionVariables
  , _awpInput            :: !J.Value
  } deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''ActionWebhookPayload)

data ActionWebhookErrorResponse
  = ActionWebhookErrorResponse
  { _awerMessage :: !Text
  , _awerCode    :: !(Maybe Text)
  } deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''ActionWebhookErrorResponse)

data ActionWebhookResponse
  = AWRArray ![Map.HashMap G.Name J.Value]
  | AWRObject !(Map.HashMap G.Name J.Value)
  deriving (Show, Eq)

instance J.FromJSON ActionWebhookResponse where
  parseJSON v = case v of
    J.Array{}  -> AWRArray <$> J.parseJSON v
    J.Object{} -> AWRObject <$> J.parseJSON v
    _          -> fail "expecting object or array of objects for action webhook response"

instance J.ToJSON ActionWebhookResponse where
  toJSON (AWRArray objects) = J.toJSON objects
  toJSON (AWRObject obj)    = J.toJSON obj

data ActionRequestInfo
  = ActionRequestInfo
  { _areqiUrl     :: !Text
  , _areqiBody    :: !J.Value
  , _areqiHeaders :: ![HeaderConf]
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 6 J.snakeCase) ''ActionRequestInfo)

data ActionResponseInfo
  = ActionResponseInfo
  { _aresiStatus  :: !Int
  , _aresiBody    :: !J.Value
  , _aresiHeaders :: ![HeaderConf]
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 6 J.snakeCase) ''ActionResponseInfo)

data ActionInternalError
  = ActionInternalError
  { _aieError    :: !J.Value
  , _aieRequest  :: !ActionRequestInfo
  , _aieResponse :: !(Maybe ActionResponseInfo)
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) ''ActionInternalError)

-- * Action handler logging related
data ActionHandlerLog
  = ActionHandlerLog
  { _ahlRequestSize  :: !Int64
  , _ahlResponseSize :: !Int64
  } deriving (Show)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase){J.omitNothingFields=True} ''ActionHandlerLog)

instance L.ToEngineLog ActionHandlerLog L.Hasura where
  toEngineLog ahl = (L.LevelInfo, L.ELTActionHandler, J.toJSON ahl)


data ActionExecuteResult
  = ActionExecuteResult
  { _aerExecution :: !ActionExecution
  , _aerHeaders   :: !HTTP.ResponseHeaders
  }

-- | Synchronously execute webhook handler and resolve response to action "output"
resolveActionExecution
  :: ( HasVersion
     , MonadError QErr m
     , MonadIO m
     , Tracing.MonadTrace m
     )
  => Env.Environment
  -> L.Logger L.Hasura
  -> UserInfo
  -> AnnActionExecution 'Postgres (UnpreparedValue 'Postgres)
  -> ActionExecContext
  -> m ActionExecuteResult
resolveActionExecution env logger userInfo annAction execContext = do
  let actionContext = ActionContext actionName
      handlerPayload = ActionWebhookPayload actionContext sessionVariables inputPayload
  (webhookRes, respHeaders) <- flip runReaderT logger $
                               callWebhook env manager outputType outputFields reqHeaders confHeaders
                               forwardClientHeaders resolvedWebhook handlerPayload timeout

  flip ActionExecuteResult respHeaders <$> case actionSource of
    -- Build client response
    ASINoSource -> pure $ ActionExecution $ pure $ AO.toEncJSON $ makeActionResponseNoRelations annFields webhookRes
    ASISource sourceConfig -> do
      let webhookResponseExpression = RS.AEInput $ UVLiteral $
            toTxtValue $ ColumnValue (ColumnScalar PGJSONB) $ PGValJSONB $ Q.JSONB $ J.toJSON webhookRes
          selectAstUnresolved = processOutputSelectionSet webhookResponseExpression
                                outputType definitionList annFields stringifyNum
      (astResolved, _expectedVariables) <- flip runStateT Set.empty $ RS.traverseAnnSimpleSelect prepareWithoutPlan selectAstUnresolved
      pure $ executeActionInDb sourceConfig astResolved
  where
    AnnActionExecution actionName outputType annFields inputPayload
      outputFields definitionList resolvedWebhook confHeaders
      forwardClientHeaders stringifyNum timeout actionSource = annAction
    ActionExecContext manager reqHeaders sessionVariables = execContext


    executeActionInDb :: SourceConfig 'Postgres -> RS.AnnSimpleSel 'Postgres -> ActionExecution
    executeActionInDb sourceConfig astResolved = ActionExecution do
      let (astResolvedWithoutRemoteJoins,maybeRemoteJoins) = RJ.getRemoteJoins astResolved
          jsonAggType = mkJsonAggSelect outputType
      liftEitherM $ runExceptT $ runLazyTx (_pscExecCtx sourceConfig) Q.ReadOnly $
        case maybeRemoteJoins of
          Just remoteJoins ->
            let query = Q.fromBuilder $ toSQL $
                        RS.mkSQLSelect jsonAggType astResolvedWithoutRemoteJoins
            in RJ.executeQueryWithRemoteJoins env manager reqHeaders userInfo query [] remoteJoins
          Nothing ->
            liftTx $ asSingleRowJsonResp (Q.fromBuilder $ toSQL $ RS.mkSQLSelect jsonAggType astResolved) []


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

On mutation, the server makes an action log record in hdb_catalog.hdb_action_log table with request headers
and input arguments. The `asyncActionsProcessor` background thread processes the async actions by executing
the webhook handler and writing back the response payload or errors if any in the database.

When an async action query/subscription is made, the server fetches the relavent data from the hdb_action_log
table provides the action response. See Note [Resolving async action query/subscription] below.
-}

-- | Resolve asynchronous action mutation which returns only the action uuid
resolveActionMutationAsync
  :: (MonadMetadataStorage m)
  => AnnActionMutationAsync
  -> [HTTP.Header]
  -> SessionVariables
  -> m EncJSON
resolveActionMutationAsync annAction reqHeaders sessionVariables = do
  actionId <- insertAction actionName sessionVariables reqHeaders inputArgs
  pure $ encJFromJValue $ actionIdToText actionId
  where
    AnnActionMutationAsync actionName inputArgs = annAction

{- Note: [Resolving async action query/subscription]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Resolving async action query involves in selecting fields from hdb_catalog.hdb_action_log table.
See Note [Async action architecture] above. See the table's Postgres schema in src-rsr/initialise.sql.
The webhook's response JSON stored in "response_payload" column has to be fetched as "output"
along with relationships (if any) to other tables. The in-built pg_catalog function `jsonb_to_record`
helps in converting any JSON object to Postgres record type. Thus generated record is used to resolve
action's type. Here, we treat the "output" field as a computed field to hdb_action_log table with
`jsonb_to_record` as custom SQL function.
-}

-- TODO: Add tracing here? Avoided now because currently the function is pure
resolveAsyncActionQuery
  :: UserInfo
  -> AnnActionAsyncQuery 'Postgres (UnpreparedValue 'Postgres)
  -> ActionLogResponse
  -> ActionExecution
resolveAsyncActionQuery userInfo annAction actionLogResponse = ActionExecution
  case actionSource of
    ASINoSource -> do
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

    ASISource sourceConfig -> do
      let jsonAggSelect = mkJsonAggSelect outputType
          annotatedFields = asyncFields <&> second \case
            AsyncTypename t -> RS.AFExpression t
            AsyncOutput annFields ->
              -- See Note [Resolving async action query/subscription]
              let inputTableArgument = RS.AETableRow $ Just $ Identifier "response_payload"
              in RS.AFComputedField $ RS.CFSTable jsonAggSelect $
                 processOutputSelectionSet inputTableArgument outputType
                 definitionList annFields stringifyNumerics

            AsyncId        -> mkAnnFldFromPGCol idColumn
            AsyncCreatedAt -> mkAnnFldFromPGCol createdAtColumn
            AsyncErrors    -> mkAnnFldFromPGCol errorsColumn

          jsonbToRecordSet = QualifiedObject "pg_catalog" $ FunctionName "jsonb_to_recordset"
          actionLogInput = UVLiteral $ S.SELit $ lbsToTxt $ J.encode [actionLogResponse]
          functionArgs = RS.FunctionArgsExp [RS.AEInput actionLogInput] mempty
          tableFromExp = RS.FromFunction jsonbToRecordSet functionArgs $ Just
                         [idColumn, createdAtColumn, responsePayloadColumn, errorsColumn, sessionVarsColumn]
          tableArguments = RS.noSelectArgs
                           { RS._saWhere = Just tableBoolExpression}
          tablePermissions = RS.TablePerm annBoolExpTrue Nothing
          annSelect = RS.AnnSelectG annotatedFields tableFromExp tablePermissions
                      tableArguments stringifyNumerics

      (selectResolved, _) <- flip runStateT Set.empty $ RS.traverseAnnSimpleSelect prepareWithoutPlan annSelect
      liftEitherM $ liftIO $ runPgSourceReadTx sourceConfig $
        asSingleRowJsonResp (Q.fromBuilder $ toSQL $ RS.mkSQLSelect jsonAggSelect selectResolved) []
  where
    AnnActionAsyncQuery _ actionId outputType asyncFields definitionList stringifyNumerics actionSource = annAction

    idColumn = (unsafePGCol "id", PGUUID)
    responsePayloadColumn = (unsafePGCol "response_payload", PGJSONB)
    createdAtColumn = (unsafePGCol "created_at", PGTimeStampTZ)
    errorsColumn = (unsafePGCol "errors", PGJSONB)
    sessionVarsColumn = (unsafePGCol "session_variables", PGJSONB)

    -- TODO (from master):- Avoid using ColumnInfo
    mkAnnFldFromPGCol = flip RS.mkAnnColumnField Nothing . mkPGColumnInfo

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
      in if isAdmin (_uiRole userInfo) then actionIdColumnEq
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
  -> m void
asyncActionsProcessor env logger cacheRef httpManager = forever $ do
  asyncInvocationsE <- runMetadataStorageT fetchUndeliveredActionEvents
  asyncInvocations <- liftIO $ onLeft asyncInvocationsE mempty
  actionCache <- scActions . lastBuiltSchemaCache . fst <$> liftIO (readIORef cacheRef)
  LA.mapConcurrently_ (callHandler actionCache) asyncInvocations
  liftIO $ threadDelay (1 * 1000 * 1000)
  where
    callHandler :: ActionCache -> ActionLogItem -> m ()
    callHandler actionCache actionLogItem = Tracing.runTraceT "async actions processor" do
      let ActionLogItem actionId actionName reqHeaders
            sessionVariables inputPayload = actionLogItem
      case Map.lookup actionName actionCache of
        Nothing -> return ()
        Just actionInfo -> do
          let definition = _aiDefinition actionInfo
              outputFields = getActionOutputFields $ _aiOutputObject actionInfo
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
          "unexpected fields in webhook response: " <> showNames extraFields

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

mkJsonAggSelect :: GraphQLType -> RS.JsonAggSelect
mkJsonAggSelect =
  bool RS.JASSingleObject RS.JASMultipleRows . isListType

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
