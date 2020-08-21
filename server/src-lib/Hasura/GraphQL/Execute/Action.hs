module Hasura.GraphQL.Execute.Action
  ( ActionExecuteTx
  , ActionExecuteResult(..)
  , resolveAsyncActionQuery
  , asyncActionsProcessor
  , resolveActionExecution
  , resolveActionMutationAsync
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                           as J
import qualified Data.Aeson.Casing                    as J
import qualified Data.Aeson.TH                        as J
import qualified Data.ByteString.Lazy                 as BL
import qualified Data.CaseInsensitive                 as CI
import qualified Data.HashMap.Strict                  as Map
import qualified Data.HashSet                         as Set
import qualified Data.Text                            as T
import qualified Data.UUID                            as UUID
import qualified Database.PG.Query                    as Q
import qualified Language.GraphQL.Draft.Syntax        as G
import qualified Network.HTTP.Client                  as HTTP
import qualified Network.HTTP.Types                   as HTTP
import qualified Network.Wreq                         as Wreq

import           Control.Concurrent                   (threadDelay)
import           Control.Exception                    (try)
import           Control.Lens
import           Data.Has
import           Data.Int                             (Int64)
import           Data.IORef

import qualified Hasura.RQL.DML.RemoteJoin            as RJ
import qualified Hasura.RQL.DML.Select                as RS
-- import qualified Hasura.GraphQL.Resolve.Select  as GRS
import           Control.Monad.Trans.Control          (MonadBaseControl)

import qualified Control.Concurrent.Async.Lifted.Safe as LA
import qualified Data.Environment                     as Env
import qualified Hasura.Logging                       as L
import qualified Hasura.Tracing                       as Tracing

import           Hasura.EncJSON
import           Hasura.GraphQL.Execute.Prepare
import           Hasura.GraphQL.Parser                hiding (column)
import           Hasura.GraphQL.Utils                 (showNames)
import           Hasura.HTTP
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.DDL.Schema.Cache
import           Hasura.RQL.DML.Select                (asSingleRowJsonResp)
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Run
import           Hasura.Server.Utils                  (mkClientHeadersForward, mkSetCookieHeaders)
import           Hasura.Server.Version                (HasVersion)
import           Hasura.Session
import           Hasura.SQL.Types
import           Hasura.SQL.Value                     (PGScalarValue (..), toTxtValue)

type ActionExecuteTx =
  forall tx. (MonadIO tx, MonadTx tx, Tracing.MonadTrace tx) => tx EncJSON

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
    _          -> fail $ "expecting object or array of objects for action webhook response"

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
  { _aerTransaction :: !ActionExecuteTx
  , _aerHeaders     :: !HTTP.ResponseHeaders
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
  -> AnnActionExecution UnpreparedValue
  -> ActionExecContext
  -> m ActionExecuteResult
resolveActionExecution env logger userInfo annAction execContext = do
  let actionContext = ActionContext actionName
      handlerPayload = ActionWebhookPayload actionContext sessionVariables inputPayload
  (webhookRes, respHeaders) <- flip runReaderT logger $ callWebhook env manager outputType outputFields reqHeaders confHeaders
                               forwardClientHeaders resolvedWebhook handlerPayload
  let webhookResponseExpression = RS.AEInput $ UVLiteral $
        toTxtValue $ WithScalarType PGJSONB $ PGValJSONB $ Q.JSONB $ J.toJSON webhookRes
      selectAstUnresolved = processOutputSelectionSet webhookResponseExpression
                            outputType definitionList annFields stringifyNum
  (astResolved, _expectedVariables) <- flip runStateT Set.empty $ RS.traverseAnnSimpleSelect prepareWithoutPlan selectAstUnresolved
  return $ ActionExecuteResult (executeAction astResolved) respHeaders
  where
    AnnActionExecution actionName outputType annFields inputPayload
      outputFields definitionList resolvedWebhook confHeaders
      forwardClientHeaders stringifyNum = annAction
    ActionExecContext manager reqHeaders sessionVariables = execContext


    executeAction :: RS.AnnSimpleSel -> ActionExecuteTx
    executeAction astResolved = do
      let (astResolvedWithoutRemoteJoins,maybeRemoteJoins) = RJ.getRemoteJoins astResolved
          jsonAggType = mkJsonAggSelect outputType
      case maybeRemoteJoins of
        Just remoteJoins ->
          let query = Q.fromBuilder $ toSQL $
                      RS.mkSQLSelect jsonAggType astResolvedWithoutRemoteJoins
          in RJ.executeQueryWithRemoteJoins env manager reqHeaders userInfo query [] remoteJoins
        Nothing ->
          liftTx $ asSingleRowJsonResp (Q.fromBuilder $ toSQL $ RS.mkSQLSelect jsonAggType astResolved) []

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
  :: ( MonadError QErr m
     , MonadTx tx
     )
  => AnnActionMutationAsync
  -> [HTTP.Header]
  -> SessionVariables
  -> m (tx EncJSON)
resolveActionMutationAsync annAction reqHeaders sessionVariables = do
  pure $ liftTx do
    actionId <- runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler [Q.sql|
      INSERT INTO
          "hdb_catalog"."hdb_action_log"
          ("action_name", "session_variables", "request_headers", "input_payload", "status")
      VALUES
          ($1, $2, $3, $4, $5)
      RETURNING "id"
              |]
      (actionName, Q.AltJ sessionVariables, Q.AltJ $ toHeadersMap reqHeaders, Q.AltJ inputArgs, "created"::Text) False

    pure $ encJFromJValue $ UUID.toText actionId
  where
    AnnActionMutationAsync actionName inputArgs = annAction
    toHeadersMap = Map.fromList . map ((bsToTxt . CI.original) *** bsToTxt)

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
  -> AnnActionAsyncQuery UnpreparedValue
  -> RS.AnnSimpleSelG UnpreparedValue
resolveAsyncActionQuery userInfo annAction =
  let annotatedFields = asyncFields <&> second \case
        AsyncTypename t -> RS.AFExpression t
        AsyncOutput annFields ->
          -- See Note [Resolving async action query/subscription]
          let inputTableArgument = RS.AETableRow $ Just $ Iden "response_payload"
              jsonAggSelect = mkJsonAggSelect outputType
          in RS.AFComputedField $ RS.CFSTable jsonAggSelect $
             processOutputSelectionSet inputTableArgument outputType
             definitionList annFields stringifyNumerics

        AsyncId        -> mkAnnFldFromPGCol "id" PGUUID
        AsyncCreatedAt -> mkAnnFldFromPGCol "created_at" PGTimeStampTZ
        AsyncErrors    -> mkAnnFldFromPGCol "errors" PGJSONB

      tableFromExp = RS.FromTable actionLogTable
      tableArguments = RS.noSelectArgs
                       { RS._saWhere = Just tableBoolExpression}
      tablePermissions = RS.TablePerm annBoolExpTrue Nothing

  in RS.AnnSelectG annotatedFields tableFromExp tablePermissions
     tableArguments stringifyNumerics
  where
    AnnActionAsyncQuery actionName actionId outputType asyncFields definitionList stringifyNumerics = annAction
    actionLogTable = QualifiedObject (SchemaName "hdb_catalog") (TableName "hdb_action_log")

    -- TODO (from master):- Avoid using PGColumnInfo
    mkAnnFldFromPGCol column' columnType =
      flip RS.mkAnnColumnField Nothing $
      PGColumnInfo (unsafePGCol column') (G.unsafeMkName column') 0 (PGColumnScalar columnType) True Nothing

    tableBoolExpression =
      let actionIdColumnInfo = PGColumnInfo (unsafePGCol "id") $$(G.litName "id")
                               0 (PGColumnScalar PGUUID) False Nothing
          actionIdColumnEq = BoolFld $ AVCol actionIdColumnInfo [AEQ True actionId]
          sessionVarsColumnInfo = PGColumnInfo (unsafePGCol "session_variables") $$(G.litName "session_variables")
                                  0 (PGColumnScalar PGJSONB) False Nothing
          sessionVarValue = flip UVParameter Nothing $ PGColumnValue (PGColumnScalar PGJSONB) $
                            WithScalarType PGJSONB $ PGValJSONB $ Q.JSONB $ J.toJSON $ _uiSession userInfo
          sessionVarsColumnEq = BoolFld $ AVCol sessionVarsColumnInfo [AEQ True sessionVarValue]

      -- For non-admin roles, accessing an async action's response should be allowed only for the user
      -- who initiated the action through mutation. The action's response is accessible for a query/subscription
      -- only when it's session variables are equal to that of action's.
      in if isAdmin (_uiRole userInfo) then actionIdColumnEq
         else BoolAnd [actionIdColumnEq, sessionVarsColumnEq]

data ActionLogItem
  = ActionLogItem
  { _aliId               :: !UUID.UUID
  , _aliActionName       :: !ActionName
  , _aliRequestHeaders   :: ![HTTP.Header]
  , _aliSessionVariables :: !SessionVariables
  , _aliInputPayload     :: !J.Value
  } deriving (Show, Eq)

-- | Process async actions from hdb_catalog.hdb_action_log table. This functions is executed in a background thread.
-- See Note [Async action architecture] above
asyncActionsProcessor
  :: forall m void
   . ( HasVersion
     , MonadIO m
     , MonadBaseControl IO m
     , LA.Forall (LA.Pure m)
     , Tracing.HasReporter m
     )
  => Env.Environment
  -> L.Logger L.Hasura
  -> IORef (RebuildableSchemaCache Run, SchemaCacheVer)
  -> Q.PGPool
  -> HTTP.Manager
  -> m void
asyncActionsProcessor env logger cacheRef pgPool httpManager = forever $ do
  asyncInvocations <- liftIO getUndeliveredEvents
  actionCache <- scActions . lastBuiltSchemaCache . fst <$> liftIO (readIORef cacheRef)
  LA.mapConcurrently_ (callHandler actionCache) asyncInvocations
  liftIO $ threadDelay (1 * 1000 * 1000)
  where
    runTx :: (Monoid a) => Q.TxE QErr a -> IO a
    runTx q = do
      res <- runExceptT $ Q.runTx' pgPool q
      either mempty return res

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
              outputType = _adOutputType definition
              actionContext = ActionContext actionName
          eitherRes <- runExceptT $ flip runReaderT logger $
                       callWebhook env httpManager outputType outputFields reqHeaders confHeaders
                         forwardClientHeaders webhookUrl $
                         ActionWebhookPayload actionContext sessionVariables inputPayload
          liftIO $ case eitherRes of
            Left e                     -> setError actionId e
            Right (responsePayload, _) -> setCompleted actionId $ J.toJSON responsePayload

    setError :: UUID.UUID -> QErr -> IO ()
    setError actionId e =
      runTx $ setErrorQuery actionId e

    setErrorQuery
      :: UUID.UUID -> QErr -> Q.TxE QErr ()
    setErrorQuery actionId e =
      Q.unitQE defaultTxErrorHandler [Q.sql|
        update hdb_catalog.hdb_action_log
        set errors = $1, status = 'error'
        where id = $2
      |] (Q.AltJ e, actionId) False

    setCompleted :: UUID.UUID -> J.Value -> IO ()
    setCompleted actionId responsePayload =
      runTx $ setCompletedQuery actionId responsePayload

    setCompletedQuery
      :: UUID.UUID -> J.Value -> Q.TxE QErr ()
    setCompletedQuery actionId responsePayload =
      Q.unitQE defaultTxErrorHandler [Q.sql|
        update hdb_catalog.hdb_action_log
        set response_payload = $1, status = 'completed'
        where id = $2
      |] (Q.AltJ responsePayload, actionId) False

    undeliveredEventsQuery
      :: Q.TxE QErr [ActionLogItem]
    undeliveredEventsQuery =
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

    getUndeliveredEvents = runTx undeliveredEventsQuery

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
  -> m (ActionWebhookResponse, HTTP.ResponseHeaders)
callWebhook env manager outputType outputFields reqHeaders confHeaders
            forwardClientHeaders resolvedWebhook actionWebhookPayload = do
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
  httpResponse <- do
    initReq <- liftIO $ HTTP.parseRequest (T.unpack url)
    let req = initReq { HTTP.method         = "POST"
                      , HTTP.requestHeaders = addDefaultHeaders hdrs
                      , HTTP.requestBody    = HTTP.RequestBodyLBS requestBody
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
                       when (not expectingArray) $
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
        when (not $ null extraFields) $ throwUnexpected $
          "unexpected fields in webhook response: " <> showNames extraFields

        void $ flip Map.traverseWithKey outputFields $ \fieldName fieldTy ->
          -- When field is non-nullable, it has to present in the response with no null value
          when (not $ G.isNullable fieldTy) $ case Map.lookup fieldName obj of
            Nothing -> throwUnexpected $
                       "field " <> fieldName <<> " expected in webhook response, but not found"
            Just v -> when (v == J.Null) $ throwUnexpected $
                      "expecting not null value for field " <>> fieldName

mkJsonAggSelect :: GraphQLType -> RS.JsonAggSelect
mkJsonAggSelect =
  bool RS.JASSingleObject RS.JASMultipleRows . isListType

processOutputSelectionSet
  :: RS.ArgumentExp v
  -> GraphQLType
  -> [(PGCol, PGScalarType)]
  -> RS.AnnFieldsG v
  -> Bool
  -> RS.AnnSimpleSelG v
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
