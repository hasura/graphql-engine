-- This pragma is needed for allowQueryActionExecuter
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Hasura.GraphQL.Resolve.Action
  ( resolveAsyncActionQuery
  , asyncActionsProcessor
  , resolveActionExecution
  , resolveActionMutationAsync
  , mkJsonAggSelect
  , QueryActionExecuter
  , allowQueryActionExecuter
  , restrictActionExecuter
  ) where

import           Hasura.Prelude

import qualified Control.Concurrent.Async       as A
import qualified Data.Aeson                     as J
import qualified Data.Aeson.Casing              as J
import qualified Data.Aeson.TH                  as J
import qualified Data.ByteString.Lazy           as BL
import qualified Data.CaseInsensitive           as CI
import qualified Data.HashMap.Strict            as Map
import qualified Data.Text                      as T
import qualified Data.UUID                      as UUID
import qualified Database.PG.Query              as Q
import qualified Language.GraphQL.Draft.Syntax  as G
import qualified Network.HTTP.Client            as HTTP
import qualified Network.HTTP.Types             as HTTP
import qualified Network.Wreq                   as Wreq

import           Control.Concurrent             (threadDelay)
import           Control.Exception              (try)
import           Control.Lens
import           Data.IORef

import qualified Hasura.RQL.DML.RemoteJoin      as RJ
import qualified Hasura.RQL.DML.Select          as RS
-- import qualified Hasura.GraphQL.Resolve.Select  as GRS

import           Hasura.EncJSON
import           Hasura.GraphQL.Execute.Prepare
import           Hasura.GraphQL.Parser
import           Hasura.GraphQL.Utils           (showNames)
import           Hasura.HTTP
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.DDL.Schema.Cache
import           Hasura.RQL.DML.Select          (asSingleRowJsonResp)
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Run
import           Hasura.Server.Utils            (mkClientHeadersForward, mkSetCookieHeaders)
import           Hasura.Server.Version          (HasVersion)
import           Hasura.Session
import           Hasura.SQL.Types
import           Hasura.SQL.Value               (PGScalarValue (..), toTxtValue)
-- import           Hasura.GraphQL.Resolve.Context
-- import           Hasura.GraphQL.Resolve.InputValue
-- import           Hasura.GraphQL.Resolve.Select     (processTableSelectionSet)
-- import           Hasura.GraphQL.Validate.SelectionSet

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

-- resolveActionMutation
--   :: ( HasVersion
--      , MonadError QErr m
--      , MonadIO m
--      , MonadReader r m
--      , Has HTTP.Manager r
--      , Has [HTTP.Header] r
--      )
--   => Field
--   -> ActionMutationExecutionContext
--   -> UserVars
--   -> m (RespTx, HTTP.ResponseHeaders)
-- resolveActionMutation field executionContext sessionVariables =
--   case executionContext of
--     ActionMutationSyncWebhook executionContextSync ->
--       resolveActionMutationSync field executionContextSync sessionVariables
--     ActionMutationAsync ->
--       (,[]) <$> resolveActionMutationAsync field sessionVariables

-- | Synchronously execute webhook handler and resolve response to action "output"
resolveActionExecution
  :: ( HasVersion
     , MonadError QErr m
     , MonadIO m
     )
  => UserInfo
  -> AnnActionExecution UnpreparedValue
  -> ActionExecContext
  -> m (RespTx, HTTP.ResponseHeaders)
resolveActionExecution userInfo annAction execContext = do
  let actionContext = ActionContext actionName
      handlerPayload = ActionWebhookPayload actionContext sessionVariables inputPayload
  (webhookRes, respHeaders) <- callWebhook manager outputType outputFields reqHeaders confHeaders
                               forwardClientHeaders resolvedWebhook handlerPayload
  let webhookResponseExpression = RS.AEInput $ UVLiteral $
        toTxtValue $ WithScalarType PGJSONB $ PGValJSONB $ Q.JSONB $ J.toJSON webhookRes
      selectAstUnresolved = processOutputSelectionSet webhookResponseExpression
                            outputType definitionList annFields stringifyNum
  astResolved <- RS.traverseAnnSimpleSelect (pure . unpreparedToTextSQL) selectAstUnresolved
  let (astResolvedWithoutRemoteJoins,maybeRemoteJoins) = RJ.getRemoteJoins astResolved
      jsonAggType = mkJsonAggSelect outputType
  return $ (,respHeaders) $
    case maybeRemoteJoins of
      Just remoteJoins ->
        let query = Q.fromBuilder $ toSQL $
                    RS.mkSQLSelect jsonAggType astResolvedWithoutRemoteJoins
        in RJ.executeQueryWithRemoteJoins manager reqHeaders userInfo query [] remoteJoins
      Nothing ->
        asSingleRowJsonResp (Q.fromBuilder $ toSQL $ RS.mkSQLSelect jsonAggType astResolved) []
  where
    AnnActionExecution actionName outputType annFields inputPayload
      outputFields definitionList resolvedWebhook confHeaders
      forwardClientHeaders stringifyNum = annAction
    ActionExecContext manager reqHeaders sessionVariables = execContext

-- QueryActionExecuter is a type for a higher function, this is being used
-- to allow or disallow where a query action can be executed. We would like
-- to explicitly control where a query action can be run.
-- Example: We do not explain a query action, so we use the `restrictActionExecuter`
-- to prevent resolving the action query.
type QueryActionExecuter =
  forall m a. (MonadError QErr m)
  => (HTTP.Manager -> [HTTP.Header] -> m a)
  -> m a

allowQueryActionExecuter :: HTTP.Manager -> [HTTP.Header] -> QueryActionExecuter
allowQueryActionExecuter manager reqHeaders actionResolver =
  actionResolver manager reqHeaders

restrictActionExecuter :: Text -> QueryActionExecuter
restrictActionExecuter errMsg _ =
  throw400 NotSupported errMsg

-- resolveActionQuery
--   :: ( HasVersion
--      , MonadReusability m
--      , MonadError QErr m
--      , MonadReader r m
--      , MonadIO m
--      , Has FieldMap r
--      , Has OrdByCtx r
--      , Has SQLGenCtx r
--      )
--   => Field
--   -> ActionExecutionContext
--   -> SessionVariables
--   -> HTTP.Manager
--   -> [HTTP.Header]
--   -> m (RS.AnnSimpleSelG UnresolvedVal)
-- resolveActionQuery field executionContext sessionVariables httpManager reqHeaders = do
--   let inputArgs = J.toJSON $ fmap annInpValueToJson $ _fArguments field
--       actionContext = ActionContext actionName
--       handlerPayload = ActionWebhookPayload actionContext sessionVariables inputArgs
--   (webhookRes, _) <- callWebhook httpManager outputType outputFields reqHeaders confHeaders
--                                forwardClientHeaders resolvedWebhook handlerPayload
--   let webhookResponseExpression = RS.AEInput $ UVSQL $
--         toTxtValue $ WithScalarType PGJSONB $ PGValJSONB $ Q.JSONB $ J.toJSON webhookRes
--   selSet <- asObjectSelectionSet $ _fSelSet field
--   selectAstUnresolved <-
--     processOutputSelectionSet webhookResponseExpression outputType definitionList
--     (_fType field) selSet
--   return selectAstUnresolved
--   where
--     ActionExecutionContext actionName outputType outputFields definitionList resolvedWebhook confHeaders
--       forwardClientHeaders = executionContext

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
  :: (MonadError QErr m)
  => AnnActionMutationAsync
  -> [HTTP.Header]
  -> SessionVariables
  -> m RespTx
resolveActionMutationAsync annAction reqHeaders sessionVariables = do
  pure $ do
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

    -- TODO:- Avoid using PGColumnInfo
    mkAnnFldFromPGCol column columnType =
      flip RS.mkAnnColumnField Nothing $
      PGColumnInfo (unsafePGCol column) (G.unsafeMkName column) 0 (PGColumnScalar columnType) True Nothing

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
  :: HasVersion
  => IORef (RebuildableSchemaCache Run, SchemaCacheVer)
  -> Q.PGPool
  -> HTTP.Manager
  -> IO void
asyncActionsProcessor cacheRef pgPool httpManager = forever $ do
  asyncInvocations <- getUndeliveredEvents
  actionCache <- scActions . lastBuiltSchemaCache . fst <$> readIORef cacheRef
  A.mapConcurrently_ (callHandler actionCache) asyncInvocations
  threadDelay (1 * 1000 * 1000)
  where
    runTx :: (Monoid a) => Q.TxE QErr a -> IO a
    runTx q = do
      res <- runExceptT $ Q.runTx' pgPool q
      either mempty return res

    callHandler :: ActionCache -> ActionLogItem -> IO ()
    callHandler actionCache actionLogItem = do
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
          eitherRes <- runExceptT $
                       callWebhook httpManager outputType outputFields reqHeaders confHeaders
                         forwardClientHeaders webhookUrl $
                         ActionWebhookPayload actionContext sessionVariables inputPayload
          case eitherRes of
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
  :: forall m. (HasVersion, MonadIO m, MonadError QErr m)
  => HTTP.Manager
  -> GraphQLType
  -> ActionOutputFields
  -> [HTTP.Header]
  -> [HeaderConf]
  -> Bool
  -> ResolvedWebhook
  -> ActionWebhookPayload
  -> m (ActionWebhookResponse, HTTP.ResponseHeaders)
callWebhook manager outputType outputFields reqHeaders confHeaders
            forwardClientHeaders resolvedWebhook actionWebhookPayload = do
  resolvedConfHeaders <- makeHeadersFromConf confHeaders
  let clientHeaders = if forwardClientHeaders then mkClientHeadersForward reqHeaders else []
      contentType = ("Content-Type", "application/json")
      options = wreqOptions manager $
                -- Using HashMap to avoid duplicate headers between configuration headers
                -- and client headers where configuration headers are preferred
                contentType : (Map.toList . Map.fromList) (resolvedConfHeaders <> clientHeaders)
      postPayload = J.toJSON actionWebhookPayload
      url = unResolvedWebhook resolvedWebhook
  httpResponse <- liftIO $ try $ Wreq.postWith options (T.unpack url) postPayload
  let requestInfo = ActionRequestInfo url postPayload $
                     confHeaders <> toHeadersConf clientHeaders
  case httpResponse of
    Left e ->
      throw500WithDetail "http exception when calling webhook" $
      J.toJSON $ ActionInternalError (J.toJSON $ HttpException e) requestInfo Nothing

    Right responseWreq -> do
      let responseBody = responseWreq ^. Wreq.responseBody
          responseStatus = responseWreq ^. Wreq.responseStatus
          mkResponseInfo respBody =
            ActionResponseInfo (HTTP.statusCode responseStatus) respBody $
            toHeadersConf $ responseWreq ^. Wreq.responseHeaders
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

-- annInpValueToJson :: AnnInpVal -> J.Value
-- annInpValueToJson annInpValue =
--   case _aivValue annInpValue of
--     AGScalar _ pgColumnValueM -> maybe J.Null pgScalarValueToJson pgColumnValueM
--     AGEnum _ enumValue        -> case enumValue of
--       AGESynthetic enumValueM   -> J.toJSON enumValueM
--       AGEReference _ enumValueM -> J.toJSON enumValueM
--     AGObject _ objectM        -> J.toJSON $ fmap (fmap annInpValueToJson) objectM
--     AGArray _ valuesM         -> J.toJSON $ fmap (fmap annInpValueToJson) valuesM

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
processOutputSelectionSet tableRowInput actionOutputType definitionList
  annotatedFields =
  RS.AnnSelectG annotatedFields selectFrom RS.noTablePermissions RS.noSelectArgs
  where
    jsonbToPostgresRecordFunction =
      QualifiedObject "pg_catalog" $ FunctionName $
      if isListType actionOutputType then
        "jsonb_to_recordset" -- Multirow array response
      else "jsonb_to_record" -- Single object response

    functionArgs = RS.FunctionArgsExp [tableRowInput] mempty
    selectFrom = RS.FromFunction jsonbToPostgresRecordFunction functionArgs $ Just definitionList
