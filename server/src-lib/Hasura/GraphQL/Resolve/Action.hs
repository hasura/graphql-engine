module Hasura.GraphQL.Resolve.Action
  ( resolveActionSelect
  , resolveActionInsert
  , asyncActionsProcessor
  , resolveAsyncResponse
  -- , resolveResponseSelectionSet

  , ActionSelect(..)
  , traverseActionSelect
  , actionSelectToSql
  ) where

import           Hasura.Prelude

import           Control.Concurrent                (threadDelay)
import           Control.Exception                 (try)
import           Control.Lens
import           Data.Has
import           Data.IORef

import qualified Control.Concurrent.Async          as A
import qualified Data.Aeson                        as J
import qualified Data.Aeson.Casing                 as J
import qualified Data.Aeson.TH                     as J
import qualified Data.CaseInsensitive              as CI
import qualified Data.HashMap.Strict               as Map
import qualified Data.Text                         as T
import qualified Data.UUID                         as UUID
import qualified Database.PG.Query                 as Q
import qualified Language.GraphQL.Draft.Syntax     as G
import qualified Network.HTTP.Client               as HTTP
import qualified Network.HTTP.Types                as HTTP
import qualified Network.Wreq                      as Wreq

import qualified Hasura.GraphQL.Resolve.Select     as GRS
import qualified Hasura.RQL.DML.Select             as RS
import qualified Hasura.SQL.DML                    as S

import           Hasura.EncJSON
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.InputValue
import           Hasura.GraphQL.Resolve.Select     (processTableSelectionSet)
import           Hasura.GraphQL.Validate.Field
import           Hasura.GraphQL.Validate.Types
import           Hasura.HTTP
import           Hasura.RQL.DDL.Headers            (HeaderConf, makeHeadersFromConf)
import           Hasura.RQL.DDL.Schema.Cache
import           Hasura.RQL.DML.Select             (asSingleRowJsonResp)
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Run
import           Hasura.Server.Utils               (mkClientHeadersForward)
import           Hasura.Server.Version             (HasVersion)
import           Hasura.SQL.Types
import           Hasura.SQL.Value                  (PGScalarValue (..), pgScalarValueToJson,
                                                    toTxtValue)

data InputFieldResolved
  = InputFieldSimple !Text
  | InputFieldTypename !G.NamedType
  deriving (Show, Eq)

data OutputFieldResolved
  = OutputFieldSimple !Text
  | OutputFieldRelationship
  | OutputFieldTypename !G.NamedType
  deriving (Show, Eq)

data ResponseFieldResolved
  = ResponseFieldOutput ![(Text, OutputFieldResolved)]
  | ResponseFieldMetadata !PGCol
  | ResponseFieldTypename !G.NamedType
  deriving (Show, Eq)

resolveOutputSelectionSet
  :: (MonadError QErr m)
  => G.NamedType
  -> SelSet
  -> m [(Text, OutputFieldResolved)]
resolveOutputSelectionSet ty selSet =
  withSelSet selSet $ \fld -> case _fName fld of
    "__typename" -> return $ OutputFieldTypename ty
    G.Name t     -> return $ OutputFieldSimple t

resolveResponseSelectionSet
  :: (MonadError QErr m)
  => G.NamedType
  -> SelSet
  -> m [(Text, ResponseFieldResolved)]
resolveResponseSelectionSet ty selSet =
  withSelSet selSet $ \fld -> case _fName fld of
    "__typename" -> return $ ResponseFieldTypename ty

    "output"     ->
      ResponseFieldOutput <$>
      resolveOutputSelectionSet (_fType fld) (_fSelSet fld)

    -- the metadata columns
    "id"         -> return $ mkMetadataField "id"
    "created_at" -> return $ mkMetadataField "created_at"
    "status"     -> return $ mkMetadataField "status"
    "errors"     -> return $ mkMetadataField "errors"

    G.Name t     -> throw500 $ "unexpected field in actions' httpResponse : " <> t

  where
    mkMetadataField = ResponseFieldMetadata . unsafePGCol


data ActionSelect v
  = ActionSelect
  { _asId        :: !v
  , _asSelection :: ![(Text, ResponseFieldResolved)]
  , _asFilter    :: !(AnnBoolExp v)
  } deriving (Show, Eq, Functor)

traverseActionSelect
  :: (Applicative f)
  => (a -> f b)
  -> ActionSelect a
  -> f (ActionSelect b)
traverseActionSelect f (ActionSelect idText selection rowFilter) =
  ActionSelect <$> f idText <*> pure selection <*> traverseAnnBoolExp f rowFilter

type ActionSelectResolved = ActionSelect S.SQLExp
type ActionSelectUnresolved = ActionSelect UnresolvedVal

actionSelectToSql :: ActionSelectResolved -> Q.Query
actionSelectToSql (ActionSelect actionIdExp selection _) =
  Q.fromBuilder $ toSQL selectAST
  where
    selectAST =
      S.mkSelect
      { S.selFrom = Just $ S.FromExp $ pure $ S.FISimple actionLogTable Nothing
      , S.selExtr = pure $ S.Extractor
                    (usingJsonBuildObj selection responseFieldToSQLExp)
                    -- we need the root alias because subscription refers
                    -- to this particular field
                    (Just $ S.toAlias $ Iden "root")
      , S.selWhere = Just $ S.WhereFrag whereExpression
      }

    whereExpression =
      S.BECompare S.SEQ (S.mkSIdenExp actionIdColumn) actionIdExp
      -- we need this annotation because ID is mapped to text
      -- and hence the prepared value will be a PGText
      -- S.SETyAnn actionIdExp $ S.TypeAnn "uuid"
      where
        actionIdColumn = unsafePGCol "id"

    actionLogTable =
      QualifiedObject (SchemaName "hdb_catalog") (TableName "hdb_action_log")

    responseFieldToSQLExp = \case
      ResponseFieldOutput fields       -> usingJsonBuildObj fields outputFieldToSQLExp
      ResponseFieldMetadata columnName -> S.SEIden $ toIden columnName
      ResponseFieldTypename ty         -> S.SELit $ G.unName $ G.unNamedType ty

    outputFieldToSQLExp = \case
      OutputFieldSimple fieldName ->
        S.SEOpApp (S.SQLOp "->>") [outputColumn, S.SELit fieldName]
      OutputFieldRelationship     -> undefined
      OutputFieldTypename ty      -> S.SELit $ G.unName $ G.unNamedType ty
      where
        outputColumn = S.SEIden $ toIden $ unsafePGCol "response_payload"

    usingJsonBuildObj :: [(Text, a)] -> (a -> S.SQLExp) -> S.SQLExp
    usingJsonBuildObj l f =
      S.applyJsonBuildObj $ flip concatMap l $
      \(alias, field) -> [S.SELit alias, f field]


resolveActionSelect
  :: ( MonadReusability m
     , MonadError QErr m
     )
  => ActionSelectOpContext
  -> Field
  -> m ActionSelectUnresolved
resolveActionSelect selectContext field = do
  actionId <- withArg (_fArguments field) "id" parseActionId
  responseSelectionSet <- resolveResponseSelectionSet (_fType field) $
                          _fSelSet field
  return $ ActionSelect actionId responseSelectionSet unresolvedFilter
  where
    unresolvedFilter =
      fmapAnnBoolExp partialSQLExpToUnresolvedVal $
      _asocFilter selectContext
    parseActionId annInpValue = do
      mkParameterizablePGValue <$> asPGColumnValue annInpValue

actionSelectToTx :: ActionSelectResolved -> RespTx
actionSelectToTx actionSelect =
  asSingleRowJsonResp (actionSelectToSql actionSelect) []

newtype ActionContext
  = ActionContext {_acName :: ActionName}
  deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''ActionContext)

data ActionWebhookPayload
  = ActionWebhookPayload
  { _awpAction           :: !ActionContext
  , _awpSessionVariables :: !UserVars
  , _awpInput            :: !J.Value
  } deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''ActionWebhookPayload)

data ActionWebhookErrorResponse
  = ActionWebhookErrorResponse
  { _awerMessage :: !Text
  , _awerCode    :: !(Maybe Text)
  } deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''ActionWebhookErrorResponse)

data ResolvePlan
  = ResolveReturn
  | ResolvePostgres [(PGCol, PGScalarType)] ![(Text, OutputFieldResolved)]
  deriving (Show, Eq)

processOutputSelectionSet
  :: ( MonadReusability m
     , MonadError QErr m
     , MonadReader r m
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     )
  => RS.SelectFromG UnresolvedVal
  -> G.NamedType -> SelSet -> m GRS.AnnSimpleSelect
processOutputSelectionSet selectFrom fldTy flds = do
  stringifyNumerics <- stringifyNum <$> asks getter
  annotatedFields <- processTableSelectionSet fldTy flds
  let selectAst = RS.AnnSelG annotatedFields selectFrom
                  RS.noTablePermissions RS.noTableArgs stringifyNumerics
  return selectAst

resolveActionInsertSync
  :: ( HasVersion
     , MonadReusability m
     , MonadError QErr m
     , MonadReader r m
     , MonadIO m
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     , Has HTTP.Manager r
     , Has [HTTP.Header] r
     )
  => Field
  -> SyncActionExecutionContext
  -> UserVars
  -> m RespTx
resolveActionInsertSync field executionContext sessionVariables = do
  let inputArgs = J.toJSON $ fmap annInpValueToJson $ _fArguments field
      actionContext = ActionContext actionName
      handlerPayload = ActionWebhookPayload actionContext sessionVariables inputArgs
  manager <- asks getter
  reqHeaders <- asks getter
  webhookRes <- callWebhook manager reqHeaders confHeaders forwardClientHeaders resolvedWebhook handlerPayload
  case returnStrategy of
    ReturnJson -> return $ return $ encJFromJValue webhookRes
    ExecOnPostgres definitionList -> do
      let webhookResponseExpression =
            toTxtValue $ WithScalarType PGJSON $ PGValJSON $ Q.JSON webhookRes
      selectAstUnresolved <-
        processOutputSelectionSet
        (mkJsonToRecordFromExpression definitionList webhookResponseExpression)
        (_fType field) $ _fSelSet field
      astResolved <- RS.traverseAnnSimpleSel resolveValTxt selectAstUnresolved
      return $ asSingleRowJsonResp (RS.selectQuerySQL True astResolved) []
  where
    SyncActionExecutionContext actionName returnStrategy resolvedWebhook confHeaders
      forwardClientHeaders = executionContext

    mkJsonToRecordFromExpression definitionList webhookResponseExpression =
      let functionName = QualifiedObject (SchemaName "pg_catalog") $
                         FunctionName "json_to_record"
          functionArgs = RS.FunctionArgsExp
                         (pure $ UVSQL webhookResponseExpression)
                         mempty
      in RS.FromFunction functionName (RS.AEInput <$> functionArgs)
            (Just definitionList)

callWebhook
  :: (HasVersion, MonadIO m, MonadError QErr m)
  => HTTP.Manager
  -> [HTTP.Header]
  -> [HeaderConf]
  -> Bool
  -> ResolvedWebhook
  -> ActionWebhookPayload
  -> m J.Value
callWebhook manager reqHeaders confHeaders forwardClientHeaders resolvedWebhook actionWebhookPayload = do
  resolvedConfHeaders <- makeHeadersFromConf confHeaders
  let clientHeaders = if forwardClientHeaders then mkClientHeadersForward reqHeaders else []
      contentType = ("Content-Type", "application/json")
      options = wreqOptions manager $
                -- Using HashMap to avoid duplicate headers between configuration headers
                -- and client headers where configuration headers are preferred
                contentType : (Map.toList . Map.fromList) (resolvedConfHeaders <> clientHeaders)
      postPayload = J.toJSON actionWebhookPayload
      url = (T.unpack $ unResolvedWebhook resolvedWebhook)
  httpResponse <- liftIO $ try $ try $
                  Wreq.asJSON =<< Wreq.postWith options url postPayload
  case httpResponse of
    Left e ->
      throw500WithDetail "http exception when calling webhook" $
      J.toJSON $ HttpException e
    Right (Left (Wreq.JSONError e)) ->
      throw500WithDetail "not a valid json response from webhook" $
      J.toJSON e
    Right (Right responseWreq) -> do
      let responseValue = responseWreq ^. Wreq.responseBody
          responseStatus = responseWreq ^. Wreq.responseStatus

      if | HTTP.statusIsSuccessful responseStatus  -> pure responseValue

         | HTTP.statusIsClientError responseStatus -> do
             ActionWebhookErrorResponse message maybeCode <-
               modifyErr ("webhook response: " <>) $ decodeValue responseValue
             let code = maybe Unexpected ActionWebhookCode maybeCode
                 qErr = QErr [] responseStatus message code Nothing
             throwError qErr

         | otherwise ->
             throw500WithDetail "internal error" $
               J.object ["webhook_response" J..= responseValue]

data ActionLogItem
  = ActionLogItem
  { _aliId               :: !UUID.UUID
  , _aliActionName       :: !ActionName
  , _aliRequestHeaders   :: ![HTTP.Header]
  , _aliSessionVariables :: !UserVars
  , _aliInputPayload     :: !J.Value
  } deriving (Show, Eq)

asyncActionsProcessor
  :: HasVersion
  => IORef (RebuildableSchemaCache Run, SchemaCacheVer)
  -> Q.PGPool
  -> HTTP.Manager
  -> IO ()
asyncActionsProcessor cacheRef pgPool httpManager = forever $ do
  asyncInvocations <- getUndeliveredEvents
  actionCache <- scActions . lastBuiltSchemaCache . fst <$> readIORef cacheRef
  A.mapConcurrently_ (callHandler actionCache) asyncInvocations
  threadDelay (1 * 1000 * 1000)
  where
    getActionDefinition actionCache actionName =
      _aiDefinition <$> Map.lookup actionName actionCache

    runTx :: (Monoid a) => Q.TxE QErr a -> IO a
    runTx q = do
      res <- runExceptT $ Q.runTx' pgPool q
      either mempty return res

    callHandler :: ActionCache -> ActionLogItem -> IO ()
    callHandler actionCache actionLogItem = do
      let ActionLogItem actionId actionName reqHeaders
            sessionVariables inputPayload = actionLogItem
      case getActionDefinition actionCache actionName of
        Nothing -> return ()
        Just definition -> do
          let webhookUrl = _adHandler definition
              forwardClientHeaders = _adForwardClientHeaders definition
              confHeaders = _adHeaders definition
              actionContext = ActionContext actionName
          res <- runExceptT $ callWebhook httpManager reqHeaders confHeaders forwardClientHeaders webhookUrl $
            ActionWebhookPayload actionContext sessionVariables inputPayload
          case res of
            Left e                -> setError actionId e
            Right responsePayload -> setCompleted actionId responsePayload

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

      -- map uncurryEvent <$>
      --   Q.listQE defaultTxErrorHandler [Q.sql|
      --     update hdb_catalog.hdb_action_log set status = 'processing'
      --     where
      --       id in (
      --         select id from hdb_catalog.hdb_action_log
      --         where status = 'created'
      --         for update skip locked limit 10
      --       ) returning action_name, session_variables, input_payload
      --   |]




resolveActionInsert
  :: ( HasVersion
     , MonadReusability m
     , MonadError QErr m
     , MonadReader r m
     , MonadIO m
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     , Has HTTP.Manager r
     , Has [HTTP.Header] r
     )
  => Field
  -> ActionExecutionContext
  -- We need the sesion variables for column presets
  -> UserVars
  -> m RespTx
resolveActionInsert field executionContext sessionVariables =
  case executionContext of
    ActionExecutionSyncWebhook executionContextSync ->
      resolveActionInsertSync field executionContextSync sessionVariables
    ActionExecutionAsync actionFilter ->
      resolveActionInsertAsync field actionFilter sessionVariables

resolveActionInsertAsync
  :: ( MonadError QErr m, MonadReader r m
     , Has [HTTP.Header] r
     )
  => Field
  -> AnnBoolExpPartialSQL
  -- We need the sesion variables for column presets
  -> UserVars
  -> m RespTx
resolveActionInsertAsync field _ sessionVariables = do

  responseSelectionSet <- resolveResponseSelectionSet (_fType field) $ _fSelSet field
  reqHeaders <- asks getter
  let inputArgs = J.toJSON $ fmap annInpValueToJson $ _fArguments field

  -- resolvedPresetFields <- resolvePresetFields

  -- The order of the union doesn't matter as the allowed input
  -- and the present fields are mutually exclusive
  -- let actionInput = OMap.union inputArgs resolvedPresetFields

  -- resolvedFilter <- resolveFilter
  let resolvedFilter = annBoolExpTrue

  return $ do
    actionId <- runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler [Q.sql|
      INSERT INTO
          "hdb_catalog"."hdb_action_log"
          ("action_name", "session_variables", "request_headers", "input_payload", "status")
      VALUES
          ($1, $2, $3, $4, $5)
      RETURNING "id"
              |]
      (actionName, Q.AltJ sessionVariables, Q.AltJ $ toHeadersMap reqHeaders, Q.AltJ inputArgs, "created"::Text) False

    actionSelectToTx $
      ActionSelect (S.SELit $ UUID.toText actionId)
      responseSelectionSet resolvedFilter
  where
    actionName = G.unName $ _fName field
    toHeadersMap = Map.fromList . map ((bsToTxt . CI.original) *** bsToTxt)

    -- resolveFilter =
    --   flip traverseAnnBoolExp (_aiocSelectFilter insertContext) $ \case
    --   PSESQLExp e -> return e
    --   PSESessVar variableTy sessVar -> do
    --     sessionVariableValueExp <- S.SELit <$> fetchSessionVariableValue sessVar
    --     return $ undefined variableTy sessionVariableValueExp

    -- fetchSessionVariableValue sessionVariable =
    --   onNothing (getVarVal sessionVariable sessionVariables) $
    --   throw500 $ "missing required session variable: " <> sessionVariable

    -- resolvePresetFields =
    --   fmap OMap.fromList $ forM (Map.toList $ _aiocPresetFields insertContext) $
    --   \(k, v) -> (unActionInputField k,) <$> case v of
    --   Left sessVariable -> J.toJSON <$> fetchSessionVariableValue sessVariable
    --   Right scalarValue -> return scalarValue


annInpValueToJson :: AnnInpVal -> J.Value
annInpValueToJson annInpValue =
  case _aivValue annInpValue of
    AGScalar _ pgColumnValueM -> maybe J.Null pgScalarValueToJson pgColumnValueM
    AGEnum _ enumValue        -> case enumValue of
      AGESynthetic enumValueM   -> J.toJSON enumValueM
      AGEReference _ enumValueM -> J.toJSON enumValueM
    AGObject _ objectM        -> J.toJSON $ fmap (fmap annInpValueToJson) objectM
    AGArray _ valuesM         -> J.toJSON $ fmap (fmap annInpValueToJson) valuesM

resolveAsyncResponse
  :: ( MonadReusability m
     , MonadError QErr m
     , MonadReader r m
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     )
  => ActionSelectOpContext
  -> Field
  -> m GRS.AnnSimpleSelect
resolveAsyncResponse selectContext field = do
  actionId <- withArg (_fArguments field) "id" parseActionId
  stringifyNumerics <- stringifyNum <$> asks getter
  annotatedFields <- forM (toList $ _fSelSet field) $ \fld -> do
    let fldName = _fName fld
    let rqlFldName = FieldName $ G.unName $ G.unAlias $ _fAlias fld
    (rqlFldName,) <$> case fldName of
      "__typename" -> return $ RS.FExp $ G.unName $ G.unNamedType $ _fType field
      "output"     -> do
        let relationshipFromExp =
              mkJsonToRecordFromExpression (_asocDefinitionList selectContext) $
              -- TODO: An absolute hack, please fix this
              RS.AEInput $ UVSQL $ S.mkQIdenExp (Iden "_0_root.base") (Iden "response_payload")
        outputSelect <- processOutputSelectionSet relationshipFromExp (_fType fld)
                        (_fSelSet fld)
        return $ RS.FObj $ RS.AnnRelG outputRelName mempty outputSelect
      -- the metadata columns
      "id"         -> return $ mkAnnFldFromPGCol "id" PGUUID
      "created_at" -> return $ mkAnnFldFromPGCol "created_at" PGTimeStampTZ
      -- "status"     -> return $ mkAnnFldFromPGCol "status"
      "errors"     -> return $ mkAnnFldFromPGCol "errors" PGJSONB
      G.Name t     -> throw500 $ "unexpected field in actions' httpResponse : " <> t
  let tableFromExp = RS.FromTable actionLogTable
      tableArguments = RS.noTableArgs
                       { RS._taWhere = Just $ mkTableBoolExpression actionId}
      tablePermissions = RS.TablePerm unresolvedFilter Nothing
      selectAstUnresolved = RS.AnnSelG annotatedFields tableFromExp tablePermissions
                            tableArguments stringifyNumerics
  return selectAstUnresolved

  -- astResolved <- RS.traverseAnnSimpleSel resolveValTxt selectAstUnresolved
  -- return $ asSingleRowJsonResp (RS.selectQuerySQL True astResolved) []
  where
    outputRelName = RelName $ mkNonEmptyTextUnsafe "output"
    actionLogTable =
      QualifiedObject (SchemaName "hdb_catalog") (TableName "hdb_action_log")
    mkAnnFldFromPGCol column columnType =
      flip RS.mkAnnColField Nothing $
      PGColumnInfo (unsafePGCol column) (G.Name column) (PGColumnScalar columnType) True Nothing
    unresolvedFilter =
      fmapAnnBoolExp partialSQLExpToUnresolvedVal $
      _asocFilter selectContext
    parseActionId annInpValue = do
      mkParameterizablePGValue <$> asPGColumnValue annInpValue
    mkTableBoolExpression actionId =
      BoolFld $ AVCol
      (PGColumnInfo (unsafePGCol "id") "id" (PGColumnScalar PGUUID) False Nothing) $
      pure $ AEQ True actionId
    mkJsonToRecordFromExpression definitionList webhookResponseExpression =
      let functionName = QualifiedObject (SchemaName "pg_catalog") $
                         FunctionName "jsonb_to_record"
          functionArgs = RS.FunctionArgsExp
                         (pure webhookResponseExpression)
                         mempty
      in RS.FromFunction functionName functionArgs
            (Just definitionList)
