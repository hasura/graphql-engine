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
import qualified Data.HashMap.Strict               as Map
import qualified Data.Text                         as T
import qualified Data.UUID                         as UUID
import qualified Database.PG.Query                 as Q
import qualified Language.GraphQL.Draft.Syntax     as G
import qualified Network.HTTP.Client               as HTTP
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
import           Hasura.RQL.DML.Select             (asSingleRowJsonResp)
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value                  (PGScalarValue (..),
                                                    pgScalarValueToJson,
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
  :: ( MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => G.NamedType
  -> SelSet
  -> m [(Text, OutputFieldResolved)]
resolveOutputSelectionSet ty selSet =
  withSelSet selSet $ \fld -> case _fName fld of
    "__typename" -> return $ OutputFieldTypename ty
    G.Name t     -> return $ OutputFieldSimple t

resolveResponseSelectionSet
  :: ( MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
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
    mkMetadataField = ResponseFieldMetadata . PGCol


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
actionSelectToSql (ActionSelect actionIdExp selection actionFilter) =
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
        actionIdColumn = PGCol "id"

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
        outputColumn = S.SEIden $ toIden $ PGCol "response_payload"

    usingJsonBuildObj :: [(Text, a)] -> (a -> S.SQLExp) -> S.SQLExp
    usingJsonBuildObj l f =
      S.applyJsonBuildObj $ flip concatMap l $
      \(alias, field) -> [S.SELit alias, f field]


resolveActionSelect
  :: ( MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r, MonadResolve m
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

data ActionWebhookPayload
  = ActionWebhookPayload
  { _awpSessionVariables :: !UserVars
  , _awpInput            :: !J.Value
  } deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''ActionWebhookPayload)

data ActionWebhookResponse
  = ActionWebhookResponse
  { _awrData   :: !(Maybe J.Value)
  , _awrErrors :: !(Maybe J.Value)
  } deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''ActionWebhookResponse)

data ResolvePlan
  = ResolveReturn
  | ResolvePostgres [(PGCol, PGScalarType)] ![(Text, OutputFieldResolved)]
  deriving (Show, Eq)

processOutputSelectionSet
  :: ( MonadResolve m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => RS.FromExpression UnresolvedVal
  -> G.NamedType -> SelSet -> m GRS.AnnSimpleSelect
processOutputSelectionSet fromExpression fldTy flds = do
  stringifyNumerics <- stringifyNum <$> asks getter
  annotatedFields <- processTableSelectionSet fldTy flds
  let selectAst = RS.AnnSelG annotatedFields fromExpression
                  RS.noTablePermissions RS.noTableArgs stringifyNumerics
  return selectAst

resolveActionInsertSync
  :: ( MonadError QErr m, MonadReader r m, Has FieldMap r
     , MonadResolve m
     , Has OrdByCtx r, Has SQLGenCtx r
     , Has HTTP.Manager r
     , MonadIO m
     )
  => Field
  -> SyncActionExecutionContext
  -> UserVars
  -> m RespTx
resolveActionInsertSync field executionContext sessionVariables = do
  let inputArgs = J.toJSON $ fmap annInpValueToJson $ _fArguments field
      handlerPayload = ActionWebhookPayload sessionVariables inputArgs
  manager <- asks getter
  webhookRes <- callWebhook manager resolvedWebhook handlerPayload
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
    resolvedWebhook = _saecWebhook executionContext
    returnStrategy = _saecStrategy executionContext

    mkJsonToRecordFromExpression definitionList webhookResponseExpression =
      let functionName = QualifiedObject (SchemaName "pg_catalog") $
                         FunctionName "json_to_record"
          functionArgs = RS.FunctionArgsExp
                         (pure $ UVSQL webhookResponseExpression)
                         mempty
      in RS.FromExpressionFunction functionName functionArgs
            (Just definitionList)

callWebhook
  :: (MonadIO m, MonadError QErr m)
  => HTTP.Manager -> ResolvedWebhook -> ActionWebhookPayload -> m J.Value
callWebhook manager resolvedWebhook actionWebhookPayload = do
  let options = wreqOptions manager [contentType]
      contentType = ("Content-Type", "application/json")
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
    Right (Right responseWreq) ->
      let response = responseWreq ^. Wreq.responseBody
      in case (_awrData response, _awrErrors response) of
      (Nothing, Nothing) ->
        throw500WithDetail "internal error" $
        J.String "webhook response has neither 'data' nor 'errors'"
      (Just _, Just _) ->
        throw500WithDetail "internal error" $
        J.String "webhook response cannot have both 'data' and 'errors'"
      (Just d, Nothing) -> return d
      (Nothing, Just e) -> throwVE $ T.pack $ show e

data ActionLogItem
  = ActionLogItem
  { _aliId               :: !UUID.UUID
  , _aliActionName       :: !ActionName
  , _aliSessionVariables :: !UserVars
  , _aliInputPayload     :: !J.Value
  } deriving (Show, Eq)

asyncActionsProcessor
  :: IORef (SchemaCache, SchemaCacheVer)
  -> Q.PGPool
  -> HTTP.Manager
  -> IO ()
asyncActionsProcessor cacheRef pgPool httpManager = forever $ do
  asyncInvocations <- getUndeliveredEvents
  actionCache <- scActions . fst <$> readIORef cacheRef
  A.mapConcurrently_ (callHandler actionCache) asyncInvocations
  threadDelay (1 * 1000 * 1000)
  where
    getActionWebhook actionCache actionName =
      _adWebhook . _aiDefinition <$> Map.lookup actionName actionCache

    runTx :: (Monoid a) => Q.TxE QErr a -> IO a
    runTx q = do
      res <- runExceptT $ Q.runTx' pgPool q
      either mempty return res

    callHandler :: ActionCache -> ActionLogItem -> IO ()
    callHandler actionCache actionLogItem = do
      let ActionLogItem actionId actionName
            sessionVariables inputPayload = actionLogItem
      case getActionWebhook actionCache actionName of
        Nothing -> return ()
        Just webhookUrl -> do
          res <- runExceptT $ callWebhook httpManager webhookUrl $
            ActionWebhookPayload sessionVariables inputPayload
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
          id, action_name, session_variables::json, input_payload::json
      |] () False
     where
       mapEvent (actionId, actionName,
                 Q.AltJ sessionVariables, Q.AltJ inputPayload) =
         ActionLogItem actionId actionName sessionVariables inputPayload

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
  :: ( MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r, Has HTTP.Manager r
     , MonadIO m
     , MonadResolve m
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
  :: ( MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => Field
  -> AnnBoolExpPartialSQL
  -- We need the sesion variables for column presets
  -> UserVars
  -> m RespTx
resolveActionInsertAsync field actionFilter sessionVariables = do

  responseSelectionSet <- resolveResponseSelectionSet (_fType field) $ _fSelSet field
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
          ("action_name", "session_variables", "input_payload", "status")
      VALUES
          ($1, $2, $3, $4)
      RETURNING "id"
              |]
      (actionName, Q.AltJ sessionVariables, Q.AltJ inputArgs, "created"::Text) False

    actionSelectToTx $
      ActionSelect (S.SELit $ UUID.toText actionId)
      responseSelectionSet resolvedFilter
  where
    actionName = G.unName $ _fName field

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
  :: ( MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r, MonadResolve m
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
              UVSQL $ S.mkQIdenExp (Iden "_0_root.base") (Iden "response_payload")
        outputSelect <- processOutputSelectionSet relationshipFromExp (_fType fld)
                        (_fSelSet fld)
        return $ RS.FObj $ RS.AnnRelG outputRelName [] outputSelect
      -- the metadata columns
      "id"         -> return $ mkAnnFldFromPGCol "id" PGUUID
      "created_at" -> return $ mkAnnFldFromPGCol "created_at" PGTimeStampTZ
      -- "status"     -> return $ mkAnnFldFromPGCol "status"
      "errors"     -> return $ mkAnnFldFromPGCol "errors" PGJSONB
      G.Name t     -> throw500 $ "unexpected field in actions' httpResponse : " <> t
  let tableFromExp = RS.FromExpressionTable actionLogTable
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
      RS.FCol (PGCol column, PGColumnScalar columnType) Nothing
    unresolvedFilter =
      fmapAnnBoolExp partialSQLExpToUnresolvedVal $
      _asocFilter selectContext
    parseActionId annInpValue = do
      mkParameterizablePGValue <$> asPGColumnValue annInpValue
    mkTableBoolExpression actionId =
      BoolFld $ AVCol
      (PGColumnInfo (PGCol "id") "id" (PGColumnScalar PGUUID) False Nothing) $
      pure $ AEQ True actionId
    mkJsonToRecordFromExpression definitionList webhookResponseExpression =
      let functionName = QualifiedObject (SchemaName "pg_catalog") $
                         FunctionName "jsonb_to_record"
          functionArgs = RS.FunctionArgsExp
                         (pure webhookResponseExpression)
                         mempty
      in RS.FromExpressionFunction functionName functionArgs
            (Just definitionList)
