module Hasura.GraphQL.Resolve.Action
  ( resolveActionSelect
  , resolveActionInsert
  -- , resolveResponseSelectionSet

  , ActionSelect(..)
  , traverseActionSelect
  , actionSelectToSql
  ) where

import           Hasura.Prelude

import           Control.Exception                 (try)
import           Control.Lens
import           Data.Has

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

import qualified Hasura.RQL.DML.Select             as RS
import qualified Hasura.SQL.DML                    as S

import           Hasura.EncJSON
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.InputValue
import           Hasura.GraphQL.Validate.Field
import           Hasura.GraphQL.Validate.Types
import           Hasura.HTTP
import           Hasura.RQL.DML.Internal           (dmlTxErrorHandler)
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
  => ObjTyInfo
  -> G.NamedType
  -> SelSet
  -> m [(Text, OutputFieldResolved)]
resolveOutputSelectionSet objTyInfo ty selSet =
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
      resolveOutputSelectionSet undefined (_fType fld) (_fSelSet fld)

    -- the metadata columns
    "id"         -> return $ mkMetadataField "id"
    "created_at" -> return $ mkMetadataField "created_at"
    "status"     -> return $ mkMetadataField "status"

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
      -- onNothing (UUID.fromText idText) $
      --   throwVE $ "invalid value for uuid: " <> idText

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

resolveActionInsertSync
  :: ( MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     , Has HTTP.Manager r
     )
  => Field
  -> ResolvedWebhook
  -> ActionOutputTypeInfo
  -> UserVars
  -> m RespTx
resolveActionInsertSync field resolvedWebhook outputTypeInfo sessionVariables = do
  inputArgs <- withArg (_fArguments field) "input" (return . annInpValueToJson)
  resolvePlan <- case outputTypeInfo of
    ActionOutputScalar _ -> return ResolveReturn
    ActionOutputEnum _ -> return ResolveReturn
    ActionOutputObject objTyInfo -> do
      let definitionList =
            flip zip (repeat PGJSON) $
            map (PGCol . G.unName . _fiName) $ Map.elems $ _otiFields objTyInfo
      ResolvePostgres definitionList <$>
        resolveOutputSelectionSet objTyInfo (_fType field) (_fSelSet field)
  manager <- asks getter
  stringifyNumerics <- stringifyNum <$> asks getter
  return $ do
    webhookRes <- callWebhook manager inputArgs
    returnResponse stringifyNumerics webhookRes resolvePlan
  where
    returnResponse stringifyNumerics webhookData = \case
      ResolveReturn -> return $ encJFromJValue webhookData
      ResolvePostgres definitionList selSet -> do
        let functionName = QualifiedObject (SchemaName "pg_catalog") $
                           FunctionName "json_to_record"
            functionArgs = RS.FunctionArgsExp
                           (pure $ toTxtValue $ WithScalarType PGJSON $
                            PGValJSON $ Q.JSON webhookData)
                           mempty
            fromExpression =
              RS.FromExpressionFunction functionName functionArgs
              (Just definitionList)
            annFields = flip map selSet $ \(alias, outputField) ->
              (FieldName alias,) $ case outputField of
              OutputFieldSimple fieldName ->
                -- TODO:
                RS.FCol ( PGCol fieldName
                        , PGColumnScalar PGJSON
                        ) Nothing
              OutputFieldTypename typeName ->
                RS.FExp $ G.unName $ G.unNamedType typeName
              OutputFieldRelationship -> undefined
        let selectAst = RS.AnnSelG annFields fromExpression
                        RS.noTablePermissions RS.noTableArgs stringifyNumerics
        asSingleRowJsonResp (RS.selectQuerySQL True selectAst) []

    callWebhook manager actionInput = do
      let options = wreqOptions manager [contentType]
          contentType = ("Content-Type", "application/json")
          postPayload = J.toJSON $ ActionWebhookPayload
                        sessionVariables actionInput
          url = (T.unpack $ unResolvedWebhook resolvedWebhook)
      httpResponse <- liftIO $ try $
                      Wreq.asJSON =<< Wreq.postWith options url postPayload
      case (^. Wreq.responseBody) <$> httpResponse of
        Left e ->
          throw500WithDetail "http exception when calling webhook" $
          J.toJSON $ HttpException e
        Right response -> case (_awrData response, _awrErrors response) of
          (Nothing, Nothing) ->
            throw500WithDetail "internal error" $
            J.String "webhook response has neither 'data' nor 'errors'"
          (Just _, Just _) ->
            throw500WithDetail "internal error" $
            J.String "webhook response cannot have both 'data' and 'errors'"
          (Just d, Nothing) -> return d
          (Nothing, Just e) -> throwVE $ T.pack $ show e

resolveActionInsert
  :: ( MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r, Has HTTP.Manager r
     )
  => Field
  -> ActionExecutionContext
  -- We need the sesion variables for column presets
  -> UserVars
  -> m RespTx
resolveActionInsert field executionContext sessionVariables =
  case executionContext of
    ActionExecutionSyncWebhook webhook outputTypeInfo ->
      resolveActionInsertSync field webhook outputTypeInfo sessionVariables
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

  inputArgs <- withArg (_fArguments field) "input" (return . annInpValueToJson)

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
