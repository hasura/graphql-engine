module Hasura.GraphQL.Resolve.Action
  ( resolveActionSelect
  , resolveActionInsert
  -- , resolveResponseSelectionSet

  , ActionSelect(..)
  , traverseActionSelect
  , actionSelectToSql
  ) where

import           Data.Has
import           Hasura.Prelude

import qualified Data.Aeson                        as J
import qualified Data.UUID                         as UUID
import qualified Database.PG.Query                 as Q
import qualified Language.GraphQL.Draft.Syntax     as G

import qualified Hasura.SQL.DML                    as S

import           Hasura.RQL.DML.Internal           (dmlTxErrorHandler)

import           Hasura.RQL.DML.Select             (asSingleRowJsonResp)

import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.InputValue
import           Hasura.GraphQL.Validate.Field
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value                  (pgScalarValueToJson)

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

    G.Name t     -> throw500 $ "unexpected field in actions' response : " <> t

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
actionSelectToSql (ActionSelect actionIdExp selection filter) =
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

resolveActionInsertSync
  :: ( MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => Field
  -> Text
  -- We need the sesion variables for column presets
  -> UserVars
  -> m RespTx
resolveActionInsertSync field executionContext sessionVariables =
  throw500 "sync actions not yet implemented"

resolveActionInsert
  :: ( MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => Field
  -> ActionExecutionContext
  -- We need the sesion variables for column presets
  -> UserVars
  -> m RespTx
resolveActionInsert field executionContext sessionVariables =
  case executionContext of
    ActionExecutionSyncWebhook webhook ->
      resolveActionInsertSync field webhook sessionVariables
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
