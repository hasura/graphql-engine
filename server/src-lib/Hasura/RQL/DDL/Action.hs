module Hasura.RQL.DDL.Action
  ( CreateAction
  , validateAndCacheAction
  , runCreateAction
  , runCreateAction_

  , UpdateAction
  , runUpdateAction

  , DropAction
  , runDropAction
  , deleteActionFromCatalog

  , fetchActions

  , CreateActionPermission
  , validateAndCacheActionPermission
  , runCreateActionPermission
  , runCreateActionPermission_

  , DropActionPermission
  , runDropActionPermission
  , deleteActionPermissionFromCatalog
  ) where

import           Hasura.EncJSON
import           Hasura.GraphQL.Context        (defaultTypes)
import           Hasura.GraphQL.Utils
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Hasura.GraphQL.Validate.Types as VT

import qualified Data.Aeson                    as J
import qualified Data.Aeson.Casing             as J
import qualified Data.Aeson.TH                 as J
import qualified Data.HashMap.Strict           as Map
import qualified Data.Text                     as T
import qualified Database.PG.Query             as Q
import qualified Language.GraphQL.Draft.Syntax as G

import           Data.URL.Template             (renderURLTemplate)
import           Language.Haskell.TH.Syntax    (Lift)
-- data RetryConf
--   = RetryConf
--   { _rcNumRetries  :: !Word64
--   , _rcIntervalSec :: !Word64
--   , _rcTimeoutSec  :: !(Maybe Word64)
--   } deriving (Show, Eq, Lift)

-- data WebhookConf
--   = WebhookConf
--   { _wcUrl     :: !Text
--   , _wcTimeout :: !Word64
--   , _wcRetry   :: !RetryConf
--   } deriving (Show, Eq)

getActionInfo
  :: (QErrM m, CacheRM m)
  => ActionName -> m ActionInfo
getActionInfo actionName = do
  actionMap <- scActions <$> askSchemaCache
  case Map.lookup actionName actionMap of
    Just actionInfo -> return actionInfo
    Nothing         ->
      throw400 NotExists $
      "action with name " <> actionName <<> " does not exist"

runCreateAction
  :: ( QErrM m, UserInfoM m
     , CacheRWM m, MonadTx m
     , MonadIO m
     )
  => CreateAction -> m EncJSON
runCreateAction q = do
  adminOnly
  runCreateAction_ q
  return successMsg

runCreateAction_
  :: (QErrM m , CacheRWM m, MonadTx m, MonadIO m)
  => CreateAction -> m ()
runCreateAction_ q@(CreateAction actionName actionDefinition comment) = do
  validateAndCacheAction q
  persistCreateAction
  where
    persistCreateAction :: (MonadTx m) => m ()
    persistCreateAction = do
      liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
        INSERT into hdb_catalog.hdb_action
          (action_name, action_defn, comment)
          VALUES ($1, $2, $3)
      |] (actionName, Q.AltJ actionDefinition, comment) True

validateAndCacheAction
  :: (QErrM m, CacheRWM m, MonadIO m)
  => CreateAction -> m ()
validateAndCacheAction q = do
  actionMap <- scActions <$> askSchemaCache
  onJust (Map.lookup actionName actionMap) $
    const $ throw400 AlreadyExists $
    "action with name " <> actionName <<> " already exists"
  actionInfo <- buildActionInfo actionName actionDefinition
  addActionToCache actionInfo
  where
    actionName  = _caName q
    actionDefinition = _caDefinition q

buildActionInfo
  :: (QErrM m, CacheRM m, MonadIO m)
  => ActionName -> ActionDefinitionInput -> m ActionInfo
buildActionInfo actionName actionDefinition = do
  let responseType = unGraphQLType $ _adOutputType actionDefinition
      responseBaseType = G.getBaseType responseType
  forM (_adArguments actionDefinition) $ \argument -> do
    let argumentBaseType = G.getBaseType $ unGraphQLType $ _argType argument
    argTypeInfo <- getNonObjectTypeInfo argumentBaseType
    case argTypeInfo of
      VT.TIScalar _ -> return ()
      VT.TIEnum _   -> return ()
      VT.TIInpObj _ -> return ()
      _ -> throw400 InvalidParams $ "the argument's base type: "
          <> showNamedTy argumentBaseType <>
          " should be a scalar/enum/input_object"
  when (hasList responseType) $ throw400 InvalidParams $
    "the output type: " <> G.showGT responseType <> " cannot be a list"

  getObjectTypeInfo responseBaseType
  -- TODO: validate the output type
  -- responseTypeInfo <- getNonObjectTypeInfo responseBaseType
  -- case responseTypeInfo of
  --   VT.TIScalar typeInfo -> return $ ActionOutputScalar typeInfo
  --   VT.TIEnum typeInfo   -> return $ ActionOutputEnum typeInfo
  --   VT.TIObj typeInfo    -> return $ ActionOutputObject typeInfo
  --   _ -> throw400 InvalidParams $ "the output type: " <>
  --        showNamedTy responseBaseType <>
  --        " should be a scalar/enum/object"
  resolvedDefinition <- traverse resolveWebhook actionDefinition
  return $ ActionInfo actionName resolvedDefinition mempty
  where
    getNonObjectTypeInfo typeName = do
      nonObjectTypeMap <- (unNonObjectTypeMap . fst . scCustomTypes) <$> askSchemaCache
      let inputTypeInfos = nonObjectTypeMap <> VT.mapFromL VT.getNamedTy defaultTypes
      onNothing (Map.lookup typeName inputTypeInfos) $
        throw400 NotExists $ "the type: " <> showNamedTy typeName <>
        " is not defined in custom types"

    resolveWebhook = \case
      IWPlain t           -> pure $ ResolvedWebhook t
      IWTemplate template -> do
        eitherRenderedTemplate <- renderURLTemplate template
        either (throw400 Unexpected . T.pack) (pure . ResolvedWebhook) eitherRenderedTemplate

    getObjectTypeInfo typeName = do
      customTypes <- (snd . scCustomTypes) <$> askSchemaCache
      onNothing (Map.lookup (ObjectTypeName typeName) customTypes) $
        throw400 NotExists $ "the type: "
        <> showNamedTy typeName <>
        " is not an object type defined in custom types"

    hasList = \case
      G.TypeList _ _  -> True
      G.TypeNamed _ _ -> False

runUpdateAction
  :: forall m. ( QErrM m, UserInfoM m
               , CacheRWM m, MonadTx m
               , MonadIO m
               )
  => UpdateAction -> m EncJSON
runUpdateAction (UpdateAction actionName actionDefinition) = do
  adminOnly
  sc <- askSchemaCache
  let actionsMap = scActions sc
  actionPerms <- fmap _aiPermissions $ onNothing (Map.lookup actionName actionsMap) $
                 throw400 NotExists $ "action with name " <> actionName <<> " not exists"
  newActionInfo <- buildActionInfo actionName actionDefinition
  -- FIXME:- This is not ideal implementation of updating ActionInfo.
  -- With incremental schema build PR (https://github.com/hasura/graphql-engine/pull/3394) going in
  -- the logic here would be just updating the catalog with definition and incrementally
  -- building schema cache for action
  let newActionInfoPerms = newActionInfo{_aiPermissions = actionPerms}
  writeSchemaCache sc{scActions = Map.insert actionName newActionInfoPerms actionsMap}
  updateActionInCatalog
  pure successMsg
  where
    updateActionInCatalog :: m ()
    updateActionInCatalog =
      liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
        UPDATE hdb_catalog.hdb_action
           SET action_defn = $2
         WHERE action_name = $1
      |] (actionName, Q.AltJ actionDefinition) True

newtype ClearActionData
  = ClearActionData { unClearActionData :: Bool }
  deriving (Show, Eq, Lift, J.FromJSON, J.ToJSON)

shouldClearActionData :: ClearActionData -> Bool
shouldClearActionData = unClearActionData

defaultClearActionData :: ClearActionData
defaultClearActionData = ClearActionData True

data DropAction
  = DropAction
  { _daName      :: !ActionName
  , _daClearData :: !(Maybe ClearActionData)
  } deriving (Show, Eq, Lift)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''DropAction)

runDropAction
  :: (QErrM m, UserInfoM m, CacheRWM m, MonadTx m)
  => DropAction -> m EncJSON
runDropAction (DropAction actionName clearDataM)= do
  adminOnly
  void $ getActionInfo actionName
  delActionFromCache actionName
  liftTx $ do
    deleteActionPermissionsFromCatalog
    deleteActionFromCatalog actionName clearDataM
  return successMsg
  where
    deleteActionPermissionsFromCatalog =
      Q.unitQE defaultTxErrorHandler [Q.sql|
          DELETE FROM hdb_catalog.hdb_action_permission
            WHERE action_name = $1
          |] (Identity actionName) True

deleteActionFromCatalog
  :: ActionName
  -> Maybe ClearActionData
  -> Q.TxE QErr ()
deleteActionFromCatalog actionName clearDataM = do
  Q.unitQE defaultTxErrorHandler [Q.sql|
      DELETE FROM hdb_catalog.hdb_action
        WHERE action_name = $1
      |] (Identity actionName) True
  when (shouldClearActionData clearData) $
    clearActionDataFromCatalog actionName
  where
    -- When clearData is not present we assume that
    -- the data needs to be retained
    clearData = fromMaybe defaultClearActionData clearDataM

clearActionDataFromCatalog :: ActionName -> Q.TxE QErr ()
clearActionDataFromCatalog actionName =
  Q.unitQE defaultTxErrorHandler [Q.sql|
      DELETE FROM hdb_catalog.hdb_action_log
        WHERE action_name = $1
      |] (Identity actionName) True

fetchActions :: Q.TxE QErr [CreateAction]
fetchActions =
  map fromRow <$> Q.listQE defaultTxErrorHandler
    [Q.sql|
     SELECT action_name, action_defn, comment
       FROM hdb_catalog.hdb_action
     |] () True
  where
    fromRow (actionName, Q.AltJ definition, comment) =
      CreateAction actionName definition comment

newtype ActionMetadataField
  = ActionMetadataField { unActionMetadataField :: Text }
  deriving (Show, Eq, J.FromJSON, J.ToJSON)


validateAndCacheActionPermission
  :: (QErrM m, CacheRWM m, MonadTx m)
  => CreateActionPermission -> m ()
validateAndCacheActionPermission createActionPermission = do
  actionInfo <- getActionInfo actionName
  onJust (Map.lookup role $ _aiPermissions actionInfo) $ \_ ->
    throw400 AlreadyExists $
    "permission for role: " <> role <<> " is already defined on " <>> actionName
  actionFilter <- buildActionFilter (_apdSelect permissionDefinition)
  addActionPermissionToCache actionName $
    ActionPermissionInfo role actionFilter
  where
    actionName = _capAction createActionPermission
    role = _capRole createActionPermission
    permissionDefinition = _capDefinition createActionPermission
    -- TODO
    buildActionFilter
      :: (QErrM m)
      => ActionPermissionSelect
      -> m AnnBoolExpPartialSQL
    buildActionFilter permission =
      return annBoolExpTrue

runCreateActionPermission_
  :: ( QErrM m , CacheRWM m, MonadTx m)
  => CreateActionPermission -> m ()
runCreateActionPermission_ createActionPermission = do
  validateAndCacheActionPermission createActionPermission
  persistCreateActionPermission
  where
    actionName = _capAction createActionPermission
    role = _capRole createActionPermission
    permissionDefinition = _capDefinition createActionPermission
    comment = _capComment createActionPermission

    persistCreateActionPermission :: (MonadTx m) => m ()
    persistCreateActionPermission = do
      liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
        INSERT into hdb_catalog.hdb_action_permission
          (action_name, role_name, definition, comment)
          VALUES ($1, $2, $3, $4)
      |] (actionName, role, Q.AltJ permissionDefinition, comment) True

runCreateActionPermission
  :: ( QErrM m, UserInfoM m
     , CacheRWM m, MonadTx m
     )
  => CreateActionPermission -> m EncJSON
runCreateActionPermission createActionPermission = do
  adminOnly
  runCreateActionPermission_ createActionPermission
  return successMsg

data DropActionPermission
  = DropActionPermission
  { _dapAction :: !ActionName
  , _dapRole   :: !RoleName
  -- , _capIfExists   :: !(Maybe IfExists)
  } deriving (Show, Eq, Lift)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''DropActionPermission)

runDropActionPermission
  :: ( QErrM m, UserInfoM m
     , CacheRWM m, MonadTx m
     )
  => DropActionPermission -> m EncJSON
runDropActionPermission dropActionPermission = do
  adminOnly
  actionInfo <- getActionInfo actionName
  void $ onNothing (Map.lookup role $ _aiPermissions actionInfo) $
    throw400 NotExists $
    "permission for role: " <> role <<> " is not defined on " <>> actionName
  delActionPermissionFromCache actionName role
  liftTx $ deleteActionPermissionFromCatalog actionName role
  return successMsg
  where
    actionName = _dapAction dropActionPermission
    role = _dapRole dropActionPermission

deleteActionPermissionFromCatalog :: ActionName -> RoleName -> Q.TxE QErr ()
deleteActionPermissionFromCatalog actionName role =
  Q.unitQE defaultTxErrorHandler [Q.sql|
      DELETE FROM hdb_catalog.hdb_action_permission
        WHERE action_name = $1
          AND role_name = $2
      |] (actionName, role) True
