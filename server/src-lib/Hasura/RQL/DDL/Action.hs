{-# LANGUAGE RecordWildCards #-}
module Hasura.RQL.DDL.Action
  ( CreateAction
  , runCreateAction
  , persistCreateAction
  , resolveAction

  , UpdateAction
  , runUpdateAction

  , DropAction
  , runDropAction
  , deleteActionFromCatalog

  , fetchActions

  , CreateActionPermission
  , runCreateActionPermission
  , persistCreateActionPermission

  , DropActionPermission
  , runDropActionPermission
  , deleteActionPermissionFromCatalog
  ) where

import           Hasura.EncJSON
import           Hasura.GraphQL.Utils
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Data.Aeson                    as J
import qualified Data.Aeson.Casing             as J
import qualified Data.Aeson.TH                 as J
import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
import qualified Data.Text                     as T
import qualified Database.PG.Query             as Q
import qualified Language.GraphQL.Draft.Syntax as G

import           Data.URL.Template             (renderURLTemplate)
import           Language.Haskell.TH.Syntax    (Lift)

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
  :: (QErrM m , CacheRWM m, MonadTx m)
  => CreateAction -> m EncJSON
runCreateAction createAction = do
  -- check if action with same name exists already
  actionMap <- scActions <$> askSchemaCache
  void $ onJust (Map.lookup actionName actionMap) $ const $
    throw400 AlreadyExists $
      "action with name " <> actionName <<> " already exists"
  persistCreateAction createAction
  buildSchemaCacheFor $ MOAction actionName
  pure successMsg
  where
    actionName = _caName createAction

persistCreateAction :: (MonadTx m) =>  CreateAction -> m ()
persistCreateAction (CreateAction actionName actionDefinition comment) = do
  liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
    INSERT into hdb_catalog.hdb_action
      (action_name, action_defn, comment)
      VALUES ($1, $2, $3)
  |] (actionName, Q.AltJ actionDefinition, comment) True

{- Note [Postgres scalars in action input arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's very comfortable to be able to reference Postgres scalars in actions
input arguments. For example, see the following action mutation:

    extend type mutation_root {
      create_user (
        name: String!
        created_at: timestamptz
      ): User
    }

The timestamptz is a Postgres scalar. We need to validate the presence of
timestamptz type in the Postgres database. So, the 'resolveAction' function
takes all Postgres scalar types as one of the inputs and returns the set of
referred scalars.
-}

resolveAction
  :: (QErrM m, MonadIO m)
  => AnnotatedCustomTypes
  -> ActionDefinitionInput
  -> HashSet PGScalarType -- See Note [Postgres scalars in custom types]
  -> m ( ResolvedActionDefinition
       , AnnotatedObjectType
       )
resolveAction AnnotatedCustomTypes{..} ActionDefinition{..} allPGScalars = do
  resolvedArguments <- forM _adArguments $ \argumentDefinition -> do
    forM argumentDefinition $ \argumentType -> do
      let gType = unGraphQLType argumentType
          argumentBaseType = G.getBaseType gType
      (gType,) <$>
        if | Just pgScalar <- lookupPGScalar argumentBaseType ->
               pure $ NOCTScalar pgScalar
           | Just nonObjectType <- Map.lookup argumentBaseType _actNonObjects ->
               pure nonObjectType
           | otherwise ->
               throw400 InvalidParams $
               "the type: " <> showName argumentBaseType
               <> " is not defined in custom types or it is not a scalar/enum/input_object"

  -- Check if the response type is an object
  let outputType = unGraphQLType _adOutputType
      outputBaseType = G.getBaseType outputType
  outputObject <- onNothing (Map.lookup outputBaseType _actObjects) $
    throw400 NotExists $ "the type: " <> showName outputBaseType
    <> " is not an object type defined in custom types"
  resolvedWebhook <- resolveWebhook _adHandler
  pure ( ActionDefinition resolvedArguments _adOutputType _adType
         _adHeaders _adForwardClientHeaders resolvedWebhook
       , outputObject
       )
  where
    resolveWebhook (InputWebhook urlTemplate) = do
      eitherRenderedTemplate <- renderURLTemplate urlTemplate
      either (throw400 Unexpected . T.pack) (pure . ResolvedWebhook) eitherRenderedTemplate

    lookupPGScalar baseType = -- see Note [Postgres scalars in custom types]
      fmap (flip ScalarTypeDefinition Nothing) $
      find ((==) baseType) $ mapMaybe (G.mkName . toSQLTxt) $
      toList allPGScalars

runUpdateAction
  :: forall m. ( QErrM m , CacheRWM m, MonadTx m)
  => UpdateAction -> m EncJSON
runUpdateAction (UpdateAction actionName actionDefinition) = do
  sc <- askSchemaCache
  let actionsMap = scActions sc
  void $ onNothing (Map.lookup actionName actionsMap) $
    throw400 NotExists $ "action with name " <> actionName <<> " not exists"
  updateActionInCatalog
  buildSchemaCacheFor $ MOAction actionName
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
  :: (QErrM m, CacheRWM m, MonadTx m)
  => DropAction -> m EncJSON
runDropAction (DropAction actionName clearDataM)= do
  void $ getActionInfo actionName
  liftTx $ do
    deleteActionPermissionsFromCatalog
    deleteActionFromCatalog actionName clearDataM
  buildSchemaCacheStrict
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
       ORDER BY action_name ASC
     |] () True
  where
    fromRow (actionName, Q.AltJ definition, comment) =
      CreateAction actionName definition comment

newtype ActionMetadataField
  = ActionMetadataField { unActionMetadataField :: Text }
  deriving (Show, Eq, J.FromJSON, J.ToJSON)

runCreateActionPermission
  :: (QErrM m , CacheRWM m, MonadTx m)
  => CreateActionPermission -> m EncJSON
runCreateActionPermission createActionPermission = do
  actionInfo <- getActionInfo actionName
  void $ onJust (Map.lookup role $ _aiPermissions actionInfo) $ const $
    throw400 AlreadyExists $ "permission for role " <> role
    <<> " is already defined on " <>> actionName
  persistCreateActionPermission createActionPermission
  buildSchemaCacheFor $ MOActionPermission actionName role
  pure successMsg
  where
    actionName = _capAction createActionPermission
    role = _capRole createActionPermission

persistCreateActionPermission :: (MonadTx m) => CreateActionPermission -> m ()
persistCreateActionPermission CreateActionPermission{..}= do
  liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
    INSERT into hdb_catalog.hdb_action_permission
      (action_name, role_name, comment)
      VALUES ($1, $2, $3)
  |] (_capAction, _capRole, _capComment) True

data DropActionPermission
  = DropActionPermission
  { _dapAction :: !ActionName
  , _dapRole   :: !RoleName
  } deriving (Show, Eq, Lift)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''DropActionPermission)

runDropActionPermission
  :: (QErrM m, CacheRWM m, MonadTx m)
  => DropActionPermission -> m EncJSON
runDropActionPermission dropActionPermission = do
  actionInfo <- getActionInfo actionName
  void $ onNothing (Map.lookup role $ _aiPermissions actionInfo) $
    throw400 NotExists $
    "permission for role: " <> role <<> " is not defined on " <>> actionName
  liftTx $ deleteActionPermissionFromCatalog actionName role
  buildSchemaCacheFor $ MOActionPermission actionName role
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
