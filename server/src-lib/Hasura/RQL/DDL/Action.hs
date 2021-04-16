module Hasura.RQL.DDL.Action
  ( CreateAction
  , runCreateAction
  , resolveAction

  , UpdateAction
  , runUpdateAction

  , DropAction
  , runDropAction
  , dropActionInMetadata

  , CreateActionPermission
  , runCreateActionPermission

  , DropActionPermission
  , runDropActionPermission
  , dropActionPermissionInMetadata
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                    as J
import qualified Data.Aeson.TH                 as J
import qualified Data.Dependent.Map            as DMap
import qualified Data.Environment              as Env
import qualified Data.HashMap.Strict           as Map
import qualified Data.HashMap.Strict.InsOrd    as OMap
import qualified Language.GraphQL.Draft.Syntax as G

import           Control.Lens                  ((.~))
import           Data.Text.Extended

import           Hasura.EncJSON
import           Hasura.Metadata.Class
import           Hasura.RQL.DDL.CustomTypes    (lookupPGScalar)
import           Hasura.RQL.Types
import           Hasura.SQL.Tag
import           Hasura.Session


getActionInfo
  :: (QErrM m, CacheRM m)
  => ActionName -> m ActionInfo
getActionInfo actionName = do
  actionMap <- scActions <$> askSchemaCache
  onNothing (Map.lookup actionName actionMap) $
    throw400 NotExists $ "action with name " <> actionName <<> " does not exist"

runCreateAction
  :: (QErrM m , CacheRWM m, MetadataM m)
  => CreateAction -> m EncJSON
runCreateAction createAction = do
  -- check if action with same name exists already
  actionMap <- scActions <$> askSchemaCache
  void $ onJust (Map.lookup actionName actionMap) $ const $
    throw400 AlreadyExists $
      "action with name " <> actionName <<> " already exists"
  let metadata = ActionMetadata actionName (_caComment createAction)
                 (_caDefinition createAction) []
  buildSchemaCacheFor (MOAction actionName)
    $ MetadataModifier
    $ metaActions %~ OMap.insert actionName metadata
  pure successMsg
  where
    actionName = _caName createAction

{-| Note [Postgres scalars in action input arguments]
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
  :: QErrM m
  => Env.Environment
  -> AnnotatedCustomTypes
  -> ActionDefinitionInput
  -> DMap.DMap BackendTag ScalarSet -- See Note [Postgres scalars in custom types]
  -> m ( ResolvedActionDefinition
       , AnnotatedObjectType
       )
resolveAction env AnnotatedCustomTypes{..} ActionDefinition{..} allScalars = do
  resolvedArguments <- forM _adArguments $ \argumentDefinition -> do
    forM argumentDefinition $ \argumentType -> do
      let gType = unGraphQLType argumentType
          argumentBaseType = G.getBaseType gType
      (gType,) <$>
        if | Just noCTScalar <- lookupPGScalar allScalars argumentBaseType (NOCTScalar . ASTReusedScalar argumentBaseType) ->
               pure noCTScalar
           | Just nonObjectType <- Map.lookup argumentBaseType _actNonObjects ->
               pure nonObjectType
           | otherwise ->
               throw400 InvalidParams $
               "the type: " <> dquote argumentBaseType
               <> " is not defined in custom types or it is not a scalar/enum/input_object"

  -- Check if the response type is an object
  let outputType = unGraphQLType _adOutputType
      outputBaseType = G.getBaseType outputType
  outputObject <- onNothing (Map.lookup outputBaseType _actObjects) $
    throw400 NotExists $ "the type: " <> dquote outputBaseType
    <> " is not an object type defined in custom types"
  resolvedWebhook <- resolveWebhook env _adHandler
  pure ( ActionDefinition resolvedArguments _adOutputType _adType
         _adHeaders _adForwardClientHeaders _adTimeout resolvedWebhook
       , outputObject
       )

runUpdateAction
  :: forall m. ( QErrM m , CacheRWM m, MetadataM m)
  => UpdateAction -> m EncJSON
runUpdateAction (UpdateAction actionName actionDefinition actionComment) = do
  sc <- askSchemaCache
  let actionsMap = scActions sc
  void $ onNothing (Map.lookup actionName actionsMap) $
    throw400 NotExists $ "action with name " <> actionName <<> " not exists"
  buildSchemaCacheFor (MOAction actionName) $ updateActionMetadataModifier actionDefinition actionComment
  pure successMsg
    where
      updateActionMetadataModifier :: ActionDefinitionInput -> Maybe Text -> MetadataModifier
      updateActionMetadataModifier def comment = MetadataModifier $
        (metaActions.ix actionName.amDefinition .~ def) .
        (metaActions.ix actionName.amComment .~ comment)

newtype ClearActionData
  = ClearActionData { unClearActionData :: Bool }
  deriving (Show, Eq, J.FromJSON, J.ToJSON)

shouldClearActionData :: ClearActionData -> Bool
shouldClearActionData = unClearActionData

defaultClearActionData :: ClearActionData
defaultClearActionData = ClearActionData True

data DropAction
  = DropAction
  { _daName      :: !ActionName
  , _daClearData :: !(Maybe ClearActionData)
  } deriving (Show, Eq)
$(J.deriveJSON hasuraJSON ''DropAction)

runDropAction
  :: ( CacheRWM m
     , MetadataM m
     , MonadMetadataStorageQueryAPI m
     )
  => DropAction -> m EncJSON
runDropAction (DropAction actionName clearDataM)= do
  void $ getActionInfo actionName
  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ dropActionInMetadata actionName
  when (shouldClearActionData clearData) $ deleteActionData actionName
  return successMsg
  where
    -- When clearData is not present we assume that
    -- the data needs to be retained
    clearData = fromMaybe defaultClearActionData clearDataM

dropActionInMetadata :: ActionName -> MetadataModifier
dropActionInMetadata name =
  MetadataModifier $ metaActions %~ OMap.delete name

newtype ActionMetadataField
  = ActionMetadataField { unActionMetadataField :: Text }
  deriving (Show, Eq, J.FromJSON, J.ToJSON)

runCreateActionPermission
  :: (QErrM m , CacheRWM m, MetadataM m)
  => CreateActionPermission -> m EncJSON
runCreateActionPermission createActionPermission = do
  actionInfo <- getActionInfo actionName
  void $ onJust (Map.lookup roleName $ _aiPermissions actionInfo) $ const $
    throw400 AlreadyExists $ "permission for role " <> roleName
    <<> " is already defined on " <>> actionName
  buildSchemaCacheFor (MOActionPermission actionName roleName)
    $ MetadataModifier
    $ metaActions.ix actionName.amPermissions
      %~ (:) (ActionPermissionMetadata roleName comment)
  pure successMsg
  where
    CreateActionPermission actionName roleName _ comment = createActionPermission

data DropActionPermission
  = DropActionPermission
  { _dapAction :: !ActionName
  , _dapRole   :: !RoleName
  } deriving (Show, Eq)
$(J.deriveJSON hasuraJSON ''DropActionPermission)

runDropActionPermission
  :: (QErrM m, CacheRWM m, MetadataM m)
  => DropActionPermission -> m EncJSON
runDropActionPermission dropActionPermission = do
  actionInfo <- getActionInfo actionName
  void $ onNothing (Map.lookup roleName $ _aiPermissions actionInfo) $
    throw400 NotExists $
    "permission for role: " <> roleName <<> " is not defined on " <>> actionName
  buildSchemaCacheFor (MOActionPermission actionName roleName) $
    dropActionPermissionInMetadata actionName roleName
  return successMsg
  where
    actionName = _dapAction dropActionPermission
    roleName = _dapRole dropActionPermission

dropActionPermissionInMetadata :: ActionName -> RoleName -> MetadataModifier
dropActionPermissionInMetadata name role = MetadataModifier $
    metaActions.ix name.amPermissions %~ filter ((/=) role . _apmRole)
