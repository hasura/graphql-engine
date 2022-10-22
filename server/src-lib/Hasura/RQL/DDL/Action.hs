{-# LANGUAGE TemplateHaskell #-}

module Hasura.RQL.DDL.Action
  ( CreateAction (..),
    runCreateAction,
    resolveAction,
    UpdateAction,
    runUpdateAction,
    DropAction,
    runDropAction,
    dropActionInMetadata,
    CreateActionPermission (..),
    runCreateActionPermission,
    DropActionPermission,
    runDropActionPermission,
    dropActionPermissionInMetadata,
    caName,
    caDefinition,
    caComment,
    uaName,
    uaDefinition,
    uaComment,
  )
where

import Control.Lens (makeLenses, (.~), (^.))
import Data.Aeson qualified as J
import Data.Aeson.TH qualified as J
import Data.Dependent.Map qualified as DMap
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.List.NonEmpty qualified as NEList
import Data.Text.Extended
import Data.URL.Template (printURLTemplate)
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.RQL.DDL.CustomTypes (lookupPGScalar)
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.CustomTypes
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.SQL.Tag
import Hasura.Session
import Language.GraphQL.Draft.Syntax qualified as G

getActionInfo ::
  (QErrM m, CacheRM m) =>
  ActionName ->
  m ActionInfo
getActionInfo actionName = do
  actionMap <- scActions <$> askSchemaCache
  onNothing (Map.lookup actionName actionMap) $
    throw400 NotExists $ "action with name " <> actionName <<> " does not exist"

data CreateAction = CreateAction
  { _caName :: !ActionName,
    _caDefinition :: !ActionDefinitionInput,
    _caComment :: !(Maybe Text)
  }

$(makeLenses ''CreateAction)

$(J.deriveJSON hasuraJSON ''CreateAction)

runCreateAction ::
  (QErrM m, CacheRWM m, MetadataM m) =>
  CreateAction ->
  m EncJSON
runCreateAction createAction = do
  -- check if action with same name exists already
  actionMap <- scActions <$> askSchemaCache
  void $
    onJust (Map.lookup actionName actionMap) $
      const $
        throw400 AlreadyExists $
          "action with name " <> actionName <<> " already exists"
  let metadata =
        ActionMetadata
          actionName
          (_caComment createAction)
          (_caDefinition createAction)
          []
  buildSchemaCacheFor (MOAction actionName) $
    MetadataModifier $
      metaActions %~ OMap.insert actionName metadata
  pure successMsg
  where
    actionName = _caName createAction

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

resolveAction ::
  QErrM m =>
  Env.Environment ->
  AnnotatedCustomTypes ->
  ActionDefinitionInput ->
  DMap.DMap BackendTag ScalarSet -> -- See Note [Postgres scalars in custom types]
  m
    ( ResolvedActionDefinition,
      AnnotatedOutputType
    )
resolveAction env AnnotatedCustomTypes {..} ActionDefinition {..} allScalars = do
  resolvedArguments <- forM _adArguments $ \argumentDefinition -> do
    forM argumentDefinition $ \argumentType -> do
      let gType = unGraphQLType argumentType
          argumentBaseType = G.getBaseType gType
      (gType,)
        <$> if
            | Just noCTScalar <- lookupPGScalar allScalars argumentBaseType (NOCTScalar . ASTReusedScalar argumentBaseType) ->
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
  outputObject <- do
    aot <-
      if
          | Just aoTScalar <- lookupPGScalar allScalars outputBaseType (AOTScalar . ASTReusedScalar outputBaseType) -> do
            pure aoTScalar
          | Just objectType <- Map.lookup outputBaseType _actObjects ->
            pure $ AOTObject objectType
          | otherwise ->
            throw400 NotExists ("the type: " <> dquote outputBaseType <> " is not an object type defined in custom types")
    -- If the Action is sync:
    --      1. Check if the output type has only top level relations (if any)
    --   If the Action is async:
    --      1. Check that the output type has no relations if the output type contains nested objects
    -- These checks ensure that the SQL we generate for the join does not have to extract nested fields
    -- from the action webhook response.
    let (nestedObjects, scalarOrEnumFields) = case aot of
          AOTObject aot' ->
            NEList.partition
              ( \ObjectFieldDefinition {..} ->
                  case snd _ofdType of
                    AOFTScalar _ -> False
                    AOFTEnum _ -> False
                    AOFTObject _ -> True
              )
              (_otdFields $ _aotDefinition aot')
          AOTScalar _ -> ([], [])
        scalarOrEnumFieldNames = fmap (\ObjectFieldDefinition {..} -> unObjectFieldName _ofdName) scalarOrEnumFields
        validateSyncAction = case aot of
          AOTObject aot' ->
            onJust (_otdRelationships $ _aotDefinition aot') $ \relLst -> do
              let relationshipsWithNonTopLevelFields =
                    NEList.filter
                      ( \TypeRelationship {..} ->
                          let objsInRel = unObjectFieldName <$> Map.keys _trFieldMapping
                           in not $ all (`elem` scalarOrEnumFieldNames) objsInRel
                      )
                      relLst
              unless (null relationshipsWithNonTopLevelFields) $
                throw400 ConstraintError $
                  "Relationships cannot be defined with nested object fields : "
                    <> commaSeparated (dquote . _trName <$> relationshipsWithNonTopLevelFields)
          AOTScalar _ -> pure ()
    case _adType of
      ActionQuery -> validateSyncAction
      ActionMutation ActionSynchronous -> validateSyncAction
      ActionMutation ActionAsynchronous ->
        let maybeRel = case aot of
              AOTObject aot' -> isJust (_otdRelationships $ _aotDefinition aot')
              AOTScalar _ -> False
         in when (maybeRel && not (null nestedObjects)) $
              throw400 ConstraintError $
                "Async action relations cannot be used with object fields : " <> commaSeparated (dquote . _ofdName <$> nestedObjects)
    pure aot
  resolvedWebhook <- resolveWebhook env _adHandler
  let webhookEnvRecord = EnvRecord (printURLTemplate $ unInputWebhook _adHandler) resolvedWebhook
  pure
    ( ActionDefinition
        resolvedArguments
        _adOutputType
        _adType
        _adHeaders
        _adForwardClientHeaders
        _adTimeout
        webhookEnvRecord
        _adRequestTransform
        _adResponseTransform,
      outputObject
    )

data UpdateAction = UpdateAction
  { _uaName :: !ActionName,
    _uaDefinition :: !ActionDefinitionInput,
    _uaComment :: !(Maybe Text)
  }

$(makeLenses ''UpdateAction)
$(J.deriveFromJSON hasuraJSON ''UpdateAction)

runUpdateAction ::
  forall m.
  (QErrM m, CacheRWM m, MetadataM m) =>
  UpdateAction ->
  m EncJSON
runUpdateAction (UpdateAction actionName actionDefinition actionComment) = do
  sc <- askSchemaCache
  let actionsMap = scActions sc
  void $
    onNothing (Map.lookup actionName actionsMap) $
      throw400 NotExists $ "action with name " <> actionName <<> " does not exist"
  buildSchemaCacheFor (MOAction actionName) $ updateActionMetadataModifier actionDefinition actionComment
  pure successMsg
  where
    updateActionMetadataModifier :: ActionDefinitionInput -> Maybe Text -> MetadataModifier
    updateActionMetadataModifier def comment =
      MetadataModifier $
        (metaActions . ix actionName . amDefinition .~ def)
          . (metaActions . ix actionName . amComment .~ comment)

newtype ClearActionData = ClearActionData {unClearActionData :: Bool}
  deriving (Show, Eq, J.FromJSON, J.ToJSON)

shouldClearActionData :: ClearActionData -> Bool
shouldClearActionData = unClearActionData

defaultClearActionData :: ClearActionData
defaultClearActionData = ClearActionData True

data DropAction = DropAction
  { _daName :: !ActionName,
    _daClearData :: !(Maybe ClearActionData)
  }
  deriving (Show, Eq)

$(J.deriveJSON hasuraJSON ''DropAction)

runDropAction ::
  ( CacheRWM m,
    MetadataM m,
    MonadMetadataStorageQueryAPI m
  ) =>
  DropAction ->
  m EncJSON
runDropAction (DropAction actionName clearDataM) = do
  void $ getActionInfo actionName
  withNewInconsistentObjsCheck $
    buildSchemaCache $
      dropActionInMetadata actionName
  when (shouldClearActionData clearData) $ deleteActionData actionName
  return successMsg
  where
    -- When clearData is not present we assume that
    -- the data needs to be retained
    clearData = fromMaybe defaultClearActionData clearDataM

dropActionInMetadata :: ActionName -> MetadataModifier
dropActionInMetadata name =
  MetadataModifier $ metaActions %~ OMap.delete name

newtype ActionMetadataField = ActionMetadataField {unActionMetadataField :: Text}
  deriving (Show, Eq, J.FromJSON, J.ToJSON)

doesActionPermissionExist :: Metadata -> ActionName -> RoleName -> Bool
doesActionPermissionExist metadata actionName roleName =
  any ((== roleName) . _apmRole) $ metadata ^. (metaActions . ix actionName . amPermissions)

data CreateActionPermission = CreateActionPermission
  { _capAction :: !ActionName,
    _capRole :: !RoleName,
    _capDefinition :: !(Maybe J.Value),
    _capComment :: !(Maybe Text)
  }

$(J.deriveFromJSON hasuraJSON ''CreateActionPermission)

runCreateActionPermission ::
  (QErrM m, CacheRWM m, MetadataM m) =>
  CreateActionPermission ->
  m EncJSON
runCreateActionPermission createActionPermission = do
  metadata <- getMetadata
  when (doesActionPermissionExist metadata actionName roleName) $
    throw400 AlreadyExists $
      "permission for role " <> roleName
        <<> " is already defined on " <>> actionName
  buildSchemaCacheFor (MOActionPermission actionName roleName) $
    MetadataModifier $
      metaActions . ix actionName . amPermissions
        %~ (:) (ActionPermissionMetadata roleName comment)
  pure successMsg
  where
    CreateActionPermission actionName roleName _ comment = createActionPermission

data DropActionPermission = DropActionPermission
  { _dapAction :: !ActionName,
    _dapRole :: !RoleName
  }
  deriving (Show, Eq)

$(J.deriveJSON hasuraJSON ''DropActionPermission)

runDropActionPermission ::
  (QErrM m, CacheRWM m, MetadataM m) =>
  DropActionPermission ->
  m EncJSON
runDropActionPermission dropActionPermission = do
  metadata <- getMetadata
  unless (doesActionPermissionExist metadata actionName roleName) $
    throw400 NotExists $
      "permission for role: " <> roleName <<> " is not defined on " <>> actionName
  buildSchemaCacheFor (MOActionPermission actionName roleName) $
    dropActionPermissionInMetadata actionName roleName
  return successMsg
  where
    actionName = _dapAction dropActionPermission
    roleName = _dapRole dropActionPermission

dropActionPermissionInMetadata :: ActionName -> RoleName -> MetadataModifier
dropActionPermissionInMetadata name role =
  MetadataModifier $
    metaActions . ix name . amPermissions %~ filter ((/=) role . _apmRole)
