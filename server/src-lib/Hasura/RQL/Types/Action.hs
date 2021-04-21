module Hasura.RQL.Types.Action
  ( ArgumentName(..)
  , ArgumentDefinition(..)

  , ActionName(..)
  , ActionMutationKind(..)
  , _ActionAsynchronous
  , ActionDefinition(..)
  , adArguments
  , adOutputType
  , adType
  , adForwardClientHeaders
  , adHeaders
  , adHandler
  , adTimeout
  , ActionType(..)
  , _ActionMutation
  , CreateAction(..)
  , UpdateAction(..)
  , ActionDefinitionInput
  , InputWebhook(..)

  , ResolvedWebhook(..)
  , ResolvedActionDefinition

  , ActionOutputFields
  , getActionOutputFields
  , ActionInfo(..)
  , aiName
  , aiOutputObject
  , aiDefinition
  , aiPermissions
  , aiForwardedClientHeaders
  , aiComment
  , defaultActionTimeoutSecs
  , ActionPermissionInfo(..)

  , ActionPermissionMap
  , CreateActionPermission(..)

  , ActionMetadata(..)
  , amName
  , amComment
  , amDefinition
  , amPermissions
  , ActionPermissionMetadata(..)

  , ActionSourceInfo(..)
  , getActionSourceInfo
  , AnnActionExecution(..)
  , traverseAnnActionExecution
  , AnnActionMutationAsync(..)
  , ActionExecContext(..)
  , AsyncActionQueryFieldG(..)
  , AnnActionAsyncQuery(..)
  , traverseAnnActionAsyncQuery

  , ActionId(..)
  , actionIdToText
  , ActionLogItem(..)
  , ActionLogResponse(..)
  , ActionLogResponseMap
  , AsyncActionStatus(..)
  , ActionsInfo(..)
  , asiName
  , asiForwardClientHeaders
  ) where


import           Hasura.Prelude

import qualified Data.Aeson                    as J
import qualified Data.Aeson.Casing             as J
import qualified Data.Aeson.TH                 as J
import qualified Data.HashMap.Strict           as Map
import qualified Data.Time.Clock               as UTC
import qualified Data.UUID                     as UUID
import qualified Database.PG.Query             as Q
import qualified Language.GraphQL.Draft.Syntax as G
import qualified Network.HTTP.Client           as HTTP
import qualified Network.HTTP.Types            as HTTP

import           Control.Lens                  (makeLenses, makePrisms)
import           Data.Text.Extended

import           Hasura.Incremental            (Cacheable)
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.IR.Select
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.CustomTypes
import           Hasura.RQL.Types.Error
import           Hasura.SQL.Backend
import           Hasura.Session


newtype ActionName
  = ActionName { unActionName :: G.Name }
  deriving ( Show, Eq, Ord, J.FromJSON, J.ToJSON, J.FromJSONKey, J.ToJSONKey
           , Hashable, ToTxt, Generic, NFData, Cacheable)

instance Q.FromCol ActionName where
  fromCol bs = do
    text <- Q.fromCol bs
    name <- G.mkName text `onNothing` Left (text <> " is not valid GraphQL name")
    pure $ ActionName name

instance Q.ToPrepArg ActionName where
  toPrepVal = Q.toPrepVal . G.unName . unActionName

data ActionMutationKind
  = ActionSynchronous
  | ActionAsynchronous
  deriving (Show, Eq, Generic)
instance NFData ActionMutationKind
instance Cacheable ActionMutationKind
$(J.deriveJSON
  J.defaultOptions { J.constructorTagModifier = J.snakeCase . drop 6}
  ''ActionMutationKind)
$(makePrisms ''ActionMutationKind)

newtype ArgumentName
  = ArgumentName { unArgumentName :: G.Name }
  deriving ( Show, Eq, J.FromJSON, J.ToJSON, J.FromJSONKey, J.ToJSONKey
           , Hashable, ToTxt, Generic, NFData, Cacheable)

data ArgumentDefinition a
  = ArgumentDefinition
  { _argName        :: !ArgumentName
  , _argType        :: !a
  , _argDescription :: !(Maybe G.Description)
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic)
instance (NFData a) => NFData (ArgumentDefinition a)
instance (Cacheable a) => Cacheable (ArgumentDefinition a)
$(J.deriveJSON hasuraJSON ''ArgumentDefinition)

data ActionType
  = ActionQuery
  | ActionMutation !ActionMutationKind
  deriving (Show, Eq, Generic)
instance NFData ActionType
instance Cacheable ActionType
$(makePrisms ''ActionType)

data ActionDefinition a b
  = ActionDefinition
  { _adArguments            :: ![a]
  , _adOutputType           :: !GraphQLType
  , _adType                 :: !ActionType
  , _adHeaders              :: ![HeaderConf]
  , _adForwardClientHeaders :: !Bool
  , _adTimeout              :: !Timeout
  -- ^ If the timeout is not provided by the user, then
  -- the default timeout of 30 seconds will be used
  , _adHandler              :: !b
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic)
instance (NFData a, NFData b) => NFData (ActionDefinition a b)
instance (Cacheable a, Cacheable b) => Cacheable (ActionDefinition a b)
$(makeLenses ''ActionDefinition)

instance (J.FromJSON a, J.FromJSON b) => J.FromJSON (ActionDefinition a b) where
  parseJSON = J.withObject "ActionDefinition" $ \o -> do
    _adArguments <- o J..:? "arguments" J..!= []
    _adOutputType <- o J..: "output_type"
    _adHeaders <- o J..:? "headers" J..!= []
    _adForwardClientHeaders <- o J..:? "forward_client_headers" J..!= False
    _adHandler <- o J..:  "handler"
    _adTimeout <- o J..:? "timeout" J..!= defaultActionTimeoutSecs
    actionType <- o J..:? "type" J..!= "mutation"
    _adType <- case actionType of
      "mutation" -> ActionMutation <$> o J..:? "kind" J..!= ActionSynchronous
      "query"    -> pure ActionQuery
      t          -> fail $ "expected mutation or query, but found " <> t
    return ActionDefinition {..}

instance (J.ToJSON a, J.ToJSON b) => J.ToJSON (ActionDefinition a b) where
  toJSON (ActionDefinition args outputType actionType headers forwardClientHeaders timeout handler) =
    let typeAndKind = case actionType of
          ActionQuery -> [ "type" J..= ("query" :: String)]
          ActionMutation kind -> [ "type" J..= ("mutation" :: String)
                                 , "kind" J..= kind]
    in J.object $
    [ "arguments"              J..= args
    , "output_type"            J..= outputType
    , "headers"                J..= headers
    , "forward_client_headers" J..= forwardClientHeaders
    , "handler"                J..= handler
    , "timeout"                J..= timeout
    ] <> typeAndKind

type ResolvedActionDefinition =
  ActionDefinition (ArgumentDefinition (G.GType, NonObjectCustomType)) ResolvedWebhook

data ActionPermissionInfo
  = ActionPermissionInfo
  { _apiRole   :: !RoleName
  } deriving (Show, Eq)
$(J.deriveToJSON hasuraJSON ''ActionPermissionInfo)

type ActionPermissionMap = Map.HashMap RoleName ActionPermissionInfo

type ActionOutputFields = Map.HashMap G.Name G.GType

getActionOutputFields :: AnnotatedObjectType -> ActionOutputFields
getActionOutputFields =
  Map.fromList . map ( (unObjectFieldName . _ofdName) &&& (fst . _ofdType)) . toList . _otdFields . _aotDefinition

data ActionInfo
  = ActionInfo
  { _aiName                   :: !ActionName
  , _aiOutputObject           :: !(G.GType, AnnotatedObjectType)
  , _aiDefinition             :: !ResolvedActionDefinition
  , _aiPermissions            :: !ActionPermissionMap
  , _aiForwardedClientHeaders :: !Bool
  , _aiComment                :: !(Maybe Text)
  } deriving (Generic)
instance J.ToJSON ActionInfo where
  toJSON = J.genericToJSON hasuraJSON
$(makeLenses ''ActionInfo)

type ActionDefinitionInput =
  ActionDefinition (ArgumentDefinition GraphQLType) InputWebhook

data CreateAction
  = CreateAction
  { _caName       :: !ActionName
  , _caDefinition :: !ActionDefinitionInput
  , _caComment    :: !(Maybe Text)
  } deriving (Show, Eq, Generic)
instance NFData CreateAction
instance Cacheable CreateAction
$(J.deriveJSON hasuraJSON ''CreateAction)

data UpdateAction
  = UpdateAction
  { _uaName       :: !ActionName
  , _uaDefinition :: !ActionDefinitionInput
  , _uaComment    :: !(Maybe Text)
  } deriving (Show, Eq)
$(J.deriveJSON hasuraJSON ''UpdateAction)

data CreateActionPermission
  = CreateActionPermission
  { _capAction     :: !ActionName
  , _capRole       :: !RoleName
  , _capDefinition :: !(Maybe J.Value)
  , _capComment    :: !(Maybe Text)
  } deriving (Show, Eq, Generic)
instance NFData CreateActionPermission
instance Cacheable CreateActionPermission
$(J.deriveJSON hasuraJSON ''CreateActionPermission)

-- representation of action permission metadata
data ActionPermissionMetadata
  = ActionPermissionMetadata
  { _apmRole    :: !RoleName
  , _apmComment :: !(Maybe Text)
  } deriving (Show, Eq, Generic)
instance NFData ActionPermissionMetadata
instance Cacheable ActionPermissionMetadata

$(J.deriveJSON
  hasuraJSON{J.omitNothingFields=True}
  ''ActionPermissionMetadata)

-- representation of action metadata
data ActionMetadata
  = ActionMetadata
  { _amName        :: !ActionName
  , _amComment     :: !(Maybe Text)
  , _amDefinition  :: !ActionDefinitionInput
  , _amPermissions :: ![ActionPermissionMetadata]
  } deriving (Show, Eq, Generic)
$(J.deriveToJSON hasuraJSON ''ActionMetadata)
$(makeLenses ''ActionMetadata)
instance NFData ActionMetadata
instance Cacheable ActionMetadata

instance J.FromJSON ActionMetadata where
  parseJSON = J.withObject "Object" $ \o ->
    ActionMetadata
      <$> o J..: "name"
      <*> o J..:? "comment"
      <*> o J..: "definition"
      <*> o J..:? "permissions" J..!= []

----------------- Resolve Types ----------------

data ActionSourceInfo b
  = ASINoSource -- ^ No relationships defined on the action output object
  | ASISource !SourceName !(SourceConfig b) -- ^ All relationships refer to tables in one source

getActionSourceInfo :: AnnotatedObjectType -> ActionSourceInfo 'Postgres
getActionSourceInfo = maybe ASINoSource (uncurry ASISource) . _aotSource

data AnnActionExecution (b :: BackendType) v
  = AnnActionExecution
  { _aaeName                 :: !ActionName
  , _aaeOutputType           :: !GraphQLType -- ^ output type
  , _aaeFields               :: !(AnnFieldsG b v) -- ^ output selection
  , _aaePayload              :: !J.Value -- ^ jsonified input arguments
  , _aaeOutputFields         :: !ActionOutputFields
  -- ^ to validate the response fields from webhook
  , _aaeDefinitionList       :: ![(Column b, ScalarType b)]
  , _aaeWebhook              :: !ResolvedWebhook
  , _aaeHeaders              :: ![HeaderConf]
  , _aaeForwardClientHeaders :: !Bool
  , _aaeStrfyNum             :: !Bool
  , _aaeTimeOut              :: !Timeout
  , _aaeSource               :: !(ActionSourceInfo b)
  }

traverseAnnActionExecution
  :: (Applicative f, Backend backend)
  => (a -> f b)
  -> AnnActionExecution backend a
  -> f (AnnActionExecution backend b)
traverseAnnActionExecution f (AnnActionExecution n ot fs p oF dl w h fch sn to s) =
  traverse (traverse $ traverseAnnField f) fs <&> \tfs -> AnnActionExecution n ot tfs p oF dl w h fch sn to s

data AnnActionMutationAsync
  = AnnActionMutationAsync
  { _aamaName                 :: !ActionName
  , _aamaForwardClientHeaders :: !Bool
  , _aamaPayload              :: !J.Value -- ^ jsonified input arguments
  } deriving (Show, Eq)

data AsyncActionQueryFieldG (b :: BackendType) v
  = AsyncTypename !Text
  | AsyncOutput !(AnnFieldsG b v)
  | AsyncId
  | AsyncCreatedAt
  | AsyncErrors

traverseAsyncActionQueryField
  :: (Applicative f, Backend backend)
  => (a -> f b)
  -> AsyncActionQueryFieldG backend a
  -> f (AsyncActionQueryFieldG backend b)
traverseAsyncActionQueryField f = \case
  AsyncTypename t    -> pure $ AsyncTypename t
  AsyncOutput fields -> AsyncOutput <$> traverse (traverse $ traverseAnnField f) fields
  AsyncId            -> pure AsyncId
  AsyncCreatedAt     -> pure AsyncCreatedAt
  AsyncErrors        -> pure AsyncErrors

type AsyncActionQueryFieldsG b v = Fields (AsyncActionQueryFieldG b v)

data AnnActionAsyncQuery (b :: BackendType) v
  = AnnActionAsyncQuery
  { _aaaqName                 :: !ActionName
  , _aaaqActionId             :: !ActionId
  , _aaaqOutputType           :: !GraphQLType
  , _aaaqFields               :: !(AsyncActionQueryFieldsG b v)
  , _aaaqDefinitionList       :: ![(Column b, ScalarType b)]
  , _aaaqStringifyNum         :: !Bool
  , _aaaqForwardClientHeaders :: !Bool
  , _aaaqSource               :: !(ActionSourceInfo b)
  }

traverseAnnActionAsyncQuery
  :: (Applicative f, Backend backend)
  => (a -> f b)
  -> AnnActionAsyncQuery backend a
  -> f (AnnActionAsyncQuery backend b)
traverseAnnActionAsyncQuery f (AnnActionAsyncQuery n aid ot fs dl sn fch s) =
  traverse (traverse $ traverseAsyncActionQueryField f) fs <&> \tfs -> AnnActionAsyncQuery n aid ot tfs dl sn fch s

data ActionExecContext
  = ActionExecContext
  { _aecManager          :: !HTTP.Manager
  , _aecHeaders          :: !HTTP.RequestHeaders
  , _aecSessionVariables :: !SessionVariables
  }

newtype ActionId = ActionId {unActionId :: UUID.UUID}
  deriving (Show, Eq, Q.ToPrepArg, Q.FromCol, J.ToJSON, J.FromJSON, Hashable)

actionIdToText :: ActionId -> Text
actionIdToText = UUID.toText . unActionId

data ActionLogItem
  = ActionLogItem
  { _aliId               :: !ActionId
  , _aliActionName       :: !ActionName
  , _aliRequestHeaders   :: ![HTTP.Header]
  , _aliSessionVariables :: !SessionVariables
  , _aliInputPayload     :: !J.Value
  } deriving (Show, Eq)

data ActionLogResponse
  = ActionLogResponse
  { _alrId               :: !ActionId
  , _alrCreatedAt        :: !UTC.UTCTime
  , _alrResponsePayload  :: !(Maybe J.Value)
  , _alrErrors           :: !(Maybe J.Value)
  , _alrSessionVariables :: !SessionVariables
  } deriving (Show, Eq)
$(J.deriveJSON hasuraJSON ''ActionLogResponse)

type ActionLogResponseMap = HashMap ActionId ActionLogResponse

data AsyncActionStatus
  = AASCompleted !J.Value
  | AASError !QErr

data ActionsInfo
  = ActionsInfo
  { _asiName                 :: !ActionName
  , _asiForwardClientHeaders :: !Bool
  }
  deriving (Show, Eq, Generic)
$(makeLenses ''ActionsInfo)
