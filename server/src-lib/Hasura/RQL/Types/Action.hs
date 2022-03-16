{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.Action
  ( ArgumentName (..),
    ArgumentDefinition (..),
    ActionName (..),
    ActionMutationKind (..),
    _ActionAsynchronous,
    ActionDefinition (..),
    adArguments,
    adOutputType,
    adType,
    adForwardClientHeaders,
    adHeaders,
    adHandler,
    adTimeout,
    adRequestTransform,
    adResponseTransform,
    ActionType (..),
    _ActionMutation,
    ActionDefinitionInput,
    InputWebhook (..),
    ResolvedWebhook (..),
    ResolvedActionDefinition,
    ActionOutputFields,
    getActionOutputFields,
    ActionInfo (..),
    aiName,
    aiOutputType,
    aiDefinition,
    aiPermissions,
    aiForwardedClientHeaders,
    aiComment,
    defaultActionTimeoutSecs,
    ActionPermissionInfo (..),
    ActionPermissionMap,
    ActionMetadata (..),
    amName,
    amComment,
    amDefinition,
    amPermissions,
    ActionPermissionMetadata (..),
    ActionSourceInfo (..),
    getActionSourceInfo,
    AnnActionExecution (..),
    aaeName,
    aaeOutputType,
    aaeFields,
    aaePayload,
    aaeOutputFields,
    aaeWebhook,
    aaeHeaders,
    aaeForwardClientHeaders,
    aaeTimeOut,
    aaeRequestTransform,
    aaeResponseTransform,
    AnnActionMutationAsync (..),
    ActionExecContext (..),
    AsyncActionQueryFieldG (..),
    _AsyncTypename,
    _AsyncOutput,
    _AsyncId,
    _AsyncCreatedAt,
    _AsyncErrors,
    AnnActionAsyncQuery (..),
    aaaqName,
    aaaqActionId,
    aaaqOutputType,
    aaaqFields,
    aaaqDefinitionList,
    aaaqStringifyNum,
    aaaqForwardClientHeaders,
    aaaqSource,
    ActionId (..),
    LockedActionEventId,
    LockedActionIdArray (..),
    actionIdToText,
    ActionLogItem (..),
    ActionLogResponse (..),
    ActionLogResponseMap,
    AsyncActionStatus (..),
    ActionsInfo (..),
    asiName,
    asiForwardClientHeaders,
  )
where

import Control.Lens (makeLenses, makePrisms)
import Data.Aeson qualified as J
import Data.Aeson.Casing qualified as J
import Data.Aeson.Extended
import Data.Aeson.TH qualified as J
import Data.HashMap.Strict qualified as Map
import Data.Kind (Type)
import Data.Text.Extended
import Data.Time.Clock qualified as UTC
import Data.UUID qualified as UUID
import Database.PG.Query qualified as Q
import Database.PG.Query.PTI qualified as PTI
import Hasura.Base.Error
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.DDL.Headers
import Hasura.RQL.DDL.Webhook.Transform (MetadataResponseTransform, RequestTransform)
import Hasura.RQL.IR.Action
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.CustomTypes
import Hasura.RQL.Types.Eventing (EventId (..))
import Hasura.SQL.Backend
import Hasura.Session
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types qualified as HTTP
import PostgreSQL.Binary.Encoding qualified as PE

newtype ActionName = ActionName {unActionName :: G.Name}
  deriving
    ( Show,
      Eq,
      Ord,
      J.FromJSON,
      J.ToJSON,
      J.FromJSONKey,
      J.ToJSONKey,
      Hashable,
      ToTxt,
      Generic,
      NFData,
      Cacheable
    )

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

$( J.deriveJSON
     J.defaultOptions {J.constructorTagModifier = J.snakeCase . drop 6}
     ''ActionMutationKind
 )
$(makePrisms ''ActionMutationKind)

newtype ArgumentName = ArgumentName {unArgumentName :: G.Name}
  deriving
    ( Show,
      Eq,
      J.FromJSON,
      J.ToJSON,
      J.FromJSONKey,
      J.ToJSONKey,
      Hashable,
      ToTxt,
      Generic,
      NFData,
      Cacheable
    )

data ArgumentDefinition a = ArgumentDefinition
  { _argName :: !ArgumentName,
    _argType :: !a,
    _argDescription :: !(Maybe G.Description)
  }
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

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

data ActionDefinition a b = ActionDefinition
  { _adArguments :: ![a],
    _adOutputType :: !GraphQLType,
    _adType :: !ActionType,
    _adHeaders :: ![HeaderConf],
    _adForwardClientHeaders :: !Bool,
    -- | If the timeout is not provided by the user, then
    -- the default timeout of 30 seconds will be used
    _adTimeout :: !Timeout,
    _adHandler :: !b,
    _adRequestTransform :: !(Maybe RequestTransform),
    _adResponseTransform :: !(Maybe MetadataResponseTransform)
  }
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

instance (NFData a, NFData b) => NFData (ActionDefinition a b)

instance (Cacheable a, Cacheable b) => Cacheable (ActionDefinition a b)

$(makeLenses ''ActionDefinition)

instance (J.FromJSON a, J.FromJSON b) => J.FromJSON (ActionDefinition a b) where
  parseJSON = J.withObject "ActionDefinition" $ \o -> do
    _adArguments <- o J..:? "arguments" J..!= []
    _adOutputType <- o J..: "output_type"
    _adHeaders <- o J..:? "headers" J..!= []
    _adForwardClientHeaders <- o J..:? "forward_client_headers" J..!= False
    _adHandler <- o J..: "handler"
    _adTimeout <- o J..:? "timeout" J..!= defaultActionTimeoutSecs
    actionType <- o J..:? "type" J..!= "mutation"
    _adType <- case actionType of
      "mutation" -> ActionMutation <$> o J..:? "kind" J..!= ActionSynchronous
      "query" -> pure ActionQuery
      t -> fail $ "expected mutation or query, but found " <> t
    _adRequestTransform <- o J..:? "request_transform"
    _adResponseTransform <- o J..:? "response_transform"
    return ActionDefinition {..}

instance (J.ToJSON a, J.ToJSON b) => J.ToJSON (ActionDefinition a b) where
  toJSON (ActionDefinition args outputType actionType headers forwardClientHeaders timeout handler requestTransform responseTransform) =
    let typeAndKind = case actionType of
          ActionQuery -> ["type" J..= ("query" :: String)]
          ActionMutation kind ->
            [ "type" J..= ("mutation" :: String),
              "kind" J..= kind
            ]
     in J.object $
          [ "arguments" J..= args,
            "output_type" J..= outputType,
            "headers" J..= headers,
            "forward_client_headers" J..= forwardClientHeaders,
            "handler" J..= handler,
            "timeout" J..= timeout
          ]
            <> catMaybes
              [ ("request_transform" J..=) <$> requestTransform,
                ("response_transform" J..=) <$> responseTransform
              ]
            <> typeAndKind

type ResolvedActionDefinition =
  ActionDefinition (ArgumentDefinition (G.GType, NonObjectCustomType)) ResolvedWebhook

data ActionPermissionInfo = ActionPermissionInfo
  { _apiRole :: !RoleName
  }
  deriving (Show, Eq)

$(J.deriveToJSON hasuraJSON ''ActionPermissionInfo)

type ActionPermissionMap = Map.HashMap RoleName ActionPermissionInfo

type ActionOutputFields = Map.HashMap G.Name G.GType

getActionOutputFields :: AnnotatedOutputType -> ActionOutputFields
getActionOutputFields inp = case inp of
  AOTObject aot -> Map.fromList . map ((unObjectFieldName . _ofdName) &&& (fst . _ofdType)) . toList . _otdFields . _aotDefinition $ aot
  AOTScalar _ -> Map.empty

data ActionInfo = ActionInfo
  { _aiName :: !ActionName,
    _aiOutputType :: !(G.GType, AnnotatedOutputType),
    _aiDefinition :: !ResolvedActionDefinition,
    _aiPermissions :: !ActionPermissionMap,
    _aiForwardedClientHeaders :: !Bool,
    _aiComment :: !(Maybe Text)
  }
  deriving (Generic)

instance J.ToJSON ActionInfo where
  toJSON = J.genericToJSON hasuraJSON

$(makeLenses ''ActionInfo)

type ActionDefinitionInput =
  ActionDefinition (ArgumentDefinition GraphQLType) InputWebhook

-- representation of action permission metadata
data ActionPermissionMetadata = ActionPermissionMetadata
  { _apmRole :: !RoleName,
    _apmComment :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

instance NFData ActionPermissionMetadata

instance Cacheable ActionPermissionMetadata

$( J.deriveJSON
     hasuraJSON {J.omitNothingFields = True}
     ''ActionPermissionMetadata
 )

-- representation of action metadata
data ActionMetadata = ActionMetadata
  { _amName :: !ActionName,
    _amComment :: !(Maybe Text),
    _amDefinition :: !ActionDefinitionInput,
    _amPermissions :: ![ActionPermissionMetadata]
  }
  deriving (Show, Eq, Generic)

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
  = -- | No relationships defined on the action output object
    ASINoSource
  | -- | All relationships refer to tables in one source
    ASISource !SourceName !(SourceConfig b)

getActionSourceInfo :: AnnotatedOutputType -> ActionSourceInfo ('Postgres 'Vanilla)
getActionSourceInfo (AOTObject aot) = maybe ASINoSource (uncurry ASISource) . _aotSource $ aot
getActionSourceInfo (AOTScalar _) = ASINoSource

data AnnActionExecution (r :: Type) = AnnActionExecution
  { _aaeName :: !ActionName,
    -- | output type
    _aaeOutputType :: !GraphQLType,
    -- | output selection
    _aaeFields :: !(ActionFieldsG r),
    -- | jsonified input arguments
    _aaePayload :: !J.Value,
    -- | to validate the response fields from webhook
    _aaeOutputFields :: !ActionOutputFields,
    _aaeWebhook :: !ResolvedWebhook,
    _aaeHeaders :: ![HeaderConf],
    _aaeForwardClientHeaders :: !Bool,
    _aaeTimeOut :: !Timeout,
    _aaeRequestTransform :: !(Maybe RequestTransform),
    _aaeResponseTransform :: !(Maybe MetadataResponseTransform)
  }
  deriving stock (Functor, Foldable, Traversable)

data AnnActionMutationAsync = AnnActionMutationAsync
  { _aamaName :: !ActionName,
    _aamaForwardClientHeaders :: !Bool,
    -- | jsonified input arguments
    _aamaPayload :: !J.Value
  }
  deriving (Show, Eq)

data AsyncActionQueryFieldG (r :: Type)
  = AsyncTypename !Text
  | AsyncOutput !(ActionFieldsG r)
  | AsyncId
  | AsyncCreatedAt
  | AsyncErrors
  deriving stock (Functor, Foldable, Traversable)

type AsyncActionQueryFieldsG r = Fields (AsyncActionQueryFieldG r)

data AnnActionAsyncQuery (b :: BackendType) (r :: Type) = AnnActionAsyncQuery
  { _aaaqName :: !ActionName,
    _aaaqActionId :: !ActionId,
    _aaaqOutputType :: !GraphQLType,
    _aaaqFields :: !(AsyncActionQueryFieldsG r),
    _aaaqDefinitionList :: ![(Column b, ScalarType b)],
    _aaaqStringifyNum :: !StringifyNumbers,
    _aaaqForwardClientHeaders :: !Bool,
    _aaaqSource :: !(ActionSourceInfo b)
  }
  deriving stock (Functor, Foldable, Traversable)

data ActionExecContext = ActionExecContext
  { _aecManager :: !HTTP.Manager,
    _aecHeaders :: !HTTP.RequestHeaders,
    _aecSessionVariables :: !SessionVariables
  }

newtype ActionId = ActionId {unActionId :: UUID.UUID}
  deriving (Show, Eq, Q.ToPrepArg, Q.FromCol, J.ToJSON, J.FromJSON, Hashable)

actionIdToText :: ActionId -> Text
actionIdToText = UUID.toText . unActionId

data ActionLogItem = ActionLogItem
  { _aliId :: !ActionId,
    _aliActionName :: !ActionName,
    _aliRequestHeaders :: ![HTTP.Header],
    _aliSessionVariables :: !SessionVariables,
    _aliInputPayload :: !J.Value
  }
  deriving (Show, Eq)

data ActionLogResponse = ActionLogResponse
  { _alrId :: !ActionId,
    _alrCreatedAt :: !UTC.UTCTime,
    _alrResponsePayload :: !(Maybe J.Value),
    _alrErrors :: !(Maybe J.Value),
    _alrSessionVariables :: !SessionVariables
  }
  deriving (Show, Eq)

$(J.deriveJSON hasuraJSON ''ActionLogResponse)

type ActionLogResponseMap = HashMap ActionId ActionLogResponse

data AsyncActionStatus
  = AASCompleted !J.Value
  | AASError !QErr

data ActionsInfo = ActionsInfo
  { _asiName :: !ActionName,
    _asiForwardClientHeaders :: !Bool
  }
  deriving (Show, Eq, Generic)

type LockedActionEventId = EventId

-- This type exists only to use the Postgres array encoding.
newtype LockedActionIdArray = LockedActionIdArray {unCohortIdArray :: [LockedActionEventId]}
  deriving (Show, Eq)

instance Q.ToPrepArg LockedActionIdArray where
  toPrepVal (LockedActionIdArray l) =
    Q.toPrepValHelper PTI.unknown encoder $ mapMaybe (UUID.fromText . unEventId) l
    where
      encoder = PE.array 2950 . PE.dimensionArray foldl' (PE.encodingArray . PE.uuid)

$(makeLenses ''AnnActionAsyncQuery)

$(makeLenses ''AnnActionExecution)

$(makeLenses ''ActionsInfo)

$(makePrisms ''AsyncActionQueryFieldG)
