{-# LANGUAGE RecordWildCards #-}
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
  , adHeaders
  , adForwardClientHeaders
  , adHandler
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
  , aiComment
  , ActionPermissionInfo(..)

  , ActionPermissionMap
  , CreateActionPermission(..)

  , ActionMetadata(..)
  , ActionPermissionMetadata(..)

  , AnnActionExecution(..)
  , AnnActionMutationAsync(..)
  , ActionExecContext(..)
  , AsyncActionQueryFieldG(..)
  , AnnActionAsyncQuery(..)
  ) where


import           Control.Lens                  (makeLenses, makePrisms)
import           Hasura.Incremental            (Cacheable)
import           Hasura.Prelude
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.DML.Select.Types
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.CustomTypes
import           Hasura.Session
import           Hasura.SQL.Types
import           Language.Haskell.TH.Syntax    (Lift)

import qualified Data.Aeson                    as J
import qualified Data.Aeson.Casing             as J
import qualified Data.Aeson.TH                 as J
import qualified Data.HashMap.Strict           as Map
import qualified Database.PG.Query             as Q
import qualified Language.GraphQL.Draft.Syntax as G
import qualified Network.HTTP.Client           as HTTP
import qualified Network.HTTP.Types            as HTTP

newtype ActionName
  = ActionName { unActionName :: G.Name }
  deriving ( Show, Eq, J.FromJSON, J.ToJSON, J.FromJSONKey, J.ToJSONKey
           , Hashable, DQuote, Lift, Generic, NFData, Cacheable)

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
  deriving (Show, Eq, Lift, Generic)
instance NFData ActionMutationKind
instance Cacheable ActionMutationKind
$(J.deriveJSON
  J.defaultOptions { J.constructorTagModifier = J.snakeCase . drop 6}
  ''ActionMutationKind)
$(makePrisms ''ActionMutationKind)

newtype ArgumentName
  = ArgumentName { unArgumentName :: G.Name }
  deriving ( Show, Eq, J.FromJSON, J.ToJSON, J.FromJSONKey, J.ToJSONKey
           , Hashable, DQuote, Lift, Generic, NFData, Cacheable)

data ArgumentDefinition a
  = ArgumentDefinition
  { _argName        :: !ArgumentName
  , _argType        :: !a
  , _argDescription :: !(Maybe G.Description)
  } deriving (Show, Eq, Functor, Foldable, Traversable, Lift, Generic)
instance (NFData a) => NFData (ArgumentDefinition a)
instance (Cacheable a) => Cacheable (ArgumentDefinition a)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''ArgumentDefinition)

data ActionType
  = ActionQuery
  | ActionMutation !ActionMutationKind
  deriving (Show, Eq, Lift, Generic)
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
  , _adHandler              :: !b
  } deriving (Show, Eq, Lift, Functor, Foldable, Traversable, Generic)
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
    actionType <- o J..:? "type" J..!= "mutation"
    _adType <- case actionType of
      "mutation" -> ActionMutation <$> o J..:? "kind" J..!= ActionSynchronous
      "query"    -> pure ActionQuery
      t          -> fail $ "expected mutation or query, but found " <> t
    return ActionDefinition {..}

instance (J.ToJSON a, J.ToJSON b) => J.ToJSON (ActionDefinition a b) where
  toJSON (ActionDefinition args outputType actionType headers forwardClientHeaders handler) =
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
    ] <> typeAndKind

type ResolvedActionDefinition =
  ActionDefinition (ArgumentDefinition (G.GType, NonObjectCustomType)) ResolvedWebhook

data ActionPermissionInfo
  = ActionPermissionInfo
  { _apiRole   :: !RoleName
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) ''ActionPermissionInfo)

type ActionPermissionMap = Map.HashMap RoleName ActionPermissionInfo

type ActionOutputFields = Map.HashMap G.Name G.GType

getActionOutputFields :: AnnotatedObjectType -> ActionOutputFields
getActionOutputFields =
  Map.fromList . map ( (unObjectFieldName . _ofdName) &&& (fst . _ofdType)) . toList . _otdFields

data ActionInfo
  = ActionInfo
  { _aiName         :: !ActionName
  , _aiOutputObject :: !AnnotatedObjectType
  , _aiDefinition   :: !ResolvedActionDefinition
  , _aiPermissions  :: !ActionPermissionMap
  , _aiComment      :: !(Maybe Text)
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 3 J.snakeCase) ''ActionInfo)
$(makeLenses ''ActionInfo)

type ActionDefinitionInput =
  ActionDefinition (ArgumentDefinition GraphQLType) InputWebhook

data CreateAction
  = CreateAction
  { _caName       :: !ActionName
  , _caDefinition :: !ActionDefinitionInput
  , _caComment    :: !(Maybe Text)
  } deriving (Show, Eq, Lift, Generic)
instance NFData CreateAction
instance Cacheable CreateAction
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''CreateAction)

data UpdateAction
  = UpdateAction
  { _uaName       :: !ActionName
  , _uaDefinition :: !ActionDefinitionInput
  } deriving (Show, Eq, Lift)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''UpdateAction)

data CreateActionPermission
  = CreateActionPermission
  { _capAction     :: !ActionName
  , _capRole       :: !RoleName
  , _capDefinition :: !(Maybe J.Value)
  , _capComment    :: !(Maybe Text)
  } deriving (Show, Eq, Lift, Generic)
instance NFData CreateActionPermission
instance Cacheable CreateActionPermission
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''CreateActionPermission)

-- representation of action permission metadata
data ActionPermissionMetadata
  = ActionPermissionMetadata
  { _apmRole    :: !RoleName
  , _apmComment :: !(Maybe Text)
  } deriving (Show, Eq, Lift, Generic)
instance NFData ActionPermissionMetadata
instance Cacheable ActionPermissionMetadata

$(J.deriveFromJSON
  (J.aesonDrop 4 J.snakeCase){J.omitNothingFields=True}
  ''ActionPermissionMetadata)

-- representation of action metadata
data ActionMetadata
  = ActionMetadata
  { _amName        :: !ActionName
  , _amComment     :: !(Maybe Text)
  , _amDefinition  :: !ActionDefinitionInput
  , _amPermissions :: ![ActionPermissionMetadata]
  } deriving (Show, Eq, Lift, Generic)
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

data AnnActionExecution v
  = AnnActionExecution
  { _aaeName                 :: !ActionName
  , _aaeOutputType           :: !GraphQLType -- ^ output type
  , _aaeFields               :: !(AnnFieldsG v) -- ^ output selection
  , _aaePayload              :: !J.Value -- ^ jsonified input arguments
  , _aaeOutputFields         :: !ActionOutputFields
  -- ^ to validate the response fields from webhook
  , _aaeDefinitionList       :: ![(PGCol, PGScalarType)]
  , _aaeWebhook              :: !ResolvedWebhook
  , _aaeHeaders              :: ![HeaderConf]
  , _aaeForwardClientHeaders :: !Bool
  , _aaeStrfyNum             :: !Bool
  } deriving (Show, Eq)

data AnnActionMutationAsync
  = AnnActionMutationAsync
  { _aamaName    :: !ActionName
  , _aamaPayload :: !J.Value -- ^ jsonified input arguments
  } deriving (Show, Eq)

data AsyncActionQueryFieldG v
  = AsyncTypename !Text
  | AsyncOutput !(AnnFieldsG v)
  | AsyncId
  | AsyncCreatedAt
  | AsyncErrors
  deriving (Show, Eq)

type AsyncActionQueryFieldsG v = Fields (AsyncActionQueryFieldG v)

data AnnActionAsyncQuery v
  = AnnActionAsyncQuery
  { _aaaqName           :: !ActionName
  , _aaaqActionId       :: !v
  , _aaaqOutputType     :: !GraphQLType
  , _aaaqFields         :: !(AsyncActionQueryFieldsG v)
  , _aaaqDefinitionList :: ![(PGCol, PGScalarType)]
  , _aaaqStringifyNum   :: !Bool
  } deriving (Show, Eq)

data ActionExecContext
  = ActionExecContext
  { _aecManager          :: !HTTP.Manager
  , _aecHeaders          :: !HTTP.RequestHeaders
  , _aecSessionVariables :: !SessionVariables
  }
