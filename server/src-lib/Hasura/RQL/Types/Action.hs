{-# LANGUAGE RecordWildCards #-}
module Hasura.RQL.Types.Action
  ( ArgumentName(..)
  , ArgumentDefinition(..)

  , ActionName(..)
  , ActionMutationKind(..)
  , ActionDefinition(..)
  , ActionType(..)
  , CreateAction(..)
  , UpdateAction(..)
  , ActionDefinitionInput
  , InputWebhook(..)

  , ResolvedWebhook(..)
  , ResolvedActionDefinition

  , ActionOutputFields
  , ActionInfo(..)
  , aiName
  , aiOutputFields
  , aiDefinition
  , aiPermissions
  , aiComment
  , ActionPermissionInfo(..)

  , ActionPermissionMap
  , CreateActionPermission(..)

  , ActionMetadata(..)
  , ActionPermissionMetadata(..)
  ) where


import           Control.Lens                  (makeLenses)
import           Data.URL.Template
import           Hasura.Incremental            (Cacheable)
import           Hasura.Prelude
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.Types.CustomTypes
import           Hasura.RQL.Types.Permission
import           Hasura.SQL.Types
import           Language.Haskell.TH.Syntax    (Lift)

import qualified Data.Aeson                    as J
import qualified Data.Aeson.Casing             as J
import qualified Data.Aeson.TH                 as J
import qualified Data.HashMap.Strict           as Map
import qualified Database.PG.Query             as Q
import qualified Language.GraphQL.Draft.Syntax as G

newtype ActionName
  = ActionName { unActionName :: G.Name }
  deriving ( Show, Eq, J.FromJSON, J.ToJSON, J.FromJSONKey, J.ToJSONKey
           , Hashable, DQuote, Lift, Generic, NFData, Cacheable)

instance Q.FromCol ActionName where
  fromCol bs = ActionName . G.Name <$> Q.fromCol bs

instance Q.ToPrepArg ActionName where
  toPrepVal = Q.toPrepVal . G.unName . unActionName

newtype ResolvedWebhook
  = ResolvedWebhook { unResolvedWebhook :: Text}
  deriving ( Show, Eq, J.FromJSON, J.ToJSON, Hashable, DQuote, Lift)

data ActionMutationKind
  = ActionSynchronous
  | ActionAsynchronous
  deriving (Show, Eq, Lift, Generic)
instance NFData ActionMutationKind
instance Cacheable ActionMutationKind
$(J.deriveJSON
  J.defaultOptions { J.constructorTagModifier = J.snakeCase . drop 6}
  ''ActionMutationKind)

newtype ArgumentName
  = ArgumentName { unArgumentName :: G.Name }
  deriving ( Show, Eq, J.FromJSON, J.ToJSON, J.FromJSONKey, J.ToJSONKey
           , Hashable, DQuote, Lift, Generic, NFData, Cacheable)

data ArgumentDefinition
  = ArgumentDefinition
  { _argName        :: !ArgumentName
  , _argType        :: !GraphQLType
  , _argDescription :: !(Maybe G.Description)
  } deriving (Show, Eq, Lift, Generic)
instance NFData ArgumentDefinition
instance Cacheable ArgumentDefinition
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''ArgumentDefinition)

data ActionType
  = ActionQuery
  | ActionMutation !ActionMutationKind
  deriving (Show, Eq, Lift, Generic)
instance NFData ActionType
instance Cacheable ActionType
$(J.deriveJSON
  J.defaultOptions { J.constructorTagModifier = J.snakeCase . drop 6}
  ''ActionType)

data ActionDefinition a
  = ActionDefinition
  { _adArguments            :: ![ArgumentDefinition]
  , _adOutputType           :: !GraphQLType
  , _adType                 :: !ActionType
  , _adHeaders              :: ![HeaderConf]
  , _adForwardClientHeaders :: !Bool
  , _adHandler              :: !a
  } deriving (Show, Eq, Lift, Functor, Foldable, Traversable, Generic)
instance (NFData a) => NFData (ActionDefinition a)
instance (Cacheable a) => Cacheable (ActionDefinition a)

instance (J.FromJSON a) => J.FromJSON (ActionDefinition a) where
  parseJSON = J.withObject "ActionDefinition" $ \o -> do
    _adArguments <- o J..: "arguments"
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

instance (J.ToJSON a) => J.ToJSON (ActionDefinition a) where
  toJSON (ActionDefinition args outputType ActionQuery headers forwardClientHeaders handler) =
    J.object
    [ "arguments"              J..= args
    , "output_type"            J..= outputType
    , "headers"                J..= headers
    , "forward_client_headers" J..= forwardClientHeaders
    , "handler"                J..= handler
    , "type"                   J..= ("query" :: String)
    ]
  toJSON (ActionDefinition args outputType (ActionMutation kind) headers forwardClientHeaders handler) =
    J.object
    [ "arguments"              J..= args
    , "output_type"            J..= outputType
    , "headers"                J..= headers
    , "forward_client_headers" J..= forwardClientHeaders
    , "handler"                J..= handler
    , "type"                   J..= ("mutation" :: String)
    , "kind"                   J..= kind
    ]


type ResolvedActionDefinition = ActionDefinition ResolvedWebhook

data ActionPermissionInfo
  = ActionPermissionInfo
  { _apiRole   :: !RoleName
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) ''ActionPermissionInfo)

type ActionPermissionMap = Map.HashMap RoleName ActionPermissionInfo
type ActionOutputFields = Map.HashMap G.Name G.GType

data ActionInfo
  = ActionInfo
  { _aiName         :: !ActionName
  , _aiOutputFields :: !ActionOutputFields
  , _aiDefinition   :: !ResolvedActionDefinition
  , _aiPermissions  :: !ActionPermissionMap
  , _aiComment      :: !(Maybe Text)
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 3 J.snakeCase) ''ActionInfo)
$(makeLenses ''ActionInfo)

newtype InputWebhook
  = InputWebhook {unInputWebhook :: URLTemplate}
  deriving (Show, Eq, Lift, Generic)
instance NFData InputWebhook
instance Cacheable InputWebhook

instance J.ToJSON InputWebhook where
  toJSON =  J.String . printURLTemplate . unInputWebhook

instance J.FromJSON InputWebhook where
  parseJSON = J.withText "String" $ \t ->
    case parseURLTemplate t of
      Left e  -> fail $ "Parsing URL template failed: " ++ e
      Right v -> pure $ InputWebhook v

type ActionDefinitionInput = ActionDefinition InputWebhook

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
