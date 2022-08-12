{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.Action
  ( -- * Metadata
    ActionMetadata (..),
    amName,
    amComment,
    amDefinition,
    amPermissions,
    ActionPermissionMetadata (..),
    ActionName (..),
    ActionId (..),
    actionIdToText,
    ActionDefinitionInput,

    -- ** Definition
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
    ActionMutationKind (..),

    -- ** Arguments
    ArgumentDefinition (..),
    ArgumentName (..),

    -- * Schema cache
    ActionInfo (..),
    aiName,
    aiComment,
    aiDefinition,
    aiOutputType,
    aiPermissions,
    aiForwardedClientHeaders,
    ActionPermissionInfo (..),
    ResolvedActionDefinition,

    -- * Execution types
    ActionExecContext (..),
    ActionLogResponse (..),
    ActionLogResponseMap,
    ActionLogItem (..),
    LockedActionEventId,
    LockedActionIdArray (..),
    AsyncActionStatus (..),
    ActionsInfo (..),
  )
where

import Control.Lens (makeLenses)
import Data.Aeson qualified as J
import Data.Aeson.Casing qualified as J
import Data.Aeson.Extended
import Data.Aeson.TH qualified as J
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
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.CustomTypes
import Hasura.RQL.Types.Eventing (EventId (..))
import Hasura.Session
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types qualified as HTTP
import PostgreSQL.Binary.Encoding qualified as PE

--------------------------------------------------------------------------------
-- Metadata

data ActionMetadata = ActionMetadata
  { _amName :: ActionName,
    _amComment :: Maybe Text,
    _amDefinition :: ActionDefinitionInput,
    _amPermissions :: [ActionPermissionMetadata]
  }
  deriving (Show, Eq, Generic)

instance NFData ActionMetadata

instance Cacheable ActionMetadata

data ActionPermissionMetadata = ActionPermissionMetadata
  { _apmRole :: RoleName,
    _apmComment :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance NFData ActionPermissionMetadata

instance Cacheable ActionPermissionMetadata

newtype ActionName = ActionName {unActionName :: G.Name}
  deriving (Show, Eq, Ord, J.FromJSON, J.ToJSON, J.FromJSONKey, J.ToJSONKey, ToTxt, Generic, NFData, Cacheable, Hashable)

newtype ActionId = ActionId {unActionId :: UUID.UUID}
  deriving (Show, Eq, Q.ToPrepArg, Q.FromCol, J.ToJSON, J.FromJSON, Hashable)

actionIdToText :: ActionId -> Text
actionIdToText = UUID.toText . unActionId

-- Required in the context of event triggers?
-- TODO: document this / get rid of it
instance Q.FromCol ActionName where
  fromCol bs = do
    text <- Q.fromCol bs
    name <- G.mkName text `onNothing` Left (text <> " is not valid GraphQL name")
    pure $ ActionName name

-- For legacy catalog format.
instance Q.ToPrepArg ActionName where
  toPrepVal = Q.toPrepVal . G.unName . unActionName

type ActionDefinitionInput =
  ActionDefinition GraphQLType InputWebhook

--------------------------------------------------------------------------------
-- Definition

data ActionDefinition arg webhook = ActionDefinition
  { _adArguments :: [ArgumentDefinition arg],
    _adOutputType :: GraphQLType,
    _adType :: ActionType,
    _adHeaders :: [HeaderConf],
    _adForwardClientHeaders :: Bool,
    -- | If the timeout is not provided by the user, then
    -- the default timeout of 30 seconds will be used
    _adTimeout :: Timeout,
    _adHandler :: webhook,
    _adRequestTransform :: Maybe RequestTransform,
    _adResponseTransform :: Maybe MetadataResponseTransform
  }
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

instance (NFData a, NFData w) => NFData (ActionDefinition a w)

instance (Cacheable a, Cacheable w) => Cacheable (ActionDefinition a w)

data ActionType
  = ActionQuery
  | ActionMutation ActionMutationKind
  deriving (Show, Eq, Generic)

instance NFData ActionType

instance Cacheable ActionType

data ActionMutationKind
  = ActionSynchronous
  | ActionAsynchronous
  deriving (Show, Eq, Generic)

instance NFData ActionMutationKind

instance Cacheable ActionMutationKind

--------------------------------------------------------------------------------
-- Arguments

data ArgumentDefinition a = ArgumentDefinition
  { _argName :: ArgumentName,
    _argType :: a,
    _argDescription :: Maybe G.Description
  }
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

instance (NFData a) => NFData (ArgumentDefinition a)

instance (Cacheable a) => Cacheable (ArgumentDefinition a)

newtype ArgumentName = ArgumentName {unArgumentName :: G.Name}
  deriving (Show, Eq, J.FromJSON, J.ToJSON, J.FromJSONKey, J.ToJSONKey, ToTxt, Generic, NFData, Cacheable)

--------------------------------------------------------------------------------
-- Schema cache

data ActionInfo = ActionInfo
  { _aiName :: ActionName,
    _aiOutputType :: (G.GType, AnnotatedOutputType),
    _aiDefinition :: ResolvedActionDefinition,
    _aiPermissions :: HashMap RoleName ActionPermissionInfo,
    _aiForwardedClientHeaders :: Bool,
    _aiComment :: Maybe Text
  }
  deriving (Generic)

type ResolvedActionDefinition =
  ActionDefinition (G.GType, AnnotatedInputType) (EnvRecord ResolvedWebhook)

newtype ActionPermissionInfo = ActionPermissionInfo
  { _apiRole :: RoleName
  }
  deriving newtype (Show, Eq, FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Execution types

-- TODO: those types are not used outside of the execution side of things, and
-- should be moved out of RQL.Types to become implementation details of
-- GraphQL.Execute.

data ActionExecContext = ActionExecContext
  { _aecManager :: HTTP.Manager,
    _aecHeaders :: HTTP.RequestHeaders,
    _aecSessionVariables :: SessionVariables
  }

data ActionLogItem = ActionLogItem
  { _aliId :: ActionId,
    _aliActionName :: ActionName,
    _aliRequestHeaders :: [HTTP.Header],
    _aliSessionVariables :: SessionVariables,
    _aliInputPayload :: J.Value
  }
  deriving (Show, Eq)

data ActionLogResponse = ActionLogResponse
  { _alrId :: ActionId,
    _alrCreatedAt :: UTC.UTCTime,
    _alrResponsePayload :: Maybe J.Value,
    _alrErrors :: Maybe J.Value,
    _alrSessionVariables :: SessionVariables
  }
  deriving (Show, Eq)

type ActionLogResponseMap = HashMap ActionId ActionLogResponse

data AsyncActionStatus
  = AASCompleted J.Value
  | AASError QErr

data ActionsInfo = ActionsInfo
  { _asiName :: ActionName,
    _asiForwardClientHeaders :: Bool
  }
  deriving (Show, Eq, Generic)

type LockedActionEventId = EventId

-- This type exists only to use the Postgres array encoding.
-- TODO: document this; what does that mean? Why is it defined here? What's the
-- common point with EventTriggers?
newtype LockedActionIdArray = LockedActionIdArray {unCohortIdArray :: [LockedActionEventId]}
  deriving (Show, Eq)

instance Q.ToPrepArg LockedActionIdArray where
  toPrepVal (LockedActionIdArray l) =
    Q.toPrepValHelper PTI.unknown encoder $ mapMaybe (UUID.fromText . unEventId) l
    where
      encoder = PE.array 2950 . PE.dimensionArray foldl' (PE.encodingArray . PE.uuid)

-------------------------------------------------------------------------------
-- Template haskell derivation
-- ...and other instances that need to live here in a particular order, due to
-- GHC 9.0 TH changes...

$(J.deriveJSON hasuraJSON {J.omitNothingFields = True} ''ActionPermissionMetadata)

$(J.deriveJSON hasuraJSON ''ArgumentDefinition)
$(J.deriveJSON J.defaultOptions {J.constructorTagModifier = J.snakeCase . drop 6} ''ActionMutationKind)

instance (J.FromJSON a, J.FromJSON b) => J.FromJSON (ActionDefinition a b) where
  parseJSON = J.withObject "ActionDefinition" $ \o -> do
    _adArguments <- o .:? "arguments" .!= []
    _adOutputType <- o .: "output_type"
    _adHeaders <- o .:? "headers" .!= []
    _adForwardClientHeaders <- o .:? "forward_client_headers" .!= False
    _adHandler <- o .: "handler"
    _adTimeout <- o .:? "timeout" .!= defaultActionTimeoutSecs
    actionType <- o .:? "type" .!= "mutation"
    _adType <- case actionType of
      "mutation" -> ActionMutation <$> o .:? "kind" .!= ActionSynchronous
      "query" -> pure ActionQuery
      t -> fail $ "expected mutation or query, but found " <> t
    _adRequestTransform <- o .:? "request_transform"
    _adResponseTransform <- o .:? "response_transform"
    pure ActionDefinition {..}

instance J.FromJSON ActionMetadata where
  parseJSON = J.withObject "ActionMetadata" $ \o ->
    ActionMetadata
      <$> o .: "name"
      <*> o .:? "comment"
      <*> o .: "definition"
      <*> o .:? "permissions" .!= []

instance (J.ToJSON a, J.ToJSON b) => J.ToJSON (ActionDefinition a b) where
  toJSON (ActionDefinition {..}) =
    let typeAndKind = case _adType of
          ActionQuery -> ["type" .= ("query" :: String)]
          ActionMutation kind ->
            [ "type" .= ("mutation" :: String),
              "kind" .= kind
            ]
     in J.object $
          [ "arguments" .= _adArguments,
            "output_type" .= _adOutputType,
            "headers" .= _adHeaders,
            "forward_client_headers" .= _adForwardClientHeaders,
            "handler" .= _adHandler,
            "timeout" .= _adTimeout
          ]
            <> catMaybes
              [ ("request_transform" .=) <$> _adRequestTransform,
                ("response_transform" .=) <$> _adResponseTransform
              ]
            <> typeAndKind

$(J.deriveToJSON hasuraJSON ''ActionLogResponse)
$(J.deriveToJSON hasuraJSON ''ActionMetadata)
$(J.deriveToJSON hasuraJSON ''ActionInfo)

$(makeLenses ''ActionMetadata)
$(makeLenses ''ActionDefinition)
$(makeLenses ''ActionInfo)
