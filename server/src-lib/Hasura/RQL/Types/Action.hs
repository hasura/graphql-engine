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

import Autodocodec (HasCodec, dimapCodec, disjointEitherCodec, optionalField', optionalFieldWith', optionalFieldWithDefault', optionalFieldWithOmittedDefault', requiredField')
import Autodocodec qualified as AC
import Autodocodec.Extended (boundedEnumCodec, discriminatorField, graphQLFieldDescriptionCodec, graphQLFieldNameCodec, typeableName)
import Control.Lens (makeLenses)
import Data.Aeson qualified as J
import Data.Aeson.Casing qualified as J
import Data.Aeson.Extended
import Data.Text.Extended
import Data.Time.Clock qualified as UTC
import Data.Typeable (Typeable)
import Data.UUID qualified as UUID
import Database.PG.Query qualified as PG
import Database.PG.Query.PTI qualified as PTI
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.CustomTypes
import Hasura.RQL.Types.Eventing (EventId (..))
import Hasura.RQL.Types.Headers
import Hasura.RQL.Types.Roles (RoleName)
import Hasura.RQL.Types.Session (SessionVariables)
import Hasura.RQL.Types.Webhook.Transform (MetadataResponseTransform, RequestTransform)
import Language.GraphQL.Draft.Syntax qualified as G
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

instance HasCodec ActionMetadata where
  codec =
    AC.object "ActionMetadata"
      $ ActionMetadata
      <$> requiredField' "name"
      AC..= _amName
        <*> optionalField' "comment"
      AC..= _amComment
        <*> requiredField' "definition"
      AC..= _amDefinition
        <*> optionalFieldWithOmittedDefault' "permissions" []
      AC..= _amPermissions

data ActionPermissionMetadata = ActionPermissionMetadata
  { _apmRole :: RoleName,
    _apmComment :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance NFData ActionPermissionMetadata

instance HasCodec ActionPermissionMetadata where
  codec =
    AC.object "ActionPermissionMetadata"
      $ ActionPermissionMetadata
      <$> requiredField' "role"
      AC..= _apmRole
        <*> optionalField' "comment"
      AC..= _apmComment

newtype ActionName = ActionName {unActionName :: G.Name}
  deriving (Show, Eq, Ord, J.FromJSON, J.ToJSON, J.FromJSONKey, J.ToJSONKey, ToTxt, Generic, NFData, Hashable)

instance HasCodec ActionName where
  codec = dimapCodec ActionName unActionName graphQLFieldNameCodec

newtype ActionId = ActionId {unActionId :: UUID.UUID}
  deriving (Show, Eq, PG.ToPrepArg, PG.FromCol, J.ToJSON, J.FromJSON, Hashable)

actionIdToText :: ActionId -> Text
actionIdToText = UUID.toText . unActionId

-- Required in the context of event triggers?
-- TODO: document this / get rid of it
instance PG.FromCol ActionName where
  fromCol bs = do
    text <- PG.fromCol bs
    name <- G.mkName text `onNothing` Left (text <> " is not valid GraphQL name")
    pure $ ActionName name

-- For legacy catalog format.
instance PG.ToPrepArg ActionName where
  toPrepVal = PG.toPrepVal . G.unName . unActionName

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

instance
  (Eq arg, HasCodec (ArgumentDefinition arg), HasCodec webhook, Typeable arg, Typeable webhook) =>
  HasCodec (ActionDefinition arg webhook)
  where
  codec =
    dimapCodec dec enc
      $ disjointEitherCodec (actionCodec (const ActionQuery)) (actionCodec ActionMutation)
    where
      actionCodec :: (ActionMutationKind -> ActionType) -> AC.JSONCodec (ActionDefinition arg webhook)
      actionCodec actionTypeConstructor =
        AC.object (typeId actionTypeConstructor)
          $ ActionDefinition
          <$> optionalFieldWithOmittedDefault' "arguments" []
          AC..= _adArguments
            <*> requiredField' "output_type"
          AC..= _adOutputType
            <*> typeAndKind actionTypeConstructor
          AC..= _adType
            <*> optionalFieldWithOmittedDefault' "headers" []
          AC..= _adHeaders
            <*> optionalFieldWithOmittedDefault' "forward_client_headers" False
          AC..= _adForwardClientHeaders
            <*> optionalFieldWithOmittedDefault' "timeout" defaultActionTimeoutSecs
          AC..= _adTimeout
            <*> requiredField' "handler"
          AC..= _adHandler
            <*> optionalField' "request_transform"
          AC..= _adRequestTransform
            <*> optionalField' "response_transform"
          AC..= _adResponseTransform

      typeAndKind :: (ActionMutationKind -> ActionType) -> AC.ObjectCodec ActionType ActionType
      typeAndKind actionTypeConstructor = case (actionTypeConstructor ActionSynchronous) of
        (ActionMutation _) ->
          ActionMutation
            <$ discriminatorField "type" "mutation"
            <*> optionalFieldWithDefault' "kind" ActionSynchronous
            AC..= \case
              (ActionMutation kind) -> kind
              ActionQuery -> ActionSynchronous
        ActionQuery -> ActionQuery <$ discriminatorField "type" "query"

      dec (Left a) = a
      dec (Right a) = a
      enc a
        | _adType a == ActionQuery = Left a
        | otherwise = Right a

      typeId actionTypeConstructor =
        let typeLabel = case (actionTypeConstructor ActionSynchronous) of
              (ActionMutation _) -> "Mutation"
              ActionQuery -> "Query"
         in "ActionDefinition_" <> typeLabel <> "_" <> typeableName @arg <> "_" <> typeableName @webhook

data ActionType
  = ActionQuery
  | ActionMutation ActionMutationKind
  deriving (Show, Eq, Generic)

instance NFData ActionType

data ActionMutationKind
  = ActionSynchronous
  | ActionAsynchronous
  deriving (Show, Bounded, Enum, Eq, Generic)

instance NFData ActionMutationKind

instance HasCodec ActionMutationKind where
  codec = boundedEnumCodec jsonStringConst

-- | Defines representation of 'ActionMutationKind' when serializing to JSON.
jsonStringConst :: ActionMutationKind -> String
jsonStringConst = \case
  ActionSynchronous -> "synchronous"
  ActionAsynchronous -> "asynchronous"

--------------------------------------------------------------------------------
-- Arguments

data ArgumentDefinition a = ArgumentDefinition
  { _argName :: ArgumentName,
    _argType :: a,
    _argDescription :: Maybe G.Description
  }
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

instance (NFData a) => NFData (ArgumentDefinition a)

instance (HasCodec a, Typeable a) => HasCodec (ArgumentDefinition a) where
  codec =
    AC.object ("ArgumentDefinition_" <> typeableName @a)
      $ ArgumentDefinition
      <$> requiredField' "name"
      AC..= _argName
        <*> requiredField' "type"
      AC..= _argType
        <*> optionalFieldWith' "description" graphQLFieldDescriptionCodec
      AC..= _argDescription

newtype ArgumentName = ArgumentName {unArgumentName :: G.Name}
  deriving (Show, Eq, J.FromJSON, J.ToJSON, J.FromJSONKey, J.ToJSONKey, ToTxt, Generic, NFData)

instance HasCodec ArgumentName where
  codec = dimapCodec ArgumentName unArgumentName graphQLFieldNameCodec

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
  { _aecHeaders :: HTTP.RequestHeaders,
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
  deriving stock (Show, Eq, Generic)

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

instance PG.ToPrepArg LockedActionIdArray where
  toPrepVal (LockedActionIdArray l) =
    PG.toPrepValHelper PTI.unknown encoder $ mapMaybe (UUID.fromText . unEventId) l
    where
      encoder = PE.array 2950 . PE.dimensionArray foldl' (PE.encodingArray . PE.uuid)

-------------------------------------------------------------------------------
-- Template haskell derivation
-- ...and other instances that need to live here in a particular order, due to
-- GHC 9.0 TH changes...

instance FromJSON ActionPermissionMetadata where
  parseJSON = genericParseJSON hasuraJSON {J.omitNothingFields = True}

instance ToJSON ActionPermissionMetadata where
  toJSON = genericToJSON hasuraJSON {J.omitNothingFields = True}
  toEncoding = genericToEncoding hasuraJSON {J.omitNothingFields = True}

instance (FromJSON arg) => FromJSON (ArgumentDefinition arg) where
  parseJSON = genericParseJSON hasuraJSON

instance (ToJSON arg) => ToJSON (ArgumentDefinition arg) where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

instance FromJSON ActionMutationKind where
  parseJSON = genericParseJSON hasuraJSON {J.constructorTagModifier = J.snakeCase . drop 6}

instance ToJSON ActionMutationKind where
  toJSON = genericToJSON hasuraJSON {J.constructorTagModifier = J.snakeCase . drop 6}
  toEncoding = genericToEncoding hasuraJSON {J.constructorTagModifier = J.snakeCase . drop 6}

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
      <$> o
      .: "name"
      <*> o
      .:? "comment"
      <*> o
      .: "definition"
      <*> o
      .:? "permissions"
      .!= []

instance (J.ToJSON a, J.ToJSON b) => J.ToJSON (ActionDefinition a b) where
  toJSON (ActionDefinition {..}) =
    let typeAndKind = case _adType of
          ActionQuery -> ["type" .= ("query" :: String)]
          ActionMutation kind ->
            [ "type" .= ("mutation" :: String),
              "kind" .= kind
            ]
     in J.object
          $ [ "arguments" .= _adArguments,
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

instance ToJSON ActionLogResponse where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

instance ToJSON ActionMetadata where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

instance ToJSON ActionInfo where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

$(makeLenses ''ActionMetadata)
$(makeLenses ''ActionDefinition)
$(makeLenses ''ActionInfo)
