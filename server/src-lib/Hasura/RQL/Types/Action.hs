module Hasura.RQL.Types.Action
  ( ActionOutputTypeInfo(..)
  , ActionInfo(..)
  , ActionName(..)

  , ActionKind(..)
  , ActionDefinition(..)
  , getActionKind
  , CreateAction(..)
  , ActionDefinitionInput

  , ResolvedWebhook(..)
  , ResolvedActionDefinition

  , ActionPermissionInfo(..)

  , ActionPermissionMap

  , ActionPermissionSelect(..)
  , ActionPermissionDefinition(..)
  , CreateActionPermission(..)
  ) where


import           Hasura.Prelude
import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.CustomTypes
import           Hasura.RQL.Types.DML
import           Hasura.RQL.Types.Permission
import           Hasura.SQL.Types
import           Language.Haskell.TH.Syntax    (Lift)

import qualified Hasura.GraphQL.Validate.Types as VT

import qualified Data.Aeson                    as J
import qualified Data.Aeson.Casing             as J
import qualified Data.Aeson.TH                 as J
import qualified Data.HashMap.Strict           as Map
import qualified Database.PG.Query             as Q
import qualified Language.GraphQL.Draft.Syntax as G

newtype ActionName
  = ActionName { unActionName :: G.Name }
  deriving ( Show, Eq, J.FromJSON, J.ToJSON, J.FromJSONKey, J.ToJSONKey
           , Hashable, DQuote, Lift)

instance Q.FromCol ActionName where
  fromCol bs = ActionName . G.Name <$> Q.fromCol bs

instance Q.ToPrepArg ActionName where
  toPrepVal = Q.toPrepVal . G.unName . unActionName

newtype ResolvedWebhook
  = ResolvedWebhook { unResolvedWebhook :: Text}
  deriving ( Show, Eq, J.FromJSON, J.ToJSON, Hashable, DQuote, Lift)

data ActionKind
  = ActionSynchronous
  | ActionAsynchronous
  deriving (Show, Eq, Lift)
$(J.deriveJSON
  J.defaultOptions { J.constructorTagModifier = J.snakeCase . drop 6}
  ''ActionKind)

data ActionDefinition a
  = ActionDefinition
  { _adInputType  :: !GraphQLType
  , _adOutputType :: !GraphQLType
  , _adKind       :: !(Maybe ActionKind)
  , _adWebhook    :: !a
  } deriving (Show, Eq, Lift, Functor)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''ActionDefinition)

getActionKind :: ActionDefinition a -> ActionKind
getActionKind = fromMaybe ActionSynchronous . _adKind

type ResolvedActionDefinition = ActionDefinition ResolvedWebhook

data ActionPermissionInfo
  = ActionPermissionInfo
  { _apiRole   :: !RoleName
  , _apiFilter :: !AnnBoolExpPartialSQL
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) ''ActionPermissionInfo)

type ActionPermissionMap
  = Map.HashMap RoleName ActionPermissionInfo

data ActionMetadataField
  = ActionMetadataFieldId
  | ActionMetadataFieldCreatedAt
  | ActionMetadataFieldStatus
  deriving (Show, Eq)

data ActionOutputTypeInfo
  = ActionOutputScalar !VT.ScalarTyInfo
  | ActionOutputEnum !VT.EnumTyInfo
  | ActionOutputObject !VT.ObjTyInfo
  deriving (Show, Eq)

-- TODO: this is terrible
instance J.ToJSON ActionOutputTypeInfo where
  toJSON = J.toJSON . show

data ActionInfo
  = ActionInfo
  { _aiName           :: !ActionName
  , _aiDefintion      :: !ResolvedActionDefinition
  , _aiOutputTypeInfo :: !ActionOutputTypeInfo
  , _aiPermissions    :: !ActionPermissionMap
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 3 J.snakeCase) ''ActionInfo)

type InputWebhook = Text
type ActionDefinitionInput = ActionDefinition InputWebhook

data CreateAction
  = CreateAction
  { _caName       :: !ActionName
  , _caDefinition :: !ActionDefinitionInput
  , _caComment    :: !(Maybe Text)
  } deriving (Show, Eq, Lift)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''CreateAction)

newtype ActionPermissionSelect
  = ActionPermissionSelect
  { _apsFilter :: BoolExp
  } deriving (Show, Eq, Lift)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''ActionPermissionSelect)

newtype ActionPermissionDefinition
  = ActionPermissionDefinition
  { _apdSelect :: ActionPermissionSelect
  } deriving (Show, Eq, Lift)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''ActionPermissionDefinition)

data CreateActionPermission
  = CreateActionPermission
  { _capAction     :: !ActionName
  , _capRole       :: !RoleName
  , _capDefinition :: !ActionPermissionDefinition
  , _capComment    :: !(Maybe Text)
  } deriving (Show, Eq, Lift)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''CreateActionPermission)
