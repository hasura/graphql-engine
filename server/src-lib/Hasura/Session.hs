module Hasura.Session
  ( RoleName
  , mkRoleName
  , adminRoleName
  , isAdmin
  , roleNameToTxt
  , SessionVariable
  , mkSessionVariable
  , SessionVariables
  , filterSessionVariables
  , SessionVariableValue
  , sessionVariableToText
  , sessionVariableToGraphQLName
  , mkSessionVariablesText
  , mkSessionVariablesHeaders
  , sessionVariablesToHeaders
  , getSessionVariableValue
  , getSessionVariablesSet
  , getSessionVariables
  , UserAdminSecret(..)
  , UserRoleBuild(..)
  , UserInfo
  , _uiRole
  , _uiSession
  , _uiBackendOnlyFieldAccess
  , mkUserInfo
  , adminUserInfo
  , BackendOnlyFieldAccess(..)
  ) where

import           Hasura.Prelude

import qualified Data.CaseInsensitive          as CI
import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
import qualified Data.Text                     as T
import qualified Database.PG.Query             as Q
import qualified Language.GraphQL.Draft.Syntax as G
import qualified Network.HTTP.Types            as HTTP

import           Data.Aeson
import           Data.Aeson.Types              (Parser, toJSONKeyText)
import           Data.Text.Extended
import           Data.Text.NonEmpty

import           Hasura.Incremental            (Cacheable)
import           Hasura.RQL.Types.Common       (adminText)
import           Hasura.RQL.Types.Error
import           Hasura.Server.Utils


newtype RoleName
  = RoleName {getRoleTxt :: NonEmptyText}
  deriving ( Show, Eq, Ord, Hashable, FromJSONKey, ToJSONKey, FromJSON
           , ToJSON, Q.FromCol, Q.ToPrepArg, Generic, Arbitrary, NFData, Cacheable )

instance ToTxt RoleName where
  toTxt = roleNameToTxt

roleNameToTxt :: RoleName -> Text
roleNameToTxt = unNonEmptyText . getRoleTxt

mkRoleName :: Text -> Maybe RoleName
mkRoleName = fmap RoleName . mkNonEmptyText

adminRoleName :: RoleName
adminRoleName = RoleName adminText

isAdmin :: RoleName -> Bool
isAdmin = (adminRoleName ==)

newtype SessionVariable = SessionVariable {unSessionVariable :: CI.CI Text}
  deriving (Show, Eq, Hashable, IsString, Cacheable, Data, NFData, Ord)

instance ToJSON SessionVariable where
  toJSON = toJSON . CI.original . unSessionVariable

instance ToJSONKey SessionVariable where
  toJSONKey = toJSONKeyText sessionVariableToText

instance ToTxt SessionVariable where
  toTxt = sessionVariableToText

-- | converts a `SessionVariable` value to a GraphQL name
sessionVariableToGraphQLName :: SessionVariable -> G.Name
sessionVariableToGraphQLName = G.unsafeMkName . T.replace "-" "_" . sessionVariableToText

parseSessionVariable :: Text -> Parser SessionVariable
parseSessionVariable t =
  if isSessionVariable t then pure $ mkSessionVariable t
  else fail $ show t <> " is not a Hasura session variable"

instance FromJSON SessionVariable where
  parseJSON = withText "String" parseSessionVariable

instance FromJSONKey SessionVariable where
  fromJSONKey = FromJSONKeyTextParser parseSessionVariable

sessionVariableToText :: SessionVariable -> Text
sessionVariableToText = T.toLower . CI.original . unSessionVariable

mkSessionVariable :: Text -> SessionVariable
mkSessionVariable = SessionVariable . CI.mk

type SessionVariableValue = Text

newtype SessionVariables =
  SessionVariables { unSessionVariables :: Map.HashMap SessionVariable SessionVariableValue}
  deriving (Show, Eq, Hashable, Semigroup, Monoid)

filterSessionVariables
  :: (SessionVariable -> SessionVariableValue -> Bool)
  -> SessionVariables -> SessionVariables
filterSessionVariables f = SessionVariables . Map.filterWithKey f . unSessionVariables

instance ToJSON SessionVariables where
  toJSON (SessionVariables varMap) =
    toJSON $ mapKeys sessionVariableToText varMap

instance FromJSON SessionVariables where
  parseJSON v = mkSessionVariablesText <$> parseJSON v

mkSessionVariablesText :: Map.HashMap Text Text -> SessionVariables
mkSessionVariablesText = SessionVariables . mapKeys mkSessionVariable

mkSessionVariablesHeaders :: [HTTP.Header] -> SessionVariables
mkSessionVariablesHeaders =
  SessionVariables
  . Map.fromList
  . map (first SessionVariable)
  . filter (isSessionVariable . CI.original . fst) -- Only x-hasura-* headers
  . map (CI.map bsToTxt *** bsToTxt)

sessionVariablesToHeaders :: SessionVariables -> [HTTP.Header]
sessionVariablesToHeaders =
  map ((CI.map txtToBs . unSessionVariable) *** txtToBs)
  . Map.toList
  . unSessionVariables

getSessionVariables :: SessionVariables -> [Text]
getSessionVariables = map sessionVariableToText . Map.keys . unSessionVariables

getSessionVariablesSet :: SessionVariables -> Set.HashSet SessionVariable
getSessionVariablesSet = Map.keysSet . unSessionVariables

getSessionVariableValue :: SessionVariable -> SessionVariables -> Maybe SessionVariableValue
getSessionVariableValue k = Map.lookup k . unSessionVariables

-- | Represent the admin secret state; whether the secret is sent
-- in the request or if actually authorization is not configured.
data UserAdminSecret
  = UAdminSecretSent
  | UAdminSecretNotSent
  | UAuthNotSet
  deriving (Show, Eq)

-- | Represents the 'X-Hasura-Use-Backend-Only-Permissions' session variable
-- and request made with 'X-Hasura-Admin-Secret' if any auth configured.
-- For more details see Note [Backend only permissions]
data BackendOnlyFieldAccess
  = BOFAAllowed
  | BOFADisallowed
  deriving (Show, Eq, Generic)
instance Hashable BackendOnlyFieldAccess

data UserInfo
  = UserInfo
  { _uiRole                   :: !RoleName
  , _uiSession                :: !SessionVariables
  , _uiBackendOnlyFieldAccess :: !BackendOnlyFieldAccess
  } deriving (Show, Eq, Generic)
instance Hashable UserInfo

-- | Represents how to build a role from the session variables
data UserRoleBuild
  = URBFromSessionVariables
  -- ^ Look for `x-hasura-role` session variable value and absence will raise an exception
  | URBFromSessionVariablesFallback !RoleName
  -- ^ Look for `x-hasura-role` session variable value, if absent fall back to given role
  | URBPreDetermined !RoleName
  -- ^ Use only the pre-determined role
  deriving (Show, Eq)

-- | Build @'UserInfo' from @'SessionVariables'
mkUserInfo
  :: forall m. (MonadError QErr m)
  => UserRoleBuild -> UserAdminSecret -> SessionVariables -> m UserInfo
mkUserInfo roleBuild userAdminSecret sessionVariables = do
  roleName <- case roleBuild of
    URBFromSessionVariables -> onNothing maybeSessionRole $
      throw400 InvalidParams $ userRoleHeader <> " not found in session variables"
    URBFromSessionVariablesFallback role -> pure $ fromMaybe role maybeSessionRole
    URBPreDetermined role -> pure role
  backendOnlyFieldAccess <- getBackendOnlyFieldAccess
  let modifiedSession = modifySessionVariables roleName sessionVariables
  pure $ UserInfo roleName modifiedSession backendOnlyFieldAccess
  where
    maybeSessionRole = maybeRoleFromSessionVariables sessionVariables

    -- | Add x-hasura-role header and remove admin secret headers
    modifySessionVariables :: RoleName -> SessionVariables -> SessionVariables
    modifySessionVariables roleName =
      SessionVariables
      . Map.insert userRoleHeader (roleNameToTxt roleName)
      . Map.delete adminSecretHeader
      . Map.delete deprecatedAccessKeyHeader
      . unSessionVariables

    -- | See Note [Backend only permissions] to know more about the function
    getBackendOnlyFieldAccess :: m BackendOnlyFieldAccess
    getBackendOnlyFieldAccess = case userAdminSecret of
      UAdminSecretNotSent -> pure BOFADisallowed
      UAdminSecretSent    -> lookForBackendOnlyPermissionsConfig
      UAuthNotSet         -> lookForBackendOnlyPermissionsConfig
      where
        lookForBackendOnlyPermissionsConfig =
          case getSessionVariableValue useBackendOnlyPermissionsHeader sessionVariables of
            Nothing     -> pure BOFADisallowed
            Just varVal ->
              case parseStringAsBool (T.unpack varVal) of
                Left err        -> throw400 BadRequest $
                  useBackendOnlyPermissionsHeader <> ": " <> T.pack err
                Right privilege -> pure $ if privilege then BOFAAllowed else BOFADisallowed


maybeRoleFromSessionVariables :: SessionVariables -> Maybe RoleName
maybeRoleFromSessionVariables sessionVariables =
  -- returns Nothing if x-hasura-role is an empty string
  getSessionVariableValue userRoleHeader sessionVariables >>= mkRoleName

adminUserInfo :: UserInfo
adminUserInfo = UserInfo adminRoleName mempty BOFADisallowed
