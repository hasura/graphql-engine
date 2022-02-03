module Hasura.Session
  ( RoleName,
    mkRoleName,
    adminRoleName,
    isAdmin,
    roleNameToTxt,
    SessionVariable,
    mkSessionVariable,
    SessionVariables,
    filterSessionVariables,
    SessionVariableValue,
    sessionVariableToText,
    sessionVariableToGraphQLName,
    mkSessionVariablesText,
    mkSessionVariablesHeaders,
    sessionVariablesToHeaders,
    getSessionVariableValue,
    getSessionVariablesSet,
    getSessionVariables,
    UserAdminSecret (..),
    UserRoleBuild (..),
    UserInfo (..),
    UserInfoM (..),
    askCurRole,
    mkUserInfo,
    adminUserInfo,
    BackendOnlyFieldAccess (..),
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser, toJSONKeyText)
import Data.CaseInsensitive qualified as CI
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.Text qualified as T
import Data.Text.Extended
import Data.Text.NonEmpty
import Database.PG.Query qualified as Q
import Hasura.Base.Error
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.Server.Utils
import Hasura.Tracing (TraceT)
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Types qualified as HTTP

newtype RoleName = RoleName {getRoleTxt :: NonEmptyText}
  deriving
    ( Show,
      Eq,
      Ord,
      Hashable,
      FromJSONKey,
      ToJSONKey,
      FromJSON,
      ToJSON,
      Q.FromCol,
      Q.ToPrepArg,
      Generic,
      NFData,
      Cacheable
    )

roleNameToTxt :: RoleName -> Text
roleNameToTxt = unNonEmptyText . getRoleTxt

instance ToTxt RoleName where
  toTxt = roleNameToTxt

mkRoleName :: Text -> Maybe RoleName
mkRoleName = fmap RoleName . mkNonEmptyText

adminRoleName :: RoleName
adminRoleName = RoleName $ mkNonEmptyTextUnsafe "admin"

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

-- | Converts a `SessionVariable` value to a GraphQL name.
-- This will fail if the session variable contains characters that are not valid
-- for a graphql names. It is the caller's responsibility to decide what to do
-- in such a case.
sessionVariableToGraphQLName :: SessionVariable -> Maybe G.Name
sessionVariableToGraphQLName = G.mkName . T.replace "-" "_" . sessionVariableToText

parseSessionVariable :: Text -> Parser SessionVariable
parseSessionVariable t =
  if isSessionVariable t
    then pure $ mkSessionVariable t
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

newtype SessionVariables = SessionVariables {unSessionVariables :: Map.HashMap SessionVariable SessionVariableValue}
  deriving (Show, Eq, Hashable, Semigroup, Monoid)

filterSessionVariables ::
  (SessionVariable -> SessionVariableValue -> Bool) ->
  SessionVariables ->
  SessionVariables
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

data UserInfo = UserInfo
  { _uiRole :: !RoleName,
    _uiSession :: !SessionVariables,
    _uiBackendOnlyFieldAccess :: !BackendOnlyFieldAccess
  }
  deriving (Show, Eq, Generic)

instance Hashable UserInfo

class (Monad m) => UserInfoM m where
  askUserInfo :: m UserInfo

instance (UserInfoM m) => UserInfoM (ReaderT r m) where
  askUserInfo = lift askUserInfo

instance (UserInfoM m) => UserInfoM (ExceptT r m) where
  askUserInfo = lift askUserInfo

instance (UserInfoM m) => UserInfoM (StateT s m) where
  askUserInfo = lift askUserInfo

instance (UserInfoM m) => UserInfoM (TraceT m) where
  askUserInfo = lift askUserInfo

askCurRole :: (UserInfoM m) => m RoleName
askCurRole = _uiRole <$> askUserInfo

-- | Represents how to build a role from the session variables
data UserRoleBuild
  = -- | Look for `x-hasura-role` session variable value and absence will raise an exception
    URBFromSessionVariables
  | -- | Look for `x-hasura-role` session variable value, if absent fall back to given role
    URBFromSessionVariablesFallback !RoleName
  | -- | Use only the pre-determined role
    URBPreDetermined !RoleName
  deriving (Show, Eq)

-- | Build @'UserInfo' from @'SessionVariables'
mkUserInfo ::
  forall m.
  (MonadError QErr m) =>
  UserRoleBuild ->
  UserAdminSecret ->
  SessionVariables ->
  m UserInfo
mkUserInfo roleBuild userAdminSecret sessionVariables = do
  roleName <- case roleBuild of
    URBFromSessionVariables ->
      onNothing maybeSessionRole $
        throw400 InvalidParams $ userRoleHeader <> " not found in session variables"
    URBFromSessionVariablesFallback roleName' -> pure $ fromMaybe roleName' maybeSessionRole
    URBPreDetermined roleName' -> pure roleName'
  backendOnlyFieldAccess <- getBackendOnlyFieldAccess
  let modifiedSession = modifySessionVariables roleName sessionVariables
  pure $ UserInfo roleName modifiedSession backendOnlyFieldAccess
  where
    maybeSessionRole = maybeRoleFromSessionVariables sessionVariables

    modifySessionVariables :: RoleName -> SessionVariables -> SessionVariables
    modifySessionVariables roleName =
      SessionVariables
        . Map.insert userRoleHeader (roleNameToTxt roleName)
        . Map.delete adminSecretHeader
        . Map.delete deprecatedAccessKeyHeader
        . unSessionVariables

    getBackendOnlyFieldAccess :: m BackendOnlyFieldAccess
    getBackendOnlyFieldAccess = case userAdminSecret of
      UAdminSecretNotSent -> pure BOFADisallowed
      UAdminSecretSent -> lookForBackendOnlyPermissionsConfig
      UAuthNotSet -> lookForBackendOnlyPermissionsConfig
      where
        lookForBackendOnlyPermissionsConfig =
          case getSessionVariableValue useBackendOnlyPermissionsHeader sessionVariables of
            Nothing -> pure BOFADisallowed
            Just varVal ->
              case parseStringAsBool (T.unpack varVal) of
                Left err ->
                  throw400 BadRequest $
                    useBackendOnlyPermissionsHeader <> ": " <> T.pack err
                Right privilege -> pure $ if privilege then BOFAAllowed else BOFADisallowed

maybeRoleFromSessionVariables :: SessionVariables -> Maybe RoleName
maybeRoleFromSessionVariables sessionVariables =
  -- returns Nothing if x-hasura-role is an empty string
  getSessionVariableValue userRoleHeader sessionVariables >>= mkRoleName

adminUserInfo :: UserInfo
adminUserInfo = UserInfo adminRoleName mempty BOFADisallowed
