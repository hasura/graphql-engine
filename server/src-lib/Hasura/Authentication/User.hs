module Hasura.Authentication.User
  ( UserAdminSecret (..),
    BackendOnlyFieldAccess (..),
    UserInfo (..),
    UserInfoM (..),
    ExtraUserInfo (..),
    UserRoleBuild (..),
    askCurRole,
    adminUserInfo,
    mkUserInfo,
  )
where

import Data.Aeson
import Data.Char qualified as Char
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Hasura.Authentication.Headers (adminSecretHeader, deprecatedAccessKeyHeader, useBackendOnlyPermissionsHeader, userRoleHeader)
import Hasura.Authentication.Role (RoleName, adminRoleName, roleNameToTxt)
import Hasura.Authentication.Session (SessionVariables (..), getSessionVariableValue, maybeRoleFromSessionVariables)
import Hasura.Base.Error (Code (..), QErr, throw400)
import Hasura.Prelude

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

instance FromJSON BackendOnlyFieldAccess where
  parseJSON = genericParseJSON hasuraJSON

instance ToJSON BackendOnlyFieldAccess where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

instance Hashable BackendOnlyFieldAccess

data UserInfo = UserInfo
  { _uiRole :: RoleName,
    _uiSession :: SessionVariables,
    _uiBackendOnlyFieldAccess :: BackendOnlyFieldAccess
  }
  deriving (Show, Eq, Generic)

instance Hashable UserInfo

instance FromJSON UserInfo where
  parseJSON = genericParseJSON hasuraJSON

instance ToJSON UserInfo where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

class (Monad m) => UserInfoM m where
  askUserInfo :: m UserInfo

instance (UserInfoM m) => UserInfoM (ReaderT r m) where
  askUserInfo = lift askUserInfo

instance (UserInfoM m) => UserInfoM (ExceptT r m) where
  askUserInfo = lift askUserInfo

instance (UserInfoM m) => UserInfoM (StateT s m) where
  askUserInfo = lift askUserInfo

-- | extra information used to identify a Hasura User
data ExtraUserInfo = ExtraUserInfo {_euiUserId :: Maybe Text}
  deriving (Show, Eq, Generic)

-- | Represents how to build a role from the session variables
data UserRoleBuild
  = -- | Look for `x-hasura-role` session variable value and absence will raise an exception
    URBFromSessionVariables
  | -- | Look for `x-hasura-role` session variable value, if absent fall back to given role
    URBFromSessionVariablesFallback !RoleName
  | -- | Use only the pre-determined role
    URBPreDetermined !RoleName
  deriving (Show, Eq)

askCurRole :: (UserInfoM m) => m RoleName
askCurRole = _uiRole <$> askUserInfo

adminUserInfo :: UserInfo
adminUserInfo = UserInfo adminRoleName mempty BOFADisallowed

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
      onNothing maybeSessionRole
        $ throw400 InvalidParams
        $ userRoleHeader
        <> " not found in session variables"
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
        . HashMap.insert userRoleHeader (roleNameToTxt roleName)
        . HashMap.delete adminSecretHeader
        . HashMap.delete deprecatedAccessKeyHeader
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
                  throw400 BadRequest
                    $ useBackendOnlyPermissionsHeader
                    <> ": "
                    <> T.pack err
                Right privilege -> pure $ if privilege then BOFAAllowed else BOFADisallowed

    parseStringAsBool :: String -> Either String Bool
    parseStringAsBool t
      | map Char.toLower t `elem` truthVals = Right True
      | map Char.toLower t `elem` falseVals = Right False
      | otherwise = Left errMsg
      where
        truthVals = ["true", "t", "yes", "y"]
        falseVals = ["false", "f", "no", "n"]

        errMsg =
          " Not a valid boolean text. "
            ++ "True values are "
            ++ show truthVals
            ++ " and  False values are "
            ++ show falseVals
            ++ ". All values are case insensitive"
