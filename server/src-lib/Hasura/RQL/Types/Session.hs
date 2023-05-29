module Hasura.RQL.Types.Session
  ( SessionVariable (..),
    SessionVariables (..),
    SessionVariableValue,
    parseSessionVariable,
    sessionVariablePrefix,
    sessionVariableToText,
    mkSessionVariable,
    mkSessionVariablesText,
    isSessionVariable,
    UserAdminSecret (..),
    BackendOnlyFieldAccess (..),
    UserInfo (..),
    UserInfoM (..),
    ExtraUserInfo (..),
    UserRoleBuild (..),
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser, toJSONKeyText)
import Data.CaseInsensitive qualified as CI
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Data.Text.Extended
import Hasura.Prelude
import Hasura.RQL.Types.Roles (RoleName)

newtype SessionVariable = SessionVariable {unSessionVariable :: CI.CI Text}
  deriving (Show, Eq, Hashable, IsString, Data, NFData, Ord)

instance ToJSON SessionVariable where
  toJSON = toJSON . CI.original . unSessionVariable

instance ToJSONKey SessionVariable where
  toJSONKey = toJSONKeyText sessionVariableToText

instance ToTxt SessionVariable where
  toTxt = sessionVariableToText

type SessionVariableValue = Text

sessionVariablePrefix :: Text
sessionVariablePrefix = "x-hasura-"

isSessionVariable :: Text -> Bool
isSessionVariable = T.isPrefixOf sessionVariablePrefix . T.toLower

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

newtype SessionVariables = SessionVariables {unSessionVariables :: HashMap.HashMap SessionVariable SessionVariableValue}
  deriving (Show, Eq, Hashable, Semigroup, Monoid)

instance ToJSON SessionVariables where
  toJSON (SessionVariables varMap) =
    toJSON $ mapKeys sessionVariableToText varMap

instance FromJSON SessionVariables where
  parseJSON v = mkSessionVariablesText <$> parseJSON v

mkSessionVariablesText :: HashMap.HashMap Text Text -> SessionVariables
mkSessionVariablesText = SessionVariables . mapKeys mkSessionVariable

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
  { _uiRole :: !RoleName,
    _uiSession :: !SessionVariables,
    _uiBackendOnlyFieldAccess :: !BackendOnlyFieldAccess
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
