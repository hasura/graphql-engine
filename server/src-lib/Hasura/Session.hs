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
  , sessionVariableToText
  , mkSessionVariablesText
  , mkSessionVariables
  , sessionVariablesToHeaders
  , getSessionVariableValue
  , getSessionVariables
  , UserAdminSecret(..)
  , UserInfo
  , _uiRole
  , _uiSession
  , _uiBackendOnlyFieldAccess
  , mkUserInfo
  , adminUserInfo
  , BackendOnlyFieldAccess(..)
  ) where

import           Hasura.Incremental         (Cacheable)
import           Hasura.Prelude
import           Hasura.RQL.Types.Common    (NonEmptyText, adminText, mkNonEmptyText,
                                             unNonEmptyText)
import           Hasura.RQL.Types.Error
import           Hasura.Server.Utils
import           Hasura.SQL.Types

import           Data.Aeson
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift)

import qualified Data.CaseInsensitive       as CI
import qualified Data.HashMap.Strict        as Map
import qualified Data.Text                  as T
import qualified Database.PG.Query          as Q
import qualified Network.HTTP.Types         as HTTP

newtype RoleName
  = RoleName {getRoleTxt :: NonEmptyText}
  deriving ( Show, Eq, Ord, Hashable, FromJSONKey, ToJSONKey, FromJSON
           , ToJSON, Q.FromCol, Q.ToPrepArg, Lift, Generic, Arbitrary, NFData, Cacheable )

instance DQuote RoleName where
  dquoteTxt = roleNameToTxt

roleNameToTxt :: RoleName -> Text
roleNameToTxt = unNonEmptyText . getRoleTxt

mkRoleName :: Text -> Maybe RoleName
mkRoleName = fmap RoleName . mkNonEmptyText

adminRoleName :: RoleName
adminRoleName = RoleName adminText

isAdmin :: RoleName -> Bool
isAdmin = (adminRoleName ==)

newtype SessionVariable = SessionVariable {unSessionVariable :: CI.CI Text}
  deriving (Show, Eq, Hashable, IsString, Cacheable, Data, NFData)

instance ToJSON SessionVariable where
  toJSON = toJSON . CI.original . unSessionVariable

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
    toJSON $ Map.fromList $ map (first sessionVariableToText) $ Map.toList varMap

instance FromJSON SessionVariables where
  parseJSON v = mkSessionVariablesText . Map.toList <$> parseJSON v

mkSessionVariablesText :: [(Text, Text)] -> SessionVariables
mkSessionVariablesText =
  SessionVariables . Map.fromList . map (first mkSessionVariable)

mkSessionVariables :: [HTTP.Header] -> SessionVariables
mkSessionVariables =
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

-- | Build user info from @'SessionVariables'
mkUserInfo
  :: (MonadError QErr m)
  => UserAdminSecret
  -> SessionVariables
  -> Maybe RoleName -- ^ Default role if x-hasura-role session variable not found
  -> m UserInfo
mkUserInfo userAdminSecret sess@(SessionVariables sessVars) defaultRole = do
  roleName <- onNothing (maybeRoleFromSession <|> defaultRole) $
              throw400 InvalidParams $ userRoleHeader <> " not found in session variables"
  -- see Note [Backend only permissions] to know more about the following logic.
  backendOnlyFieldAccess <- case userAdminSecret of
      UAdminSecretNotSent -> pure BOFADisallowed
      UAdminSecretSent    -> lookForBackendOnlyPermissionsConfig
      UAuthNotSet         -> lookForBackendOnlyPermissionsConfig
  let modifiedSession = SessionVariables $ modifySessionVariables roleName sessVars
  pure $ UserInfo roleName modifiedSession backendOnlyFieldAccess
  where
    -- Add x-hasura-role header and remove admin secret headers
    modifySessionVariables roleName
      = Map.insert userRoleHeader (roleNameToTxt roleName)
        . Map.delete adminSecretHeader
        . Map.delete deprecatedAccessKeyHeader

    -- returns Nothing if x-hasura-role is an empty string
    maybeRoleFromSession =
      getSessionVariableValue userRoleHeader sess >>= mkRoleName

    lookForBackendOnlyPermissionsConfig =
      case getSessionVariableValue useBackendOnlyPermissionsHeader sess of
        Nothing     -> pure BOFADisallowed
        Just varVal ->
          case parseStringAsBool (T.unpack varVal) of
            Left err        -> throw400 BadRequest $
              useBackendOnlyPermissionsHeader <> ": " <> T.pack err
            Right privilege -> pure $ if privilege then BOFAAllowed else BOFADisallowed

adminUserInfo :: UserInfo
adminUserInfo = UserInfo adminRoleName mempty BOFADisallowed
