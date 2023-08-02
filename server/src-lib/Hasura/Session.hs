module Hasura.Session
  ( SessionVariable,
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
    ExtraUserInfo (..),
    maybeRoleFromSessionVariables,
  )
where

import Data.CaseInsensitive qualified as CI
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as Set
import Data.Text qualified as T
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.Roles (RoleName, adminRoleName, mkRoleName, roleNameToTxt)
import Hasura.RQL.Types.Session (BackendOnlyFieldAccess (..), ExtraUserInfo (..), SessionVariable (..), SessionVariableValue, SessionVariables (..), UserInfo (..), UserInfoM (..), UserRoleBuild (..), mkSessionVariable, mkSessionVariablesText, sessionVariableToText)
import Hasura.Server.Utils
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Types qualified as HTTP

-- | Converts a `SessionVariable` value to a GraphQL name.
-- This will fail if the session variable contains characters that are not valid
-- for a graphql names. It is the caller's responsibility to decide what to do
-- in such a case.
sessionVariableToGraphQLName :: SessionVariable -> Maybe G.Name
sessionVariableToGraphQLName = G.mkName . T.replace "-" "_" . sessionVariableToText

filterSessionVariables ::
  (SessionVariable -> SessionVariableValue -> Bool) ->
  SessionVariables ->
  SessionVariables
filterSessionVariables f = SessionVariables . HashMap.filterWithKey f . unSessionVariables

mkSessionVariablesHeaders :: [HTTP.Header] -> SessionVariables
mkSessionVariablesHeaders =
  SessionVariables
    . HashMap.fromList
    . map (first SessionVariable)
    . filter (isSessionVariable . CI.original . fst) -- Only x-hasura-* headers
    . map (CI.map bsToTxt *** bsToTxt)

sessionVariablesToHeaders :: SessionVariables -> [HTTP.Header]
sessionVariablesToHeaders =
  map ((CI.map txtToBs . unSessionVariable) *** txtToBs)
    . HashMap.toList
    . unSessionVariables

getSessionVariables :: SessionVariables -> [Text]
getSessionVariables = map sessionVariableToText . HashMap.keys . unSessionVariables

getSessionVariablesSet :: SessionVariables -> Set.HashSet SessionVariable
getSessionVariablesSet = HashMap.keysSet . unSessionVariables

getSessionVariableValue :: SessionVariable -> SessionVariables -> Maybe SessionVariableValue
getSessionVariableValue k = HashMap.lookup k . unSessionVariables

-- | Represent the admin secret state; whether the secret is sent
-- in the request or if actually authorization is not configured.
data UserAdminSecret
  = UAdminSecretSent
  | UAdminSecretNotSent
  | UAuthNotSet
  deriving (Show, Eq)

askCurRole :: (UserInfoM m) => m RoleName
askCurRole = _uiRole <$> askUserInfo

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

maybeRoleFromSessionVariables :: SessionVariables -> Maybe RoleName
maybeRoleFromSessionVariables sessionVariables =
  -- returns Nothing if x-hasura-role is an empty string
  getSessionVariableValue userRoleHeader sessionVariables >>= mkRoleName

adminUserInfo :: UserInfo
adminUserInfo = UserInfo adminRoleName mempty BOFADisallowed
