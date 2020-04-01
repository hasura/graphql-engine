module Hasura.User
  ( RoleName
  , mkRoleName
  , adminRoleName
  , isAdmin
  , roleNameToTxt
  , SessionVariable
  , mkSessionVariable
  , SessionVariables
  , sessionVariableToText
  , mkSessionVariablesText
  , mkSessionVariables
  , roleFromSession
  , sessionVariablesToHeaders
  , getSessionVariableValue
  , getSessionVariables
  , UserAdminSecret(..)
  , UserInfo
  , _uiRole
  , _uiSession
  , _uiAdminSecret
  , mkUserInfo
  , adminUserInfo
  ) where

import           Hasura.Incremental         (Cacheable)
import           Hasura.Prelude
import           Hasura.RQL.Types.Common    (NonEmptyText, adminText, mkNonEmptyText,
                                             unNonEmptyText)
import           Hasura.Server.Utils        (adminSecretHeader, deprecatedAccessKeyHeader,
                                             isSessionVariable, userRoleHeader)
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

-- returns Nothing if x-hasura-role is an empty string
roleFromSession :: SessionVariables -> Maybe RoleName
roleFromSession uv =
  getSessionVariableValue userRoleHeader uv >>= mkRoleName

getSessionVariableValue :: SessionVariable -> SessionVariables -> Maybe SessionVariableValue
getSessionVariableValue k = Map.lookup k . unSessionVariables

data UserAdminSecret
  = UAdminSecretPresent
  | UAdminSecretAbsent
  | UNoAuthSet
  deriving (Show, Eq, Generic)
instance Hashable UserAdminSecret

data UserInfo
  = UserInfo
  { _uiRole        :: !RoleName
  , _uiSession     :: !SessionVariables
  , _uiAdminSecret :: !UserAdminSecret
  } deriving (Show, Eq, Generic)
instance Hashable UserInfo

mkUserInfo :: RoleName -> SessionVariables -> UserAdminSecret -> UserInfo
mkUserInfo roleName (SessionVariables v) =
  UserInfo roleName $ SessionVariables $ Map.insert userRoleHeader (roleNameToTxt roleName) $
  foldl (flip Map.delete) v [adminSecretHeader, deprecatedAccessKeyHeader]

adminUserInfo :: UserInfo
adminUserInfo = mkUserInfo adminRoleName mempty UAdminSecretAbsent
