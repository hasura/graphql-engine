module Hasura.RQL.Types.Roles where

import           Hasura.Prelude

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH

import           Hasura.Incremental (Cacheable)
import           Hasura.Session

newtype ParentRoles =
  ParentRoles { _unParentRoles :: HashSet RoleName }
  deriving (Show, Eq, ToJSON, FromJSON, Generic)
instance Hashable ParentRoles
instance Cacheable ParentRoles

-- | The `Role` type represents a role by
--   containing its name and the names of its parent roles.
--   This type is used externally in the `add_inherited_role`
--   metadata API and is also used internally
--   in the permission building
--   part of the schema cache building process
data Role
  = Role
  { _rRoleName    :: !RoleName
  , _rParentRoles :: !ParentRoles
  -- ^ set of the parent role names, in case of
  -- non-inherited roles it will be an empty set
  } deriving (Show, Eq, Generic)
instance Hashable Role
instance Cacheable Role

instance ToJSON Role where
  toJSON (Role roleName parentRoles) =
    object
    [ "role_name" .= roleName
    -- the key for parent roles is "role_set"
    -- in the JSON encoding of the `Role` type
    -- is because when this feature
    -- was introduced, it was added as "role_set"
    , "role_set"  .= parentRoles
    ]

instance FromJSON Role where
  parseJSON = withObject "Role" $ \o ->
    Role <$> o .: "role_name" <*> o .: "role_set"

type InheritedRole = Role

newtype DropInheritedRole
  = DropInheritedRole
  { _ddrRoleName :: RoleName
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 4 snakeCase) ''DropInheritedRole)
