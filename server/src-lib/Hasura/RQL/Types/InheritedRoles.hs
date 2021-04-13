module Hasura.RQL.Types.InheritedRoles where

import           Hasura.Prelude

import           Data.Aeson.Casing
import           Data.Aeson.TH

import qualified Data.HashSet       as Set

import           Hasura.Incremental (Cacheable)
import           Hasura.Session

data AddInheritedRole
  = AddInheritedRole
  { _adrRoleName :: !RoleName
  , _adrRoleSet  :: !(Set.HashSet RoleName)
  } deriving (Show, Eq, Ord, Generic)
instance Hashable AddInheritedRole
$(deriveJSON (aesonDrop 4 snakeCase) ''AddInheritedRole)
instance Cacheable AddInheritedRole

newtype DropInheritedRole
  = DropInheritedRole
  { _ddrRoleName :: RoleName
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 4 snakeCase) ''DropInheritedRole)
