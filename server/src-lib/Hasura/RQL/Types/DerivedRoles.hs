module Hasura.RQL.Types.DerivedRoles where

import           Hasura.Prelude

import           Data.Aeson.Casing
import           Data.Aeson.TH

import qualified Data.HashSet                 as Set

import           Hasura.Session

data AddDerivedRole
  = AddDerivedRole
  { _adrRoleName :: !RoleName
  , _adrRoleSet  :: !(Set.HashSet RoleName)
  } deriving (Show, Eq, Ord, Generic)
$(deriveJSON (aesonDrop 4 snakeCase) ''AddDerivedRole)

newtype DropDerivedRole
  = DropDerivedRole
  { _ddrRoleName :: RoleName
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 4 snakeCase) ''DropDerivedRole)
