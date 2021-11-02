module Hasura.RQL.Types.GraphqlSchemaIntrospection where

import Data.Aeson.TH
import Data.HashSet qualified as Set
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.Session

newtype SetGraphqlIntrospectionOptions = SetGraphqlIntrospectionOptions {_idrDisabledForRoles :: (Set.HashSet RoleName)}
  deriving (Show, Eq, Generic, Semigroup, Monoid)

instance NFData SetGraphqlIntrospectionOptions

instance Cacheable SetGraphqlIntrospectionOptions

instance Hashable SetGraphqlIntrospectionOptions

$(deriveJSON hasuraJSON ''SetGraphqlIntrospectionOptions)
