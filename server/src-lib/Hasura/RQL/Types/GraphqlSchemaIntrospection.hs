{-# LANGUAGE TemplateHaskell #-}

module Hasura.RQL.Types.GraphqlSchemaIntrospection
  ( SetGraphqlIntrospectionOptions (..),
  )
where

import Data.Aeson.TH
import Data.HashSet qualified as Set
import Hasura.Prelude
import Hasura.Session

newtype SetGraphqlIntrospectionOptions = SetGraphqlIntrospectionOptions {_idrDisabledForRoles :: (Set.HashSet RoleName)}
  deriving (Show, Eq, Generic, Semigroup, Monoid)

instance NFData SetGraphqlIntrospectionOptions

instance Hashable SetGraphqlIntrospectionOptions

$(deriveJSON hasuraJSON ''SetGraphqlIntrospectionOptions)
