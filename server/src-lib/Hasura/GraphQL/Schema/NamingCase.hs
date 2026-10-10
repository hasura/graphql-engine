module Hasura.GraphQL.Schema.NamingCase
  ( isGraphqlCase,
    hasNamingConventionChanged,
  )
where

import Hasura.Prelude
import Hasura.RQL.Types.NamingCase

isGraphqlCase :: NamingCase -> Bool
isGraphqlCase GraphqlCase = True
isGraphqlCase _ = False

-- | Check if naming convention has changed
-- The value of naming convention depends on whether the naming convention is enabled
-- in experimental features and what the default naming convention
-- (`HASURA_GRAPHQL_DEFAULT_NAMING_CONVENTION`) is hence use both these values to
-- decide if naming convention has changed
hasNamingConventionChanged :: NamingCase -> NamingCase -> Bool
hasNamingConventionChanged prevDefaultNamingCase currDefaultNamingCase = 
    prevDefaultNamingCase /= currDefaultNamingCase