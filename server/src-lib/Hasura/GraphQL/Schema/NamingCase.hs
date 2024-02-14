module Hasura.GraphQL.Schema.NamingCase
  ( isGraphqlCase,
    hasNamingConventionChanged,
  )
where

import Data.HashSet qualified as Set
import Hasura.Prelude
import Hasura.RQL.Types.NamingCase
import Hasura.Server.Types (ExperimentalFeature (..))

isGraphqlCase :: NamingCase -> Bool
isGraphqlCase GraphqlCase = True
isGraphqlCase _ = False

-- | Check if naming convention has changed
-- The value of naming convention depends on whether the naming convention is enabled
-- in experimental features and what the default naming convention
-- (`HASURA_GRAPHQL_DEFAULT_NAMING_CONVENTION`) is hence use both these values to
-- decide if naming convention has changed
hasNamingConventionChanged :: (Set.HashSet ExperimentalFeature, NamingCase) -> (Set.HashSet ExperimentalFeature, NamingCase) -> Bool
hasNamingConventionChanged (prevExperimentalFeatures, prevDefaultNamingCase) (currExperimentalFeatures, currDefaultNamingCase) =
  case ((EFNamingConventions `elem` prevExperimentalFeatures, prevDefaultNamingCase), (EFNamingConventions `elem` currExperimentalFeatures, currDefaultNamingCase)) of
    -- naming convention has been enabled, and the default naming convention is not
    -- HasuraCase then the naming convention has changed
    ((False, _), (True, GraphqlCase)) -> True
    -- naming is enabled but the default naming convention changes, then the naming
    -- convention has changed
    ((True, GraphqlCase), (True, HasuraCase)) -> True
    ((True, HasuraCase), (True, GraphqlCase)) -> True
    -- graphql-case naming convention has been disabled, then the naming convention
    -- changes
    ((True, GraphqlCase), (False, _)) -> True
    _ -> False
