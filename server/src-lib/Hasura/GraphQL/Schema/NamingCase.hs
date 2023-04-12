{-# LANGUAGE OverloadedLists #-}

module Hasura.GraphQL.Schema.NamingCase
  ( NamingCase (..),
    isGraphqlCase,
    parseNamingConventionFromText,
    hasNamingConventionChanged,
  )
where

import Autodocodec qualified as AC
import Data.Aeson qualified as Aeson
import Data.HashSet qualified as Set
import Hasura.Prelude
import Hasura.Server.Types (ExperimentalFeature (..))

-- | Represents the different possible type cases for fields and types, i.e.
--   @HasuraCase@ and @GraphqlCase@ (@CamelCase@ fields and @PascalCase@ types).
data NamingCase = HasuraCase | GraphqlCase
  deriving (Eq, Show, Generic)

instance AC.HasCodec NamingCase where
  codec =
    AC.named "NamingCase" $
      AC.stringConstCodec [(HasuraCase, "hasura-default"), (GraphqlCase, "graphql-default")]

instance Aeson.ToJSON NamingCase where
  toJSON HasuraCase = Aeson.String "hasura-default"
  toJSON GraphqlCase = Aeson.String "graphql-default"

instance Aeson.FromJSON NamingCase where
  parseJSON = Aeson.withText "NamingCase" $ \s -> case parseNamingConventionFromText s of
    (Right nc) -> pure nc
    (Left err) -> fail err

-- Used for both the environment variable and JSON.
parseNamingConventionFromText :: Text -> Either String NamingCase
parseNamingConventionFromText "hasura-default" = Right HasuraCase
parseNamingConventionFromText "graphql-default" = Right GraphqlCase
parseNamingConventionFromText _ = Left "naming_convention can either be \"hasura-default\" or \"graphql-default\""

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
