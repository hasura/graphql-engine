{-# LANGUAGE OverloadedLists #-}

module Hasura.RQL.Types.NamingCase
  ( NamingCase (..),
    parseNamingConventionFromText,
  )
where

import Autodocodec qualified as AC
import Data.Aeson qualified as J
import Hasura.Prelude

-- | Represents the different possible type cases for fields and types, i.e.
--   @HasuraCase@ and @GraphqlCase@ (@CamelCase@ fields and @PascalCase@ types).
data NamingCase = HasuraCase | GraphqlCase
  deriving (Eq, Show, Generic)

instance AC.HasCodec NamingCase where
  codec =
    AC.named "NamingCase"
      $ AC.stringConstCodec [(HasuraCase, "hasura-default"), (GraphqlCase, "graphql-default")]

instance J.ToJSON NamingCase where
  toJSON HasuraCase = J.String "hasura-default"
  toJSON GraphqlCase = J.String "graphql-default"

instance J.FromJSON NamingCase where
  parseJSON = J.withText "NamingCase" $ \s -> case parseNamingConventionFromText s of
    (Right nc) -> pure nc
    (Left err) -> fail err

-- Used for both the environment variable and JSON.
parseNamingConventionFromText :: Text -> Either String NamingCase
parseNamingConventionFromText "hasura-default" = Right HasuraCase
parseNamingConventionFromText "graphql-default" = Right GraphqlCase
parseNamingConventionFromText _ = Left "naming_convention can either be \"hasura-default\" or \"graphql-default\""
