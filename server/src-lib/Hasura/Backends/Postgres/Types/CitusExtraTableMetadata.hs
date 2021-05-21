module Hasura.Backends.Postgres.Types.CitusExtraTableMetadata
  ( ExtraTableMetadata(..)
  ) where

import           Hasura.Prelude

import           Data.Typeable      (Typeable)

import qualified Data.Aeson.Casing  as JC
import qualified Data.Aeson.TH      as J

import           Hasura.Incremental (Cacheable)


data ExtraTableMetadata
  = Local
  | Reference
  | Distributed { distributionColumn :: Text }
  deriving stock (Show, Eq, Generic, Typeable)

instance Hashable ExtraTableMetadata
instance Cacheable ExtraTableMetadata
instance NFData ExtraTableMetadata
instance Arbitrary ExtraTableMetadata where
  arbitrary = genericArbitrary

$(J.deriveJSON J.defaultOptions{ J.constructorTagModifier = JC.snakeCase, J.fieldLabelModifier = JC.snakeCase } ''ExtraTableMetadata)
