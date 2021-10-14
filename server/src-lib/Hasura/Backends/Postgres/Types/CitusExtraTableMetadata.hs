module Hasura.Backends.Postgres.Types.CitusExtraTableMetadata
  ( ExtraTableMetadata (..),
  )
where

import Data.Aeson.Casing qualified as JC
import Data.Aeson.TH qualified as J
import Data.Typeable (Typeable)
import Hasura.Incremental (Cacheable)
import Hasura.Prelude

data ExtraTableMetadata
  = Local
  | Reference
  | Distributed {distributionColumn :: Text}
  deriving stock (Show, Eq, Generic, Typeable)

instance Hashable ExtraTableMetadata

instance Cacheable ExtraTableMetadata

instance NFData ExtraTableMetadata

$(J.deriveJSON J.defaultOptions {J.constructorTagModifier = JC.snakeCase, J.fieldLabelModifier = JC.snakeCase} ''ExtraTableMetadata)
