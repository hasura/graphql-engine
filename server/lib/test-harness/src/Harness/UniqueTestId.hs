-- | Extracted from `TestEnvironment` to stop circular dep hell
module Harness.UniqueTestId
  ( UniqueTestId (..),
  )
where

import Data.Char qualified
import Data.UUID (UUID)
import Hasura.Prelude

newtype UniqueTestId = UniqueTestId {getUniqueTestId :: UUID}

-- | Sanitise UUID for use in BigQuery dataset name
-- must be alphanumeric (plus underscores)
instance Show UniqueTestId where
  show (UniqueTestId uuid) =
    fmap
      ( \a ->
          if Data.Char.isAlphaNum a
            then a
            else '_'
      )
      . show
      $ uuid
