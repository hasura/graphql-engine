-- | PassthroughEnvVars separated into own file for circular dep reasons
module Harness.PassthroughEnvVars
  ( PassthroughEnvVars (..),
  )
where

import Hasura.Prelude

-- | When spawning new HGE instances from a binary, we may want to pass through
-- some environment variables (for database credentials, for instance).
newtype PassthroughEnvVars
  = PassthroughEnvVars [(String, String)]
