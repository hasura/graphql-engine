module Hasura.Backends.DataConnector.IR.Function (Name) where

--------------------------------------------------------------------------------

import Hasura.Backends.DataConnector.IR.Name qualified as IR.N

--------------------------------------------------------------------------------

-- | An alias for 'Name.Function' 'Name.Name's.
--
-- This alias is defined in its own module primarily for the convenience of
-- importing it qualified.
--
-- For example:
-- @
--   import Data.Coerce (coerce)
--   import Hasura.Experimental.IR.Function qualified as Function (Name)
--
--   example :: Function.Name
--   example = coerce @Text @Function.Name "function_name"
-- @
type Name = IR.N.Name 'IR.N.Function
