module Hasura.Experimental.IR.Function (Name) where

--------------------------------------------------------------------------------

import Hasura.Experimental.IR.Name qualified as Name

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
type Name = Name.Name 'Name.Function
