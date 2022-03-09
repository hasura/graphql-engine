module Hasura.Backends.DataWrapper.IR.Table
  ( Name,
  )
where

--------------------------------------------------------------------------------

import Hasura.Backends.DataWrapper.IR.Name qualified as Name

--------------------------------------------------------------------------------

-- | An alias for 'Name.Table' 'Name.Name's.
--
-- This alias is defined in its own module primarily for the convenience of
-- importing it qualified.
--
-- For example:
-- @
--   import Data.Coerce (coerce)
--   import Hasura.Experimental.IR.Table qualified as Table (Name)
--
--   example :: Table.Name
--   example = coerce @Text @Table.Name "table_name"
-- @
type Name = Name.Name 'Name.Table
