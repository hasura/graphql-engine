module Hasura.Backends.DataWrapper.IR.Column
  ( Name,
  )
where

--------------------------------------------------------------------------------

import Hasura.Backends.DataWrapper.IR.Name qualified as Name

--------------------------------------------------------------------------------

-- | An alias for 'Name.Column' 'Name.Name's.
--
-- This alias is defined in its own module primarily for the convenience of
-- importing it qualified.
--
-- For example:
-- @
--   import Data.Coerce (coerce)
--   import Hasura.Experimental.IR.Column qualified as Column (Name)
--
--   example :: Column.Name
--   example = coerce @Text @Column.Name "column_name"
-- @
type Name = Name.Name 'Name.Column
