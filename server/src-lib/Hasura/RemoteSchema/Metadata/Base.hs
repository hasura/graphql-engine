module Hasura.RemoteSchema.Metadata.Base
  ( RemoteSchemaName (..),
  )
where

import Data.Aeson qualified as J
import Data.Text.Extended
import Data.Text.NonEmpty
import Database.PG.Query qualified as PG
import Hasura.Incremental (Cacheable)
import Hasura.Prelude

-- | Remote schema identifier.
--
-- NOTE: no validation on the character set is done here; it's likely there is
-- a bug (FIXME) where this interacts with remote relationships and some name
-- mangling needs to happen.
newtype RemoteSchemaName = RemoteSchemaName
  {unRemoteSchemaName :: NonEmptyText}
  deriving
    ( Show,
      Eq,
      Ord,
      Hashable,
      J.ToJSON,
      J.ToJSONKey,
      J.FromJSON,
      PG.ToPrepArg,
      PG.FromCol,
      ToTxt,
      NFData,
      Generic,
      Cacheable
    )
