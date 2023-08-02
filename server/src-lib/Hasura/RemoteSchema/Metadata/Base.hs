module Hasura.RemoteSchema.Metadata.Base
  ( RemoteSchemaName (..),
  )
where

import Autodocodec (HasCodec (codec), dimapCodec)
import Data.Aeson qualified as J
import Data.Text.Extended
import Data.Text.NonEmpty
import Database.PG.Query qualified as PG
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
      J.FromJSONKey,
      PG.ToPrepArg,
      PG.FromCol,
      ToTxt,
      NFData,
      Generic
    )

instance HasCodec RemoteSchemaName where
  codec = dimapCodec RemoteSchemaName unRemoteSchemaName codec
