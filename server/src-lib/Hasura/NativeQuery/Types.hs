-- | This module houses the types that are necessary to even talk about native
-- queries abstract of a concrete implementation.
--
-- The default implementation is given in modules
-- 'Hasura.NativeQuery.Metadata', and 'Hasura.NativeQuery.API', but backends
-- are free to provide their own as needed.
module Hasura.NativeQuery.Types
  ( NativeQueryName (..),
  )
where

import Autodocodec (HasCodec (codec), dimapCodec)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Text.Extended (ToTxt)
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax qualified as G

-- The name of a native query. This appears as a root field name in the graphql schema.
newtype NativeQueryName = NativeQueryName {getNativeQueryName :: G.Name}
  deriving newtype (Eq, Ord, Show, Hashable, NFData, ToJSON, FromJSON, ToTxt)
  deriving stock (Generic)

instance HasCodec NativeQueryName where
  codec = dimapCodec NativeQueryName getNativeQueryName codec

instance FromJSONKey NativeQueryName

instance ToJSONKey NativeQueryName
