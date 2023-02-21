-- | A name for a logical model as it is recognized by the graphql schema.
module Hasura.NativeQuery.Types
  ( LogicalModelName (..),
  )
where

import Autodocodec (HasCodec (codec), dimapCodec)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Text.Extended (ToTxt)
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax qualified as G

-- The name of a logical model. This appears as a root field name in the graphql schema.
newtype LogicalModelName = LogicalModelName {getLogicalModelName :: G.Name}
  deriving newtype (Eq, Ord, Show, Hashable, NFData, ToJSON, FromJSON, ToTxt)
  deriving stock (Generic)

instance HasCodec LogicalModelName where
  codec = dimapCodec LogicalModelName getLogicalModelName codec

instance FromJSONKey LogicalModelName

instance ToJSONKey LogicalModelName
