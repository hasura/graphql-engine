-- | A name for a native query as it is recognized by the graphql schema.
module Hasura.NativeQuery.Types
  ( NativeQueryName (..),
    NullableScalarType (..),
  )
where

import Autodocodec (HasCodec (codec), dimapCodec)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Text.Extended (ToTxt)
import Hasura.LogicalModel.NullableScalarType
import Hasura.Prelude hiding (first)
import Language.GraphQL.Draft.Syntax qualified as G
import Language.Haskell.TH.Syntax (Lift)

-- The name of a native query. This appears as a root field name in the graphql schema.
newtype NativeQueryName = NativeQueryName {getNativeQueryName :: G.Name}
  deriving newtype (Eq, Ord, Show, Hashable, NFData, ToJSON, FromJSON, ToTxt)
  deriving stock (Data, Generic, Lift)

instance HasCodec NativeQueryName where
  codec = dimapCodec NativeQueryName getNativeQueryName codec

instance FromJSONKey NativeQueryName

instance ToJSONKey NativeQueryName
