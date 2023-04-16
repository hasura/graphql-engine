-- | A name for a custom return type as it is recognized by the graphql schema.
module Hasura.CustomReturnType.Types
  ( CustomReturnTypeName (..),
  )
where

import Autodocodec (HasCodec (codec), dimapCodec)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Text.Extended (ToTxt)
import Hasura.Prelude hiding (first)
import Language.GraphQL.Draft.Syntax qualified as G
import Language.Haskell.TH.Syntax (Lift)

-- The name of a custom return type. This appears as a root field name in the graphql schema.
newtype CustomReturnTypeName = CustomReturnTypeName {getCustomReturnTypeName :: G.Name}
  deriving newtype (Eq, Ord, Show, Hashable, NFData, ToJSON, FromJSON, ToTxt)
  deriving stock (Data, Generic, Lift)

instance HasCodec CustomReturnTypeName where
  codec = dimapCodec CustomReturnTypeName getCustomReturnTypeName codec

instance FromJSONKey CustomReturnTypeName

instance ToJSONKey CustomReturnTypeName
