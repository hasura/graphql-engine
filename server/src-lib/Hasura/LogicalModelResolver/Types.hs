-- | Common types shared between similar logical model resolvers.
module Hasura.LogicalModelResolver.Types
  ( ArgumentName (..),
    NullableScalarType (..),
    nullableScalarTypeMapCodec,
  )
where

import Autodocodec (HasCodec (codec), dimapCodec)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Hasura.Base.ErrorValue (squote)
import Hasura.Base.ToErrorValue
import Hasura.LogicalModel.NullableScalarType
import Hasura.Prelude hiding (first)

-- | A name of an argument to a native query or stored procedure.
newtype ArgumentName = ArgumentName
  { getArgumentName :: Text
  }
  deriving newtype (Eq, Ord, Show, Hashable)
  deriving stock (Generic)

instance ToErrorValue ArgumentName where
  toErrorValue (ArgumentName txt) = squote txt

instance HasCodec ArgumentName where
  codec = dimapCodec ArgumentName getArgumentName codec

deriving newtype instance ToJSON ArgumentName

deriving newtype instance FromJSON ArgumentName

deriving newtype instance ToJSONKey ArgumentName

deriving newtype instance FromJSONKey ArgumentName

instance NFData ArgumentName
