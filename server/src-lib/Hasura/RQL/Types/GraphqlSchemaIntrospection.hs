module Hasura.RQL.Types.GraphqlSchemaIntrospection
  ( SetGraphqlIntrospectionOptions (..),
  )
where

import Autodocodec (HasCodec (codec), dimapCodec)
import Autodocodec.Extended (hashSetCodec)
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON)
import Data.HashSet qualified as Set
import Hasura.Prelude
import Hasura.RQL.Types.Roles (RoleName)

newtype SetGraphqlIntrospectionOptions = SetGraphqlIntrospectionOptions {_idrDisabledForRoles :: (Set.HashSet RoleName)}
  deriving (Show, Eq, Generic, Semigroup, Monoid)

instance NFData SetGraphqlIntrospectionOptions

instance Hashable SetGraphqlIntrospectionOptions

instance HasCodec SetGraphqlIntrospectionOptions where
  codec = dimapCodec SetGraphqlIntrospectionOptions _idrDisabledForRoles hashSetCodec

instance FromJSON SetGraphqlIntrospectionOptions where
  parseJSON = genericParseJSON hasuraJSON

instance ToJSON SetGraphqlIntrospectionOptions where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON
