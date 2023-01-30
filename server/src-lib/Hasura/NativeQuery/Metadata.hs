{-# LANGUAGE UndecidableInstances #-}

-- | This module houses the types and functions associated with the default
-- implementation of the metadata of native queries.
module Hasura.NativeQuery.Metadata
  ( NativeQueryArgumentName (..),
    NativeQueryNameImpl (..),
    NativeQueryInfoImpl (..),
    TrackNativeQueryImpl (..),
    defaultNativeQueryTrackToInfo,
    module Hasura.NativeQuery.Types,
  )
where

import Autodocodec
import Autodocodec qualified as AC
import Data.Aeson
import Data.Text.Extended (ToTxt)
import Data.Voidable
import Hasura.Metadata.DTO.Utils (codecNamePrefix)
import Hasura.NativeQuery.Types
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.SQL.Backend

-- The name of a native query. This appears as a root field name in the graphql schema.
newtype NativeQueryNameImpl = NativeQueryNameImpl {getNativeQueryNameImpl :: Text}
  deriving (Eq, Ord, Show, Generic, Hashable, NFData, ToJSON, FromJSON, ToTxt)

instance FromJSONKey NativeQueryNameImpl

instance ToJSONKey NativeQueryNameImpl

deriving instance Eq (Voidable NativeQueryNameImpl)

deriving instance Hashable (Voidable NativeQueryNameImpl)

deriving instance FromJSON (Voidable NativeQueryNameImpl)

deriving instance FromJSONKey (Voidable NativeQueryNameImpl)

deriving instance Show (Voidable NativeQueryNameImpl)

deriving instance ToJSON (Voidable NativeQueryNameImpl)

deriving instance ToJSONKey (Voidable NativeQueryNameImpl)

instance HasCodec NativeQueryNameImpl where
  codec = coerceCodec @Text

-- | Default implementation of the Native Query metadata info object.
data NativeQueryInfoImpl (b :: BackendType) = NativeQueryInfoImpl
  { nqiiRootFieldName :: NativeQueryNameImpl,
    nqiiCode :: Text,
    nqiiReturns :: TableName b,
    nqiiArguments :: HashMap NativeQueryArgumentName (ScalarType b),
    nqiiDescription :: Maybe Text
  }
  deriving (Generic)

deriving instance Backend b => Eq (NativeQueryInfoImpl b)

deriving instance Backend b => Show (NativeQueryInfoImpl b)

instance Backend b => Hashable (NativeQueryInfoImpl b)

instance Backend b => NFData (NativeQueryInfoImpl b)

instance (Backend b, HasCodec (ScalarType b)) => HasCodec (NativeQueryInfoImpl b) where
  codec =
    CommentCodec
      ("A query in expressed in native code (SQL) to add to the GraphQL schema with configuration.")
      $ AC.object (codecNamePrefix @b <> "NativeQueryInfo")
      $ NativeQueryInfoImpl
        <$> requiredField "root_field_name" fieldNameDoc
          AC..= nqiiRootFieldName
        <*> requiredField "code" sqlDoc
          AC..= nqiiCode
        <*> requiredField "returns" returnsDoc
          AC..= nqiiReturns
        <*> requiredField "arguments" argumentDoc
          AC..= nqiiArguments
        <*> optionalField "description" descriptionDoc
          AC..= nqiiDescription
    where
      fieldNameDoc = "Root field name for the native query"
      sqlDoc = "Native code expression (SQL) to run"
      argumentDoc = "Free variables in the expression and their types"
      returnsDoc = "Return type (table) of the expression"
      descriptionDoc = "A description of the query which appears in the graphql schema"

instance (Backend b, HasCodec (ScalarType b)) => HasCodec (Voidable [NativeQueryInfoImpl b]) where
  codec = coerceCodec @([NativeQueryInfoImpl b])

deriving instance (Backend b) => Eq (Voidable [NativeQueryInfoImpl b])

deriving via
  (Autodocodec (Voidable [NativeQueryInfoImpl b]))
  instance
    (Backend b, HasCodec (ScalarType b)) => (FromJSON (Voidable [NativeQueryInfoImpl b]))

deriving via
  (Autodocodec (Voidable [NativeQueryInfoImpl b]))
  instance
    (Backend b, HasCodec (ScalarType b)) => (ToJSON (Voidable [NativeQueryInfoImpl b]))

deriving via
  (Autodocodec (NativeQueryInfoImpl b))
  instance
    (Backend b, HasCodec (ScalarType b)) => (FromJSON (NativeQueryInfoImpl b))

deriving via
  (Autodocodec (NativeQueryInfoImpl b))
  instance
    (Backend b, HasCodec (ScalarType b)) => (ToJSON (NativeQueryInfoImpl b))

deriving newtype instance (Backend b, HasCodec (ScalarType b)) => FromJSON (Voidable (NativeQueryInfoImpl b))

deriving newtype instance (Backend b, HasCodec (ScalarType b)) => ToJSON (Voidable (NativeQueryInfoImpl b))

newtype NativeQueryArgumentName = NativeQueryArgumentName {getNativeQueryArgumentName :: Text}
  deriving (Eq, Ord, Show, Generic, Hashable)

deriving newtype instance ToJSON NativeQueryArgumentName

deriving newtype instance FromJSON NativeQueryArgumentName

deriving newtype instance ToJSONKey NativeQueryArgumentName

deriving newtype instance FromJSONKey NativeQueryArgumentName

instance NFData (NativeQueryArgumentName)

-- | Default implementation of the 'track_native_query' request payload.
data TrackNativeQueryImpl (b :: BackendType) = TrackNativeQueryImpl
  { tnqSource :: SourceName,
    tnqRootFieldName :: NativeQueryNameImpl,
    tnqCode :: Text,
    tnqArguments :: HashMap NativeQueryArgumentName (ScalarType b),
    tnqDescription :: Maybe Text,
    tnqReturns :: TableName b
  }

-- | Default implementation of the method 'nativeQueryTrackToInfo'.
defaultNativeQueryTrackToInfo :: TrackNativeQueryImpl b -> NativeQueryInfoImpl b
defaultNativeQueryTrackToInfo TrackNativeQueryImpl {..} =
  NativeQueryInfoImpl {..}
  where
    nqiiRootFieldName = tnqRootFieldName
    nqiiCode = tnqCode
    nqiiReturns = tnqReturns
    nqiiArguments = tnqArguments
    nqiiDescription = tnqDescription

deriving instance (Backend b, HasCodec (ScalarType b)) => FromJSON (Voidable (TrackNativeQueryImpl b))

deriving instance (Backend b, HasCodec (ScalarType b)) => ToJSON (Voidable (TrackNativeQueryImpl b))

instance (Backend b, HasCodec (ScalarType b)) => HasCodec (TrackNativeQueryImpl b) where
  codec =
    CommentCodec
      ("A request to track a native query")
      $ AC.object (codecNamePrefix @b <> "TrackNativeQuery")
      $ TrackNativeQueryImpl
        <$> requiredField "source" sourceDoc
          AC..= tnqSource
        <*> requiredField "root_field_name" rootFieldDoc
          AC..= tnqRootFieldName
        <*> requiredField "code" codeDoc
          AC..= tnqCode
        <*> requiredField "arguments" argumentsDoc
          AC..= tnqArguments
        <*> optionalField "description" descriptionDoc
          AC..= tnqDescription
        <*> requiredField "returns" returnsDoc
          AC..= tnqReturns
    where
      sourceDoc = "The source in whic this native query should be tracked"
      rootFieldDoc = "Root field name for the native query"
      codeDoc = "Native code expression (SQL) to run"
      argumentsDoc = "Free variables in the expression and their types"
      returnsDoc = "Return type (table) of the expression"
      descriptionDoc = "A description of the query which appears in the graphql schema"

deriving via
  (Autodocodec (TrackNativeQueryImpl b))
  instance
    (Backend b, HasCodec (ScalarType b)) => FromJSON (TrackNativeQueryImpl b)

deriving via
  (Autodocodec (TrackNativeQueryImpl b))
  instance
    (Backend b, HasCodec (ScalarType b)) => ToJSON (TrackNativeQueryImpl b)
