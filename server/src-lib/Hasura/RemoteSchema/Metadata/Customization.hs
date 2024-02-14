module Hasura.RemoteSchema.Metadata.Customization
  ( RemoteTypeCustomization (..),
    RemoteFieldCustomization (..),
    RemoteSchemaCustomization (..),
  )
where

import Autodocodec (HasCodec, codec, hashMapCodec, object, optionalField', optionalFieldWith', requiredFieldWith', (.=))
import Autodocodec.Extended (graphQLFieldNameCodec)
import Data.Aeson qualified as J
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax qualified as G

-- NOTE: Prefix and suffix use 'G.Name' so that we can '<>' to form a new valid
-- by-construction 'G.Name'.
data RemoteTypeCustomization = RemoteTypeCustomization
  { _rtcPrefix :: Maybe G.Name,
    _rtcSuffix :: Maybe G.Name,
    _rtcMapping :: HashMap G.Name G.Name
  }
  deriving (Show, Eq, Generic)

instance NFData RemoteTypeCustomization

instance Hashable RemoteTypeCustomization

instance HasCodec RemoteTypeCustomization where
  codec =
    object "RemoteTypeCustomization"
      $ RemoteTypeCustomization
      <$> optionalFieldWith' "prefix" graphQLFieldNameCodec
      .= _rtcPrefix
        <*> optionalFieldWith' "suffix" graphQLFieldNameCodec
      .= _rtcSuffix
        <*> requiredFieldWith' "mapping" (hashMapCodec graphQLFieldNameCodec)
      .= _rtcMapping

instance J.ToJSON RemoteTypeCustomization where
  toJSON = J.genericToJSON hasuraJSON {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding hasuraJSON {J.omitNothingFields = True}

instance J.FromJSON RemoteTypeCustomization where
  parseJSON = J.withObject "RemoteTypeCustomization" $ \o ->
    RemoteTypeCustomization
      <$> o
      J..:? "prefix"
      <*> o
      J..:? "suffix"
      <*> o
      J..:? "mapping"
      J..!= mempty

data RemoteFieldCustomization = RemoteFieldCustomization
  { _rfcParentType :: G.Name,
    _rfcPrefix :: Maybe G.Name,
    _rfcSuffix :: Maybe G.Name,
    _rfcMapping :: HashMap G.Name G.Name
  }
  deriving (Show, Eq, Generic)

instance NFData RemoteFieldCustomization

instance Hashable RemoteFieldCustomization

instance HasCodec RemoteFieldCustomization where
  codec =
    object "RemoteFieldCustomization"
      $ RemoteFieldCustomization
      <$> requiredFieldWith' "parent_type" graphQLFieldNameCodec
      .= _rfcParentType
        <*> optionalFieldWith' "prefix" graphQLFieldNameCodec
      .= _rfcPrefix
        <*> optionalFieldWith' "suffix" graphQLFieldNameCodec
      .= _rfcSuffix
        <*> requiredFieldWith' "mapping" (hashMapCodec graphQLFieldNameCodec)
      .= _rfcMapping

instance J.ToJSON RemoteFieldCustomization where
  toJSON = J.genericToJSON hasuraJSON {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding hasuraJSON {J.omitNothingFields = True}

instance J.FromJSON RemoteFieldCustomization where
  parseJSON = J.withObject "RemoteFieldCustomization" $ \o ->
    RemoteFieldCustomization
      <$> o
      J..: "parent_type"
      <*> o
      J..:? "prefix"
      <*> o
      J..:? "suffix"
      <*> o
      J..:? "mapping"
      J..!= mempty

data RemoteSchemaCustomization = RemoteSchemaCustomization
  { _rscRootFieldsNamespace :: Maybe G.Name,
    _rscTypeNames :: Maybe RemoteTypeCustomization,
    _rscFieldNames :: Maybe [RemoteFieldCustomization]
  }
  deriving (Show, Eq, Generic)

instance NFData RemoteSchemaCustomization

instance Hashable RemoteSchemaCustomization

instance HasCodec RemoteSchemaCustomization where
  codec =
    object "RemoteSchemaCustomization"
      $ RemoteSchemaCustomization
      <$> optionalFieldWith' "root_fields_namespace" graphQLFieldNameCodec
      .= _rscRootFieldsNamespace
        <*> optionalField' "type_names"
      .= _rscTypeNames
        <*> optionalField' "field_names"
      .= _rscFieldNames

instance J.FromJSON RemoteSchemaCustomization where
  parseJSON = J.genericParseJSON hasuraJSON {J.omitNothingFields = True}

instance J.ToJSON RemoteSchemaCustomization where
  toJSON = J.genericToJSON hasuraJSON {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding hasuraJSON {J.omitNothingFields = True}
