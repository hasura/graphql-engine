{-# LANGUAGE TemplateHaskell #-}

module Hasura.RemoteSchema.Metadata.Customization
  ( RemoteTypeCustomization (..),
    RemoteFieldCustomization (..),
    RemoteSchemaCustomization (..),
  )
where

import Data.Aeson qualified as J
import Data.Aeson.TH qualified as J
import Hasura.Incremental (Cacheable)
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

instance Cacheable RemoteTypeCustomization

instance Hashable RemoteTypeCustomization

$(J.deriveToJSON hasuraJSON {J.omitNothingFields = True} ''RemoteTypeCustomization)

instance J.FromJSON RemoteTypeCustomization where
  parseJSON = J.withObject "RemoteTypeCustomization" $ \o ->
    RemoteTypeCustomization
      <$> o J..:? "prefix"
      <*> o J..:? "suffix"
      <*> o J..:? "mapping" J..!= mempty

data RemoteFieldCustomization = RemoteFieldCustomization
  { _rfcParentType :: G.Name,
    _rfcPrefix :: Maybe G.Name,
    _rfcSuffix :: Maybe G.Name,
    _rfcMapping :: HashMap G.Name G.Name
  }
  deriving (Show, Eq, Generic)

instance NFData RemoteFieldCustomization

instance Cacheable RemoteFieldCustomization

instance Hashable RemoteFieldCustomization

$(J.deriveToJSON hasuraJSON {J.omitNothingFields = True} ''RemoteFieldCustomization)

instance J.FromJSON RemoteFieldCustomization where
  parseJSON = J.withObject "RemoteFieldCustomization" $ \o ->
    RemoteFieldCustomization
      <$> o J..: "parent_type"
      <*> o J..:? "prefix"
      <*> o J..:? "suffix"
      <*> o J..:? "mapping" J..!= mempty

data RemoteSchemaCustomization = RemoteSchemaCustomization
  { _rscRootFieldsNamespace :: Maybe G.Name,
    _rscTypeNames :: Maybe RemoteTypeCustomization,
    _rscFieldNames :: Maybe [RemoteFieldCustomization]
  }
  deriving (Show, Eq, Generic)

instance NFData RemoteSchemaCustomization

instance Cacheable RemoteSchemaCustomization

instance Hashable RemoteSchemaCustomization

$(J.deriveJSON hasuraJSON {J.omitNothingFields = True} ''RemoteSchemaCustomization)
