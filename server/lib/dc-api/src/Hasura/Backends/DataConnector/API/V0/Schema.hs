{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}

module Hasura.Backends.DataConnector.API.V0.Schema
  ( SchemaRequest (..),
    SchemaFilters (..),
    DetailLevel (..),
    SchemaResponse (..),
    ObjectTypeDefinition (..),
  )
where

import Autodocodec
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.Column qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Function qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Table qualified as API.V0
import Language.GraphQL.Draft.Syntax qualified as G
import Servant.API qualified as Servant
import Prelude

--------------------------------------------------------------------------------
-- Schema Response

data SchemaRequest = SchemaRequest
  { _srFilters :: SchemaFilters,
    _srDetailLevel :: DetailLevel
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec SchemaRequest

instance HasCodec SchemaRequest where
  codec =
    object "SchemaRequest" $
      SchemaRequest
        <$> optionalFieldWithOmittedDefault "filters" mempty "Optional schema filtering settings" .= _srFilters
        <*> optionalFieldWithOmittedDefault "detail_level" Everything "Only return names for schema items" .= _srDetailLevel

data SchemaFilters = SchemaFilters
  { _sfOnlyTables :: Maybe [API.V0.TableName],
    _sfOnlyFunctions :: Maybe [API.V0.FunctionName]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec SchemaFilters

instance Semigroup SchemaFilters where
  a <> b =
    SchemaFilters
      { _sfOnlyTables = _sfOnlyTables a <> _sfOnlyTables b,
        _sfOnlyFunctions = _sfOnlyFunctions a <> _sfOnlyFunctions b
      }

instance Monoid SchemaFilters where
  mempty = SchemaFilters Nothing Nothing

instance HasCodec SchemaFilters where
  codec =
    object "SchemaFilters" $
      SchemaFilters
        <$> optionalField "only_tables" "Only get the schemas for these tables" .= _sfOnlyTables
        <*> optionalField "only_functions" "Only get the schemas for these functions" .= _sfOnlyFunctions

data DetailLevel = Everything | BasicInfo
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec DetailLevel

instance HasCodec DetailLevel where
  codec =
    named "DetailLevel" $
      stringConstCodec [(Everything, "everything"), (BasicInfo, "basic_info")]
        <??> [ "How much information to return about the schema. Values:",
               "- 'everything': All information about the schema.",
               "- 'basic_info': For tables, only the table name and table type, for functions, only the function name and function type."
             ]

-- | The Schema Response provides the schemas for tracked tables and
-- 'Capabilities' supported by the service.
data SchemaResponse = SchemaResponse
  { _srTables :: [API.V0.TableInfo],
    _srFunctions :: [API.V0.FunctionInfo],
    _srObjectTypes :: Maybe (NonEmpty ObjectTypeDefinition)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec SchemaResponse

instance HasCodec SchemaResponse where
  codec =
    object "SchemaResponse" $
      SchemaResponse
        <$> requiredField "tables" "Available tables" .= _srTables
        <*> optionalFieldWithOmittedDefault "functions" [] "Available functions" .= _srFunctions
        <*> optionalField "objectTypes" "Object type definitions referenced in this schema" .= _srObjectTypes

instance Servant.HasStatus SchemaResponse where
  type StatusOf SchemaResponse = 200

data ObjectTypeDefinition = ObjectTypeDefinition
  { _otdName :: G.Name,
    _otdDescription :: Maybe Text,
    _otdColumns :: NonEmpty API.V0.ColumnInfo
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ObjectTypeDefinition

instance HasCodec ObjectTypeDefinition where
  codec =
    object "ObjectTypeDefinition" $
      ObjectTypeDefinition
        <$> requiredField "name" "The name of the type" .= _otdName
        <*> optionalField "description" "The description of the type" .= _otdDescription
        <*> requiredField "columns" "The columns of the type" .= _otdColumns
