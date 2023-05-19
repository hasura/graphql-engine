{-# LANGUAGE DeriveAnyClass #-}

module Hasura.Backends.DataConnector.API.V0.Schema
  ( SchemaResponse (..),
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
