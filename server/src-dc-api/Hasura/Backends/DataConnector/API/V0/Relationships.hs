{-# LANGUAGE OverloadedLists #-}

module Hasura.Backends.DataConnector.API.V0.Relationships
  ( TableRelationships (..),
    Relationship (..),
    RelationshipName (..),
    RelationshipType (..),
    SourceColumnName,
    TargetColumnName,
  )
where

import Autodocodec.Extended
import Autodocodec.OpenAPI ()
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Data (Data)
import Data.HashMap.Strict qualified as M
import Data.Hashable (Hashable)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.Column qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Table qualified as API.V0
import Prelude

data TableRelationships = TableRelationships
  { _trSourceTable :: API.V0.TableName,
    _trRelationships :: M.HashMap RelationshipName Relationship
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec TableRelationships

instance HasCodec TableRelationships where
  codec =
    object "TableRelationships" $
      TableRelationships
        <$> requiredField "source_table" "The name of the source table in the relationship" .= _trSourceTable
        <*> requiredField "relationships" "A map of relationships from the source table to target tables. The key of the map is the relationship name" .= _trRelationships

data Relationship = Relationship
  { _rTargetTable :: API.V0.TableName,
    _rRelationshipType :: RelationshipType,
    _rColumnMapping :: M.HashMap SourceColumnName TargetColumnName
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec Relationship

instance HasCodec Relationship where
  codec =
    object "Relationship" $
      Relationship
        <$> requiredField "target_table" "The name of the target table in the relationship" .= _rTargetTable
        <*> requiredField "relationship_type" "The type of the relationship" .= _rRelationshipType
        <*> requiredField "column_mapping" "A mapping between columns on the source table to columns on the target table" .= _rColumnMapping

newtype RelationshipName = RelationshipName {unRelationshipName :: Text}
  deriving stock (Data, Generic)
  deriving newtype (Eq, Hashable, Ord, Show, ToJSONKey, FromJSONKey, NFData)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec Text

instance HasCodec RelationshipName where
  codec = dimapCodec RelationshipName unRelationshipName codec

data RelationshipType = ObjectRelationship | ArrayRelationship
  deriving stock (Eq, Ord, Show, Generic, Data, Enum, Bounded)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec RelationshipType

instance HasCodec RelationshipType where
  codec =
    named "RelationshipType" $
      stringConstCodec [(ObjectRelationship, "object"), (ArrayRelationship, "array")]

type SourceColumnName = API.V0.ColumnName

type TargetColumnName = API.V0.ColumnName
