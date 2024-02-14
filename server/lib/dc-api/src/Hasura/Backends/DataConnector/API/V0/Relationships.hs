{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.Backends.DataConnector.API.V0.Relationships
  ( Relationships (..),
    pattern RTableRelationships,
    pattern RFunctionRelationships,
    FunctionRelationships (..),
    TableRelationships (..),
    InterpolatedRelationships (..),
    trelSourceTable,
    trelRelationships,
    frelRelationships,
    frelSourceFunction,
    Relationship (..),
    rTarget,
    rRelationshipType,
    rColumnMapping,
    RelationshipName (..),
    RelationshipType (..),
    SourceColumnName,
    TargetColumnName,
  )
where

import Autodocodec.Extended
import Autodocodec.OpenAPI ()
import Control.DeepSeq (NFData)
import Control.Lens (makeLenses, makePrisms)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Data (Data)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.Column qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Function qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.InterpolatedQuery qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Table qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Target qualified as API.V0
import Prelude

data Relationships = RTable TableRelationships | RFunction FunctionRelationships | RInterpolated InterpolatedRelationships
  deriving stock (Eq, Ord, Show, Generic)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec Relationships

pattern RTableRelationships :: API.V0.TableName -> HashMap RelationshipName Relationship -> Relationships
pattern RTableRelationships source rels = RTable (TableRelationships source rels)

pattern RFunctionRelationships :: API.V0.FunctionName -> HashMap RelationshipName Relationship -> Relationships
pattern RFunctionRelationships source rels = RFunction (FunctionRelationships source rels)

instance HasCodec Relationships where
  codec =
    named "Relationships" $
      object "Relationships" $
        discriminatedUnionCodec "type" enc dec
    where
      enc = \case
        RTable rt -> ("table", mapToEncoder rt objectCodec)
        RFunction rf -> ("function", mapToEncoder rf objectCodec)
        RInterpolated ri -> ("interpolated", mapToEncoder ri objectCodec)
      dec =
        HashMap.fromList
          [ ("table", ("TableRelationships", mapToDecoder RTable objectCodec)),
            ("function", ("FunctionRelationships", mapToDecoder RFunction objectCodec)),
            ("interpolated", ("InterpolatedRelationships", mapToDecoder RInterpolated objectCodec))
          ]

data InterpolatedRelationships = InterpolatedRelationships
  { _irSource :: API.V0.InterpolatedQueryId,
    _irRelationships :: HashMap RelationshipName Relationship
  }
  deriving stock (Eq, Ord, Show, Generic)

instance HasObjectCodec InterpolatedRelationships where
  objectCodec =
    InterpolatedRelationships
      <$> requiredField "source_interpolated_query" "The source interpolated query involved in the relationship" .= _irSource
      <*> requiredField "relationships" "A map of relationships from the interpolated table to targets. The key of the map is the relationship name" .= _irRelationships

-- NOTE: Prefix is `trel` due to TableRequest conflicting with `tr` prefix.
data TableRelationships = TableRelationships
  { _trelSourceTable :: API.V0.TableName,
    _trelRelationships :: HashMap RelationshipName Relationship
  }
  deriving stock (Eq, Ord, Show, Generic)

instance HasObjectCodec TableRelationships where
  objectCodec =
    TableRelationships
      <$> requiredField "source_table" "The name of the source table in the relationship" .= _trelSourceTable
      <*> requiredField "relationships" "A map of relationships from the source table to targets. The key of the map is the relationship name" .= _trelRelationships

data FunctionRelationships = FunctionRelationships
  { _frelSourceFunction :: API.V0.FunctionName,
    _frelRelationships :: HashMap RelationshipName Relationship
  }
  deriving stock (Eq, Ord, Show, Generic)

instance HasObjectCodec FunctionRelationships where
  objectCodec =
    FunctionRelationships
      <$> requiredField "source_function" "The name of the source function in the relationship" .= _frelSourceFunction
      <*> requiredField "relationships" "A map of relationships from the source function to targets. The key of the map is the relationship name" .= _frelRelationships

-- Top level seperation of tables and functions should be adopted here too.
data Relationship = Relationship
  { _rTarget :: API.V0.Target,
    _rRelationshipType :: RelationshipType,
    _rColumnMapping :: API.V0.ColumnPathMapping
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec Relationship

instance HasCodec Relationship where
  codec =
    object "Relationship" $
      Relationship
        <$> requiredField "target" "The name of the target table in the relationship" .= _rTarget
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

type SourceColumnName = API.V0.ColumnSelector

type TargetColumnName = API.V0.ColumnSelector

$(makeLenses 'TableRelationships)
$(makeLenses 'Relationship)
$(makeLenses 'FunctionRelationships)
$(makePrisms ''Relationships)
