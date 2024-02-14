{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.Backends.DataConnector.API.V0.OrderBy
  ( OrderBy (..),
    OrderByRelation (..),
    OrderByElement (..),
    obeTargetPath,
    obeTarget,
    obeOrderDirection,
    OrderByTarget (..),
    _OrderByColumn,
    _OrderByStarCountAggregate,
    _OrderBySingleColumnAggregate,
    OrderDirection (..),
  )
where

import Autodocodec
import Autodocodec.OpenAPI ()
import Control.DeepSeq (NFData)
import Control.Lens (makeLenses, makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.Aggregate qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Column qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Expression qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Relationships qualified as API.V0
import Prelude

--------------------------------------------------------------------------------

data OrderBy = OrderBy
  { _obRelations :: HashMap API.V0.RelationshipName OrderByRelation,
    _obElements :: NonEmpty OrderByElement
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec OrderBy

instance HasCodec OrderBy where
  codec =
    object "OrderBy" $
      OrderBy
        <$> requiredField "relations" "A map of relationships from the current query table to target tables. The key of the map is the relationship name. The relationships are used within the order by elements." .= _obRelations
        <*> requiredField "elements" "The elements to order by, in priority order" .= _obElements

data OrderByRelation = OrderByRelation
  { _obrWhere :: Maybe API.V0.Expression,
    _obrSubrelations :: HashMap API.V0.RelationshipName OrderByRelation
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable, NFData)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec OrderByRelation

instance HasCodec OrderByRelation where
  codec =
    named "OrderByRelation" $
      object "OrderByRelation" $
        OrderByRelation
          <$> optionalFieldOrNull "where" "An expression to apply to the relationship's target table to filter it" .= _obrWhere
          <*> requiredField "subrelations" "Further relationships to follow from the relationship's target table. The key of the map is the relationship name." .= _obrSubrelations

data OrderByElement = OrderByElement
  { _obeTargetPath :: [API.V0.RelationshipName],
    _obeTarget :: OrderByTarget,
    _obeOrderDirection :: OrderDirection
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec OrderByElement

instance HasCodec OrderByElement where
  codec =
    object "OrderByElement" $
      OrderByElement
        <$> requiredField "target_path" "The relationship path from the current query table to the table that contains the target to order by. This is always non-empty for aggregate order by targets" .= _obeTargetPath
        <*> requiredField "target" "The target column or aggregate to order by" .= _obeTarget
        <*> requiredField "order_direction" "The direction of ordering to apply" .= _obeOrderDirection

data OrderByTarget
  = OrderByColumn API.V0.ColumnSelector (Maybe API.V0.RedactionExpressionName)
  | OrderByStarCountAggregate
  | OrderBySingleColumnAggregate API.V0.SingleColumnAggregate
  deriving stock (Eq, Generic, Ord, Show)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec OrderByTarget

instance HasCodec OrderByTarget where
  codec =
    object "OrderByTarget" $
      discriminatedUnionCodec "type" enc dec
    where
      columnCodec =
        (,)
          <$> requiredField' "column" .= fst
          <*> optionalFieldOrNull "redaction_expression" "If present, the name of the redaction expression to evaluate. If the expression is false, the column value must be nulled out before being ordered over." .= snd
      starAggregateCodec = pureCodec ()
      singleColumnAggregateCodec = API.V0.singleColumnAggregateObjectCodec
      enc = \case
        OrderByColumn c r -> ("column", mapToEncoder (c, r) columnCodec)
        OrderByStarCountAggregate -> ("star_count_aggregate", mapToEncoder () starAggregateCodec)
        OrderBySingleColumnAggregate agg -> ("single_column_aggregate", mapToEncoder agg singleColumnAggregateCodec)
      dec =
        HashMap.fromList
          [ ("column", ("OrderByColumn", mapToDecoder (uncurry OrderByColumn) columnCodec)),
            ("star_count_aggregate", ("OrderByStarCountAggregate", mapToDecoder (const OrderByStarCountAggregate) starAggregateCodec)),
            ("single_column_aggregate", ("OrderBySingleColumnAggregate", mapToDecoder OrderBySingleColumnAggregate singleColumnAggregateCodec))
          ]

data OrderDirection
  = Ascending
  | Descending
  deriving stock (Data, Eq, Generic, Ord, Show, Enum, Bounded)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec OrderDirection

instance HasCodec OrderDirection where
  codec =
    named "OrderDirection" $
      stringConstCodec [(Ascending, "asc"), (Descending, "desc")]

$(makeLenses 'OrderByElement)
$(makePrisms ''OrderByTarget)
