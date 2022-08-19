{-# LANGUAGE DeriveAnyClass #-}

module Hasura.Backends.DataConnector.IR.OrderBy
  ( OrderBy (..),
    OrderByRelation (..),
    OrderByElement (..),
    OrderByTarget (..),
    OrderDirection (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (ToJSON)
import Data.Aeson qualified as J
import Data.Bifunctor (bimap)
import Data.HashMap.Strict qualified as HashMap
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.IR.Aggregate qualified as IR.A
import Hasura.Backends.DataConnector.IR.Column qualified as IR.C
import Hasura.Backends.DataConnector.IR.Expression qualified as IR.E
import Hasura.Backends.DataConnector.IR.Relationships qualified as IR.R
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Witch qualified

--------------------------------------------------------------------------------

data OrderBy = OrderBy
  { _obRelations :: HashMap IR.R.RelationshipName OrderByRelation,
    _obElements :: NonEmpty OrderByElement
  }
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, Hashable, NFData)

instance ToJSON OrderBy where
  toJSON = J.genericToJSON J.defaultOptions

instance Witch.From OrderBy API.OrderBy where
  from OrderBy {..} =
    API.OrderBy
      { _obRelations = HashMap.fromList $ bimap Witch.from Witch.from <$> HashMap.toList _obRelations,
        _obElements = Witch.from <$> _obElements
      }

data OrderByRelation = OrderByRelation
  { _obrWhere :: Maybe IR.E.Expression,
    _obrSubrelations :: HashMap IR.R.RelationshipName OrderByRelation
  }
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, Hashable, NFData)

instance ToJSON OrderByRelation where
  toJSON = J.genericToJSON J.defaultOptions

instance Witch.From OrderByRelation API.OrderByRelation where
  from OrderByRelation {..} =
    API.OrderByRelation
      { _obrWhere = Witch.from <$> _obrWhere,
        _obrSubrelations = HashMap.fromList $ bimap Witch.from Witch.from <$> HashMap.toList _obrSubrelations
      }

data OrderByElement = OrderByElement
  { _obeTargetPath :: [IR.R.RelationshipName],
    _obeTarget :: OrderByTarget,
    _obeOrderDirection :: OrderDirection
  }
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, Hashable, NFData)

instance ToJSON OrderByElement where
  toJSON = J.genericToJSON J.defaultOptions

instance Witch.From OrderByElement API.OrderByElement where
  from OrderByElement {..} =
    API.OrderByElement
      { _obeTargetPath = Witch.from <$> _obeTargetPath,
        _obeTarget = Witch.from _obeTarget,
        _obeOrderDirection = Witch.from _obeOrderDirection
      }

data OrderByTarget
  = OrderByColumn IR.C.Name
  | OrderByStarCountAggregate
  | OrderBySingleColumnAggregate IR.A.SingleColumnAggregate
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, Hashable, NFData)

instance ToJSON OrderByTarget where
  toJSON = J.genericToJSON J.defaultOptions

instance Witch.From OrderByTarget API.OrderByTarget where
  from = \case
    OrderByColumn name -> API.OrderByColumn $ Witch.from name
    OrderByStarCountAggregate -> API.OrderByStarCountAggregate
    OrderBySingleColumnAggregate aggregate -> API.OrderBySingleColumnAggregate $ Witch.from aggregate

data OrderDirection
  = Ascending
  | Descending
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, Hashable, NFData)

instance ToJSON OrderDirection where
  toJSON = J.genericToJSON J.defaultOptions

instance Witch.From API.OrderDirection OrderDirection where
  from API.Ascending = Ascending
  from API.Descending = Descending

instance Witch.From OrderDirection API.OrderDirection where
  from Ascending = API.Ascending
  from Descending = API.Descending
