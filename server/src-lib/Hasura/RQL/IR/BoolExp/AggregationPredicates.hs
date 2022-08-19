{-# LANGUAGE UndecidableInstances #-}

-- | This module contains the default types and function that model aggregation
-- predicates.
module Hasura.RQL.IR.BoolExp.AggregationPredicates
  ( AggregationPredicatesImplementation (..),
    AggregationPredicate (..),
    AggregationPredicateArguments (..),
  )
where

import Data.Aeson
import Data.Aeson.Extended (ToJSONKeyValue (..))
import Data.Aeson.Key (fromText)
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (AnnBoolExp, OpExpG)
import Hasura.RQL.Types.Backend (Backend (Column))
import Hasura.RQL.Types.Backend qualified as B
import Hasura.RQL.Types.Relationships.Local (RelInfo)
import Hasura.SQL.Backend (BackendType)

-- | This type the default non-empty implementation of the
-- 'AggregationPredicates' type family of 'class Backend'.
--
-- This represents an _applied_ aggregation predicate, i.e. _not_ an aggegation
-- function in isolation.
--
-- In the default schema implementation, this type results from parsing graphql such as:
--
--    table(_where(
--             <relation>_aggregate:
--             {<functionname>: {
--                arguments: <arguments>,
--                predicate: <predicate>,
--                distinct: bool
--                }
--             }
--         ))
--      { ... }
--
-- Note that we make no attemt at modelling window functions or so-called
-- 'analytical' functions such as 'percentile_cont'.
data AggregationPredicatesImplementation (b :: BackendType) field = AggregationPredicatesImplementation
  { aggRelation :: RelInfo b,
    aggPredicates :: [AggregationPredicate b field]
  }
  deriving stock (Foldable, Traversable, Functor, Generic)

deriving instance
  ( B.Backend b,
    Eq (AggregationPredicate b field)
  ) =>
  Eq (AggregationPredicatesImplementation b field)

deriving instance
  ( B.Backend b,
    Show (AggregationPredicate b field)
  ) =>
  Show (AggregationPredicatesImplementation b field)

instance
  ( B.Backend b,
    Cacheable (AggregationPredicate b field)
  ) =>
  Cacheable (AggregationPredicatesImplementation b field)

instance
  ( B.Backend b,
    Hashable (AggregationPredicate b field)
  ) =>
  Hashable (AggregationPredicatesImplementation b field)

instance
  ( B.Backend b,
    NFData (AggregationPredicate b field)
  ) =>
  NFData (AggregationPredicatesImplementation b field)

instance
  ( ToJSON (AnnBoolExp b field),
    ToJSON (AggregationPredicate b field),
    ToJSON (OpExpG b field),
    B.Backend b
  ) =>
  ToJSON (AggregationPredicatesImplementation b field)

instance
  ( Backend b,
    ToJSONKeyValue (AggregationPredicate b field)
  ) =>
  ToJSONKeyValue (AggregationPredicatesImplementation b field)
  where
  toJSONKeyValue AggregationPredicatesImplementation {..} =
    ( fromText "aggregation_predicates",
      toJSON
        [ ("predicates" :: Text, toJSON $ map toJSONKeyValue aggPredicates),
          ("relation", toJSON aggRelation)
        ]
    )

data AggregationPredicate (b :: BackendType) field = AggregationPredicate
  { aggPredFunctionName :: Text,
    aggPredDistinct :: Bool,
    aggPredFilter :: Maybe (AnnBoolExp b field),
    aggPredArguments :: AggregationPredicateArguments b,
    aggPredPredicate :: [OpExpG b field]
  }
  deriving stock (Foldable, Traversable, Functor, Generic)

deriving instance
  ( B.Backend b,
    Eq (AnnBoolExp b field),
    Eq (OpExpG b field),
    Eq (AggregationPredicateArguments b)
  ) =>
  Eq (AggregationPredicate b field)

deriving instance
  ( B.Backend b,
    Show (AnnBoolExp b field),
    Show (OpExpG b field),
    Show (AggregationPredicateArguments b)
  ) =>
  Show (AggregationPredicate b field)

instance
  ( B.Backend b,
    Hashable (B.BooleanOperators b field),
    Hashable (AnnBoolExp b field),
    Hashable field
  ) =>
  Hashable (AggregationPredicate b field)

instance
  ( B.Backend b,
    Cacheable (AggregationPredicateArguments b),
    Cacheable (AnnBoolExp b field),
    Cacheable (OpExpG b field)
  ) =>
  Cacheable (AggregationPredicate b field)

instance
  ( B.Backend b,
    NFData (AggregationPredicateArguments b),
    NFData (AnnBoolExp b field),
    NFData (OpExpG b field)
  ) =>
  NFData (AggregationPredicate b field)

instance
  ( ToJSON (AggregationPredicateArguments b),
    ToJSON (AnnBoolExp b field),
    ToJSONKeyValue (OpExpG b field)
  ) =>
  ToJSONKeyValue (AggregationPredicate b field)
  where
  toJSONKeyValue AggregationPredicate {..} =
    ( fromText aggPredFunctionName,
      toJSON
        [ ("aggPredDistinct" :: Text, toJSON aggPredDistinct),
          ("aggPredFilter", toJSON aggPredFilter),
          ("aggPredArguments", toJSON aggPredArguments),
          ("aggPredPredicate", toJSON (map toJSONKeyValue aggPredPredicate))
        ]
    )

data AggregationPredicateArguments (b :: BackendType)
  = AggregationPredicateArgumentsStar
  | AggregationPredicateArguments (NonEmpty (Column b))
  deriving stock (Generic)

deriving instance B.Backend b => Eq (AggregationPredicateArguments b)

deriving instance B.Backend b => Show (AggregationPredicateArguments b)

instance Backend b => Hashable (AggregationPredicateArguments b)

instance Backend b => Cacheable (AggregationPredicateArguments b)

instance Backend b => NFData (AggregationPredicateArguments b)

instance (Backend b) => ToJSON (AggregationPredicateArguments b)
