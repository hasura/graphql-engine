-- | This module defines the schema dependency gathering aspect of the default
-- implementation of aggregation predicates.
module Hasura.RQL.Types.SchemaCache.AggregationPredicates
  ( defaultGetAggregationPredicateDeps,
  )
where

import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (PartialSQLExp)
import Hasura.RQL.IR.BoolExp.AggregationPredicates
  ( AggregationPredicate (..),
    AggregationPredicatesImplementation (AggregationPredicatesImplementation),
  )
import Hasura.RQL.Types.Relationships.Local
  ( RelInfo (..),
    RelTarget (..),
  )
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCacheTypes
import Hasura.SQL.AnyBackend qualified as AB

defaultGetAggregationPredicateDeps ::
  forall b.
  (GetAggregationPredicatesDeps b) =>
  AggregationPredicatesImplementation b (PartialSQLExp b) ->
  BoolExpM b [SchemaDependency]
defaultGetAggregationPredicateDeps (AggregationPredicatesImplementation relInfo _rowPermissions functions) = do
  BoolExpCtx {source, currTable} <- ask
  let relationshipName = riName relInfo
      relationshipTable = case riTarget relInfo of
        RelTargetNativeQuery _ -> error "defaultGetAggregationPredicateDeps RelTargetNativeQuery"
        RelTargetTable tn -> tn
      schemaDependency =
        SchemaDependency
          ( SOSourceObj source
              $ AB.mkAnyBackend
              $ SOITableObj @b currTable (TORel relationshipName)
          )
          DROnType
   in (schemaDependency :) <$> local (\e -> e {currTable = relationshipTable}) (getFunctionDeps functions)
  where
    getFunctionDeps :: AggregationPredicate b (PartialSQLExp b) -> BoolExpM b [SchemaDependency]
    getFunctionDeps AggregationPredicate {..} =
      do
        BoolExpCtx {source, currTable} <- ask
        let filterDeps = maybe [] (getBoolExpDeps source currTable) aggPredFilter
        predicateDeps <- getOpExpDeps aggPredPredicate
        return $ filterDeps ++ predicateDeps
