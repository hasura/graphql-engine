{-# LANGUAGE ApplicativeDo #-}

-- | This module defines the schema aspect of the default implementation of
-- aggregation predicates.
module Hasura.GraphQL.Schema.BoolExp.AggregationPredicates
  ( defaultAggregationPredicatesParser,

    -- * Data types describing aggregation functions supported by a backend
    FunctionSignature (..),
    ArgumentsSignature (..),
    ArgumentSignature (..),
  )
where

import Data.Functor.Compose
import Data.List.NonEmpty qualified as NE
import Hasura.GraphQL.Parser qualified as P
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.BoolExp
import Hasura.GraphQL.Schema.Common (MonadBuildSchemaBase, askTableInfo, textToName)
import Hasura.GraphQL.Schema.Parser
  ( InputFieldsParser,
    Kind (..),
    Parser,
  )
import Hasura.GraphQL.Schema.Table
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp.AggregationPredicates
import Hasura.RQL.IR.Value
import Hasura.RQL.Types.Backend qualified as B
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common (relNameToTxt)
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.SchemaCache hiding (askTableInfo)
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.Table
import Hasura.SQL.Backend (BackendType)
import Language.GraphQL.Draft.Syntax qualified as G

-- | This function is meant to serve as the default schema for Aggregation
-- Predicates represented in the IR by the type
-- 'Hasura.RQL.IR.BoolExp.AggregationPredicates.AggregationPredicates'.
defaultAggregationPredicatesParser ::
  forall b r m n.
  ( MonadBuildSchemaBase r m n,
    BackendSchema b,
    AggregationPredicatesSchema b
  ) =>
  [FunctionSignature b] ->
  SourceInfo b ->
  TableInfo b ->
  m (Maybe (InputFieldsParser n [AggregationPredicatesImplementation b (UnpreparedValue b)]))
defaultAggregationPredicatesParser aggFns si ti = runMaybeT do
  arrayRelationships <- fails $ return $ nonEmpty $ tableArrayRelationships ti
  aggregationFunctions <- fails $ return $ nonEmpty aggFns

  buildAnyOptionalFields $
    arrayRelationships <&> \rel -> do
      relTable <- askTableInfo si (riRTable rel)
      relGqlName <- textToName $ relNameToTxt $ riName rel
      typeGqlName <- (<> Name.__ <> relGqlName) <$> getTableGQLName relTable

      -- We only make a field for aggregations over a relation if at least
      -- some aggregation predicates are callable.
      relAggregateField rel relGqlName typeGqlName
        -- We only return an InputFieldsParser for aggregation predicates,
        -- if we parse at least one aggregation predicate
        <$> buildAnyOptionalFields
          ( aggregationFunctions <&> \FunctionSignature {..} -> do
              aggPredicateField fnGQLName typeGqlName <$> unfuse do
                aggPredArguments <-
                  -- We only include an aggregation predicate if we are able to
                  -- access columns all its arguments. This might fail due to
                  -- permissions or due to no columns of suitable types
                  -- existing on the table.
                  case fnArguments of
                    ArgumentsStar ->
                      maybe AggregationPredicateArgumentsStar AggregationPredicateArguments . nonEmpty
                        <$> fuse (fieldOptionalDefault Name._arguments Nothing [] . P.list <$> fails (tableSelectColumnsEnum si relTable))
                    Arguments args ->
                      AggregationPredicateArguments
                        <$> fuse
                          ( P.field Name._arguments Nothing
                              . P.object (typeGqlName <> Name.__ <> fnGQLName <> Name.__ <> Name._arguments) Nothing
                              <$> buildAllFieldsNE
                                ( args `for` \ArgumentSignature {..} ->
                                    P.field argName Nothing <$> fails (tableSelectColumnsPredEnum (== (ColumnScalar argType)) relGqlName si relTable)
                                )
                          )

                aggPredDistinct <- fuse $ return $ fieldOptionalDefault Name._distinct Nothing False P.boolean
                let aggPredFunctionName = fnName
                aggPredPredicate <- fuse $ P.field Name._predicate Nothing <$> lift (comparisonExps @b (ColumnScalar fnReturnType))
                aggPredFilter <- fuse $ P.fieldOptional Name._filter Nothing <$> lift (boolExp si relTable)
                pure $ AggregationPredicate {..}
          )
  where
    -- Input field of the aggregation predicates for one array relation.
    relAggregateField ::
      RelInfo b ->
      G.Name ->
      G.Name ->
      (InputFieldsParser n [AggregationPredicate b (UnpreparedValue b)]) ->
      (InputFieldsParser n (Maybe (AggregationPredicatesImplementation b (UnpreparedValue b))))
    relAggregateField rel typeGqlName relGqlName =
      P.fieldOptional (relGqlName <> Name.__ <> Name._aggregate) Nothing
        . P.object typeGqlName Nothing
        . fmap (AggregationPredicatesImplementation rel)

    -- Input field for a single aggregation predicate.
    aggPredicateField ::
      G.Name ->
      G.Name ->
      InputFieldsParser n (AggregationPredicate b (UnpreparedValue b)) ->
      InputFieldsParser n (Maybe (AggregationPredicate b (UnpreparedValue b)))
    aggPredicateField fnGQLName typeGqlName =
      P.fieldOptional fnGQLName Nothing . P.object (typeGqlName <> Name.__ <> fnGQLName) Nothing

    buildAnyOptionalFields :: NonEmpty (MaybeT m (InputFieldsParser n (Maybe c))) -> MaybeT m (InputFieldsParser n [c])
    buildAnyOptionalFields = fmap collectOptionalFields . collectBranchesNE
      where
        -- Collect a non-empty list of optional input field parsers into one input field
        -- parser parsing a list of the specified values.
        collectOptionalFields :: NonEmpty (InputFieldsParser n (Maybe a)) -> InputFieldsParser n [a]
        collectOptionalFields = fmap (catMaybes . NE.toList) . sequenceA

    buildAllFieldsNE :: MaybeT m (NonEmpty (InputFieldsParser n c)) -> MaybeT m (InputFieldsParser n (NonEmpty c))
    buildAllFieldsNE = fmap sequenceA

    -- Collect all the non-failed branches, failing if all branches failed.
    collectBranchesNE :: forall f a. Applicative f => NonEmpty (MaybeT f a) -> MaybeT f (NonEmpty a)
    collectBranchesNE xs = MaybeT $ NE.nonEmpty . catMaybes . NE.toList <$> sequenceA (xs <&> runMaybeT)

    -- Mark a computation as potentially failing.
    fails :: m (Maybe a) -> MaybeT m a
    fails = MaybeT

    -- Compose our monad with InputFieldsParser into one fused Applicative that
    -- acts on the parsed values directly.
    fuse :: MaybeT m (InputFieldsParser n a) -> Compose (MaybeT m) (InputFieldsParser n) a
    fuse = Compose

    -- The inverse of 'fuse'.
    unfuse :: Compose (MaybeT m) (InputFieldsParser n) a -> MaybeT m (InputFieldsParser n a)
    unfuse = getCompose

    -- Optional input field with a default value when the field is elided or null.
    fieldOptionalDefault ::
      forall k a. ('Input P.<: k) => G.Name -> Maybe G.Description -> a -> Parser k n a -> InputFieldsParser n a
    fieldOptionalDefault n d a p = fromMaybe a <$> P.fieldOptional n d p

data FunctionSignature (b :: BackendType) = FunctionSignature
  { fnName :: Text,
    fnGQLName :: G.Name,
    fnArguments :: ArgumentsSignature b,
    fnReturnType :: B.ScalarType b
  }

data ArgumentsSignature (b :: BackendType)
  = ArgumentsStar
  | Arguments (NonEmpty (ArgumentSignature b))

data ArgumentSignature (b :: BackendType) = ArgumentSignature
  { argType :: B.ScalarType b,
    argName :: G.Name
  }
