module Hasura.Backends.Postgres.RQLGenerator.GenSelectArgsG
  ( genSelectArgsG,
  )
where

import Data.Int (Int64)
import Hasura.Backends.Postgres.RQLGenerator.GenAssociatedTypes
  ( genBooleanOperators,
    genColumn,
    genFunctionArgumentExp,
    genFunctionName,
    genScalarType,
    genTableName,
    genXComputedField,
  )
import Hasura.Backends.Postgres.SQL.DML (NullsOrder (..), OrderType (..))
import Hasura.Generator.Common (defaultRange)
import Hasura.Prelude hiding (maybe, nonEmpty)
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Generator
  ( genAnnBoolExp,
    genAnnBoolExpFld,
    genAnnRedactionExp,
    genAnnotatedOrderByElement,
    genAnnotatedOrderByItemG,
  )
import Hasura.RQL.IR.Select (AnnDistinctColumn (..), AnnotatedOrderByItemG, SelectArgsG (..))
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen

--------------------------------------------------------------------------------
-- Exported

genSelectArgsG :: forall m a. (MonadGen m) => m a -> m (SelectArgsG ('Postgres 'Vanilla) a)
genSelectArgsG genA = do
  _saWhere <- where'
  _saOrderBy <- orderBy
  _saLimit <- limit
  _saOffset <- offset
  _saDistinct <- distinct
  pure SelectArgs {..}
  where
    where' :: m (Maybe (AnnBoolExp ('Postgres 'Vanilla) a))
    where' =
      Gen.maybe
        $ genAnnBoolExp
          ( genAnnBoolExpFld
              genColumn
              genColumn
              genTableName
              genScalarType
              genFunctionName
              genXComputedField
              (genBooleanOperators genA)
              (genFunctionArgumentExp genA)
              genA
          )
          genTableName

    orderBy :: m (Maybe (NonEmpty (AnnotatedOrderByItemG ('Postgres 'Vanilla) a)))
    orderBy =
      Gen.maybe
        . Gen.nonEmpty defaultRange
        $ genAnnotatedOrderByItemG @_ @('Postgres 'Vanilla)
          genBasicOrderType
          genNullsOrderType
          ( genAnnotatedOrderByElement @_ @('Postgres 'Vanilla)
              genColumn
              genColumn
              genTableName
              genScalarType
              genFunctionName
              genXComputedField
              (genBooleanOperators genA)
              (genFunctionArgumentExp genA)
              genA
          )

    limit :: m (Maybe Int)
    limit = Gen.maybe $ Gen.integral defaultRange

    offset :: m (Maybe Int64)
    offset = Gen.maybe $ Gen.integral defaultRange

    distinct :: m (Maybe (NonEmpty (AnnDistinctColumn ('Postgres 'Vanilla) a)))
    distinct =
      Gen.maybe
        . Gen.nonEmpty defaultRange
        $ AnnDistinctColumn
        <$> genColumn
        <*> genAnnRedactionExp
          genColumn
          genColumn
          genTableName
          genScalarType
          genFunctionName
          genXComputedField
          (genBooleanOperators genA)
          (genFunctionArgumentExp genA)
          genA

--------------------------------------------------------------------------------
-- Unexported Helpers

genBasicOrderType :: (MonadGen m) => m (BasicOrderType ('Postgres 'Vanilla))
genBasicOrderType = Gen.element [OTAsc, OTDesc]

genNullsOrderType :: (MonadGen m) => m (NullsOrderType ('Postgres 'Vanilla))
genNullsOrderType = Gen.element [NullsFirst, NullsLast]
