module Hasura.Backends.Postgres.RQLGenerator.GenSelectArgsG
  ( genSelectArgsG,
  )
where

import Data.Int (Int64)
import Hasura.Backends.Postgres.RQLGenerator.GenAssociatedTypes
  ( genBooleanOperators,
    genColumn,
    genFunctionName,
    genScalarType,
    genTableName,
    genXComputedField,
  )
import Hasura.Backends.Postgres.SQL.DML (NullsOrder (..), OrderType (..))
import Hasura.Generator.Common (defaultRange)
import Hasura.Prelude hiding (maybe, nonEmpty)
import Hasura.RQL.IR.Generator
  ( genAnnBoolExp,
    genAnnBoolExpFld,
    genAnnotatedOrderByElement,
    genAnnotatedOrderByItemG,
  )
import Hasura.RQL.IR.Select (AnnotatedOrderByItemG, SelectArgsG (..))
import Hasura.RQL.Types (AnnBoolExp, BackendType (Postgres), BasicOrderType, Column, NullsOrderType, PostgresKind (Vanilla))
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen

--------------------------------------------------------------------------------
-- Exported

genSelectArgsG :: forall m a. MonadGen m => m a -> m (SelectArgsG ('Postgres 'Vanilla) a)
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
      Gen.maybe $
        genAnnBoolExp
          ( genAnnBoolExpFld
              genColumn
              genTableName
              genScalarType
              genFunctionName
              genXComputedField
              (genBooleanOperators genA)
              genA
          )
          genTableName

    orderBy :: m (Maybe (NonEmpty (AnnotatedOrderByItemG ('Postgres 'Vanilla) a)))
    orderBy =
      Gen.maybe . Gen.nonEmpty defaultRange $
        genAnnotatedOrderByItemG @_ @('Postgres 'Vanilla)
          genBasicOrderType
          genNullsOrderType
          ( genAnnotatedOrderByElement @_ @('Postgres 'Vanilla)
              genColumn
              genTableName
              genScalarType
              genFunctionName
              genXComputedField
              (genBooleanOperators genA)
              genA
          )

    limit :: m (Maybe Int)
    limit = Gen.maybe $ Gen.integral defaultRange

    offset :: m (Maybe Int64)
    offset = Gen.maybe $ Gen.integral defaultRange

    distinct :: m (Maybe (NonEmpty (Column ('Postgres 'Vanilla))))
    distinct = Gen.maybe . Gen.nonEmpty defaultRange $ genColumn

--------------------------------------------------------------------------------
-- Unexported Helpers

genBasicOrderType :: MonadGen m => m (BasicOrderType ('Postgres 'Vanilla))
genBasicOrderType = Gen.element [OTAsc, OTDesc]

genNullsOrderType :: MonadGen m => m (NullsOrderType ('Postgres 'Vanilla))
genNullsOrderType = Gen.element [NFirst, NLast]
