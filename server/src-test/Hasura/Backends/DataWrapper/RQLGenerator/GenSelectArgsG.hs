--
module Hasura.Backends.DataWrapper.RQLGenerator.GenSelectArgsG
  ( genSelectArgsG,
  )
where

import Data.Int (Int64)
import Hasura.Backends.DataWrapper.IR.OrderBy qualified as IR
import Hasura.Backends.DataWrapper.RQLGenerator.GenCommon (genAnnBoolExpFld, genAnnotatedOrderByElement, genColumn, genTableName)
import Hasura.Generator.Common (defaultRange)
import Hasura.Prelude (Int, Maybe, NonEmpty, pure, ($), (.))
import Hasura.RQL.IR.Generator (genAnnBoolExp, genAnnotatedOrderByItemG)
import Hasura.RQL.IR.Select (AnnotatedOrderByItemG, SelectArgsG (..))
import Hasura.RQL.Types (AnnBoolExp, Backend (BasicOrderType, Column, NullsOrderType), BackendType (DataWrapper))
import Hedgehog (MonadGen)
import Hedgehog.Gen (element, maybe, nonEmpty)
import Hedgehog.Internal.Gen (integral)

--------------------------------------------------------------------------------
-- Exported

genSelectArgsG :: forall m a. MonadGen m => m a -> m (SelectArgsG 'DataWrapper a)
genSelectArgsG genA = do
  _saWhere <- where'
  _saOrderBy <- orderBy
  _saLimit <- limit
  _saOffset <- offset
  _saDistinct <- distinct
  pure SelectArgs {..}
  where
    where' :: m (Maybe (AnnBoolExp 'DataWrapper a))
    where' = maybe $ genAnnBoolExp (genAnnBoolExpFld genA) genTableName

    orderBy :: m (Maybe (NonEmpty (AnnotatedOrderByItemG 'DataWrapper a)))
    orderBy =
      maybe . nonEmpty defaultRange $
        genAnnotatedOrderByItemG @_ @'DataWrapper
          genBasicOrderType
          genNullsOrderType
          (genAnnotatedOrderByElement genA)

    limit :: m (Maybe Int)
    limit = maybe $ integral defaultRange

    offset :: m (Maybe Int64)
    offset = maybe $ integral defaultRange

    distinct :: m (Maybe (NonEmpty (Column 'DataWrapper)))
    distinct = maybe . nonEmpty defaultRange $ genColumn

genBasicOrderType :: MonadGen m => m (BasicOrderType 'DataWrapper)
genBasicOrderType = element [IR.Ascending, IR.Descending]

genNullsOrderType :: MonadGen m => m (NullsOrderType 'DataWrapper)
genNullsOrderType = element [(), ()]
