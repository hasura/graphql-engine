--
module Hasura.Backends.DataConnector.RQLGenerator.GenSelectArgsG
  ( genSelectArgsG,
  )
where

import Data.Int (Int64)
import Hasura.Backends.DataConnector.IR.OrderBy qualified as IR
import Hasura.Backends.DataConnector.RQLGenerator.GenCommon (genAnnBoolExpFld, genAnnotatedOrderByElement, genColumn, genTableName)
import Hasura.Generator.Common (defaultRange)
import Hasura.Prelude (Int, Maybe, NonEmpty, pure, ($), (.))
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Generator (genAnnBoolExp, genAnnotatedOrderByItemG)
import Hasura.RQL.IR.Select (AnnotatedOrderByItemG, SelectArgsG (..))
import Hasura.RQL.Types.Backend
import Hasura.SQL.Backend (BackendType (..))
import Hedgehog (MonadGen)
import Hedgehog.Gen (element, maybe, nonEmpty)
import Hedgehog.Internal.Gen (integral)

--------------------------------------------------------------------------------
-- Exported

genSelectArgsG :: forall m a. MonadGen m => m a -> m (SelectArgsG 'DataConnector a)
genSelectArgsG genA = do
  _saWhere <- where'
  _saOrderBy <- orderBy
  _saLimit <- limit
  _saOffset <- offset
  _saDistinct <- distinct
  pure SelectArgs {..}
  where
    where' :: m (Maybe (AnnBoolExp 'DataConnector a))
    where' = maybe $ genAnnBoolExp (genAnnBoolExpFld genA) genTableName

    orderBy :: m (Maybe (NonEmpty (AnnotatedOrderByItemG 'DataConnector a)))
    orderBy =
      maybe . nonEmpty defaultRange $
        genAnnotatedOrderByItemG @_ @'DataConnector
          genBasicOrderType
          genNullsOrderType
          (genAnnotatedOrderByElement genA)

    limit :: m (Maybe Int)
    limit = maybe $ integral defaultRange

    offset :: m (Maybe Int64)
    offset = maybe $ integral defaultRange

    distinct :: m (Maybe (NonEmpty (Column 'DataConnector)))
    distinct = maybe . nonEmpty defaultRange $ genColumn

genBasicOrderType :: MonadGen m => m (BasicOrderType 'DataConnector)
genBasicOrderType = element [IR.Ascending, IR.Descending]

genNullsOrderType :: MonadGen m => m (NullsOrderType 'DataConnector)
genNullsOrderType = element [(), ()]
