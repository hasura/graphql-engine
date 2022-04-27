module Hasura.Backends.DataWrapper.RQLGenerator.GenSelectFromG
  ( genSelectFromG,
  )
where

import Hasura.Backends.DataWrapper.RQLGenerator.GenCommon
  ( genColumn,
    genFunctionName,
    genScalarType,
    genTableName,
  )
import Hasura.Generator.Common (defaultRange)
import Hasura.Prelude hiding (choice, maybe)
import Hasura.RQL.IR.Generator
  ( genArgumentExp,
    genFunctionArgsExpG,
    genIdentifier,
  )
import Hasura.RQL.IR.Select (SelectFromG (..))
import Hasura.SQL.Backend (BackendType (..))
import Hedgehog (MonadGen)
import Hedgehog.Gen (choice, list, maybe)

--------------------------------------------------------------------------------

genSelectFromG :: MonadGen m => m a -> m (SelectFromG 'DataWrapper a)
genSelectFromG genA = choice [fromTable, fromIdentifier, fromFunction]
  where
    fromTable = FromTable <$> genTableName
    fromIdentifier = FromIdentifier <$> genIdentifier
    fromFunction = do
      funcName <- genFunctionName
      funcArgsExp <- genFunctionArgsExpG $ genArgumentExp genA
      defList <- maybe . list defaultRange $ liftA2 (,) genColumn genScalarType
      pure $ FromFunction funcName funcArgsExp defList
