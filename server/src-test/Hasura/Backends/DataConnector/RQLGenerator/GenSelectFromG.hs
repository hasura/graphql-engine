module Hasura.Backends.DataConnector.RQLGenerator.GenSelectFromG
  ( genSelectFromG,
  )
where

import Hasura.Backends.DataConnector.RQLGenerator.GenCommon
  ( genColumn,
    genFunctionArgumentExp,
    genFunctionName,
    genScalarType,
    genTableName,
  )
import Hasura.Generator.Common (defaultRange)
import Hasura.Prelude hiding (choice, maybe)
import Hasura.RQL.IR.Generator
  ( genFunctionArgsExpG,
    genIdentifier,
  )
import Hasura.RQL.IR.Select (SelectFromG (..))
import Hasura.SQL.Backend (BackendType (..))
import Hedgehog (MonadGen)
import Hedgehog.Gen (choice, list, maybe)

--------------------------------------------------------------------------------

genSelectFromG :: MonadGen m => m (SelectFromG 'DataConnector a)
genSelectFromG = choice [fromTable, fromIdentifier, fromFunction]
  where
    fromTable = FromTable <$> genTableName
    fromIdentifier = FromIdentifier <$> genIdentifier
    fromFunction = do
      funcName <- genFunctionName
      funcArgsExp <- genFunctionArgsExpG genFunctionArgumentExp
      defList <- maybe . list defaultRange $ liftA2 (,) genColumn genScalarType
      pure $ FromFunction funcName funcArgsExp defList
