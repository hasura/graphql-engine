module Hasura.Backends.Postgres.RQLGenerator.GenSelectFromG
  ( genSelectFromG,
  )
where

import Hasura.Backends.Postgres.RQLGenerator.GenAssociatedTypes hiding (genIdentifier)
import Hasura.Generator.Common (defaultRange)
import Hasura.Prelude hiding (choice, maybe)
import Hasura.RQL.IR.Generator (genFunctionArgsExpG, genIdentifier)
import Hasura.RQL.IR.Select (SelectFromG (..))
import Hasura.RQL.Types.BackendType (BackendType (..), PostgresKind (..))
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen

--------------------------------------------------------------------------------

-- | @genA@ is a generator for some type @a@ which is threaded through
-- the 'SelectFromG' AST. At the leaf nodes this is in an 'ArgumentExp
-- a' term.
genSelectFromG :: (MonadGen m) => m a -> m (SelectFromG ('Postgres 'Vanilla) a)
genSelectFromG genA = Gen.choice [fromTable, fromIdentifier, fromFunction]
  where
    fromTable = FromTable <$> genTableName
    fromIdentifier = FromIdentifier <$> genIdentifier
    fromFunction = do
      funcName <- genFunctionName
      funcArgsExp <- genFunctionArgsExpG $ genFunctionArgumentExp genA
      defList <- Gen.maybe . Gen.list defaultRange $ liftA2 (,) genColumn genScalarType
      pure $ FromFunction funcName funcArgsExp defList
