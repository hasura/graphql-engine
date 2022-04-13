module Hasura.Backends.Postgres.RQLGenerator.GenAnnSelectG
  ( genAnnSelectG,
  )
where

import Hasura.Backends.Postgres.RQLGenerator.GenSelectArgsG (genSelectArgsG)
import Hasura.Backends.Postgres.RQLGenerator.GenSelectFromG (genSelectFromG)
import Hasura.Backends.Postgres.RQLGenerator.GenTablePermG (genTablePermG)
import Hasura.Generator.Common (defaultRange)
import Hasura.Prelude hiding (bool)
import Hasura.RQL.IR.Generator (genFields)
import Hasura.RQL.IR.Select (AnnSelectG (AnnSelectG))
import Hasura.RQL.Types (BackendType (Postgres), PostgresKind (Vanilla), StringifyNumbers (..))
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen

--------------------------------------------------------------------------------

genAnnSelectG :: forall m f a. MonadGen m => m a -> m (f a) -> m (AnnSelectG ('Postgres 'Vanilla) f a)
genAnnSelectG genA genFA =
  AnnSelectG
    <$> genFields genFA defaultRange defaultRange
    <*> genSelectFromG genA
    <*> genTablePermG genA
    <*> genArgs
    <*> genStringifyNumbers
  where
    genStringifyNumbers =
      Gen.bool <&> \case
        False -> LeaveNumbersAlone
        True -> StringifyNumbers
    genArgs = genSelectArgsG genA
