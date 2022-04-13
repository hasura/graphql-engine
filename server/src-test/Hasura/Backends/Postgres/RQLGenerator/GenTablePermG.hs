module Hasura.Backends.Postgres.RQLGenerator.GenTablePermG
  ( genTablePermG,
  )
where

import Hasura.Backends.Postgres.RQLGenerator.GenAssociatedTypes
  ( genBooleanOperators,
    genColumn,
    genFunctionName,
    genScalarType,
    genTableName,
    genXComputedField,
  )
import Hasura.Generator.Common (defaultRange)
import Hasura.Prelude
import Hasura.RQL.IR.Generator (genAnnBoolExp, genAnnBoolExpFld)
import Hasura.RQL.IR.Select (TablePermG (..))
import Hasura.SQL.Backend (BackendType (..), PostgresKind (..))
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen

--------------------------------------------------------------------------------

genTablePermG :: MonadGen m => m a -> m (TablePermG ('Postgres 'Vanilla) a)
genTablePermG genA = do
  let genV = genAnnBoolExpFld @_ @('Postgres 'Vanilla) genColumn genTableName genScalarType genFunctionName genXComputedField (genBooleanOperators genA) genA
  gBoolExp <- genAnnBoolExp @_ @_ @('Postgres 'Vanilla) genV genTableName
  limit <- Gen.maybe (Gen.integral defaultRange)
  pure $ TablePerm gBoolExp limit
