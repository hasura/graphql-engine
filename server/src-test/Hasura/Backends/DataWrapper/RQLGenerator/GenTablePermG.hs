module Hasura.Backends.DataWrapper.RQLGenerator.GenTablePermG
  ( genTablePermG,
  )
where

import Hasura.Backends.DataWrapper.RQLGenerator.GenCommon (genAnnBoolExpFld, genTableName)
import Hasura.Generator.Common (defaultRange)
import Hasura.Prelude (pure, ($))
import Hasura.RQL.IR.Generator (genAnnBoolExp)
import Hasura.RQL.IR.Select (TablePermG (TablePerm))
import Hasura.RQL.Types
  ( BackendType (DataWrapper),
  )
import Hedgehog (MonadGen)
import Hedgehog.Gen
  ( integral,
    maybe,
  )

--------------------------------------------------------------------------------
-- Exported

genTablePermG :: MonadGen m => m a -> m (TablePermG 'DataWrapper a)
genTablePermG genA = do
  let genV = genAnnBoolExpFld genA
  gBoolExp <- genAnnBoolExp @_ @_ @('DataWrapper) genV genTableName
  limit <- maybe (integral defaultRange)
  pure $ TablePerm gBoolExp limit
