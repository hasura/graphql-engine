module Hasura.Backends.DataConnector.RQLGenerator.GenTablePermG
  ( genTablePermG,
  )
where

import Hasura.Backends.DataConnector.RQLGenerator.GenCommon (genAnnBoolExpFld, genTableName)
import Hasura.Generator.Common (defaultRange)
import Hasura.Prelude (pure, ($))
import Hasura.RQL.IR.Generator (genAnnBoolExp)
import Hasura.RQL.IR.Select (TablePermG (TablePerm))
import Hasura.SQL.Backend
  ( BackendType (DataConnector),
  )
import Hedgehog (MonadGen)
import Hedgehog.Gen
  ( integral,
    maybe,
  )

--------------------------------------------------------------------------------
-- Exported

genTablePermG :: MonadGen m => m a -> m (TablePermG 'DataConnector a)
genTablePermG genA = do
  let genV = genAnnBoolExpFld genA
  gBoolExp <- genAnnBoolExp @_ @_ @('DataConnector) genV genTableName
  limit <- maybe (integral defaultRange)
  pure $ TablePerm gBoolExp limit
