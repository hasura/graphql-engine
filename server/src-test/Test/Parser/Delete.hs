module Test.Parser.Delete
  ( AnnotatedDeleteBuilder (..),
    mkAnnotatedDelete,
  )
where

import Hasura.Backends.Postgres.SQL.Types (QualifiedTable)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (GBoolExp (..), OpExpG)
import Hasura.RQL.IR.Delete (AnnDelG (..))
import Hasura.RQL.IR.Returning (MutationOutputG (..))
import Hasura.RQL.IR.Value (UnpreparedValue)
import Hasura.RQL.Types.BackendType (BackendType (Postgres), PostgresKind (Vanilla))
import Hasura.RQL.Types.Column (ColumnInfo)
import Hasura.RQL.Types.Instances ()
import Test.Parser.Expectation qualified as E

type PG = 'Postgres 'Vanilla

type Output = MutationOutputG PG Void (UnpreparedValue PG)

-- | Internal use only. The intended use is through
-- 'Test.Parser.Expectation.runTest'.
--
-- Build an 'AnnDelG', to be used with 'mkAnnotatedDelete'.
data AnnotatedDeleteBuilder = AnnotatedDeleteBuilder
  { -- | the main table for the update
    adbTable :: QualifiedTable,
    -- | the where clause(s)
    adbWhere :: [(ColumnInfo PG, [OpExpG PG (UnpreparedValue PG)])],
    -- | the 'Output' clause, e.g., selection set, affected_rows, etc.
    adbOutput :: Output,
    -- | the table columns (all of them)
    adbColumns :: [ColumnInfo PG]
  }

mkAnnotatedDelete :: AnnotatedDeleteBuilder -> AnnDelG PG Void (UnpreparedValue PG)
mkAnnotatedDelete AnnotatedDeleteBuilder {..} =
  AnnDel
    { _adTable = adbTable,
      _adWhere = (BoolAnd [], E.toBoolExp adbWhere),
      _adOutput = adbOutput,
      _adAllCols = adbColumns,
      _adNamingConvention = Nothing,
      _adValidateInput = Nothing,
      _adIsDeleteByPk = False
    }
