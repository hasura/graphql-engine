module Test.Parser.Insert
  ( InsertQueryBuilder (..),
    mkInsertQuery,
  )
where

import Hasura.Backends.Postgres.SQL.Types (QualifiedTable)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (GBoolExp (..))
import Hasura.RQL.IR.Insert (InsertQueryP1 (..))
import Hasura.RQL.IR.Returning (MutationOutputG (..))
import Hasura.RQL.IR.Value (UnpreparedValue (..))
import Hasura.RQL.Types.Column (ColumnInfo (..))
import Test.Backend.Postgres.Misc

type Output = MutationOutputG PG Void (UnpreparedValue PG)

-- | Internal use only. The intended use is through
-- 'Test.Backend.Postgres.Insert.runTest'.
--
-- Build an 'InsertQueryP1', to be used with 'mkInsertQuery'.
data InsertQueryBuilder = InsertQueryBuilder
  { -- | the main table for the update
    iqbTable :: QualifiedTable,
    -- | the columns used in the insert statement
    iqbInsertColumns :: [ColumnInfo PG],
    -- | the rows of values to be inserted
    iqbValues :: [[UnpreparedValue PG]],
    -- | the 'Output' clause, e.g., selection set, affected_rows, etc.
    iqbOutput :: Output,
    -- | the table columns (all of them)
    iqbAllColumns :: [ColumnInfo PG]
  }

mkInsertQuery :: InsertQueryBuilder -> InsertQueryP1 PG
mkInsertQuery InsertQueryBuilder {..} =
  InsertQueryP1
    { iqp1Table = iqbTable,
      iqp1Cols = ciColumn <$> iqbInsertColumns,
      iqp1Tuples = fmap unpreparedValueToSQLExp <$> iqbValues,
      iqp1Conflict = Nothing,
      iqp1CheckCond = (BoolAnd [], Nothing),
      iqp1Output = unpreparedValueToSQLExp <$> iqbOutput,
      iqp1AllCols = iqbAllColumns
    }
