module Test.Parser.Insert
  ( InsertQueryBuilder (..),
    mkInsertQuery,
  )
where

import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types (PGScalarType, QualifiedTable)
import Hasura.Backends.Postgres.SQL.Value (txtEncoder, withScalarTypeAnn)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (GBoolExp (..))
import Hasura.RQL.IR.Insert (InsertQueryP1 (..))
import Hasura.RQL.IR.Returning (MutationOutputG (..))
import Hasura.RQL.IR.Value (UnpreparedValue (..))
import Hasura.RQL.Types.Column (ColumnInfo (..), ColumnType (..), ColumnValue (..))
import Hasura.RQL.Types.Instances ()
import Hasura.SQL.Backend (BackendType (Postgres), PostgresKind (Vanilla))

type PG = 'Postgres 'Vanilla

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

unpreparedValueToSQLExp :: UnpreparedValue PG -> S.SQLExp
unpreparedValueToSQLExp = \case
  UVLiteral sqlExp -> sqlExp
  UVParameter _varInfo cval -> withScalarTypeAnn (go $ cvType cval) (txtEncoder $ cvValue cval)
  _ -> error "unexpected value"
  where
    go :: ColumnType PG -> PGScalarType
    go = \case
      ColumnScalar t -> t
      ColumnEnumReference _ -> error "unexpected enum in translating column type"
