module Test.Backend.Postgres.Misc
  ( idColumn,
    nameColumn,
    descColumn,
    idColumnBuilder,
    nameColumnBuilder,
    descColumnBuilder,
    integerValue,
    textValue,
    textOld,
    textNew,
    textOther,
    integerOne,
    integerTwo,
    PG,
  )
where

import Hasura.Backends.Postgres.SQL.Types (PGScalarType (..))
import Hasura.Backends.Postgres.SQL.Value (PGScalarValue (..))
import Hasura.Prelude
import Hasura.RQL.IR.Value (UnpreparedValue (..))
import Hasura.RQL.Types.Column (ColumnInfo, ColumnType (..), ColumnValue (..))
import Hasura.SQL.Backend (BackendType (Postgres), PostgresKind (Vanilla))
import Test.Parser.Expectation qualified as Expect

type PG = 'Postgres 'Vanilla

idColumnBuilder :: Expect.ColumnInfoBuilder
idColumnBuilder =
  Expect.ColumnInfoBuilder
    { cibName = "id",
      cibType = ColumnScalar PGInteger,
      cibNullable = False,
      cibIsPrimaryKey = True
    }

nameColumnBuilder :: Expect.ColumnInfoBuilder
nameColumnBuilder =
  Expect.ColumnInfoBuilder
    { cibName = "name",
      cibType = ColumnScalar PGText,
      cibNullable = False,
      cibIsPrimaryKey = False
    }

descColumnBuilder :: Expect.ColumnInfoBuilder
descColumnBuilder =
  Expect.ColumnInfoBuilder
    { cibName = "description",
      cibType = ColumnScalar PGText,
      cibNullable = False,
      cibIsPrimaryKey = False
    }

idColumn :: ColumnInfo PG
idColumn = Expect.mkColumnInfo idColumnBuilder

nameColumn :: ColumnInfo PG
nameColumn = Expect.mkColumnInfo nameColumnBuilder

descColumn :: ColumnInfo PG
descColumn = Expect.mkColumnInfo descColumnBuilder

integerValue :: UnpreparedValue PG
integerValue =
  UVParameter Nothing $
    ColumnValue
      { cvType = ColumnScalar PGInteger,
        cvValue = PGValInteger 1
      }

textValue :: UnpreparedValue PG
textValue =
  UVParameter Nothing $
    ColumnValue
      { cvType = ColumnScalar PGText,
        cvValue = PGValText "new name"
      }

textOld :: UnpreparedValue PG
textOld =
  UVParameter Nothing $
    ColumnValue
      { cvType = ColumnScalar PGText,
        cvValue = PGValText "old name"
      }

textNew :: UnpreparedValue PG
textNew =
  UVParameter Nothing $
    ColumnValue
      { cvType = ColumnScalar PGText,
        cvValue = PGValText "new name"
      }

textOther :: UnpreparedValue PG
textOther =
  UVParameter Nothing $
    ColumnValue
      { cvType = ColumnScalar PGText,
        cvValue = PGValText "other"
      }

integerOne :: UnpreparedValue PG
integerOne =
  UVParameter Nothing $
    ColumnValue
      { cvType = ColumnScalar PGInteger,
        cvValue = PGValInteger 1
      }

integerTwo :: UnpreparedValue PG
integerTwo =
  UVParameter Nothing $
    ColumnValue
      { cvType = ColumnScalar PGInteger,
        cvValue = PGValInteger 2
      }
