module Test.Backend.Postgres.Misc
  ( unpreparedValueToSQLExp,
    idColumn,
    nameColumn,
    descColumn,
    idColumnBuilder,
    nameColumnBuilder,
    descColumnBuilder,
    textOld,
    textNew,
    textOther,
    integerOne,
    integerTwo,
    PG,
    dummyUserInfo,
  )
where

import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types (PGScalarType (..))
import Hasura.Backends.Postgres.SQL.Value (PGScalarValue (..), txtEncoder, withScalarTypeAnn)
import Hasura.Prelude
import Hasura.RQL.IR.Value (Provenance (FreshVar), UnpreparedValue (..))
import Hasura.RQL.Types.BackendType (BackendType (Postgres), PostgresKind (Vanilla))
import Hasura.RQL.Types.Column (ColumnInfo, ColumnType (..), ColumnValue (..))
import Hasura.RQL.Types.Roles (adminRoleName)
import Hasura.RQL.Types.Session (BackendOnlyFieldAccess (BOFADisallowed), SessionVariables, UserInfo (..))
import Test.Parser.Expectation qualified as Expect

type PG = 'Postgres 'Vanilla

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

idColumnBuilder :: Expect.ColumnInfoBuilder
idColumnBuilder =
  Expect.ColumnInfoBuilder
    { cibName = "id",
      cibPosition = 0,
      cibType = ColumnScalar PGInteger,
      cibNullable = False,
      cibIsPrimaryKey = True
    }

nameColumnBuilder :: Expect.ColumnInfoBuilder
nameColumnBuilder =
  Expect.ColumnInfoBuilder
    { cibName = "name",
      cibPosition = 1,
      cibType = ColumnScalar PGText,
      cibNullable = False,
      cibIsPrimaryKey = False
    }

descColumnBuilder :: Expect.ColumnInfoBuilder
descColumnBuilder =
  Expect.ColumnInfoBuilder
    { cibName = "description",
      cibPosition = 2,
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

textOld :: UnpreparedValue PG
textOld =
  UVParameter FreshVar
    $ ColumnValue
      { cvType = ColumnScalar PGText,
        cvValue = PGValText "old name"
      }

textNew :: UnpreparedValue PG
textNew =
  UVParameter FreshVar
    $ ColumnValue
      { cvType = ColumnScalar PGText,
        cvValue = PGValText "new name"
      }

textOther :: UnpreparedValue PG
textOther =
  UVParameter FreshVar
    $ ColumnValue
      { cvType = ColumnScalar PGText,
        cvValue = PGValText "other"
      }

integerOne :: UnpreparedValue PG
integerOne =
  UVParameter FreshVar
    $ ColumnValue
      { cvType = ColumnScalar PGInteger,
        cvValue = PGValInteger 1
      }

integerTwo :: UnpreparedValue PG
integerTwo =
  UVParameter FreshVar
    $ ColumnValue
      { cvType = ColumnScalar PGInteger,
        cvValue = PGValInteger 2
      }

dummyUserInfo :: UserInfo
dummyUserInfo =
  UserInfo
    adminRoleName
    (mempty @SessionVariables)
    BOFADisallowed
