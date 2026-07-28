{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "avoid Language.GraphQL.Draft.Syntax.unsafeMkName" #-}

-- | Tests translating query IR into TSQL 'Select' values, in particular the
-- ORDER BY null handling: the @IIF(.. IS NULL, ..)@ sort key must only be
-- generated when the ordering expression can actually evaluate to NULL.
module Hasura.Backends.MSSQL.FromIr.QuerySpec (spec) where

import Data.Text qualified as T
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Backends.MSSQL.FromIr (runFromIrUseCTEs)
import Hasura.Backends.MSSQL.FromIr.Query (fromSelect)
import Hasura.Backends.MSSQL.Instances.Types ()
import Hasura.Backends.MSSQL.ToQuery qualified as ToQuery
import Hasura.Backends.MSSQL.Types.Internal qualified as TSQL
import Hasura.Base.Error (showQErr)
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column qualified as IR
import Hasura.RQL.Types.Common qualified as IR
import Hasura.RQL.Types.Schema.Options qualified as Options
import Language.GraphQL.Draft.Syntax qualified as G
import Test.Hspec

spec :: Spec
spec = describe "fromSelect: ORDER BY null handling" do
  let nullableCol = mkColumnInfo "title" True
      nonNullableCol = mkColumnInfo "id" False

  it "emits an IIF sort key for a nullable column with an explicit nulls order" do
    sql <- translate (mkSelect [orderBy nullableCol (Just TSQL.AscOrder) (Just TSQL.NullsFirst) IR.NoRedaction])
    sql `shouldSatisfy` T.isInfixOf "IIF"

  it "omits the IIF sort key for a non-nullable column with an explicit nulls order" do
    sql <- translate (mkSelect [orderBy nonNullableCol (Just TSQL.AscOrder) (Just TSQL.NullsFirst) IR.NoRedaction])
    sql `shouldSatisfy` T.isInfixOf "ORDER BY"
    sql `shouldSatisfy` (not . T.isInfixOf "IIF")

  it "omits the IIF sort key when no nulls order is requested" do
    sql <- translate (mkSelect [orderBy nullableCol (Just TSQL.DescOrder) Nothing IR.NoRedaction])
    sql `shouldSatisfy` T.isInfixOf "ORDER BY"
    sql `shouldSatisfy` (not . T.isInfixOf "IIF")

  it "omits the IIF sort key for NullsAnyOrder on a nullable column (plain asc/desc)" do
    sql <- translate (mkSelect [orderBy nullableCol (Just TSQL.AscOrder) (Just TSQL.NullsAnyOrder) IR.NoRedaction])
    sql `shouldSatisfy` T.isInfixOf "ORDER BY"
    sql `shouldSatisfy` (not . T.isInfixOf "IIF")

  it "keeps the IIF sort key for a redacted non-nullable column" do
    -- Redaction can turn any value into NULL, so the null handling has to
    -- stay even if the underlying column is non-nullable.
    sql <- translate (mkSelect [orderBy nonNullableCol (Just TSQL.AscOrder) (Just TSQL.NullsFirst) (IR.RedactIfFalse IR.annBoolExpTrue)])
    sql `shouldSatisfy` T.isInfixOf "IIF"

  it "handles nullable and non-nullable order-by columns independently" do
    sql <-
      translate
        ( mkSelect
            [ orderBy nonNullableCol (Just TSQL.AscOrder) (Just TSQL.NullsLast) IR.NoRedaction,
              orderBy nullableCol (Just TSQL.DescOrder) (Just TSQL.NullsLast) IR.NoRedaction
            ]
        )
    sql `shouldSatisfy` T.isInfixOf "[title] IS NULL, 1, 0)"
    sql `shouldSatisfy` (not . T.isInfixOf "[id] IS NULL")

--------------------------------------------------------------------------------
-- Test helpers

mkColumnInfo :: Text -> Bool -> IR.ColumnInfo 'MSSQL
mkColumnInfo name isNullable =
  IR.ColumnInfo
    { ciColumn = TSQL.ColumnName name,
      ciName = G.unsafeMkName name,
      ciPosition = 0,
      ciType = IR.ColumnScalar TSQL.IntegerType,
      ciIsNullable = isNullable,
      ciDescription = Nothing,
      ciMutability = IR.ColumnMutability {_cmIsInsertable = False, _cmIsUpdatable = False}
    }

orderBy ::
  IR.ColumnInfo 'MSSQL ->
  Maybe TSQL.Order ->
  Maybe TSQL.NullsOrder ->
  IR.AnnRedactionExp 'MSSQL TSQL.Expression ->
  IR.AnnotatedOrderByItemG 'MSSQL TSQL.Expression
orderBy columnInfo obiType obiNulls redactionExp =
  IR.OrderByItemG
    { obiType,
      obiColumn = IR.AOCColumn columnInfo redactionExp,
      obiNulls
    }

mkSelect ::
  [IR.AnnotatedOrderByItemG 'MSSQL TSQL.Expression] ->
  IR.AnnSelectG 'MSSQL (IR.AnnFieldG 'MSSQL Void) TSQL.Expression
mkSelect orderByItems =
  IR.AnnSelectG
    { _asnFields = [(IR.FieldName "id", IR.AFColumn (mkAnnColumnField (mkColumnInfo "id" False)))],
      _asnFrom = IR.FromTable (TSQL.TableName "test" (TSQL.SchemaName "dbo")),
      _asnPerm = IR.TablePerm {_tpFilter = IR.annBoolExpTrue, _tpLimit = Nothing},
      _asnArgs = IR.noSelectArgs {IR._saOrderBy = nonEmpty orderByItems},
      _asnStrfyNum = Options.Don'tStringifyNumbers,
      _asnNamingConvention = Nothing
    }
  where
    mkAnnColumnField columnInfo =
      IR.AnnColumnField
        { _acfColumn = IR.ciColumn columnInfo,
          _acfType = IR.ciType columnInfo,
          _acfAsText = False,
          _acfArguments = Nothing,
          _acfRedactionExpression = IR.NoRedaction
        }

translate :: IR.AnnSelectG 'MSSQL (IR.AnnFieldG 'MSSQL Void) TSQL.Expression -> IO Text
translate annSelect =
  case runExcept (runFromIrUseCTEs (fromSelect IR.JASMultipleRows annSelect)) of
    Left err -> expectationFailure' ("translation failed: " <> T.unpack (showQErr err))
    Right queryWithDDL ->
      pure (ODBC.renderQuery (ToQuery.toQueryFlat (ToQuery.fromSelect (TSQL.qwdQuery queryWithDDL))))
  where
    expectationFailure' msg = expectationFailure msg >> error "unreachable"
