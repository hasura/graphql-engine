-- | Test Backend Postgres Insert
--
-- Helpers to build 'Hasura.RQL.IR.Insert.InsertQueryP1' and test the results
-- of running 'Hasura.Backends.Postgres.Translate.Insert.mkInsertCTE' as raw
-- SQL.
--
-- See 'Hasura.Backends.Postgres.Translate.InserSpec.spec' for usage examples.
module Test.Backend.Postgres.Insert
  ( TestBuilder (..),
    runTest,
  )
where

import Data.Aeson (toJSON)
import Hasura.Backends.Postgres.SQL.Types (QualifiedTable)
import Hasura.Backends.Postgres.Translate.Insert qualified as Insert
import Hasura.Prelude
import Hasura.RQL.IR.Returning (MutationOutputG (..))
import Hasura.RQL.IR.Value (UnpreparedValue (..))
import Hasura.RQL.Types.BackendType (PostgresKind (Vanilla))
import Hasura.RQL.Types.Column (ColumnInfo)
import Hasura.SQL.Types (toSQLTxt)
import Test.Backend.Postgres.Misc (PG, dummyUserInfo)
import Test.Hspec
import Test.Parser.Insert qualified as Expect
import Test.SIString qualified as SI

-- | Describes a /mkInsertCTE/ test.
data TestBuilder = TestBuilder
  { -- | test name
    name :: String,
    -- | table details
    table :: QualifiedTable,
    -- | columns used in the insert statement
    insertColumns :: [ColumnInfo PG],
    -- | rows of values to be inserted
    values :: [[UnpreparedValue PG]],
    -- | table columns
    columns :: [ColumnInfo PG],
    -- | expected output fields
    mutationOutput :: MutationOutputG PG Void (UnpreparedValue PG),
    -- | expected SQL
    expectedSQL :: Text
  }

-- | Runs a test for insert queries.
runTest :: TestBuilder -> Spec
runTest TestBuilder {..} =
  it name do
    let ins =
          Expect.mkInsertQuery
            Expect.InsertQueryBuilder
              { iqbTable = table,
                iqbInsertColumns = insertColumns,
                iqbValues = values,
                iqbOutput = mutationOutput,
                iqbAllColumns = columns
              }
    insertCTE' <- runExceptT (Insert.mkInsertCTE @'Vanilla dummyUserInfo ins)
    insertCTE <- onLeft insertCTE' $ error . show . toJSON
    (SI.fromText . toSQLTxt $ insertCTE)
      `shouldBe` SI.fromText expectedSQL
