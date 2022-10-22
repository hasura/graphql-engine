-- | Test Backend Postgres Delete
--
-- Helpers to build 'Hasura.RQL.IR.Delete.AnnDelG' and test the results
-- of running 'Hasura.Backends.Postgres.Translate.Delete.mkDelete' as raw
-- SQL.
--
-- See 'Hasura.Backends.Postgres.Translate.DeleteSpec.spec' for usage examples.
module Test.Backend.Postgres.Delete
  ( TestBuilder (..),
    runTest,
  )
where

import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types (QualifiedTable)
import Hasura.Backends.Postgres.Translate.Delete qualified as Delete
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (OpExpG (..))
import Hasura.RQL.IR.Returning (MutationOutputG (..))
import Hasura.RQL.IR.Value (UnpreparedValue (..))
import Hasura.RQL.Types.Column (ColumnInfo)
import Hasura.SQL.Backend (PostgresKind (Vanilla))
import Hasura.SQL.Types (toSQLTxt)
import Test.Backend.Postgres.Misc (PG)
import Test.Hspec
import Test.Parser.Delete qualified as Expect
import Test.SIString qualified as SI

-- | Describes a /mkDelete/ test.
data TestBuilder = TestBuilder
  { -- | test name
    name :: String,
    -- | table details
    table :: QualifiedTable,
    -- | table columnd
    columns :: [ColumnInfo PG],
    -- | expected output fields
    mutationOutput :: MutationOutputG PG Void (UnpreparedValue PG),
    -- | where clause for the query
    where_ :: [(ColumnInfo PG, [OpExpG PG (UnpreparedValue PG)])],
    -- | expected SQL
    expectedSQL :: Text
  }

-- | Runs a test for delete queries.
runTest :: TestBuilder -> Spec
runTest TestBuilder {..} =
  it name do
    let del =
          (`evalState` 0) $
            traverse go $
              Expect.mkAnnotatedDelete
                Expect.AnnotatedDeleteBuilder
                  { Expect.adbTable = table,
                    Expect.adbOutput = mutationOutput,
                    Expect.adbColumns = columns,
                    Expect.adbWhere = where_
                  }
    (SI.fromText . toSQLTxt . Delete.mkDelete @'Vanilla $ del)
      `shouldBe` SI.fromText expectedSQL
  where
    go :: UnpreparedValue PG -> State Int S.SQLExp
    go = \case
      UVLiteral sqlExp -> pure sqlExp
      UVParameter _varInfo _cval -> do
        index <- get
        modify (+ 1)
        pure $ S.SEPrep index
      _ -> error "unexpected value"
