-- | Test Backend Postgres Update
--
-- Helpers to build 'Hasura.RQL.IR.Update.AnnotatedUpdateG' and test the results
-- of running 'Hasura.Backends.Postgres.Translate.Update.mkUpdateCTE' as raw
-- SQL.
--
-- See 'Hasura.Backends.Postgres.Translate.UpdateSpec.spec' for usage examples.
module Test.Backend.Postgres.Update
  ( TestBuilder (..),
    runTest,
    runMultipleUpdates,
  )
where

import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types (QualifiedTable)
import Hasura.Backends.Postgres.Translate.Update qualified as Update
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (OpExpG (..))
import Hasura.RQL.IR.Returning (MutationOutputG (..))
import Hasura.RQL.IR.Value (UnpreparedValue (..))
import Hasura.RQL.Types.Column (ColumnInfo)
import Hasura.SQL.Backend (BackendType (Postgres), PostgresKind (Vanilla))
import Hasura.SQL.Types (toSQLTxt)
import Test.HUnit.Base (assertFailure)
import Test.Hspec
import Test.Parser.Expectation qualified as Expect
import Test.SIString qualified as SI

type PG = 'Postgres 'Vanilla

-- | Describes a /mkUpdateCTE/ test.
data TestBuilder e = TestBuilder
  { -- | test name
    name :: String,
    -- | table details
    table :: QualifiedTable,
    -- | table columnd
    columns :: [ColumnInfo PG],
    -- | expected output fields
    output :: MutationOutputG PG Void (UnpreparedValue PG),
    -- | where clause for the query (usually empty for /update_many/)
    where_ :: [(ColumnInfo PG, [OpExpG PG (UnpreparedValue PG)])],
    -- | update clause
    update :: Expect.BackendUpdateBuilder (ColumnInfo PG),
    -- | expected result; this is either 'Text' or '[Text]'
    expected :: e
  }

-- | Runs a test for single updates.
runTest :: TestBuilder Text -> Spec
runTest TestBuilder {..} =
  it name do
    let upd =
          (`evalState` 0) $
            traverse go $
              Expect.mkAnnotatedUpdate @Void
                Expect.AnnotatedUpdateBuilder
                  { Expect.aubTable = table,
                    Expect.aubOutput = output,
                    Expect.aubColumns = columns,
                    Expect.aubWhere = where_,
                    Expect.aubUpdate = update
                  }
    case Update.mkUpdateCTE @'Vanilla upd of
      (Update.Update cte) -> (SI.fromText $ toSQLTxt cte) `shouldBe` SI.fromText expected
      _ -> assertFailure "expected single update, got multiple updates"
  where
    go :: UnpreparedValue PG -> State Int S.SQLExp
    go = \case
      UVLiteral sqlExp -> pure sqlExp
      UVParameter _varInfo _cval -> do
        index <- get
        modify (+ 1)
        pure $ S.SEPrep index
      _ -> error "unexpected value"

-- | Runs a test for /update_many/
runMultipleUpdates :: TestBuilder [Text] -> Spec
runMultipleUpdates TestBuilder {..} =
  it name do
    let upd =
          (`evalState` 0) $
            traverse go $
              Expect.mkAnnotatedUpdate @Void
                Expect.AnnotatedUpdateBuilder
                  { Expect.aubTable = table,
                    Expect.aubOutput = output,
                    Expect.aubColumns = columns,
                    Expect.aubWhere = where_,
                    Expect.aubUpdate = update
                  }
    case Update.mkUpdateCTE @'Vanilla upd of
      (Update.MultiUpdate ctes) -> SI.fromText . toSQLTxt <$> ctes `shouldBe` SI.fromText <$> expected
      _ -> assertFailure "expedted update_many, got single update"
  where
    go :: UnpreparedValue PG -> State Int S.SQLExp
    go = \case
      UVLiteral sqlExp -> pure sqlExp
      UVParameter _varInfo _cval -> do
        index <- get
        modify (+ 1)
        pure $ S.SEPrep index
      _ -> error "unexpected value"
