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

import Data.Aeson (ToJSON (toJSON))
import Hasura.Backends.Postgres.SQL.Types (QualifiedTable)
import Hasura.Backends.Postgres.Translate.Update qualified as Update
import Hasura.Prelude
import Hasura.RQL.IR.Returning (MutationOutputG (..))
import Hasura.RQL.IR.Value (UnpreparedValue (..))
import Hasura.RQL.Types.BackendType (PostgresKind (Vanilla))
import Hasura.RQL.Types.Column (ColumnInfo)
import Hasura.SQL.Types (toSQLTxt)
import Test.Backend.Postgres.Misc
import Test.HUnit.Base (assertFailure)
import Test.Hspec
import Test.Parser.Expectation qualified as Expect
import Test.SIString qualified as SI

-- | Describes a /mkUpdateCTE/ test.
data TestBuilder e = TestBuilder
  { -- | test name
    name :: String,
    -- | table details
    table :: QualifiedTable,
    -- | table columnd
    columns :: [ColumnInfo PG],
    -- | expected output fields
    mutationOutput :: MutationOutputG PG Void (UnpreparedValue PG),
    -- | update clause
    updateVariant :: Expect.UpdateVariantBuilder (ColumnInfo PG),
    -- | expected result; this is either 'Text' or '[Text]'
    expectedSQL :: e
  }

-- | Runs a test for single updates.
runTest :: TestBuilder Text -> Spec
runTest TestBuilder {..} =
  it name do
    let upd =
          unpreparedValueToSQLExp
            <$> Expect.mkAnnotatedUpdate @Void
              Expect.AnnotatedUpdateBuilder
                { Expect.aubTable = table,
                  Expect.aubOutput = mutationOutput,
                  Expect.aubColumns = columns,
                  Expect.aubUpdateVariant = updateVariant
                }
    updateCTE <- runExceptT (Update.mkUpdateCTE @'Vanilla dummyUserInfo upd)
    case updateCTE of
      Right (Update.Update cte) -> SI.fromText (toSQLTxt cte) `shouldBe` SI.fromText expectedSQL
      Right (Update.MultiUpdate _) -> assertFailure "expected single update, got multiple updates"
      Left qErr -> assertFailure $ show $ toJSON qErr

-- | Runs a test for /update_many/
runMultipleUpdates :: TestBuilder [Text] -> Spec
runMultipleUpdates TestBuilder {..} =
  it name do
    let upd =
          unpreparedValueToSQLExp
            <$> Expect.mkAnnotatedUpdate @Void
              Expect.AnnotatedUpdateBuilder
                { Expect.aubTable = table,
                  Expect.aubOutput = mutationOutput,
                  Expect.aubColumns = columns,
                  Expect.aubUpdateVariant = updateVariant
                }
    updateCTE <- runExceptT (Update.mkUpdateCTE @'Vanilla dummyUserInfo upd)
    case updateCTE of
      Right (Update.Update _) -> assertFailure "expected update_many, got single update"
      Right (Update.MultiUpdate ctes) ->
        SI.fromText
          . toSQLTxt
          <$> ctes
          `shouldBe` SI.fromText
            <$> expectedSQL
      Left qErr -> assertFailure $ show $ toJSON qErr
