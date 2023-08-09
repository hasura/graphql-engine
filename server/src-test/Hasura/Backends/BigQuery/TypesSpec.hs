-- | JSON encoding round trips for BigQuery types
-- lots of these are newtypes around primitives like Text, Int64
-- so we just check that what comes back is the same rather than trying too
-- hard to create Hedgehog Gen functions that match the domain
module Hasura.Backends.BigQuery.TypesSpec (spec) where

import Hasura.Backends.BigQuery.Types qualified as BQ
import Hasura.Prelude
import Hedgehog qualified as HH
import Hedgehog.Gen qualified as HH
import Hedgehog.Generic
import Hedgehog.Range (constant, constantBounded)
import Test.Aeson.Utils (jsonRoundTrip)
import Test.Hspec

genText :: HH.Gen Text
genText = HH.text (constant 0 1000) HH.unicode

genInt64 :: HH.Gen BQ.Int64
genInt64 = BQ.Int64 . tshow <$> HH.int constantBounded

genFieldName :: HH.Gen BQ.FieldName
genFieldName = BQ.FieldName <$> genText <*> genText

genBigDecimal :: HH.Gen BQ.BigDecimal
genBigDecimal = do
  i1 <- HH.int constantBounded
  i2 <- HH.int constantBounded
  pure $ BQ.BigDecimal $ tshow i1 <> "." <> tshow i2

genTableName :: HH.Gen BQ.TableName
genTableName = BQ.TableName <$> genText <*> genText

genTop :: HH.Gen BQ.Top
genTop = BQ.Top . fromIntegral <$> HH.int constantBounded

genFunctionName :: HH.Gen BQ.FunctionName
genFunctionName = BQ.FunctionName <$> genText <*> HH.maybe genText

spec :: Spec
spec = do
  describe "BigQuery" do
    describe "Types roundtrip" $ do
      jsonRoundTrip @BQ.Cardinality hgen
      jsonRoundTrip @BQ.NullsOrder hgen
      jsonRoundTrip @BQ.Op hgen
      jsonRoundTrip @BQ.Order hgen
      jsonRoundTrip @BQ.ScalarType hgen
      jsonRoundTrip @BQ.AsStruct hgen

      jsonRoundTrip @BQ.BigDecimal genBigDecimal
      jsonRoundTrip @BQ.FieldName genFieldName
      jsonRoundTrip @BQ.ColumnName (BQ.ColumnName <$> genText)
      jsonRoundTrip @BQ.Date (BQ.Date <$> genText)
      jsonRoundTrip @BQ.Datetime (BQ.Datetime <$> genText)
      jsonRoundTrip @BQ.Decimal (BQ.Decimal <$> genText)
      jsonRoundTrip @BQ.EntityAlias (BQ.EntityAlias <$> genText)
      jsonRoundTrip @BQ.Geography (BQ.Geography <$> genText)
      jsonRoundTrip @BQ.Int64 genInt64
      jsonRoundTrip @BQ.TableName genTableName
      jsonRoundTrip @BQ.Time (BQ.Time <$> genText)
      jsonRoundTrip @BQ.Timestamp (BQ.Timestamp <$> genText)
      jsonRoundTrip @BQ.Top genTop
      jsonRoundTrip @BQ.FunctionName genFunctionName
