{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataWrapper.API.V0.OrderBySpec (spec, genOrderBy, genOrderType) where

import Data.Aeson.QQ.Simple (aesonQQ)
import Hasura.Backends.DataWrapper.API.V0.API
import Hasura.Backends.DataWrapper.API.V0.ColumnSpec (genColumnName)
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Test.Aeson.Utils (jsonOpenApiProperties, testToFromJSONToSchema)
import Test.Hspec

spec :: Spec
spec = do
  describe "OrderBy" $ do
    testToFromJSONToSchema
      (OrderBy (ColumnName "my_column_name") Ascending)
      [aesonQQ|
        { "column": "my_column_name",
          "ordering": "asc"
        }
      |]
    jsonOpenApiProperties genOrderBy
  describe "OrderType" $ do
    describe "Ascending" $
      testToFromJSONToSchema Ascending [aesonQQ|"asc"|]
    describe "Descending" $
      testToFromJSONToSchema Descending [aesonQQ|"desc"|]
    jsonOpenApiProperties genOrderType

genOrderBy :: MonadGen m => m OrderBy
genOrderBy =
  OrderBy
    <$> genColumnName
    <*> genOrderType

genOrderType :: MonadGen m => m OrderType
genOrderType = Gen.enumBounded
