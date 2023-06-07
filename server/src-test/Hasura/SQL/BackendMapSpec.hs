{-# LANGUAGE QuasiQuotes #-}

module Hasura.SQL.BackendMapSpec (spec) where

import Autodocodec (parseJSONViaCodec, toJSONViaCodec)
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types (parseEither)
import Data.Either.Combinators (fromRight')
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Hasura.Backends.DataConnector.Adapter.Types (DataConnectorOptions (..), mkDataConnectorName)
import Hasura.Prelude
import Hasura.RQL.Types.BackendType (BackendType (..))
import Hasura.RQL.Types.Metadata.Common (BackendConfigWrapper (BackendConfigWrapper))
import Hasura.SQL.BackendMap (BackendMap)
import Hasura.SQL.BackendMap qualified as BM
import Language.GraphQL.Draft.Syntax qualified as GQL
import Servant.Client qualified as S
import Test.Hspec
import Test.Hspec.Expectations.Json (shouldBeJson)

spec :: Spec
spec = describe "BackendMap" do
  it "serializes via Autodocodec" do
    let mssqlConfig = BM.singleton @'MSSQL @BackendConfigWrapper (BackendConfigWrapper ())
    let dataconnectorConfig =
          BM.singleton @'DataConnector @BackendConfigWrapper
            ( BackendConfigWrapper
                $ Map.singleton
                  (fromRight' $ mkDataConnectorName $ fromJust $ GQL.mkName "MyConnector")
                  ( DataConnectorOptions
                      { _dcoUri = fromRight' $ S.parseBaseUrl "https://somehost.org/",
                        _dcoDisplayName = Just "My Connector"
                      }
                  )
            )
    let configs = mssqlConfig <> dataconnectorConfig

    let expected =
          [aesonQQ|
            {
              "mssql": [],
              "dataconnector": {
                "MyConnector": {
                  "uri": "https://somehost.org",
                  "display_name": "My Connector"
                }
              }
            }
          |]
    let json = toJSONViaCodec configs
    json `shouldBeJson` expected

    let decoded = parseEither (parseJSONViaCodec @(BackendMap BackendConfigWrapper)) json
    decoded `shouldBe` Right configs
