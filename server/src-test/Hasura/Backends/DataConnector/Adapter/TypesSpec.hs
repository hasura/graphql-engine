module Hasura.Backends.DataConnector.Adapter.TypesSpec (spec) where

import Control.Lens (preview, _Right)
import Hasura.Backends.DataConnector.Adapter.Types
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax qualified as GQL
import Test.Hspec

spec :: Spec
spec = describe "DataConnectorName" do
  it "rejects underscores" do
    (preview _Right . mkDataConnectorName =<< GQL.mkName "hello_world") `shouldBe` Nothing

  it "rejects invalid GQL names" do
    (GQL.mkName "hello-world") `shouldBe` Nothing
    (preview _Right . mkDataConnectorName =<< GQL.mkName "hello-world") `shouldBe` Nothing

  it "accepts valid names" do
    (preview _Right . mkDataConnectorName =<< GQL.mkName "helloworld") `shouldNotBe` Nothing
