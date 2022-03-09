module Hasura.RQL.Types.TableSpec (spec) where

import Data.Aeson (parseJSON, toJSON)
import Data.Aeson.Types (parseEither)
import Hasura.Prelude
import Hasura.QuickCheck.Instances ()
import Hasura.RQL.Types.Table (CustomRootField (..), TableCustomRootFields (..), emptyCustomRootFields)
import Language.GraphQL.Draft.Syntax qualified as G
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = describe "TableCustomRootFields" do
  prop "JSON serialization roundtrips" $ \(tableCustomRootFields :: TableCustomRootFields) ->
    parseEither parseJSON (toJSON tableCustomRootFields) `shouldBe` Right tableCustomRootFields

  it "Duplicated field names fail to deserialize" $ do
    let emptyTrcf@TableCustomRootFields {..} = emptyCustomRootFields
    let trcfWithDupes =
          emptyTrcf
            { _tcrfSelect = _tcrfSelect {_crfName = G.mkName "DuplicateFieldName"},
              _tcrfInsert = _tcrfInsert {_crfName = G.mkName "DuplicateFieldName"}
            }
    parseEither parseJSON (toJSON trcfWithDupes) `shouldBe` (Left "Error in $: the following custom root field names are duplicated: DuplicateFieldName" :: Either String TableCustomRootFields)
