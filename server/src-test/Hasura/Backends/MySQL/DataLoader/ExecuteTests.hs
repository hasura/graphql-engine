module Hasura.Backends.MySQL.DataLoader.ExecuteTests
  ( spec,
  )
where

import Data.HashMap.Strict.InsOrd qualified as HMS
import Data.Vector qualified as V
import Hasura.Backends.MySQL.DataLoader.Execute
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen
import Hedgehog.Range
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec = do
  describe "joinObjectRows" $ do
    joinObjectRowsThrowsIfRightRowsIsEmpty
    joinObjectRowsThrowsIfRightRowsIsLargerThanOne
  describe "leftObjectJoin" $ do
    leftObjectJoinThrowsIfRightRowsIsEmpty
    leftObjectJoinThrowsIfRightRowsIsLargerThanOne

joinObjectRowsThrowsIfRightRowsIsEmpty :: Spec
joinObjectRowsThrowsIfRightRowsIsEmpty =
  it "throws if rightRows is empty" $
    joinObjectRows
      Nothing
      ""
      HMS.empty
      empty
      `shouldSatisfy` invariant

joinObjectRowsThrowsIfRightRowsIsLargerThanOne :: Spec
joinObjectRowsThrowsIfRightRowsIsLargerThanOne = do
  it "throws if rightRows is two or more"
    . hedgehog
    $ do
      size <- forAll $ integral (linear 2 100)
      let result =
            joinObjectRows
              Nothing
              ""
              HMS.empty
              (V.replicate size HMS.empty)
      assert $ invariant result

leftObjectJoinThrowsIfRightRowsIsEmpty :: Spec
leftObjectJoinThrowsIfRightRowsIsEmpty =
  it "throws if rightRows is empty" $
    leftObjectJoin
      Nothing
      ""
      []
      (RecordSet Nothing (V.singleton HMS.empty) Nothing)
      (RecordSet Nothing mempty Nothing)
      `shouldSatisfy` invariant

leftObjectJoinThrowsIfRightRowsIsLargerThanOne :: Spec
leftObjectJoinThrowsIfRightRowsIsLargerThanOne =
  it "throws if rightRows is two or more"
    . hedgehog
    $ do
      size <- forAll $ integral (linear 2 100)
      let result =
            leftObjectJoin
              Nothing
              ""
              []
              (RecordSet Nothing (V.singleton HMS.empty) Nothing)
              (RecordSet Nothing (V.replicate size HMS.empty) Nothing)
      assert $ invariant result

invariant :: Either ExecuteProblem a -> Bool
invariant =
  \case
    Left (BrokenJoinInvariant _) -> True
    _ -> False
