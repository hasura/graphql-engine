module Hasura.Generator.Common
  ( genHashMap,
    genInt,
    genText,
    genNonEmptyText,
    genArbitraryUnicodeText,
    genArbitraryAlphaNumText,
    genArbitraryAlphaNumTextExcluding,
    genFieldName,
    genGName,
    genDescription,
    defaultRange,
    jsonRoundTrip,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as HashMap
import Data.Text.NonEmpty (NonEmptyText, mkNonEmptyText)
import Hasura.Prelude
import Hasura.RQL.Types.Common (FieldName (FieldName))
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Language.GraphQL.Draft.Syntax qualified as G
import Test.Hspec
import Test.Hspec.Hedgehog

genHashMap ::
  (MonadGen m) =>
  (Hashable a) =>
  m a ->
  m b ->
  Range Int ->
  m (HashMap a b)
genHashMap genA genB range = fmap HashMap.fromList . Gen.list range $ (,) <$> genA <*> genB

genInt :: Gen Int
genInt = fromIntegral <$> Gen.int32 (Range.linear 1 99999)

genText :: Gen Text
genText = Gen.text (Range.linear 0 11) Gen.unicode

genNonEmptyText :: (MonadGen m) => Range Int -> m NonEmptyText
genNonEmptyText range = mkNonEmptyText `Gen.mapMaybeT` genArbitraryUnicodeText range

genArbitraryUnicodeText :: (MonadGen m) => Range Int -> m Text
genArbitraryUnicodeText range = Gen.text range Gen.unicode

genArbitraryAlphaNumText :: (MonadGen m) => Range Int -> m Text
genArbitraryAlphaNumText range = Gen.text range Gen.alphaNum

genArbitraryAlphaNumTextExcluding :: (MonadGen m, GenBase m ~ Identity) => [Text] -> Range Int -> m Text
genArbitraryAlphaNumTextExcluding excluded = Gen.filter (`notElem` excluded) . genArbitraryAlphaNumText

genFieldName :: (MonadGen m) => Range Int -> m FieldName
genFieldName range = FieldName <$> genArbitraryUnicodeText range

genGName :: (MonadGen m) => Range Int -> m G.Name
genGName range = G.mkName `Gen.mapMaybeT` genArbitraryAlphaNumText range

genDescription :: (MonadGen m) => Range Int -> m G.Description
genDescription range = G.Description <$> genArbitraryUnicodeText range

-- | A reasonable range size to generate data on dev machines without
-- blowing up.
defaultRange :: (Integral a) => Range a
defaultRange = Range.linear 0 8

-- | Given 'Gen' @a@, assert that @a@'s Aeson instances are isomorphic.
jsonRoundTrip :: forall a. (FromJSON a, ToJSON a, Eq a, Show a) => Gen a -> String -> Spec
jsonRoundTrip gen ty = do
  it ty $ hedgehog $ do
    term <- forAll gen
    tripping term J.toJSON J.fromJSON
