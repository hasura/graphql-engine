module Hasura.Generator.Common
  ( genHashMap,
    genNonEmptyText,
    genArbitraryUnicodeText,
    genArbitraryAlphaNumText,
    genFieldName,
    genGName,
    genDescription,
    defaultRange,
  )
where

import Data.HashMap.Strict qualified as HM
import Data.Text.NonEmpty (NonEmptyText, mkNonEmptyText)
import Hasura.Prelude
import Hasura.RQL.Types.Common (FieldName (FieldName))
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Language.GraphQL.Draft.Syntax qualified as G

genHashMap ::
  MonadGen m =>
  Eq a =>
  Hashable a =>
  m a ->
  m b ->
  Range Int ->
  m (HashMap a b)
genHashMap genA genB range = fmap HM.fromList . Gen.list range $ (,) <$> genA <*> genB

genNonEmptyText :: MonadGen m => Range Int -> m NonEmptyText
genNonEmptyText range = mkNonEmptyText `Gen.mapMaybeT` genArbitraryUnicodeText range

genArbitraryUnicodeText :: MonadGen m => Range Int -> m Text
genArbitraryUnicodeText range = Gen.text range Gen.unicode

genArbitraryAlphaNumText :: MonadGen m => Range Int -> m Text
genArbitraryAlphaNumText range = Gen.text range Gen.alphaNum

genFieldName :: MonadGen m => Range Int -> m FieldName
genFieldName range = FieldName <$> genArbitraryUnicodeText range

genGName :: MonadGen m => Range Int -> m G.Name
genGName range = G.mkName `Gen.mapMaybeT` genArbitraryAlphaNumText range

genDescription :: MonadGen m => Range Int -> m G.Description
genDescription range = G.Description <$> genArbitraryUnicodeText range

-- | A reasonable range size to generate data on dev machines without
-- blowing up.
defaultRange :: Integral a => Range a
defaultRange = Range.linear 0 8
