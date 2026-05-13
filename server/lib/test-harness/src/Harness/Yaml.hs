-- | Utility functions related to yaml
module Harness.Yaml
  ( mapObject,
    sortArray,
    shouldReturnYaml,
    shouldReturnYamlF,
    shouldReturnYamlFInternal,
    ShouldReturnYamlF (..),
    shouldBeYaml,
    shouldAtLeastBe,
    Visual (..),
    parseToMatch,
    ignoreWhitespace,
  )
where

import Data.Aeson (Value (..))
import Data.Aeson qualified as J
import Data.Aeson.KeyMap qualified as KM
import Data.Char
import Data.Has
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error qualified as TE
import Data.These
import Data.Vector qualified as V
import Data.Vector qualified as Vector
import Data.Yaml qualified
import Harness.Test.CustomOptions (Options (..))
import Hasura.Prelude
import Instances.TH.Lift ()
import Test.Hspec (HasCallStack, expectationFailure, shouldBe)

mapObject :: (Value -> Value) -> Value -> Value
mapObject f (Object x) = Object $ fmap f x
mapObject _ _ = error "mapObject can only be called on Object Values"

sortArray :: Value -> Value
sortArray (Array a) = Array (V.fromList (sort (V.toList a)))
sortArray _ = error "sortArray can only be called on Array Values"

-------------------------------------------------------------------

newtype ShouldReturnYamlF = ShouldReturnYamlF
  { getShouldReturnYamlF :: (Value -> IO Value) -> IO Value -> Value -> IO ()
  }

-- * Expectations

-- | The action @actualIO@ should produce the @expected@ YAML,
-- represented (by the yaml package) as an aeson 'Value'.
--
-- We use 'Visual' internally to easily display the 'Value' as YAML
-- when the test suite uses its 'Show' instance.
shouldReturnYaml ::
  ( HasCallStack,
    Has ShouldReturnYamlF testEnvironment
  ) =>
  testEnvironment ->
  IO Value ->
  Value ->
  IO ()
shouldReturnYaml testEnvironment = shouldReturnYamlF testEnvironment pure

-- | Because JSON supports numbers only up to 32 bits, some backends (such as
-- BigQuery) send numbers as strings instead. So, for example, the floating
-- point @2.0@ will reach us as @'2.0'@.
--
-- This presents an issue when we want to write tests across multiple backends:
-- what do we do if a test should match @2.0@ for Postgres and @'2.0'@ for
-- BigQuery? Worse still, what if it should match @'2.0'@ for BigQuery and
-- @'2.00000000'@ for CockroachDB?
--
-- This function attempts to solve the problem by zipping the expected output
-- and the actual output together, looking for any instance where we expect a
-- number, but find a string. In these instances, we replace the string with
-- the result of parsing that string _into_ a number - specifically, a
-- @Scientific@. This works because @Scientific@ can support 64-bit numbers
-- (and any arbitrary precision, for that matter), so it should be able to
-- handle any stringified number we receive.
--
-- If the zipping doesn't line up, we assume this is probably a bad result and
-- consequently should result in a failing test. In these cases, we leave the
-- actual output exactly as-is, and wait for the test to fail.
parseToMatch :: Value -> Value -> Value
parseToMatch (Array expected) (Array actual) =
  Array (Vector.zipWith parseToMatch expected actual)
parseToMatch (Number _) (String text) =
  case readMaybe (T.unpack text) of
    Just actual -> Number actual
    Nothing -> String text
parseToMatch (Object expected) (Object actual) = do
  let walk :: KM.KeyMap Value -> J.Key -> Value -> Value
      walk reference key current =
        case KM.lookup key reference of
          Just this -> parseToMatch this current
          Nothing -> current

  Object (KM.mapWithKey (walk expected) actual)
parseToMatch _ actual = actual

-- | The function @transform@ converts the returned YAML
-- prior to comparison. It exists in IO in order to be able
-- to easily throw exceptions for hspec purposes.
--
-- The action @actualIO@ should produce the @expected@ YAML,
-- represented (by the yaml package) as an aeson 'Value'.
--
-- We use 'Visual' internally to easily display the 'Value' as YAML
-- when the test suite uses its 'Show' instance.
shouldReturnYamlF :: (HasCallStack, Has ShouldReturnYamlF testEnvironment) => testEnvironment -> (Value -> IO Value) -> IO Value -> Value -> IO ()
shouldReturnYamlF = getShouldReturnYamlF . getter

shouldReturnYamlFInternal :: (HasCallStack) => Options -> (Value -> IO Value) -> IO Value -> Value -> IO ()
shouldReturnYamlFInternal options transform actualIO expected = do
  actual <-
    actualIO >>= transform >>= \actual ->
      pure
        if stringifyNumbers options
          then parseToMatch expected actual
          else actual

  actual `shouldBeYaml` expected

-- | We use 'Visual' internally to easily display the 'Value' as YAML
-- when the test suite uses its 'Show' instance.
--
-- NOTE(jkachmar): A lot of the matchers we define in this module are
-- implemented in the @hspec-expectations-json@ package.
--
-- Since @Data.Yaml@ uses the same underlying 'Value' type as
-- @Data.Aeson@, we could pull that in as a dependency and alias
-- some of these functions accordingly.
shouldBeYaml :: (HasCallStack) => Value -> Value -> IO ()
shouldBeYaml actual expected = do
  shouldBe (Visual actual) (Visual expected)

ignoreWhitespace :: Value -> Value
ignoreWhitespace (J.Array sub) = J.Array (fmap ignoreWhitespace sub)
ignoreWhitespace (J.Object sub) = J.Object (fmap ignoreWhitespace sub)
ignoreWhitespace (J.String sub) = J.String (T.filter (not . isSpace) sub)
ignoreWhitespace x = x

-- | Assert that the expected json value should be a subset of the actual value, in the sense of 'jsonSubsetOf'.
shouldAtLeastBe :: (HasCallStack) => Value -> Value -> IO ()
shouldAtLeastBe actual expected | expected `jsonSubsetOf` actual = return ()
shouldAtLeastBe actual expected =
  expectationFailure $ "The expected value:\n\n" <> show (Visual expected) <> "\nis not a subset of the actual value:\n\n" <> show (Visual actual)

-- | Compute whether one json value 'sub' is a subset of another value 'sup', in the sense that:
--
-- * For arrays, there is a contiguous segment in 'sup' in which all elements are subset-related with 'sub' in order
-- * For objects, the keys of 'sub' are a subset of those of 'sup', and all their associated values are also subset-related
-- * Leaf values are identical
jsonSubsetOf :: J.Value -> J.Value -> Bool
jsonSubsetOf (J.Array sub) (J.Array sup) = sub `subarrayOf` sup
jsonSubsetOf (J.Object sub) (J.Object sup) = sub `subobjectOf` sup
jsonSubsetOf (J.String sub) (J.String sup) = sub == sup
jsonSubsetOf (J.Number sub) (J.Number sup) = sub == sup
jsonSubsetOf (J.Bool sub) (J.Bool sup) = sub == sup
jsonSubsetOf J.Null J.Null = True
jsonSubsetOf _sub _sup = False

subobjectOf :: KM.KeyMap J.Value -> KM.KeyMap J.Value -> Bool
subobjectOf sub sup =
  KM.foldr (&&) True
    $ KM.alignWith
      ( \case
          This _ -> False -- key is only in the sub
          That _ -> True -- key is only in sup
          These l r -> l `jsonSubsetOf` r
      )
      sub
      sup

subarrayOf :: V.Vector J.Value -> V.Vector J.Value -> Bool
subarrayOf sub sup | V.length sub > V.length sup = False
subarrayOf sub sup | V.and $ V.zipWith jsonSubsetOf sub sup = True
subarrayOf sub sup = subarrayOf sub (V.tail sup)

-- | For the test suite: diff structural, but display in a readable
-- way.
newtype Visual = Visual {unVisual :: Value}
  deriving stock (Eq)

instance Show Visual where
  show = T.unpack . decodeUtf8With TE.lenientDecode . Data.Yaml.encode . unVisual
