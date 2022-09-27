-- | Utility functions related to yaml
module Harness.Yaml
  ( combinationsObject,
    fromObject,
    combinationsObjectUsingValue,
    shouldReturnYaml,
    shouldReturnYamlF,
    shouldReturnOneOfYaml,
    shouldBeYaml,
  )
where

import Data.Aeson
  ( Object,
    Value (..),
  )
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.KeyMap.Extended qualified as KM (mapWithKey)
import Data.List (permutations)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error qualified as TE
import Data.Vector qualified as V
import Data.Vector qualified as Vector
import Data.Yaml qualified
import Harness.Test.Fixture qualified as Fixture (Options (..))
import Hasura.Prelude
import Instances.TH.Lift ()
import Test.Hspec (HasCallStack, shouldBe, shouldContain)

fromObject :: Value -> Object
fromObject (Object x) = x
fromObject v = error $ "fromObject: Expected object, received" <> show v

-- | Compute all variations of an object and construct a list of
-- 'Value' based on the higher order function that is passed to it.  A
-- single variation of 'Object' is constructed as an 'Array' before
-- it's transformed by the passed function.
--
-- Typical usecase of this function is to use it with
-- 'shouldReturnOneOfYaml' function.
combinationsObject :: (Value -> Value) -> [Object] -> [Value]
combinationsObject fn variants =
  let toArray :: [Value]
      toArray = map ((Array . V.fromList) . (map Object)) (permutations variants)
   in map fn toArray

-- | Same as 'combinationsObject' but the second parameter is a list
-- of 'Value`. We assume that 'Value' internally has only 'Object', if
-- not it will throw exception.
combinationsObjectUsingValue :: (Value -> Value) -> [Value] -> [Value]
combinationsObjectUsingValue fn variants = combinationsObject fn (map fromObject variants)

-------------------------------------------------------------------

-- * Expectations

-- | The action @actualIO@ should produce the @expected@ YAML,
-- represented (by the yaml package) as an aeson 'Value'.
--
-- We use 'Visual' internally to easily display the 'Value' as YAML
-- when the test suite uses its 'Show' instance.
shouldReturnYaml :: HasCallStack => Fixture.Options -> IO Value -> Value -> IO ()
shouldReturnYaml = shouldReturnYamlF pure

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
  let walk :: KM.KeyMap Value -> Aeson.Key -> Value -> Value
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
shouldReturnYamlF :: HasCallStack => (Value -> IO Value) -> Fixture.Options -> IO Value -> Value -> IO ()
shouldReturnYamlF transform options actualIO expected = do
  actual <-
    actualIO >>= transform >>= \actual ->
      pure
        if Fixture.stringifyNumbers options
          then parseToMatch expected actual
          else actual

  actual `shouldBe` expected

-- | The action @actualIO@ should produce the @expected@ YAML,
-- represented (by the yaml package) as an aeson 'Value'.
--
-- We use 'Visual' internally to easily display the 'Value' as YAML
-- when the test suite uses its 'Show' instance.
shouldReturnOneOfYaml :: HasCallStack => Fixture.Options -> IO Value -> [Value] -> IO ()
shouldReturnOneOfYaml Fixture.Options {stringifyNumbers} actualIO candidates = do
  actual <- actualIO

  let expecteds :: Set Value
      expecteds = Set.fromList candidates

      actuals :: Set Value
      actuals
        | stringifyNumbers = Set.map (`parseToMatch` actual) expecteds
        | otherwise = Set.singleton actual

  case Set.lookupMin (Set.intersection expecteds actuals) of
    Just match -> Visual match `shouldBe` Visual actual
    Nothing -> map Visual (Set.toList expecteds) `shouldContain` [Visual actual]

-- | We use 'Visual' internally to easily display the 'Value' as YAML
-- when the test suite uses its 'Show' instance.
--
-- NOTE(jkachmar): A lot of the matchers we define in this module are
-- implemented in the @hspec-expectations-json@ package.
--
-- Since @Data.Yaml@ uses the same underlying 'Value' type as
-- @Data.Aeson@, we could pull that in as a dependency and alias
-- some of these functions accordingly.
shouldBeYaml :: HasCallStack => Value -> Value -> IO ()
shouldBeYaml actual expected = do
  shouldBe (Visual actual) (Visual expected)

-- | For the test suite: diff structural, but display in a readable
-- way.
newtype Visual = Visual {unVisual :: Value}
  deriving stock (Eq)

instance Show Visual where
  show = T.unpack . decodeUtf8With TE.lenientDecode . Data.Yaml.encode . unVisual
