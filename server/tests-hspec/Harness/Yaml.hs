-- | Utility functions related to yaml
module Harness.Yaml
  ( combinationsObject,
    fromObject,
    combinationsObjectUsingValue,
    shouldReturnYaml,
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
import Data.Aeson.KeyMap.Extended qualified as KM
import Data.Aeson.Text qualified as Aeson.Text
import Data.List (permutations)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error qualified as TE
import Data.Text.Lazy qualified as LT
import Data.Vector qualified as V
import Data.Vector qualified as Vector
import Data.Yaml qualified
import Harness.Test.Context qualified as Context (Options (..))
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
shouldReturnYaml :: HasCallStack => Context.Options -> IO Value -> Value -> IO ()
shouldReturnYaml options actualIO rawExpected = do
  actual <- actualIO

  let Context.Options {stringifyNumbers} = options
      expected =
        if stringifyNumbers
          then stringifyExpectedToActual rawExpected actual
          else rawExpected

  shouldBeYaml actual expected

-- | TODO(jkachmar): Document.
stringifyExpectedToActual :: Value -> Value -> Value
stringifyExpectedToActual (Aeson.Number n) (Aeson.String _) =
  Aeson.String (LT.toStrict . Aeson.Text.encodeToLazyText $ n)
stringifyExpectedToActual (Aeson.Object km) (Aeson.Object km') =
  let stringifyKV k v =
        case KM.lookup k km' of
          Just v' -> stringifyExpectedToActual v v'
          Nothing -> v
   in Aeson.Object (KM.mapWithKey stringifyKV km)
stringifyExpectedToActual (Aeson.Array as) (Aeson.Array bs) =
  Aeson.Array (Vector.zipWith stringifyExpectedToActual as bs)
stringifyExpectedToActual expected _ = expected

-- | The action @actualIO@ should produce the @expected@ YAML,
-- represented (by the yaml package) as an aeson 'Value'.
--
-- We use 'Visual' internally to easily display the 'Value' as YAML
-- when the test suite uses its 'Show' instance.
shouldReturnOneOfYaml :: HasCallStack => Context.Options -> IO Value -> [Value] -> IO ()
shouldReturnOneOfYaml options actualIO expecteds = do
  actual <- actualIO

  let Context.Options {stringifyNumbers} = options
      fixNumbers expected =
        if stringifyNumbers
          then stringifyExpectedToActual expected actual
          else expected

  shouldContain (map (Visual . fixNumbers) expecteds) [Visual actual]

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
