{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Templating yaml files.
module Harness.Quoter.Yaml
  ( yaml,
    shouldReturnYaml,
    shouldReturnOneOfYaml,
    shouldBeYaml,
  )
where

import Control.Exception.Safe (Exception, impureThrow, throwM)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Aeson (Value)
import Data.Aeson qualified
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.KeyMap.Extended qualified as KM
import Data.Aeson.Text qualified as Aeson.Text
import Data.Conduit (runConduitRes, (.|))
import Data.Conduit.List qualified as CL
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error qualified as TE
import Data.Text.Lazy qualified as LT
import Data.Vector qualified as Vector
import Data.Yaml qualified
import Data.Yaml.Internal qualified
import Harness.Test.Context qualified as Context (Options (..))
import Instances.TH.Lift ()
import Language.Haskell.TH (Exp, Q, listE, mkName, runIO, varE)
import Language.Haskell.TH.Lift (Lift)
import Language.Haskell.TH.Lift qualified as TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec (HasCallStack, shouldBe, shouldContain)
import Text.Libyaml qualified as Libyaml
import Prelude

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

-------------------------------------------------------------------

-- * Quasi quoters

yaml :: QuasiQuoter
yaml =
  QuasiQuoter
    { quoteExp = templateYaml,
      quotePat = \_ -> fail "invalid",
      quoteType = \_ -> fail "invalid",
      quoteDec = \_ -> fail "invalid"
    }

-- | Template a YAML file contents. Throws a bunch of exception types:
--  'YamlTemplateException' or 'YamlException' or 'ParseException'.
--
-- Produces 'Value'.
templateYaml :: String -> Q Exp
templateYaml inputString = do
  events <-
    runIO
      ( runConduitRes
          (Libyaml.decode inputBytes .| CL.mapM processor .| CL.consume)
      )
  [|
    unsafePerformIO
      ( do
          bs <-
            runConduitRes
              (CL.sourceList (concat $(listE events)) .| Libyaml.encode)
          case Data.Yaml.decodeEither' bs of
            Left e -> impureThrow e
            Right (v :: Aeson.Value) -> pure v
      )
    |]
  where
    inputBytes = encodeUtf8 $ T.pack inputString

-- | Process the events as they come in, potentially expanding any
-- aliases to objects.
processor :: Libyaml.Event -> ResourceT IO (Q Exp)
processor =
  \case
    Libyaml.EventAlias anchorName ->
      pure
        [|
          Data.Yaml.Internal.objToEvents
            Data.Yaml.Internal.defaultStringStyle
            (Data.Aeson.toJSON $(varE (mkName anchorName)))
            []
          |]
    -- We disable anchors because aliases are used only to refer to
    -- Haskell variables, not YAML anchors.
    (Libyaml.EventScalar _ _ _ (Just {})) -> throwM AnchorsAreDisabled
    (Libyaml.EventSequenceStart _ _ (Just {})) -> throwM AnchorsAreDisabled
    (Libyaml.EventMappingStart _ _ (Just {})) -> throwM AnchorsAreDisabled
    event -> pure (TH.lift [event])

-------------------------------------------------------------------

-- * YAML types

-- | Exceptions that will be thrown mercilessly.
data YamlTemplateException
  = AnchorsAreDisabled
  | YamlEncodingProblem TE.UnicodeException
  deriving stock (Show)
  deriving anyclass (Exception)

deriving instance Lift Libyaml.Event

deriving instance Lift Libyaml.Style

deriving instance Lift Libyaml.Tag

deriving instance Lift Libyaml.SequenceStyle

deriving instance Lift Libyaml.MappingStyle

-- | For the test suite: diff structural, but display in a readable
-- way.
newtype Visual = Visual {unVisual :: Value}
  deriving stock (Eq)

instance Show Visual where
  show = T.unpack . decodeUtf8With TE.lenientDecode . Data.Yaml.encode . unVisual
