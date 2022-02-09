{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Templating yaml files.
module Harness.Quoter.Yaml
  ( yaml,
    shouldReturnYaml,
    shouldReturnOneOfYaml,
  )
where

import Control.Exception
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.ByteString.Char8 qualified as BS8
import Data.Conduit
import Data.Conduit.List qualified as CL
import Data.HashMap.Strict qualified as Map
import Data.Text.Encoding.Error qualified as T
import Data.Text.Lazy qualified as LT
import Data.Vector qualified as V
import Data.Yaml qualified
import Data.Yaml.Internal qualified
import Harness.Test.Feature (BackendOptions (..))
import Instances.TH.Lift ()
import Language.Haskell.TH
import Language.Haskell.TH.Lift as TH
import Language.Haskell.TH.Quote
import System.IO.Unsafe
import Test.Hspec (shouldBe, shouldContain)
import Text.Libyaml qualified as Libyaml
import Prelude

-------------------------------------------------------------------

-- * Expectations

-- | The action @actualIO@ should produce the @expected@ YAML,
-- represented (by the yaml package) as an aeson 'Value'.
--
-- We use 'Visual' internally to easily display the 'Value' as YAML
-- when the test suite uses its 'Show' instance.
shouldReturnYaml :: BackendOptions -> IO Value -> Value -> IO ()
shouldReturnYaml BackendOptions {stringifyNumbers} actualIO expected = do
  actual <- actualIO
  let expected' =
        if stringifyNumbers then stringifyExpectedToActual expected actual else expected
  shouldBe (Visual actual) (Visual expected')

stringifyExpectedToActual :: Value -> Value -> Value
stringifyExpectedToActual (Number n) (String _) = String (LT.toStrict $ encodeToLazyText n)
stringifyExpectedToActual (Object hm) (Object hm') =
  let stringifyKV k v =
        case Map.lookup k hm' of
          Just v' -> stringifyExpectedToActual v v'
          Nothing -> v
   in Object (Map.mapWithKey stringifyKV hm)
stringifyExpectedToActual (Array as) (Array bs) = Array (V.zipWith stringifyExpectedToActual as bs)
stringifyExpectedToActual expected _ = expected

-- | The action @actualIO@ should produce the @expected@ YAML,
-- represented (by the yaml package) as an aeson 'Value'.
--
-- We use 'Visual' internally to easily display the 'Value' as YAML
-- when the test suite uses its 'Show' instance.
shouldReturnOneOfYaml :: BackendOptions -> IO Value -> [Value] -> IO ()
shouldReturnOneOfYaml BackendOptions {stringifyNumbers} actualIO expecteds = do
  actual <- actualIO
  let fixNumbers expected =
        if stringifyNumbers then stringifyExpectedToActual expected actual else expected
  shouldContain (map (Visual . fixNumbers) expecteds) [Visual actual]

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
            Left e -> throw e
            Right (v :: Aeson.Value) -> pure v
      )
    |]
  where
    inputBytes = BS8.pack inputString

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
  | YamlEncodingProblem T.UnicodeException
  deriving stock (Show)

instance Exception YamlTemplateException

deriving instance Lift Libyaml.Event

deriving instance Lift Libyaml.Style

deriving instance Lift Libyaml.Tag

deriving instance Lift Libyaml.SequenceStyle

deriving instance Lift Libyaml.MappingStyle

-- | For the test suite: diff structural, but display in a readable
-- way.
newtype Visual = Visual {unVisual :: Value}
  deriving (Eq)

instance Show Visual where
  show = BS8.unpack . Data.Yaml.encode . unVisual
