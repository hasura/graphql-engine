{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Templating yaml files.
module Harness.Yaml
  ( yaml,
    shouldReturnYaml,
  )
where

import Control.Exception
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS8
import Data.Conduit
import Data.Conduit.List qualified as CL
import Data.Text.Encoding.Error qualified as T
import Data.Yaml qualified
import Data.Yaml.Internal qualified
import Instances.TH.Lift ()
import Language.Haskell.TH
import Language.Haskell.TH.Lift as TH
import Language.Haskell.TH.Quote
import System.IO.Unsafe
import Test.Hspec (shouldBe)
import Text.Libyaml qualified as Libyaml
import Prelude

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

-- | The action @actualIO@ should produce the @expected@ YAML,
-- represented (by the yaml package) as an aeson 'Value'.
--
-- We use 'Visual' internally to easily display the 'Value' as YAML
-- when the test suite uses its 'Show' instance.
shouldReturnYaml :: IO Value -> Value -> IO ()
shouldReturnYaml actualIO expected = do
  actual <- actualIO
  shouldBe (Visual actual) (Visual expected)

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
