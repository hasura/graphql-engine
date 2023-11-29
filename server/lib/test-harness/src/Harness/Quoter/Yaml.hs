{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Templating yaml files.
module Harness.Quoter.Yaml
  ( yaml,
    fromYaml,
    interpolateYaml,
    ToYamlString (..),
  )
where

import Control.Exception.Safe (Exception, impureThrow, throwM)
import Control.Monad.Identity
import Control.Monad.Trans.Resource (ResourceT)
import Data.Aeson qualified as J
import Data.Conduit (runConduitRes, (.|))
import Data.Conduit.List qualified as CL
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding.Error qualified as TE
import Data.Yaml qualified
import Data.Yaml.Internal qualified
import Harness.Quoter.Yaml.InterpolateYaml
import Hasura.Prelude
import Instances.TH.Lift ()
import Language.Haskell.TH
import Language.Haskell.TH.Lift (Lift)
import Language.Haskell.TH.Lift qualified as TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import System.IO.Unsafe (unsafePerformIO)
import Text.Libyaml qualified as Libyaml

-------------------------------------------------------------------

-- * Quasi quoters

-- | Treats Yaml like JSON, and allows other ToJSON instances to be
-- combined with it using Yaml anchors
-- ie '*haskellVar: otherStuff'
-- or 'key: *haskellValue'
yaml :: QuasiQuoter
yaml =
  QuasiQuoter
    { quoteExp = templateYaml,
      quotePat = \_ -> fail "invalid",
      quoteType = \_ -> fail "invalid",
      quoteDec = \_ -> fail "invalid"
    }

-- | Combines `yaml` with `fromJson` for convenience.
fromYaml :: QuasiQuoter
fromYaml =
  QuasiQuoter
    { quoteExp = templateFromYaml,
      quotePat = \_ -> fail "invalid",
      quoteType = \_ -> fail "invalid",
      quoteDec = \_ -> fail "invalid"
    }

templateFromYaml :: String -> Q Exp
templateFromYaml inputString = do
  e <- templateYaml inputString
  [|
    case fromJSON ($(pure e)) of
      J.Error err -> error err
      J.Success s -> s
    |]

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
            Right (v :: J.Value) -> pure v
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
            (J.toJSON $(varE (mkName anchorName)))
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
