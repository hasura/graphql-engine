module Test.Expectations
  ( jsonShouldBe,
    rowsShouldBe,
    mutationResponseShouldBe,
    yamlShow,
  )
where

import Control.Lens ((%~), (&), _Just)
import Control.Monad (unless)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON (..), Value)
import Data.Algorithm.Diff (Diff, PolyDiff (..), getDiff)
import Data.HashMap.Strict (HashMap)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Data.Yaml qualified as Yaml
import GHC.Stack (HasCallStack)
import Hasura.Backends.DataConnector.API
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), hSupportsANSIColor, setSGRCode)
import System.IO (stdout)
import Test.Sandwich (expectationFailure)
import Prelude

newtype YamlShow = YamlShow {unYamlShow :: Value}
  deriving newtype (Eq)

instance Show YamlShow where
  show = T.unpack . TE.decodeUtf8With TE.lenientDecode . Yaml.encode . unYamlShow

yamlShow :: (ToJSON value) => value -> String
yamlShow = show . YamlShow . toJSON

-- | Compares two JSON values for equality, but prints their diff upon failure
-- as formatted YAML, which is a much nicer way to visualise the difference in
-- expected vs actual.
jsonShouldBe :: (HasCallStack, ToJSON value, MonadThrow m, MonadIO m) => value -> value -> m ()
jsonShouldBe actual expected =
  shouldBeWithLineDiff (YamlShow $ toJSON actual) (YamlShow $ toJSON expected)

-- | Compares two lists of response rows, but normalizes them first to remove
-- immaterial differences that show up when diffing in JSON/YAML
rowsShouldBe :: (HasCallStack, MonadThrow m, MonadIO m) => [HashMap FieldName FieldValue] -> [HashMap FieldName FieldValue] -> m ()
rowsShouldBe actual expected =
  (normalize <$> actual) `jsonShouldBe` (normalize <$> expected)

-- | Normalizes a response row so that immaterial differences are removed.
--
-- Immaterial differences can show up because 'FieldValue's are not actually decoded
-- into Haskell types and are instead decoded as appropriate while traversing the Query
-- IR (see 'FieldType' Haddocks for more info).
-- This causes any JSON-based diff to show up immaterial differences even though two
-- 'FieldValue's are equal, because the JSON contains differences that are ignored
-- for equality purposes (eg the difference between a missing rows property and a
-- null one).
-- Normalization simply removes these differences by fully deserializing and then
-- reserializing the JSON. It can do this by making the assumption that there are no
-- custom scalar types that look like relationship field values (true for the Chinook
-- data set used by the agent tests).
normalize :: HashMap FieldName FieldValue -> HashMap FieldName FieldValue
normalize =
  fmap
    ( \fieldValue ->
        case deserializeAsRelationshipFieldValue fieldValue of
          Left _ -> fieldValue
          Right queryResponse ->
            mkRelationshipFieldValue $
              queryResponse & qrRows . traverse . traverse %~ normalize
    )

shouldBeWithLineDiff :: (HasCallStack, Show value, Eq value, MonadThrow m, MonadIO m) => value -> value -> m ()
shouldBeWithLineDiff actual expected =
  unless (actual == expected) $
    expectationFailure =<< renderDiffError actual expected

renderDiffError :: (Show value, MonadIO m) => value -> value -> m String
renderDiffError actual expected = do
  useColor <- liftIO $ hSupportsANSIColor stdout
  pure $ renderDiffString useColor (show actual) (show expected)

renderDiffString :: Bool -> String -> String -> String
renderDiffString useColor actual expected =
  unlines $ resetHspecErrorColor <$> explanation <> diffLines
  where
    resetHspecErrorColor line = colorCode Reset ++ line

    explanation =
      [ "━━━",
        colorSpan Red "--- present, but not expected",
        colorSpan Green "+++ expected, but not present",
        "━━━"
      ]

    diffLines = annotateDiffLine <$> getDiff (lines actual) (lines expected)

    annotateDiffLine :: Diff String -> String
    annotateDiffLine = \case
      Both _ s -> "    " ++ s
      First s -> colorSpan Red $ "--- " ++ s
      Second s -> colorSpan Green $ "+++ " ++ s

    colorSpan c s = colorCode (SetColor Foreground Dull c) ++ s ++ colorCode Reset
    colorCode sgr = if useColor then setSGRCode [sgr] else ""

mutationResponseShouldBe :: (HasCallStack, MonadThrow m, MonadIO m) => MutationResponse -> MutationResponse -> m ()
mutationResponseShouldBe actual expected =
  (normalizeMutationResponse actual) `jsonShouldBe` (normalizeMutationResponse expected)

normalizeMutationResponse :: MutationResponse -> MutationResponse
normalizeMutationResponse response =
  response & mrOperationResults . traverse %~ (morReturning . _Just . traverse %~ normalize)
