module Hasura.FeatureMatrix (render, parseLogs, extractFeatures, renderFeatureMatrix) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString as Atto
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Lucid
import Text.RawString.QQ (r)
import Prelude hiding (interact)

type FeatureName = Text

type Features = Map FeatureName [TestRun]

data TestRun = TestRun
  { -- Extra data we capture but do not currently use.
    _testRunRequirement :: Text,
    testRunGroups :: [Text],
    testRunPassed :: Bool,
    -- Extra data we capture but do not currently use.
    _testRunHspecItem :: HspecItem
  }

data HspecEventItemDone = HspecEventItemDone
  { heidGroups :: [Text],
    heidItem :: HspecItem,
    heidRequirement :: Text
  }

{- Decode things from events such as:

    {
        "event_tag": "ItemDone",
        "groups": [
            "Test.HelloWorld",
            "Postgres",
            "Example test"
        ],
        "item": {
            "duration": 1.1321041419996618,
            "info": "",
            "location": {
                "column": 5,
                "file": "test/Test/HelloWorldSpec.hs",
                "line": 60
            },
            "result": {
                "result": "Success"
            }
        },
        "requirement": "Works as expected",
        "type": "Hspec Event"
    }
-}

instance FromJSON HspecEventItemDone where
  parseJSON = withObject "Hspec Event ItemDone" \o -> do
    let tags = parseMaybe (\o' -> (,) <$> o' .: "type" <*> o' .: "event_tag") o
    unless
      (tags == Just ("Hspec Event" :: Text, "ItemDone" :: Text))
      (fail "Not a Hspec Event ItemDone")
    HspecEventItemDone <$> o .: "groups" <*> o .: "item" <*> o .: "requirement"

data HspecItem = HspecItem
  { hiResult :: HspecItemResult
  }

instance FromJSON HspecItem where
  parseJSON =
    withObject
      "Hspec Item"
      ( \o -> HspecItem <$> o .: "result"
      )

data HspecItemResult = HIRSuccess | HIRPending Text | HIRFailure Value -- Failures have details that we don't care about just yet.

instance FromJSON HspecItemResult where
  parseJSON =
    withObject
      "Hspec Item Result"
      ( \o -> do
          result :: String <- o .: "result"
          case result of
            "Success" -> pure HIRSuccess
            "Failure" -> HIRFailure <$> (o .: "reason")
            "Pending" -> HIRPending <$> (o .: "message")
            _ -> fail $ "Unknown result type: " ++ result
      )

render :: ByteString -> ByteString
render input =
  let parsedLogs = parseLogs input
      features = runExcept . flip execStateT mempty . traverse extractFeatures <$> parsedLogs
   in renderFeatureMatrix features

parseLogs :: ByteString -> Atto.Result [Value]
parseLogs = noPartial . Atto.parse (json `sepBy` (string "\n") <* (option () (void (string "\n")) *> endOfInput))

noPartial :: Atto.Result a -> Atto.Result a
noPartial (Partial c) = c "" -- You signal Attoparsec that you're done by
-- giving an empty string to partial results. A bit weird...
noPartial result = result

extractFeatures :: Value -> (StateT Features (Except Text)) ()
extractFeatures v = do
  case parseEither parseJSON v of
    -- Skip this entry, as it's uninteresting.
    Left "Error in $: Not a Hspec Event ItemDone" -> return ()
    -- We do want to report other errors though, because those are indicative
    -- of format changes that we need to catch in order to ensure we're
    -- reporting correctly.
    Left err -> throwError (T.pack err)
    Right (itemDone :: HspecEventItemDone) -> do
      case testRun itemDone of
        Nothing -> return ()
        Just run -> modify (M.unionWith (<>) (M.singleton (featureName itemDone) [run]))
  where
    featureName :: HspecEventItemDone -> FeatureName
    featureName HspecEventItemDone {..} = case heidGroups of
      [] -> "empty??"
      name : _ -> T.takeWhile (/= '.') $ fromMaybe name (T.stripPrefix "Test." name)

    testRun :: HspecEventItemDone -> Maybe TestRun
    testRun HspecEventItemDone {..} =
      TestRun heidRequirement
        <$> pure heidGroups
        <*> case hiResult heidItem of
          HIRSuccess -> Just True
          HIRPending {} -> Nothing -- We don't care about pending items
          HIRFailure {} -> Just False
        <*> pure heidItem

-- | Output the rendered feature matrix to stdout.
renderFeatureMatrix :: Atto.Result (Either Text Features) -> ByteString
renderFeatureMatrix =
  LBS.toStrict . renderBS . \case
    Fail unconsumed contexts errorMsg ->
      reportHead do
        h1_ "Failed to decode json"
        pre_ (toHtml errorMsg)
        pre_ (toHtml unconsumed)
        mapM_ (pre_ . toHtml) contexts
    Partial {} ->
      reportHead do
        h1_ "Failed to decode json (`Partial` result, impossible?)"
    Done unconsumed (Left err) ->
      reportHead do
        h1_ "Failed to gather feature information"
        pre_ (toHtml err)
        h1_ "Maybe unconsumed input remains"
        pre_ (toHtml unconsumed)
    Done "" (Right features) ->
      reportHead do
        h1_ reportTitle
        table_ $ traverse_ featureRow (M.toList features)
    Done unconsumed (Right _) ->
      reportHead do
        h1_ "Parsing succeded, but unconsumed input remains (impossible?)"
        pre_ (toHtml unconsumed)
  where
    reportHead :: Html () -> Html ()
    reportHead content =
      doctypehtml_ do
        head_ do
          meta_ [charset_ "utf-8"]
          title_ reportTitle
          style_
            [r|
              body {
                max-width: 900px;
                margin: auto;
                font-family: sans-serif;
              }
              h1 { text-align: center; }
              table {
                width: 70%;
                margin: auto;
                font-size: 20px;
              }
              td { padding: 5px; padding-left: 15px; padding-right: 15px; }
              tr { border-bottom: 1px solid #dadada; }
              tr:hover { background-color: #eee; }
            |]
        body_ content

    reportTitle = "Feature Matrix Compatibility Report"

    featureRow :: (FeatureName, [TestRun]) -> Html ()
    featureRow (name, runs) = tr_ $ do
      td_ (toHtml name)
      td_ [style_ "text-align: right", title_ (runNames runs)] (toHtml $ runsStatus runs)

    -- Construct a sensible list of indications of which tests have been run.
    runNames :: [TestRun] -> Text
    runNames =
      T.unlines
        . Set.toList
        . Set.fromList
        . filter (not . T.null)
        . map (\testRun -> runsStatus [testRun] <> " " <> inlineGroups (testRunGroups $ testRun))

    inlineGroups :: [Text] -> Text
    inlineGroups groups =
      T.intercalate
        "."
        (filter (/= "Postgres") $ tail groups)

    runsStatus :: [TestRun] -> Text
    runsStatus runs
      | all testRunPassed runs = "✅"
      | otherwise = "❌"
