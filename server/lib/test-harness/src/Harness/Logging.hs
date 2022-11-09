module Harness.Logging
  ( addLoggingFormatter,
    loggingFormatter,
    contextualizeLogger,
    module Messages,
  )
where

import Data.Text qualified as T
import Harness.Logging.Messages as Messages
import Harness.TestEnvironment
import Hasura.Prelude
import Test.Hspec.Core.Format qualified as Hspec
import Test.Hspec.Core.Runner
import Test.Hspec.Core.Spec

-- | Make the logger in the 'TestEnvironment' add context about the specs that use it.
contextualizeLogger :: SpecWith TestEnvironment -> SpecWith TestEnvironment
contextualizeLogger = mapSpecForest (map contextualizeTree)

contextualizeTree :: SpecTree TestEnvironment -> SpecTree TestEnvironment
contextualizeTree spectree = go [] spectree
  where
    go :: [Text] -> SpecTree TestEnvironment -> SpecTree TestEnvironment
    go ps (Node path children) = Node path (map (go (T.pack path : ps)) children)
    go ps (NodeWithCleanup loc action children) =
      NodeWithCleanup
        loc
        action
        (map (go ps) children)
    go ps (Leaf item) =
      Leaf $
        item
          { itemExample =
              \params actionRunner progressCallback ->
                itemExample
                  item
                  params
                  (actionRunner . (\action -> action . attachPrefix ps))
                  progressCallback
          }

    attachPrefix :: [Text] -> TestEnvironment -> TestEnvironment
    attachPrefix prefixes te =
      te
        { logger = Logger $ \msg -> runLogger (logger te) $ LogWithContext prefixes (fromLoggableMessage msg)
        }

-- | A Hspec 'Formatter' that outputs to a 'Logger'.
loggingFormatter :: Logger -> Hspec.FormatConfig -> IO Hspec.Format
loggingFormatter logger _formatConfig =
  return $ liftIO . runLogger logger . LogHspecEvent

-- Add the logging Hspec 'Formatter' on top of the existing formatter.
addLoggingFormatter :: Logger -> Config -> Config
addLoggingFormatter logger config =
  config
    { configFormat =
        Just $ \formatConfig -> do
          logFmt <- loggingFormatter logger formatConfig
          originalFmt <- firstOrChosenFormatter $ formatConfig
          return $ \event -> do
            logFmt event
            originalFmt event
    }
  where
    emptyFormatter :: Hspec.FormatConfig -> IO Hspec.Format
    emptyFormatter _ = return $ const (return ())

    firstOrChosenFormatter
      | Just f <- configFormat config = f -- The formatter chosen via cmdline args
      | (_, f) : _ <- configAvailableFormatters config = f -- The first of the predefined formatters
      | otherwise = emptyFormatter
