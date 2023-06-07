module Harness.Logging
  ( addLoggingFormatter,
    loggingFormatter,
    contextualizeLogger,
    module Messages,
  )
where

import Data.Has
import Data.Text qualified as T
import Harness.Logging.Messages as Messages
import Hasura.Prelude
import Test.Hspec.Core.Format qualified as Hspec
import Test.Hspec.Core.Runner
import Test.Hspec.Core.Spec

-- | Make the logger in the 'GlobalTestEnvironment' add context about the specs that use it.
contextualizeLogger :: (Has Logger a) => SpecWith a -> SpecWith a
contextualizeLogger = mapSpecForest (map contextualizeTree)

contextualizeTree :: forall a. (Has Logger a) => SpecTree a -> SpecTree a
contextualizeTree spectree = go [] spectree
  where
    go :: [Text] -> SpecTree a -> SpecTree a
    go ps (Node path children) = Node path (map (go (T.pack path : ps)) children)
    go ps (NodeWithCleanup loc action children) =
      NodeWithCleanup
        loc
        action
        (map (go ps) children)
    go ps (Leaf item) =
      Leaf
        $ item
          { itemExample =
              \params actionRunner progressCallback ->
                itemExample
                  item
                  params
                  (actionRunner . (\action -> action . attachPrefix ps))
                  progressCallback
          }

    attachPrefix :: [Text] -> a -> a
    attachPrefix prefixes = modifier (\logger -> Logger $ \msg -> runLogger logger $ LogWithContext prefixes (fromLoggableMessage msg))

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
