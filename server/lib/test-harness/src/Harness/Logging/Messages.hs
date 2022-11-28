{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Harness.Logging.Messages
  ( Logger (..),
    TraceString,
    LoggableMessage (..),
    LogTrace (..),
    logTrace,
    LogHspecEvent (..),
    LogWithContext (..),
    LogHarness (..),
    logHarness,
    -- FastLogger integration
    flLogger,
  )
where

import Data.Aeson hiding (Error, Result, Success)
import Data.Aeson.Types (Pair)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Text.Lazy qualified as LT
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Hasura.Prelude hiding (Seconds)
import System.Log.FastLogger qualified as FL
import Test.Hspec.Core.Format

-- | Newtype wrapper around logging action to encapsulate existential type.
newtype Logger = Logger {runLogger :: forall a. LoggableMessage a => a -> IO ()}

-- | Type class to make it convenient to construct trace messages from various
-- text string types. You should likely not define new instances of this class.
class TraceString a where
  toTraceString :: a -> Text

instance TraceString String where
  toTraceString = T.pack

instance TraceString Text where
  toTraceString = id

instance TraceString LT.Text where
  toTraceString = LT.toStrict

instance TraceString LBS.ByteString where
  toTraceString = decodeUtf8 . LBS.toStrict

instance TraceString BS.ByteString where
  toTraceString = decodeUtf8

-- | Type class for message types which are loggable.
-- This module defines most instances which we expect to have, but it's
-- conceivable that certain spec or harness modules could legitimately define
-- their own.
--
-- Expectations of message format stability may differ from instance to
-- instance.
class LoggableMessage a where
  fromLoggableMessage :: a -> Value

-- | We want the code to deliberately give _some_ semantics to the messages
-- that are being logged, so we do not permit logging raw JSON values.
--
-- If you find yourself wanting to do this, consider defining a new, bespoke
-- message type that describes what you want to log.
instance TypeError ('Text "Please define a custom message type rather than logging raw JSON values") => LoggableMessage Value where
  fromLoggableMessage = undefined

newtype LogTrace = LogTrace Text

instance LoggableMessage LogTrace where
  fromLoggableMessage (LogTrace msg) =
    object [("type", String "LogTrace"), ("message", String msg)]

logTrace :: TraceString a => a -> LogTrace
logTrace = LogTrace . toTraceString

newtype LogHspecEvent = LogHspecEvent {unLogHspecEvent :: Event}

instance LoggableMessage LogHspecEvent where
  fromLoggableMessage (LogHspecEvent event) =
    case event of
      Started -> encEvent "Started" []
      GroupStarted path -> encEvent "GroupStarted" (encPath path)
      GroupDone path -> encEvent "GroupDone" (encPath path)
      Progress path progress -> encEvent "Progress" (encPath path <> encProgress progress)
      ItemStarted path -> encEvent "ItemStarted" (encPath path)
      ItemDone path item -> encEvent "ItemDone" (encPath path <> encItem item)
      Done items -> encEvent "Done" ([("no_items", toJSON (length items))])
    where
      encEvent :: Text -> [Pair] -> Value
      encEvent eventTag eventFields =
        object $
          [ ("type", String "Hspec Event"),
            ("event_tag", toJSON eventTag)
          ]
            <> eventFields

      encPath :: ([String], String) -> [Pair]
      encPath (groups, req) =
        [ ("groups", toJSON groups),
          ("requirement", toJSON req)
        ]

      encProgress :: Progress -> [Pair]
      encProgress progress = [("progress", toJSON progress)]

      encLocation :: Maybe Location -> Value
      encLocation Nothing = Null
      encLocation (Just Location {locationFile, locationLine, locationColumn}) =
        object
          [ ("file", toJSON locationFile),
            ("line", toJSON locationLine),
            ("column", toJSON locationColumn)
          ]

      encSeconds :: Seconds -> Value
      encSeconds (Seconds secs) = toJSON secs

      encResult :: Result -> Value
      encResult result = case result of
        Success -> object [("result", String "Success")]
        Pending loc msg ->
          object
            [ ("result", String "Pending"),
              ("location", encLocation loc),
              ("message", toJSON msg)
            ]
        Failure loc failureReason ->
          object
            [ ("result", String "Failure"),
              ("location", encLocation loc),
              ("reason", encFailureReason failureReason)
            ]

      encFailureReason :: FailureReason -> Value
      encFailureReason = \case
        NoReason -> object [("failure_reason", String "NoReason")]
        Reason reason ->
          object
            [ ("failure_reason", String "Reason"),
              ("reason", toJSON reason)
            ]
        ExpectedButGot msg expected actual ->
          object
            [ ("failure_reason", String "ExpectedButGot"),
              ("message", toJSON msg),
              ("expected", toJSON expected),
              ("actual", toJSON actual)
            ]
        Error msg exn ->
          object
            [ ("failure_reason", String "Error"),
              ("message", toJSON msg),
              ("exception", toJSON (show exn))
            ]

      encItem :: Item -> [Pair]
      encItem Item {itemLocation, itemDuration, itemInfo, itemResult} =
        [ ( "item",
            object
              [ ("location", encLocation itemLocation),
                ("duration", encSeconds itemDuration),
                ("info", toJSON itemInfo),
                ("result", encResult itemResult)
              ]
          )
        ]

data LogWithContext = LogWithContext
  { lwcContext :: [Text],
    lwcLog :: Value
  }

instance LoggableMessage LogWithContext where
  fromLoggableMessage LogWithContext {..} =
    object
      [ ("type", String "LogWithContext"),
        ("context", toJSON lwcContext),
        ("log", lwcLog)
      ]

-- | Temporary message type for messages logged from within the Harness modules.
-- Ideally these should have more bespoke message types to make the logs easier
-- to sort through.
newtype LogHarness = LogHarness {unLogHarness :: Text}

logHarness :: TraceString a => a -> LogHarness
logHarness = LogHarness . toTraceString

instance LoggableMessage LogHarness where
  fromLoggableMessage (LogHarness msg) =
    object
      [ ("type", String "LogHarness"),
        ("message", String msg)
      ]

-- | 'fast-logger' integration.
flLogger :: (FL.LogStr -> IO ()) -> Logger
flLogger logAction = Logger (logAction . msgToLogStr)

msgToLogStr :: LoggableMessage a => a -> FL.LogStr
msgToLogStr = FL.toLogStr . (<> "\n") . encode . fromLoggableMessage
