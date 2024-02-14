module Hasura.Tracing.TraceState
  ( TraceState,
    Key (..),
    Value (..),
    emptyTraceState,
    toTraceStateList,
    decodeTraceStateHeader,
  )
where

import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly, string, takeWhile, try)
import Data.ByteString (ByteString)
import Data.Char (isAsciiLower, isDigit)
import Hasura.Prelude hiding (empty, takeWhile, toList)

newtype Key = Key Text
  deriving (Show, Eq, Ord)

newtype Value = Value Text
  deriving (Show, Eq, Ord)

-- | Data structure compliant with the storage and serialization needs of the W3C tracestate header.
-- https://www.w3.org/TR/trace-context/#tracestate-header
newtype TraceState = TraceState [(Key, Value)]
  deriving (Show, Eq, Ord)

-- | An empty 'TraceState' key-value pair dictionary
emptyTraceState :: TraceState
emptyTraceState = TraceState []

-- | Convert the 'TraceState' to a list.
toTraceStateList :: TraceState -> [(Key, Value)]
toTraceStateList (TraceState ts) = ts

traceStateParser :: Parser TraceState
traceStateParser = do
  pairs <- many stateItemParser
  pure $ TraceState pairs
  where
    isValid c = isDigit c || (isAsciiLower c)
    -- The tracestate field value is a list of list-members separated by commas (,)
    -- e.g. vendorname1=opaqueValue1,vendorname2=opaqueValue2
    stateItemParser :: Parser (Key, Value)
    stateItemParser = do
      key <- takeWhile isValid
      _ <- string "="
      value <- takeWhile isValid
      _ <- try $ string ","
      pure $ (Key (bsToTxt key), Value (bsToTxt value))

decodeTraceStateHeader :: Data.ByteString.ByteString -> Maybe TraceState
decodeTraceStateHeader ts = case parseOnly traceStateParser ts of
  Left _ -> Nothing
  Right ok -> Just ok
