{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module:      Data.Aeson.Parser.Time
-- Copyright:   (c) 2015-2016 Bryan O'Sullivan
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Parsers for parsing dates and times.

module Hasura.SQL.Time
    ( ZonedTimeOfDay(..)
    ) where

import           Control.Monad        (void, when)
import           Data.Attoparsec.Text as A
import           Data.Bits            ((.&.))
import           Data.Char            (isDigit, ord)
import           Data.Fixed           (Fixed (MkFixed), Pico)
import           Data.Int             (Int64)
import           Data.Maybe           (fromMaybe)
import           Hasura.Prelude

import qualified Data.Aeson.Types     as Aeson
import qualified Data.Text            as T
import qualified Data.Time.LocalTime  as Local

toPico :: Integer -> Pico
toPico = MkFixed

-- | Parse a two-digit integer (e.g. day of month, hour).
twoDigits :: Parser Int
twoDigits = do
  a <- digit
  b <- digit
  let c2d c = ord c .&. 15
  return $! c2d a * 10 + c2d b

-- | Parse a time of the form @HH:MM[:SS[.SSS]]@.
timeOfDay :: Parser Local.TimeOfDay
timeOfDay = do
  h <- twoDigits
  m <- char ':' *> twoDigits
  s <- option 0 (char ':' *> seconds)
  if h < 24 && m < 60 && s < 61
    then return (Local.TimeOfDay h m s)
    else fail "invalid time"

data T = T {-# UNPACK #-} !Int {-# UNPACK #-} !Int64

-- | Parse a count of seconds, with the integer part being two digits
-- long.
seconds :: Parser Pico
seconds = do
  real <- twoDigits
  mc <- peekChar
  case mc of
    Just '.' -> do
      t <- anyChar *> takeWhile1 isDigit
      return $! parsePicos real t
    _ -> return $! fromIntegral real
 where
  parsePicos a0 t = toPico (fromIntegral (t' * 10^n))
    where T n t'  = T.foldl' step (T 12 (fromIntegral a0)) t
          step ma@(T m a) c
              | m <= 0    = ma
              | otherwise = T (m-1) (10 * a + fromIntegral (ord c) .&. 15)

-- | Parse a time zone, and return 'Nothing' if the offset from UTC is
-- zero. (This makes some speedups possible.)
timeZone :: Parser (Maybe Local.TimeZone)
timeZone = do
  let maybeSkip c = do ch <- peekChar'; when (ch == c) (void anyChar)
  maybeSkip ' '
  ch <- satisfy $ \c -> c == 'Z' || c == '+' || c == '-'
  if ch == 'Z'
    then return Nothing
    else do
      h <- twoDigits
      mm <- peekChar
      m <- case mm of
             Just ':' -> anyChar *> twoDigits
             Just d   | isDigit d -> twoDigits
             _        -> return 0
      let off | ch == '-' = negate off0
              | otherwise = off0
          off0 = h * 60 + m
      case undefined of
        _   | off == 0 ->
              return Nothing
            | off < -720 || off > 840 || m > 59 ->
              fail "invalid time zone offset"
            | otherwise ->
              let !tz = Local.minutesToTimeZone off
              in return (Just tz)

data ZonedTimeOfDay
  = ZonedTimeOfDay
  { ztodTime :: Local.TimeOfDay
  , ztodZone :: Local.TimeZone
  } deriving (Show, Eq)

utc :: Local.TimeZone
utc = Local.TimeZone 0 False ""

zonedTimeOfDay :: T.Text -> Aeson.Parser ZonedTimeOfDay
zonedTimeOfDay t =
  case A.parseOnly (p <* endOfInput) t of
    Left err -> fail $ "could not parse timetz: " ++ err
    Right r  -> return r
  where
    p = ZonedTimeOfDay <$> timeOfDay <*> (fromMaybe utc <$> timeZone)

instance Aeson.FromJSON ZonedTimeOfDay where
  parseJSON (Aeson.String t) = zonedTimeOfDay t
  parseJSON _                = fail "Expecting a string for timetz"
