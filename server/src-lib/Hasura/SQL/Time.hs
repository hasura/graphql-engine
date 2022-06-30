module Hasura.SQL.Time
  ( ZonedTimeOfDay (..),
  )
where

import Data.Aeson.Types qualified as Aeson
import Data.Attoparsec.Text as A
import Data.Attoparsec.Time (timeOfDay, timeZone)
import Data.Time.LocalTime qualified as Local
import Hasura.Prelude

data ZonedTimeOfDay = ZonedTimeOfDay
  { ztodTime :: Local.TimeOfDay,
    ztodZone :: Local.TimeZone
  }
  deriving (Show, Eq)

utc :: Local.TimeZone
utc = Local.TimeZone 0 False ""

zonedTimeOfDay :: Text -> Aeson.Parser ZonedTimeOfDay
zonedTimeOfDay t =
  A.parseOnly (p <* endOfInput) t
    `onLeft` \err -> fail $ "could not parse timetz: " ++ err
  where
    p = ZonedTimeOfDay <$> timeOfDay <*> (fromMaybe utc <$> timeZone)

instance Aeson.FromJSON ZonedTimeOfDay where
  parseJSON (Aeson.String t) = zonedTimeOfDay t
  parseJSON _ = fail "Expecting a string for timetz"
