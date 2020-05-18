module Hasura.SQL.Time
    ( ZonedTimeOfDay(..)
    ) where

import           Data.Attoparsec.Text as A
import           Data.Attoparsec.Time (timeOfDay, timeZone)
import           Hasura.Prelude

import qualified Data.Aeson.Types     as Aeson
import qualified Data.Text            as T
import qualified Data.Time.LocalTime  as Local

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
