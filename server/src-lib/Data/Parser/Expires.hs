module Data.Parser.Expires
  ( parseExpirationTime
  ) where

import           Control.Monad.Except
import           Data.Maybe              (maybe)
import qualified Data.String.Conversions as CS
import qualified Data.Text               as T
import           Data.Time.Clock
import           Data.Time.Format        (defaultTimeLocale, parseTimeM)

import           Hasura.Prelude


-- | Extracts an absolute expiration time from a Expires header.
parseExpirationTime :: MonadError String m => T.Text -> m UTCTime
parseExpirationTime header = maybe (throwError errorMessage) return
                           $ parseTimeM True defaultTimeLocale timeFormat
                           $ CS.cs header
  where timeFormat = "%a, %d %b %Y %T GMT"
        errorMessage = "Value of Expires header is not a valid timestamp"
