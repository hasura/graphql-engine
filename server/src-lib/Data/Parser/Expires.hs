module Data.Parser.Expires
  ( parseExpirationTime
  ) where

import           Control.Monad.Except
import           Data.Text.Conversions
import           Data.Time.Clock
import           Data.Time.Format      (defaultTimeLocale, parseTimeM)

import           Hasura.Prelude


-- | Extracts an absolute expiration time from a Expires header.
parseExpirationTime :: MonadError String m => Text -> m UTCTime
parseExpirationTime = fromText
  >>> parseTimeM True defaultTimeLocale "%a, %d %b %Y %T GMT"
  >>> (`onNothing` throwError "Value of Expires header is not a valid timestamp")
