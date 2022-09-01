-- | Space Insensitive String.
--
-- Used for things like comparing SQL strings where one of them might be
-- formatted differently for easier reading.
module Test.SIString
  ( SIString (..),
    fromText,
  )
where

import Data.Char (isSpace)
import Data.Text qualified as T
import Hasura.Prelude

newtype SIString = SIString {getSIString :: String}
  deriving newtype (Show)

fromText :: Text -> SIString
fromText = SIString . T.unpack

instance Eq SIString where
  (==) = (==) `on` filter (not . isSpace) . getSIString
