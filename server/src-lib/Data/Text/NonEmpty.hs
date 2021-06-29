module Data.Text.NonEmpty
  ( NonEmptyText
  , mkNonEmptyTextUnsafe
  , mkNonEmptyText
  , unNonEmptyText
  , nonEmptyText
  ) where

import           Hasura.Prelude

import qualified Data.Text                  as T
import qualified Database.PG.Query          as Q
import qualified Test.QuickCheck            as QC

import           Data.Aeson
import           Data.Text.Extended
import           Language.Haskell.TH.Syntax (Lift, Q, TExp)


newtype NonEmptyText = NonEmptyText { unNonEmptyText :: Text }
  deriving (Show, Eq, Ord, Hashable, ToJSON, ToJSONKey, Lift, Q.ToPrepArg, ToTxt, Generic, NFData)

instance Arbitrary NonEmptyText where
  arbitrary = NonEmptyText . T.pack <$> QC.listOf1 (QC.elements alphaNumerics)

mkNonEmptyText :: Text -> Maybe NonEmptyText
mkNonEmptyText ""   = Nothing
mkNonEmptyText text = Just $ NonEmptyText text

mkNonEmptyTextUnsafe :: Text -> NonEmptyText
mkNonEmptyTextUnsafe = NonEmptyText

parseNonEmptyText :: MonadFail m => Text -> m NonEmptyText
parseNonEmptyText text = mkNonEmptyText text `onNothing` fail "empty string not allowed"

nonEmptyText :: Text -> Q (TExp NonEmptyText)
nonEmptyText = parseNonEmptyText >=> \text -> [|| text ||]

instance FromJSON NonEmptyText where
  parseJSON = withText "String" parseNonEmptyText

instance FromJSONKey NonEmptyText where
  fromJSONKey = FromJSONKeyTextParser parseNonEmptyText

instance Q.FromCol NonEmptyText where
  fromCol bs = mkNonEmptyText <$> Q.fromCol bs
    >>= maybe (Left "empty string not allowed") Right
