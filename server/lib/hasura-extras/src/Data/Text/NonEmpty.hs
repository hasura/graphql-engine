{-# LANGUAGE TemplateHaskellQuotes #-}

module Data.Text.NonEmpty
  ( NonEmptyText,
    mkNonEmptyTextUnsafe,
    mkNonEmptyText,
    unNonEmptyText,
    nonEmptyText,
    nonEmptyTextCodec,
    nonEmptyTextQQ,
  )
where

import Autodocodec (HasCodec (codec), JSONCodec, bimapCodec, textCodec)
import Data.Aeson
import Data.Text qualified as T
import Data.Text.Extended
import Database.PG.Query qualified as PG
import Hasura.Prelude hiding (lift)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Code, Lift, Q, bindCode, lift)
import Test.QuickCheck qualified as QC

newtype NonEmptyText = NonEmptyText {unNonEmptyText :: Text}
  deriving (Show, Eq, Ord, Hashable, ToJSON, ToJSONKey, Lift, PG.ToPrepArg, ToTxt, Generic, NFData, Semigroup)

instance QC.Arbitrary NonEmptyText where
  arbitrary = NonEmptyText . T.pack <$> QC.listOf1 (QC.elements alphaNumerics)

mkNonEmptyText :: Text -> Maybe NonEmptyText
mkNonEmptyText "" = Nothing
mkNonEmptyText text = Just $ NonEmptyText text

mkNonEmptyTextUnsafe :: Text -> NonEmptyText
mkNonEmptyTextUnsafe = NonEmptyText

parseNonEmptyText :: (MonadFail m) => Text -> m NonEmptyText
parseNonEmptyText text = mkNonEmptyText text `onNothing` fail "empty string not allowed"

nonEmptyText :: Text -> Code Q NonEmptyText
nonEmptyText textDirty = parseNonEmptyText textDirty `bindCode` \text -> [||text||]

nonEmptyTextCodec :: JSONCodec NonEmptyText
nonEmptyTextCodec = bimapCodec dec enc textCodec
  where
    dec = maybeToEither "empty string not allowed" . parseNonEmptyText
    enc = unNonEmptyText

-- | Construct 'NonEmptyText' literals at compile-time via quasiquotation.
nonEmptyTextQQ :: QuasiQuoter
nonEmptyTextQQ =
  QuasiQuoter {quoteExp, quotePat, quoteType, quoteDec}
  where
    quotePat _ = error "nonEmptyTextQQ does not support quoting patterns"
    quoteType _ = error "nonEmptyTextQQ does not support quoting types"
    quoteDec _ = error "nonEmptyTextQQ does not support quoting declarations"
    quoteExp s = case mkNonEmptyText (T.pack s) of
      Just result -> lift result
      Nothing -> fail "empty string not allowed"

instance FromJSON NonEmptyText where
  parseJSON = withText "String" parseNonEmptyText

instance FromJSONKey NonEmptyText where
  fromJSONKey = FromJSONKeyTextParser parseNonEmptyText

instance PG.FromCol NonEmptyText where
  fromCol bs =
    mkNonEmptyText
      <$> PG.fromCol bs
      >>= maybe (Left "empty string not allowed") Right

instance HasCodec NonEmptyText where
  codec = nonEmptyTextCodec
