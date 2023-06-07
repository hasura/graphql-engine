module Data.Text.Extended
  ( ToTxt (..),
    bquote,
    squote,
    dquote,
    dquoteList,
    commaSeparated,
    paren,
    parenB,
    (<~>),
    (<>>),
    (<<>),
  )
where

import Data.Text as DT
import Hasura.Prelude
import Language.GraphQL.Draft.Printer qualified as G
import Language.GraphQL.Draft.Syntax qualified as G
import Text.Builder qualified as TB

class ToTxt a where
  toTxt :: a -> Text

instance ToTxt Text where
  toTxt = id
  {-# INLINE toTxt #-}

instance ToTxt Char where
  toTxt = DT.singleton

instance ToTxt G.Name where
  toTxt = G.unName

deriving instance ToTxt G.EnumValue

instance ToTxt () where
  toTxt = tshow

instance ToTxt Void where
  toTxt = absurd

instance ToTxt (G.Value Void) where
  toTxt = TB.run . G.value

bquote :: (ToTxt t) => t -> Text
bquote t = DT.singleton '`' <> toTxt t <> DT.singleton '`'

squote :: (ToTxt t) => t -> Text
squote t = DT.singleton '\'' <> toTxt t <> DT.singleton '\''

dquote :: (ToTxt t) => t -> Text
dquote t = DT.singleton '"' <> toTxt t <> DT.singleton '"'

paren :: (ToTxt t) => t -> Text
paren t = "(" <> toTxt t <> ")"

parenB :: TB.Builder -> TB.Builder
parenB t = TB.char '(' <> t <> TB.char ')'

dquoteList :: (ToTxt t, Foldable f) => f t -> Text
dquoteList = DT.intercalate ", " . fmap dquote . toList

commaSeparated :: (ToTxt t, Foldable f) => f t -> Text
commaSeparated = DT.intercalate ", " . fmap toTxt . toList

infixr 6 <>>

(<>>) :: (ToTxt t) => Text -> t -> Text
(<>>) lTxt a = lTxt <> dquote a

infixr 6 <<>

(<<>) :: (ToTxt t) => t -> Text -> Text
(<<>) a rTxt = dquote a <> rTxt

infixr 6 <~>

(<~>) :: TB.Builder -> TB.Builder -> TB.Builder
(<~>) l r = l <> TB.char ' ' <> r
