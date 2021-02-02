module Data.Text.Extended
       ( ToTxt(..)
       , bquote
       , squote
       , dquote
       , dquoteList
       , commaSeparated
       , paren
       , parenB
       , (<->)
       , (<~>)
       , (<>>)
       , (<<>)
       ) where

import           Hasura.Prelude

import qualified Language.GraphQL.Draft.Syntax  as G
import qualified Language.GraphQL.Draft.Printer as G
import qualified Text.Builder                   as TB

import           Data.Text                      as DT


class ToTxt a where
  toTxt :: a -> Text

instance ToTxt Text where
  toTxt = id
  {-# INLINE toTxt #-}

instance ToTxt G.Name where
  toTxt = G.unName

deriving instance ToTxt G.EnumValue

instance ToTxt (G.Value Void) where
  toTxt = TB.run . G.value

bquote :: ToTxt t => t -> Text
bquote t = DT.singleton '`' <> toTxt t <> DT.singleton '`'
{-# INLINE bquote #-}

squote :: ToTxt t => t -> Text
squote t = DT.singleton '\'' <> toTxt t <> DT.singleton '\''
{-# INLINE squote #-}

dquote :: ToTxt t => t -> Text
dquote t = DT.singleton '"' <> toTxt t <> DT.singleton '"'
{-# INLINE dquote #-}

paren :: ToTxt t => t -> Text
paren t = "(" <> toTxt t <> ")"
{-# INLINE paren #-}

parenB :: TB.Builder -> TB.Builder
parenB t = TB.char '(' <> t <> TB.char ')'
{-# INLINE parenB #-}

dquoteList :: (ToTxt t, Foldable f) => f t -> Text
dquoteList = DT.intercalate ", " . fmap dquote . toList
{-# INLINE dquoteList #-}

commaSeparated :: (ToTxt t, Foldable f) => f t -> Text
commaSeparated = DT.intercalate ", " . fmap toTxt . toList
{-# INLINE commaSeparated #-}


infixr 6 <->
(<->) :: ToTxt t => t -> t -> Text
(<->) l r = toTxt l <> DT.singleton ' ' <> toTxt r
{-# INLINE (<->) #-}

infixr 6 <>>
(<>>) :: ToTxt t => Text -> t -> Text
(<>>) lTxt a = lTxt <> dquote a
{-# INLINE (<>>) #-}

infixr 6 <<>
(<<>) :: ToTxt t => t -> Text -> Text
(<<>) a rTxt = dquote a <> rTxt
{-# INLINE (<<>) #-}

infixr 6 <~>
(<~>) :: TB.Builder -> TB.Builder -> TB.Builder
(<~>) l r = l <> TB.char ' ' <> r
{-# INLINE (<~>) #-}
