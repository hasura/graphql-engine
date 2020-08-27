module Hasura.SQL.Text where


import           Hasura.Prelude

import qualified Data.Text.Extended            as T
import qualified Language.GraphQL.Draft.Syntax as G
import qualified Text.Builder                  as TB


class ToTxt a where
  toTxt :: a -> Text

instance ToTxt T.Text where
  toTxt = id
  {-# INLINE toTxt #-}

instance ToTxt G.Name where
  toTxt = G.unName

deriving instance ToTxt G.EnumValue


dquote :: ToTxt a => a -> T.Text
dquote = T.dquote . toTxt
{-# INLINE dquote #-}

squote :: ToTxt a => a -> T.Text
squote = T.squote . toTxt
{-# INLINE squote #-}

bquote :: ToTxt a => a -> T.Text
bquote = T.bquote . toTxt
{-# INLINE bquote #-}

dquoteList :: (ToTxt a, Foldable t) => t a -> T.Text
dquoteList = T.intercalate ", " . map dquote . toList
{-# INLINE dquoteList #-}

infixr 6 <>>
(<>>) :: ToTxt a => T.Text -> a -> T.Text
(<>>) lTxt a = lTxt <> dquote a
{-# INLINE (<>>) #-}

infixr 6 <<>
(<<>) :: ToTxt a => a -> T.Text -> T.Text
(<<>) a rTxt = dquote a <> rTxt
{-# INLINE (<<>) #-}

infixr 6 <->
(<->) :: TB.Builder -> TB.Builder -> TB.Builder
(<->) l r = l <> TB.char ' ' <> r
{-# INLINE (<->) #-}

paren :: TB.Builder -> TB.Builder
paren t = TB.char '(' <> t <> TB.char ')'
{-# INLINE paren #-}
