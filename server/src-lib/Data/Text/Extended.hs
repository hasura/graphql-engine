module Data.Text.Extended
       ( module DT
       , squote
       , dquote
       , paren
       , (<->)
       ) where

import           Hasura.Prelude

import           Data.Text      as DT

squote :: DT.Text -> DT.Text
squote t = DT.singleton '\'' <> t <> DT.singleton '\''
{-# INLINE squote #-}

dquote :: DT.Text -> DT.Text
dquote t = DT.singleton '"' <> t <> DT.singleton '"'
{-# INLINE dquote #-}

paren :: DT.Text -> DT.Text
paren t = "(" <> t <> ")"
{-# INLINE paren #-}

infixr 6 <->
(<->) :: DT.Text -> DT.Text -> DT.Text
(<->) l r = l <> DT.singleton ' ' <> r
{-# INLINE (<->) #-}
