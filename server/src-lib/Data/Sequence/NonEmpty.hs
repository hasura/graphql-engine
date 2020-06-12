{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Data.Sequence.NonEmpty
  ( NESeq
  , pattern (:<||)
  , pattern (:||>)
  , (<|)
  , (|>)
  , singleton
  , head
  , tail
  , toSeq
  ) where

import           Prelude       hiding (head, tail)

import qualified Data.Sequence as Seq

import           Data.Foldable

infixr 5 <|
infixl 5 |>

data NESeq a = NESeq
  { head :: a
  , tail :: Seq.Seq a
  } deriving (Show, Eq, Functor, Traversable)

instance Semigroup (NESeq a) where
  NESeq x xs <> NESeq y ys = NESeq x (xs Seq.>< y Seq.<| ys)

instance Foldable NESeq where
  null _ = False
  toList (NESeq x xs) = x : toList xs
  length (NESeq _ xs) = 1 + length xs
  foldl1 f (NESeq x xs) = foldl f x xs

  fold       = fold       . toSeq
  foldMap f  = foldMap f  . toSeq
  foldl  f v = foldl  f v . toSeq
  foldl' f v = foldl' f v . toSeq
  foldr  f v = foldr  f v . toSeq
  foldr' f v = foldr' f v . toSeq
  foldr1 f   = foldr1 f   . toSeq

singleton :: a -> NESeq a
singleton a = NESeq a Seq.empty

(|>) :: NESeq a -> a -> NESeq a
NESeq h l |> v = NESeq h (l Seq.|> v)

(<|) :: a -> NESeq a -> NESeq a
v <| NESeq h l = NESeq v (h Seq.<| l)

toSeq :: NESeq a -> Seq.Seq a
toSeq (NESeq v l) = v Seq.<| l

pattern (:<||) :: a -> Seq.Seq a -> NESeq a
pattern x :<|| xs = NESeq x xs
{-# COMPLETE (:<||) #-}

unsnoc :: NESeq a -> (Seq.Seq a, a)
unsnoc (x :<|| (xs Seq.:|> y)) = (x Seq.:<| xs, y)
unsnoc (x :<|| Seq.Empty     ) = (Seq.Empty   , x)
{-# INLINE unsnoc #-}

pattern (:||>) :: Seq.Seq a -> a -> NESeq a
pattern xs :||> x <- (unsnoc->(!xs, x))
  where
    (x Seq.:<| xs) :||> y = x :<|| (xs Seq.:|> y)
    Seq.Empty      :||> y = y :<|| Seq.Empty
{-# COMPLETE (:||>) #-}
