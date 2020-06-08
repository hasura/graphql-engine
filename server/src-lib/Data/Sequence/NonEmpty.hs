module Data.Sequence.NonEmpty
  ( NESeq
  , (<|)
  , (|>)
  , init
  , head
  , tail
  , toSeq
  ) where

import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import qualified Data.Functor  as Functor
import           Prelude       (Eq, Show, fst, snd, (.), Semigroup(..))

infixr 5 <|
infixl 5 |>

newtype NESeq a
  = NESeq { unNESeq :: (a, Seq.Seq a)}
  deriving (Show, Eq)

instance Functor.Functor NESeq where
  fmap f (NESeq (a, rest))
    = NESeq (f a, Functor.fmap f rest)

instance Foldable.Foldable NESeq where
  foldr f v = Foldable.foldr f v . toSeq

init :: a -> NESeq a
init a = NESeq (a, Seq.empty)

head :: NESeq a -> a
head = fst . unNESeq

tail :: NESeq a -> Seq.Seq a
tail = snd . unNESeq

(|>) :: NESeq a -> a -> NESeq a
(NESeq (h, l)) |> v = NESeq (h, l Seq.|> v)

(<|) :: a -> NESeq a -> NESeq a
v <| (NESeq (h, l)) = NESeq (v, h Seq.<| l)

toSeq :: NESeq a -> Seq.Seq a
toSeq (NESeq (v, l)) = v Seq.<| l

instance Semigroup (NESeq a) where
  (NESeq (h, l)) <> r =
    NESeq (h, l <> toSeq r)
