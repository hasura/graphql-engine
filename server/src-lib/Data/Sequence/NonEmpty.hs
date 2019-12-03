module Data.Sequence.NonEmpty
  ( NESeq
  , (<|)
  , (|>)
  , init
  , head
  , toSeq
  ) where

import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import           Prelude       (Eq, Show, fst, (.))

infixr 5 <|
infixl 5 |>

newtype NESeq a
  = NESeq { unNESeq :: (a, Seq.Seq a)}
  deriving (Show, Eq)

instance F.Foldable NESeq where
  foldr f v = F.foldr f v . toSeq

init :: a -> NESeq a
init a = NESeq (a, Seq.empty)

head :: NESeq a -> a
head = fst . unNESeq

(|>) :: NESeq a -> a -> NESeq a
(NESeq (h, l)) |> v = NESeq (h, l Seq.|> v)

(<|) :: a -> NESeq a -> NESeq a
v <| (NESeq (h, l)) = NESeq (v, h Seq.<| l)

toSeq :: NESeq a -> Seq.Seq a
toSeq (NESeq (v, l)) = v Seq.<| l
