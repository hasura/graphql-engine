module Data.Sequence.NonEmpty
  ( NESeq(..)
  , (<|)
  , (|>)
  , init
  , head
  , tail
  , toSeq
  , fromSeq
  , toNonEmpty
  ) where

import qualified Data.Foldable      as Foldable
import qualified Data.Functor       as Functor
import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence      as Seq

import           Data.Aeson
import           Hasura.Incremental (Cacheable)
import           Hasura.Prelude     hiding (head, tail)

infixr 5 <|
infixl 5 |>

newtype NESeq a
  = NESeq { unNESeq :: (a, Seq.Seq a)}
  deriving (Show, Eq, Generic, Traversable)
instance (NFData a) => NFData (NESeq a)
instance (Cacheable a) => Cacheable (NESeq a)

instance Functor.Functor NESeq where
  fmap f (NESeq (a, rest))
    = NESeq (f a, Functor.fmap f rest)

instance Foldable.Foldable NESeq where
  foldr f v = Foldable.foldr f v . toSeq

instance FromJSON a => FromJSON (NESeq a) where
  parseJSON v = do
    seqList <- parseJSON v
    maybe (fail "expected non empty list") pure $ fromSeq seqList

instance ToJSON a => ToJSON (NESeq a) where
  toJSON = toJSON . toSeq

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

fromSeq :: Seq.Seq a -> Maybe (NESeq a)
fromSeq = \case
  Seq.Empty   -> Nothing
  h Seq.:<| l -> Just $ NESeq (h, l)

toNonEmpty :: NESeq a -> NonEmpty a
toNonEmpty (NESeq (v, l)) = v NE.:| toList l

instance Semigroup (NESeq a) where
  (NESeq (h, l)) <> r =
    NESeq (h, l <> toSeq r)
