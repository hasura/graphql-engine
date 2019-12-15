{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hasura.Prelude
  ( module M
  , alphaNumerics
  , onNothing
  , onJust
  , onLeft
  , choice
  , bsToTxt
  , txtToBs
  , spanMaybeM
  , findWithIndex
  ) where

import           Control.Applicative               as M (Alternative (..))
import           Control.Arrow                     as M (first, second, (&&&), (***))
import           Control.Monad                     as M (void, when)
import           Control.Monad.Base                as M
import           Control.Monad.Except              as M
import           Control.Monad.Fail                as M (MonadFail)
import           Control.Monad.Identity            as M
import           Control.Monad.Reader              as M
import           Control.Monad.State.Strict        as M
import           Data.Bool                         as M (bool)
import           Data.Data                         as M (Data (..))
import           Data.Either                       as M (lefts, partitionEithers, rights)
import           Data.Foldable                     as M (asum, foldrM, for_, toList, traverse_)
import           Data.Function                     as M (on, (&))
import           Data.Functor                      as M (($>), (<&>))
import           Data.Hashable                     as M (Hashable)
import           Data.List                         as M (find, findIndex, foldl', group,
                                                         intercalate, intersect, lookup, sort,
                                                         sortBy, sortOn, union, unionBy, (\\))
import           Data.Maybe                        as M (catMaybes, fromMaybe, isJust, isNothing,
                                                         listToMaybe, mapMaybe, maybeToList)
import           Data.Ord                          as M (comparing)
import           Data.Semigroup                    as M (Semigroup (..))
import           Data.Sequence                     as M (Seq)
import           Data.String                       as M (IsString)
import           Data.Text                         as M (Text)
import           Data.Traversable                  as M (for)
import           Data.Word                         as M (Word64)
import           GHC.Generics                      as M (Generic)
import           Prelude                           as M hiding (fail, init, lookup)
import           Test.QuickCheck.Arbitrary.Generic as M
import           Text.Read                         as M (readEither, readMaybe)

import qualified Data.ByteString                   as B
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as TE
import qualified Data.Text.Encoding.Error          as TE
import qualified Test.QuickCheck                   as QC

alphaNumerics :: String
alphaNumerics = ['a'..'z'] ++ ['A'..'Z'] ++ "0123456789 "

instance Arbitrary Text where
  arbitrary = T.pack <$> QC.listOf (QC.elements alphaNumerics)

onNothing :: (Monad m) => Maybe a -> m a -> m a
onNothing m act = maybe act return m

onJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
onJust m action = maybe (return ()) action m

onLeft :: (Monad m) => Either e a -> (e -> m a) -> m a
onLeft e f = either f return e

choice :: (Alternative f) => [f a] -> f a
choice = asum

bsToTxt :: B.ByteString -> Text
bsToTxt = TE.decodeUtf8With TE.lenientDecode

txtToBs :: Text -> B.ByteString
txtToBs = TE.encodeUtf8

-- Like 'span', but monadic and with a function that produces 'Maybe' instead of 'Bool'
spanMaybeM
  :: (Foldable f, Monad m)
  => (a -> m (Maybe b)) -> f a -> m ([b], [a])
spanMaybeM f = go . toList
  where
    go []     = pure ([], [])
    go l@(x:xs) = f x >>= \case
      Just y  -> first (y:) <$> go xs
      Nothing -> pure ([], l)

findWithIndex :: (a -> Bool) -> [a] -> Maybe (a, Int)
findWithIndex p l = do
  v <- find p l
  i <- findIndex p l
  pure (v, i)
