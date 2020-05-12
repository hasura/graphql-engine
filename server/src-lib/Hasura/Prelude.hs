{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hasura.Prelude
  ( module M
  , alphabet
  , alphaNumerics
  , onNothing
  , onJust
  , onLeft
  , choice
  , afold
  , bsToTxt
  , txtToBs
  , spanMaybeM
  , findWithIndex
  , mapFromL
  -- * Measuring and working with moments and durations
  , withElapsedTime
  , startTimer
  , module Data.Time.Clock.Units
  ) where

import           Control.Applicative               as M (Alternative (..))
import           Control.Arrow                     as M (first, second, (&&&), (***), (<<<), (>>>))
import           Control.DeepSeq                   as M (NFData, deepseq, force)
import           Control.Monad                     as M (void, when)
import           Control.Monad.Base                as M
import           Control.Monad.Except              as M
import           Control.Monad.Fail                as M (MonadFail)
import           Control.Monad.Identity            as M
import           Control.Monad.Reader              as M
import           Control.Monad.State.Strict        as M
import           Control.Monad.Writer.Strict       as M (MonadWriter (..), WriterT (..),
                                                         execWriterT, runWriterT)
import           Data.Align                        as M (Align (align, alignWith))
import           Data.Align.Key                    as M (AlignWithKey (..))
import           Data.Bool                         as M (bool)
import           Data.Data                         as M (Data (..))
import           Data.Either                       as M (lefts, partitionEithers, rights)
import           Data.Foldable                     as M (asum, foldrM, for_, toList, traverse_)
import           Data.Function                     as M (on, (&))
import           Data.Functor                      as M (($>), (<&>))
import           Data.Hashable                     as M (Hashable)
import           Data.HashMap.Strict               as M (HashMap)
import           Data.HashSet                      as M (HashSet)
import           Data.List                         as M (find, findIndex, foldl', group,
                                                         intercalate, intersect, lookup, sort,
                                                         sortBy, sortOn, union, unionBy, (\\))
import           Data.List.NonEmpty                as M (NonEmpty (..))
import           Data.Maybe                        as M (catMaybes, fromMaybe, isJust, isNothing,
                                                         listToMaybe, mapMaybe, maybeToList)
import           Data.Monoid                       as M (getAlt)
import           Data.Ord                          as M (comparing)
import           Data.Semigroup                    as M (Semigroup (..))
import           Data.Sequence                     as M (Seq)
import           Data.String                       as M (IsString)
import           Data.Text                         as M (Text)
import           Data.These                        as M (These (..), fromThese, mergeThese,
                                                         mergeTheseWith, these)
import           Data.Time.Clock.Units
import           Data.Traversable                  as M (for)
import           Data.Word                         as M (Word64)
import           GHC.Generics                      as M (Generic)
import           Prelude                           as M hiding (fail, init, lookup)
import           Test.QuickCheck.Arbitrary.Generic as M
import           Text.Read                         as M (readEither, readMaybe)

import qualified Data.ByteString                   as B
import qualified Data.HashMap.Strict               as Map
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as TE
import qualified Data.Text.Encoding.Error          as TE
import qualified GHC.Clock                         as Clock
import qualified Test.QuickCheck                   as QC

alphabet :: String
alphabet = ['a'..'z'] ++ ['A'..'Z']

alphaNumerics :: String
alphaNumerics = alphabet ++ "0123456789"

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

afold :: (Foldable t, Alternative f) => t a -> f a
afold = getAlt . foldMap pure

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

-- TODO: Move to Data.HashMap.Strict.Extended; rename to fromListWith?
mapFromL :: (Eq k, Hashable k) => (a -> k) -> [a] -> Map.HashMap k a
mapFromL f = Map.fromList . map (\v -> (f v, v))

-- | Time an IO action, returning the time with microsecond precision. The
-- result of the input action will be evaluated to WHNF.
--
-- The result 'DiffTime' is guarenteed to be >= 0.
withElapsedTime :: MonadIO m=> m a -> m (DiffTime, a)
withElapsedTime ma = do
  bef <- liftIO Clock.getMonotonicTimeNSec
  !a <- ma
  aft <- liftIO Clock.getMonotonicTimeNSec
  let !dur = nanoseconds $ fromIntegral (aft - bef)
  return (dur, a)

-- | Start timing and return an action to return the elapsed time since 'startTimer' was called.
--
-- @
--   timer <- startTimer
--   someStuffToTime
--   elapsed <- timer
--   moreStuff
--   elapsedBoth <- timer
-- @
startTimer :: (MonadIO m, MonadIO n)=> m (n DiffTime)
startTimer = do
  !bef <- liftIO Clock.getMonotonicTimeNSec
  return $ do
    aft <- liftIO Clock.getMonotonicTimeNSec
    return $ nanoseconds $ fromIntegral (aft - bef)
