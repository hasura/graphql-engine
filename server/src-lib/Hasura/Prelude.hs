{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Hasura.Prelude
  ( module M,
    alphabet,
    alphaNumerics,
    onNothing,
    onJust,
    withJust,
    onLeft,
    mapLeft,
    whenMaybe,
    choice,
    afold,
    bsToTxt,
    lbsToTxt,
    txtToBs,
    base64Decode,
    spanMaybeM,
    liftEitherM,
    hoistMaybe,
    hoistEither,
    readJson,
    tshow,

    -- * Trace debugging
    ltrace,
    ltraceM,

    -- * Efficient coercions
    coerce,
    findWithIndex,

    -- * Map-related utilities
    mapFromL,
    mapKeys,
    oMapFromL,

    -- * Measuring and working with moments and durations
    withElapsedTime,
    startTimer,

    -- * Aeson options
    hasuraJSON,

    -- * Extensions to @Data.Foldable@
    fold',
    module Data.Time.Clock.Units,
  )
where

import Control.Applicative as M (Alternative (..), liftA2)
import Control.Arrow as M (first, second, (&&&), (***), (<<<), (>>>))
import Control.DeepSeq as M (NFData, deepseq, force)
import Control.Lens as M (ix, (%~))
import Control.Monad.Base as M
import Control.Monad.Except as M
import Control.Monad.Identity as M
import Control.Monad.Reader as M
import Control.Monad.State.Strict as M
import Control.Monad.Trans.Maybe as M (MaybeT (..))
import Control.Monad.Writer.Strict as M
  ( MonadWriter (..),
    WriterT (..),
    execWriterT,
    runWriterT,
  )
import Data.Aeson qualified as J
import Data.Aeson.Casing qualified as J
import Data.Bool as M (bool)
import Data.ByteString qualified as B
import Data.ByteString.Base64.Lazy qualified as Base64
import Data.ByteString.Lazy qualified as BL
import Data.Coerce
import Data.Data as M (Data (..))
import Data.Either as M (lefts, partitionEithers, rights)
import Data.Foldable as M
  ( asum,
    fold,
    foldMap',
    foldlM,
    foldrM,
    for_,
    toList,
    traverse_,
  )
import Data.Function as M (on, (&))
import Data.Functor as M (($>), (<&>))
import Data.Functor.Const as M (Const)
import Data.HashMap.Strict as M (HashMap)
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.InsOrd as M (InsOrdHashMap)
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.HashSet as M (HashSet)
import Data.Hashable as M (Hashable)
import Data.Hashable qualified as H
import Data.List as M
  ( find,
    findIndex,
    foldl',
    group,
    intercalate,
    intersect,
    lookup,
    sort,
    sortBy,
    sortOn,
    union,
    unionBy,
    (\\),
  )
import Data.List.NonEmpty as M (NonEmpty (..), nonEmpty)
import Data.Maybe as M
  ( catMaybes,
    fromMaybe,
    isJust,
    isNothing,
    listToMaybe,
    mapMaybe,
    maybeToList,
  )
import Data.Monoid as M (getAlt)
import Data.Ord as M (comparing)
import Data.Semigroup as M (Semigroup (..))
import Data.Sequence as M (Seq)
import Data.Sequence.NonEmpty as M (NESeq)
import Data.String as M (IsString)
import Data.Text as M (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Time.Clock.Units
import Data.Traversable as M (for)
import Data.Void as M (Void, absurd)
import Data.Word as M (Word64)
import Debug.Trace qualified as Debug (trace, traceM)
import GHC.Clock qualified as Clock
import GHC.Generics as M (Generic)
import Text.Pretty.Simple qualified as PS
import Text.Read as M (readEither, readMaybe)
import Prelude as M hiding (fail, init, lookup)

alphabet :: String
alphabet = ['a' .. 'z'] ++ ['A' .. 'Z']

alphaNumerics :: String
alphaNumerics = alphabet ++ "0123456789"

onNothing :: Applicative m => Maybe a -> m a -> m a
onNothing m act = maybe act pure m

onJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
onJust m action = maybe (pure ()) action m

withJust :: Applicative m => Maybe a -> (a -> m (Maybe b)) -> m (Maybe b)
withJust m action = maybe (pure Nothing) action m

onLeft :: Applicative m => Either e a -> (e -> m a) -> m a
onLeft e f = either f pure e

mapLeft :: (e1 -> e2) -> Either e1 a -> Either e2 a
mapLeft f (Left e1) = Left (f e1)
mapLeft _ (Right a) = Right a

whenMaybe :: Applicative m => Bool -> m a -> m (Maybe a)
whenMaybe True = fmap Just
whenMaybe False = const $ pure Nothing

choice :: Alternative f => [f a] -> f a
choice = asum

afold :: (Foldable t, Alternative f) => t a -> f a
afold = getAlt . foldMap pure

bsToTxt :: B.ByteString -> Text
bsToTxt = TE.decodeUtf8With TE.lenientDecode

lbsToTxt :: BL.ByteString -> Text
lbsToTxt = bsToTxt . BL.toStrict

txtToBs :: Text -> B.ByteString
txtToBs = TE.encodeUtf8

base64Decode :: Text -> BL.ByteString
base64Decode =
  Base64.decodeLenient . BL.fromStrict . txtToBs

-- Like `liftEither`, but accepts a monadic action
liftEitherM :: MonadError e m => m (Either e a) -> m a
liftEitherM action = action >>= liftEither

-- Like 'span', but monadic and with a function that produces 'Maybe' instead of 'Bool'
spanMaybeM ::
  (Foldable f, Monad m) =>
  (a -> m (Maybe b)) ->
  f a ->
  m ([b], [a])
spanMaybeM f = go . toList
  where
    go [] = pure ([], [])
    go l@(x : xs) =
      f x >>= \case
        Just y -> first (y :) <$> go xs
        Nothing -> pure ([], l)

findWithIndex :: (a -> Bool) -> [a] -> Maybe (a, Int)
findWithIndex p l = do
  v <- find p l
  i <- findIndex p l
  pure (v, i)

-- TODO (from main): Move to Data.HashMap.Strict.Extended; rename to fromListWith?
mapFromL :: (Eq k, Hashable k) => (a -> k) -> [a] -> Map.HashMap k a
mapFromL f = Map.fromList . map (\v -> (f v, v))

-- | re-key a map. In the case that @f@ is not injective you may end up with a
-- smaller map than what you started with.
--
-- This may be a code smell.
mapKeys :: (Eq k2, Hashable k2) => (k1 -> k2) -> Map.HashMap k1 a -> Map.HashMap k2 a
mapKeys f = Map.fromList . map (first f) . Map.toList

oMapFromL :: (Eq k, Hashable k) => (a -> k) -> [a] -> InsOrdHashMap k a
oMapFromL f = OMap.fromList . map (\v -> (f v, v))

-- | Time an IO action, returning the time with microsecond precision. The
-- result of the input action will be evaluated to WHNF.
--
-- The result 'DiffTime' is guarenteed to be >= 0.
withElapsedTime :: MonadIO m => m a -> m (DiffTime, a)
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
startTimer :: (MonadIO m, MonadIO n) => m (n DiffTime)
startTimer = do
  !bef <- liftIO Clock.getMonotonicTimeNSec
  return $ do
    aft <- liftIO Clock.getMonotonicTimeNSec
    return $ nanoseconds $ fromIntegral (aft - bef)

-- | Upgrade a 'Maybe' to a 'MaybeT'.
--
-- cf. http://hackage.haskell.org/package/errors-2.3.0/docs/src/Control.Error.Util.html#hoistMaybe
hoistMaybe :: Applicative m => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure

-- | Upgrade an 'Either' to an 'ExceptT'.
--
-- cf. http://hackage.haskell.org/package/errors-2.3.0/docs/src/Control.Error.Util.html#hoistEither
hoistEither :: Applicative m => Either e a -> ExceptT e m a
hoistEither = ExceptT . pure

tshow :: Show a => a -> Text
tshow = T.pack . show

readJson :: (J.FromJSON a) => String -> Either String a
readJson = J.eitherDecodeStrict . txtToBs . T.pack

-- | Customized 'J.Options' which apply "snake case" to Generic or Template
-- Haskell JSON derivations.
--
-- For example, a Haskell field @fooBar@ would be de/serialized from/to JSON as
-- @foo_bar@.
hasuraJSON :: J.Options
hasuraJSON = J.aesonPrefix J.snakeCase

instance (Hashable a) => Hashable (Seq a) where
  hashWithSalt i = H.hashWithSalt i . toList

-- | Given a structure with elements whose type is a 'Monoid', combine them via
-- the monoid's @('<>')@ operator.
--
-- This fold is right-associative and strict in the accumulator; it's defined
-- as @foldMap id@, per the documentation in @Data.Foldable@.
fold' :: (Monoid m, Foldable t) => t m -> m
fold' = foldMap' id

-- Fancy trace debugging

-- | Labeled, prettified traceShowId
ltrace :: Show a => String -> a -> a
ltrace lbl x = Debug.trace (lbl <> ": " <> TL.unpack (PS.pShow x)) x
{-# WARNING ltrace "ltrace left in code" #-}

-- | Labeled, prettified traceShowM
ltraceM :: Applicative m => Show a => String -> a -> m ()
ltraceM lbl x = Debug.traceM (lbl <> ": " <> TL.unpack (PS.pShow x))
{-# WARNING ltraceM "ltraceM left in code" #-}
