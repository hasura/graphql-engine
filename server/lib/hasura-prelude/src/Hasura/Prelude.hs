{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Hasura.Prelude
  ( module M,
    HashMap,
    InsOrdHashMap,
    mapKeys,

    -- * Maybe
    catMaybes,
    onNothing,
    onNothingM,
    onJust,
    mapMaybe,
    whenMaybe,
    maybeToEither,
    eitherToMaybe,
    spanMaybeM,
    hoistMaybe,

    -- * Either
    onLeft,
    onLeftM,
    mapLeft,
    liftEitherM,
    hoistEither,

    -- * Alternative
    choice,
    afold,

    -- * Text
    bsToTxt,
    lbsToTxt,
    txtToBs,
    txtToLbs,
    base64Decode,
    tshow,

    -- * Trace debugging
    ltrace,
    ltraceM,
    traceToFile,
    traceToFileM,

    -- * Efficient coercions
    coerce,

    -- * Map-related utilities
    mapFromL,
    oMapFromL,

    -- * Measuring and working with moments and durations
    withElapsedTime,
    startTimer,

    -- * JSON
    hasuraJSON,
    readJson,

    -- * NonEmpty.Seq
    nonEmptySeqToNonEmptyList,

    -- * Misc
    applyWhen,
    findWithIndex,
    alphabet,
    alphaNumerics,

    -- * Extensions to @Data.Foldable@
    module Data.Time.Clock.Units,
  )
where

--------------------------------------------------------------------------------

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
import Data.Bifunctor (first)
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
import Data.HashMap.Strict as HashMap (HashMap, mapKeys)
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashSet as M (HashSet)
import Data.Hashable as M (Hashable)
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
  ( fromMaybe,
    isJust,
    isNothing,
    listToMaybe,
    maybeToList,
  )
import Data.Monoid as M (getAlt)
import Data.Ord as M (comparing)
import Data.Semigroup as M (Semigroup (..))
import Data.Sequence as M (Seq)
import Data.Sequence.NonEmpty as M (NESeq)
import Data.Sequence.NonEmpty qualified as NESeq
import Data.String as M (IsString)
import Data.Text as M (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TLIO
import Data.Time.Clock.Units
import Data.Traversable as M (for)
import Data.Void as M (Void, absurd)
import Data.Word as M (Word64)
import Debug.Trace qualified as Debug (trace, traceM)
import GHC.Clock qualified as Clock
import GHC.Generics as M (Generic)
import System.IO.Unsafe (unsafePerformIO) -- for custom trace functions
import Text.Pretty.Simple qualified as PS
import Text.Read as M (readEither, readMaybe)
import Witherable (catMaybes, mapMaybe)
import Prelude as M hiding (fail, init, lookup)

--------------------------------------------------------------------------------
-- Maybe

-- | Performs default 'Applicative' action if 'Nothing' is
-- given. Otherwise returns content of 'Just' pured to 'Applicative'.
onNothing :: (Applicative m) => Maybe a -> m a -> m a
onNothing m act = maybe act pure m

-- | Monadic version of 'onNothing'.
onNothingM :: (Monad m) => m (Maybe a) -> m a -> m a
onNothingM m act = m >>= (`onNothing` act)

-- | Perform some operation on 'Just', given the field inside the
-- 'Just'. Like most good things in life, this is a specialized 'for_'.
onJust :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
onJust = for_

-- | Like 'when', but return either 'Nothing' if the predicate was 'False',
--   of 'Just' with the result of the computation.
--
-- > whenMaybe True  (print 1) == fmap Just (print 1)
-- > whenMaybe False (print 1) == pure Nothing
whenMaybe :: (Applicative m) => Bool -> m a -> m (Maybe a)
whenMaybe True = fmap Just
whenMaybe False = const $ pure Nothing

-- | Transform a 'Maybe' into an 'Either' given a default value.
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither def Nothing = Left def
maybeToEither _def (Just b) = Right b

-- | Convert an 'Either' to a 'Maybe', forgetting the 'Left' values.
--
-- > eitherToMaybe (Left a) == Nothing
-- > eitherToMaybe (Right b) == Just b
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

-- | Like 'span', but monadic and with a function that produces 'Maybe' instead of 'Bool'.
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
        Just y -> M.first (y :) <$> go xs
        Nothing -> pure ([], l)

-- | Upgrade a 'Maybe' to a 'MaybeT'.
--
-- cf. http://hackage.haskell.org/package/errors-2.3.0/docs/src/Control.Error.Util.html#hoistMaybe
hoistMaybe :: (Applicative m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure

--------------------------------------------------------------------------------
-- Either

-- | Eliminate an 'Either' by puring the 'Right' value and applying an
-- applicative action to the 'Left' value.
onLeft :: (Applicative m) => Either e a -> (e -> m a) -> m a
onLeft e f = either f pure e

-- | Similar to 'onLeft', but accepts a monadic action on its LHS.
onLeftM :: (Monad m) => m (Either e a) -> (e -> m a) -> m a
onLeftM e f = e >>= (`onLeft` f)

-- | Map over the 'Left' value of an 'Either'. This is a
-- specialization of 'Data.Bifunctor.first'.
mapLeft :: (e1 -> e2) -> Either e1 a -> Either e2 a
mapLeft = Data.Bifunctor.first

-- | Like 'liftEither', but accepts a monadic action.
liftEitherM :: (MonadError e m) => m (Either e a) -> m a
liftEitherM action = action >>= liftEither

-- | Upgrade an 'Either' to an 'ExceptT'.
--
-- cf. http://hackage.haskell.org/package/errors-2.3.0/docs/src/Control.Error.Util.html#hoistEither
hoistEither :: (Applicative m) => Either e a -> ExceptT e m a
hoistEither = ExceptT . pure

--------------------------------------------------------------------------------
-- Alternative

-- | 'choice' ps tries to apply the actions in the list ps in order,
-- until one of them succeeds. Returns the value of the succeeding
-- action.
choice :: (Alternative f) => [f a] -> f a
choice = asum

-- | Nondeterministically choose an element from a Foldable collection.
afold :: (Foldable t, Alternative f) => t a -> f a
afold = getAlt . foldMap pure

--------------------------------------------------------------------------------
-- Text

-- | Convert a 'B.ByteString' to 'Text' using 'TE.lenientDecode'.
bsToTxt :: B.ByteString -> Text
bsToTxt = TE.decodeUtf8With TE.lenientDecode

-- | Convert a 'BL.ByteString' to 'Text' using 'TE.lenientDecode'.
lbsToTxt :: BL.ByteString -> Text
lbsToTxt = bsToTxt . BL.toStrict

-- | UTF8 encode a 'Text' to 'B.ByteString'.
txtToBs :: Text -> B.ByteString
txtToBs = TE.encodeUtf8

-- | UTF8 encode a 'Text' to 'BL.ByteString'.
txtToLbs :: Text -> BL.ByteString
txtToLbs = BL.fromStrict . TE.encodeUtf8

-- | Base64 decode a 'Text' to 'B.ByteString'.
base64Decode :: Text -> BL.ByteString
base64Decode =
  Base64.decodeLenient . BL.fromStrict . txtToBs

-- | Given 'Show' @a@, convert @a@ into a 'Text'.
tshow :: (Show a) => a -> Text
tshow = T.pack . show

--------------------------------------------------------------------------------
-- Trace debugging

-- | Labeled, prettified 'traceShowId'.
ltrace :: (Show a) => String -> a -> a
ltrace lbl x = Debug.trace (lbl <> ": " <> TL.unpack (PS.pShow x)) x

{- HLINT ignore ltrace -}

-- | Labeled, prettified 'traceShowM'.
ltraceM :: (Applicative m) => (Show a) => String -> a -> m ()
ltraceM lbl x = Debug.traceM (lbl <> ": " <> TL.unpack (PS.pShow x))

{- HLINT ignore ltraceM -}

-- | Trace a prettified value to a file.
traceToFile :: (Show a) => FilePath -> a -> a
traceToFile filepath x =
  Debug.trace
    ("tracing to " <> filepath)
    (unsafePerformIO (TLIO.writeFile filepath (PS.pShowNoColor x) $> x))

{- HLINT ignore traceToFile -}

-- | Trace a prettified value to a file in an Applicative context.
traceToFileM :: (Applicative m) => (Show a) => FilePath -> a -> m ()
traceToFileM filepath x =
  Debug.traceM $
    unwords
      [ "tracing to",
        filepath,
        show $ unsafePerformIO $ TLIO.writeFile filepath $ PS.pShowNoColor x
      ]

{- HLINT ignore traceToFileM -}

--------------------------------------------------------------------------------
-- Map-related utilities

-- | Construct a 'HashMap.HashMap' from a '[]' given a key builder
-- function @a -> k@.
--
-- TODO (from main): Move to 'Data.HashMap.Strict.Extended'; rename to
-- fromListWith?
mapFromL :: (Hashable k) => (a -> k) -> [a] -> HashMap.HashMap k a
mapFromL f = HashMap.fromList . map (\v -> (f v, v))

-- | Construct an 'InsOrdHashMap' from a '[]' given a key builder
-- function @a -> k@.
oMapFromL :: (Hashable k) => (a -> k) -> [a] -> InsOrdHashMap k a
oMapFromL f = InsOrdHashMap.fromList . map (\v -> (f v, v))

--------------------------------------------------------------------------------
-- Measuring and working with moments and durations

-- | Time an IO action, returning the time with microsecond precision. The
-- result of the input action will be evaluated to WHNF.
--
-- The result 'DiffTime' is guaranteed to be >= 0.
withElapsedTime :: (MonadIO m) => m a -> m (DiffTime, a)
withElapsedTime ma = do
  stopTimer <- startTimer
  !a <- ma
  dur <- stopTimer
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

--------------------------------------------------------------------------------
-- JSON

-- | Deserialize JSON from a 'String'.
readJson :: (J.FromJSON a) => String -> Either String a
readJson = J.eitherDecodeStrict . txtToBs . T.pack

-- | Customized 'J.Options' which apply "snake case" to Generic or Template
-- Haskell JSON derivations.
--
-- For example, a Haskell field @fooBar@ would be de/serialized from/to JSON as
-- @foo_bar@.
hasuraJSON :: J.Options
hasuraJSON = J.aesonPrefix J.snakeCase

--------------------------------------------------------------------------------
-- NonEmpty.Seq

-- | Convert a non-empty sequence to a non-empty list.
nonEmptySeqToNonEmptyList :: NESeq a -> NonEmpty a
nonEmptySeqToNonEmptyList (x NESeq.:<|| xs) =
  x M.:| toList xs

--------------------------------------------------------------------------------
-- Misc

-- | Conditionally apply a datatransformation. This is especially
-- helpful for conditionally applying lenses.
applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f x = f x
applyWhen False _ x = x

-- | Find an element satisfying a predicate and return it with its index.
findWithIndex :: (a -> Bool) -> [a] -> Maybe (a, Int)
findWithIndex p l = do
  i <- findIndex p l
  let v = l !! i
  pure (v, i)

-- | A list containing the English alphabet in lower and upper case.
--
-- Don't inline, to avoid the risk of unreasonably long code being generated
{-# NOINLINE alphabet #-}
alphabet :: String
alphabet = ['a' .. 'z'] ++ ['A' .. 'Z']

-- | 'alphabet' with the digits 0-9 appended.
--
-- Don't inline, to avoid the risk of unreasonably long code being generated
{-# NOINLINE alphaNumerics #-}
alphaNumerics :: String
alphaNumerics = alphabet ++ "0123456789"
