{-# OPTIONS_GHC -Wno-inline-rule-shadowing -Wno-orphans #-} -- see Note [Arrow rewrite rules]

{-# LANGUAGE Arrows               #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

-- | The missing standard library for arrows. Some of the functionality in this module is similar to
-- Paterson’s original @arrows@ library, but it has been modernized to work with recent versions of
-- GHC.
module Control.Arrow.Extended
  ( module Control.Arrow
  , (>->)
  , (<-<)

  , foldlA'
  , traverseA
  , onNothingA

  , ArrowTrans(..)

  , ArrowKleisli(..)
  , bindA

  , ArrowError(..)
  , mapErrorA
  , ErrorA(..)

  , ArrowReader(..)
  , ReaderA(..)

  , ArrowWriter(..)
  , WriterA(WriterA, runWriterA)
  ) where

import           Prelude                    hiding (id, (.))

import           Control.Arrow
import           Control.Category
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.Reader.Class
import           Control.Monad.Writer.Class
import           Data.Foldable
import           Data.Tuple                 (swap)

infixl 1 >->
infixr 1 <-<

-- | The analog to '>>=' for arrow commands. In @proc@ notation, '>->' can be used to chain the
-- output of one command into the input of another.
--
-- See also Note [Weird control operator types].
(>->) :: (Arrow arr) => arr (e, s) a -> arr (e, (a, s)) b -> arr (e, s) b
f >-> g = proc (e, s) -> do
  x <- f -< (e, s)
  g -< (e, (x, s))
{-# INLINE (>->) #-}

(<-<) :: (Arrow arr) => arr (e, (a, s)) b -> arr (e, s) a -> arr (e, s) b
(<-<) = flip (>->)
{-# INLINE (<-<) #-}

-- | 'foldl'' lifted to arrows. See also Note [Weird control operator types].
foldlA' :: (ArrowChoice arr, Foldable t) => arr (e, (b, (a, s))) b -> arr (e, (b, (t a, s))) b
foldlA' f = arr (\(e, (v, (xs, s))) -> (e, (v, (toList xs, s)))) >>> go where
  go = uncons >>> (id ||| step)
  uncons = arr \(e, (v, (xs, s))) -> case xs of
    []    -> Left v
    x:xs' -> Right ((e, (v, (x, s))), (e, (xs', s)))
  step = first f >>> arr (\(!v, (e, (xs, s))) -> (e, (v, (xs, s)))) >>> go

-- | An indexed version of Twan van Laarhoven’s @FunList@ type (see
-- <https://twanvl.nl/blog/haskell/non-regular1>). A value of type @'Traversal' a b (t b)@ is a
-- concrete representation of a traversal applied to a data structure of type @t a@ and producing a
-- value of type @t b@. This explicit representation is used to implement 'traverseA' using only
-- 'ArrowChoice'.
data Traversal a r b
  = Done b
  | Yield a !(r -> Traversal a r b)

instance Functor (Traversal a r) where
  fmap f = \case
    Done x -> Done (f x)
    Yield v k -> Yield v (fmap f . k)

instance Applicative (Traversal a r) where
  pure = Done
  tf <*> tx = case tf of
    Done f    -> fmap f tx
    Yield v k -> Yield v ((<*> tx) . k)

traversal :: (Traversable t) => t a -> Traversal a b (t b)
traversal = traverse (flip Yield Done)

-- | 'traverse' lifted to arrows. See also Note [Weird control operator types].
traverseA :: (ArrowChoice arr, Traversable t) => arr (e, (a, s)) b -> arr (e, (t a, s)) (t b)
traverseA f = second (first $ arr traversal) >>> go where
  go = proc (e, (as, s)) -> case as of
    Done bs -> returnA -< bs
    Yield a k -> do
      b <- f -< (e, (a, s))
      go -< (e, (k b, s))

onNothingA :: (ArrowChoice arr) => arr (e, s) a -> arr (e, (Maybe a, s)) a
onNothingA f = proc (e, (v, s)) -> case v of
  Just a  -> returnA -< a
  Nothing -> f -< (e, s)

{-# RULES -- These rules are missing from Control.Arrow; see Note [Arrow rewrite rules]
"arr/arr/R"   forall f g h. arr f . (arr g . h) = arr (f . g) . h

"first/push"  [~1] forall f g.  first (f . g) =  first f .  first g
"second/push" [~1] forall f g. second (f . g) = second f . second g
"left/push"   [~1] forall f g.   left (f . g) =   left f .   left g
"right/push"  [~1] forall f g.  right (f . g) =  right f .  right g

"first/pull"   [1] forall f g.  first f .  first g =  first (f . g)
"second/pull"  [1] forall f g. second f . second g = second (f . g)
"left/pull"    [1] forall f g.   left f .   left g =   left (f . g)
"right/pull"   [1] forall f g.  right f .  right g =  right (f . g)
#-}

-- | The class of /Kleisli arrows/, arrows made from monadic functions. Instances should satisfy
-- the following laws:
--
--   * @'arrM' ('pure' '.' /f/)@ ≡ @'arr' /f/@
--   * @('arrM' /f/ '>>>' 'arrM' /g/)@ ≡ @'arrM' (/f/ '>=>' /g/)@.
class (Monad m, Arrow arr) => ArrowKleisli m arr | arr -> m where
  arrM :: (a -> m b) -> arr a b

{-# RULES -- see Note [Arrow rewrite rules]
"arr/arrM"    forall f g. arr  f . arrM g = arrM (fmap f  .  g)
"arrM/arr"    forall f g. arrM f . arr  g = arrM      (f  .  g)
"arrM/arrM"   forall f g. arrM f . arrM g = arrM      (f <=< g)

"arr/arrM/R"  forall f g h. arr  f . (arrM g . h) = arrM (fmap f  .  g) . h
"arrM/arr/R"  forall f g h. arrM f . (arr  g . h) = arrM      (f  .  g) . h
"arrM/arrM/R" forall f g h. arrM f . (arrM g . h) = arrM      (f <=< g) . h

"first/arrM"  forall f.  first (arrM f) = arrM (runKleisli  (first (Kleisli f)))
"second/arrM" forall f. second (arrM f) = arrM (runKleisli (second (Kleisli f)))
"left/arrM"   forall f.   left (arrM f) = arrM (runKleisli   (left (Kleisli f)))
"right/arrM"  forall f.  right (arrM f) = arrM (runKleisli  (right (Kleisli f)))

"***/arrM"    forall f g. arrM f *** arrM g = arrM (runKleisli (Kleisli f *** Kleisli g))
"&&&/arrM"    forall f g. arrM f &&& arrM g = arrM (runKleisli (Kleisli f &&& Kleisli g))
"+++/arrM"    forall f g. arrM f +++ arrM g = arrM (runKleisli (Kleisli f +++ Kleisli g))
"|||/arrM"    forall f g. arrM f ||| arrM g = arrM (runKleisli (Kleisli f ||| Kleisli g))
#-}

-- | A combinator that serves a similar role to 'returnA' in arrow notation, except that the
-- argument is a monadic action instead of a pure value. Just as 'returnA' is actually just
-- @'arr' 'id'@, 'ruleA' is just @'arrM' 'id'@, but it is provided as a separate function for
-- clarity.
--
-- 'bindA' is useful primarily because it allows executing a monadic action using arrow inputs
-- currently in scope. For example:
--
-- @
-- proc (a, b) -> do
--   x <- foo -< a
--   y <- bar -< b
--   'bindA' -< f x y
-- @
--
-- The last statement is equivalent to @'arrM' ('uncurry' f) -< (x, y)@, but the use of 'bindA'
-- allows it to be expressed more directly.
bindA :: (ArrowKleisli m arr) => arr (m a) a
bindA = arrM id
{-# INLINE bindA #-}

instance (Monad m) => ArrowKleisli m (Kleisli m) where
  arrM = Kleisli

class (Arrow arr, Arrow (t arr)) => ArrowTrans t arr where
  liftA :: arr a b -> t arr a b

class (Arrow arr) => ArrowError e arr | arr -> e where
  throwA :: arr e a
  -- see Note [Weird control operator types]
  catchA :: arr (a, s) b -> arr (a, (e, s)) b -> arr (a, s) b

mapErrorA :: (ArrowError e arr) => arr (a, s) b -> arr (a, ((e -> e), s)) b
mapErrorA f = proc (a, (g, s)) -> (f -< (a, s)) `catchA` \e -> throwA -< g e

class (Arrow arr) => ArrowReader r arr | arr -> r where
  askA :: arr a r
  -- see Note [Weird control operator types]
  localA :: arr (a, s) b -> arr (a, (r, s)) b

class (Monoid w, Arrow arr) => ArrowWriter w arr | arr -> w where
  tellA :: arr w ()
  listenA :: arr a b -> arr a (b, w)

instance (MonadError e m) => ArrowError e (Kleisli m) where
  throwA = Kleisli throwError
  catchA (Kleisli f) (Kleisli g) = Kleisli \(a, s) -> f (a, s) `catchError` \e -> g (a, (e, s))

instance (MonadReader r m) => ArrowReader r (Kleisli m) where
  askA = Kleisli $ const ask
  localA (Kleisli f) = Kleisli \(a, (r, s)) -> local (const r) (f (a, s))

instance (MonadWriter w m) => ArrowWriter w (Kleisli m) where
  tellA = Kleisli tell
  listenA (Kleisli f) = Kleisli (listen . f)

newtype ErrorA e arr a b = ErrorA { runErrorA :: arr a (Either e b) }
  deriving (Functor)

instance (ArrowChoice arr) => Category (ErrorA e arr) where
  id = ErrorA (arr Right)
  ErrorA f . ErrorA g = ErrorA ((arr Left ||| f) . g)

instance (ArrowChoice arr) => Arrow (ErrorA e arr) where
  arr f = ErrorA (arr (Right . f))
  first (ErrorA f) = ErrorA (arr (fmap swap . sequence . swap) . first f)

instance (ArrowChoice arr) => ArrowChoice (ErrorA e arr) where
  left (ErrorA f) = ErrorA (arr (either (either Left (Right . Left)) (Right . Right)) . left f)

instance (ArrowChoice arr, ArrowApply arr) => ArrowApply (ErrorA e arr) where
  app = ErrorA (app . first (arr runErrorA))

instance (ArrowChoice arr) => ArrowTrans (ErrorA e) arr where
  liftA f = ErrorA (arr Right . f)

instance (ArrowChoice arr) => ArrowError e (ErrorA e arr) where
  throwA = ErrorA (arr Left)
  catchA (ErrorA f) (ErrorA g) = ErrorA proc (a, s) -> do
    r <- f -< (a, s)
    case r of
      Left  e -> g -< (a, (e, s))
      Right v -> returnA -< Right v

instance (ArrowKleisli m arr, ArrowChoice arr) => ArrowKleisli m (ErrorA e arr) where
  arrM = liftA . arrM
instance (ArrowReader r arr, ArrowChoice arr) => ArrowReader r (ErrorA e arr) where
  askA = liftA askA
  localA (ErrorA f) = ErrorA (localA f)
instance (ArrowWriter w arr, ArrowChoice arr) => ArrowWriter w (ErrorA e arr) where
  tellA = liftA tellA
  listenA (ErrorA f) = ErrorA (arr (\(r, w) -> (, w) <$> r) . listenA f)

newtype ReaderA r arr a b = ReaderA { runReaderA :: arr (a, r) b }

instance (Arrow arr) => Category (ReaderA r arr) where
  id = ReaderA (arr fst)
  ReaderA f . ReaderA g = ReaderA proc (a, r) -> do
    b <- g -< (a, r)
    f -< (b, r)

instance (Arrow arr) => Arrow (ReaderA r arr) where
  arr f = ReaderA (arr (f . fst))
  first (ReaderA f) = ReaderA proc ((a, c), r) -> do
    b <- f -< (a, r)
    returnA -< (b, c)

instance (ArrowChoice arr) => ArrowChoice (ReaderA r arr) where
  left (ReaderA f) = ReaderA proc (e, r) -> case e of
    Left  a -> arr Left . f -< (a, r)
    Right b -> returnA -< Right b

instance (ArrowApply arr) => ArrowApply (ReaderA r arr) where
  app = ReaderA (app . arr \((ReaderA f, x), r) -> (f, (x, r)))

instance (Arrow arr) => ArrowTrans (ReaderA r) arr where
  liftA f = ReaderA (f . arr fst)

instance (Arrow arr) => ArrowReader r (ReaderA r arr) where
  askA = ReaderA (arr snd)
  localA (ReaderA f) = ReaderA proc ((a, (r, s)), _) -> f -< ((a, s), r)

instance (ArrowKleisli m arr) => ArrowKleisli m (ReaderA r arr) where
  arrM = liftA . arrM
instance (ArrowError e arr) => ArrowError e (ReaderA r arr) where
  throwA = liftA throwA
  catchA (ReaderA f) (ReaderA g) = ReaderA proc ((a, s), r) ->
    (f -< ((a, s), r)) `catchA` \e -> g -< ((a, (e, s)), r)
instance (ArrowWriter w arr) => ArrowWriter w (ReaderA r arr) where
  tellA = liftA tellA
  listenA (ReaderA f) = ReaderA (listenA f)

newtype WriterA w arr a b
  -- Internally defined using state passing to avoid space leaks. The real constructor should be
  -- left unexported to avoid misuse.
  = MkWriterA (arr (a, w) (b, w))

pattern WriterA :: (Monoid w, Arrow arr) => arr a (b, w) -> WriterA w arr a b
pattern WriterA { runWriterA } <- MkWriterA ((\f -> f . arr (, mempty)) -> runWriterA)
  where
    WriterA f = MkWriterA (arr (\((b, w), w1) -> let !w2 = w1 <> w in (b, w2)) . first f)
{-# COMPLETE WriterA #-}

instance (Category arr) => Category (WriterA w arr) where
  id = MkWriterA id
  MkWriterA f . MkWriterA g = MkWriterA (f . g)

instance (Arrow arr) => Arrow (WriterA w arr) where
  arr f = MkWriterA (arr $ first f)
  first (MkWriterA f) = MkWriterA proc ((a1, b), w1) -> do
    (a2, w2) <- f -< (a1, w1)
    returnA -< ((a2, b), w2)

instance (ArrowChoice arr) => ArrowChoice (WriterA w arr) where
  left (MkWriterA f) = MkWriterA proc (e, w) -> case e of
    Left  a -> arr (first Left) . f -< (a, w)
    Right b -> returnA -< (Right b, w)

instance (ArrowApply arr) => ArrowApply (WriterA w arr) where
  app = MkWriterA (app . arr \((MkWriterA f, x), w) -> (f, (x, w)))

instance (Arrow arr) => ArrowTrans (WriterA w) arr where
  liftA = MkWriterA . first

instance (Monoid w, Arrow arr) => ArrowWriter w (WriterA w arr) where
  tellA = MkWriterA $ arr \(w, w1) -> let !w2 = w1 <> w in ((), w2)
  listenA (WriterA f) = WriterA (arr (\(a, w) -> ((a, w), w)) . f)

instance (ArrowKleisli m arr) => ArrowKleisli m (WriterA w arr) where
  arrM = liftA . arrM
instance (ArrowError e arr) => ArrowError e (WriterA w arr) where
  throwA = liftA throwA
  catchA (MkWriterA f) (MkWriterA g) = MkWriterA proc ((a, s), w) ->
    (f -< ((a, s), w)) `catchA` \e -> g -< ((a, (e, s)), w)
instance (ArrowReader r arr) => ArrowReader r (WriterA w arr) where
  askA = liftA askA
  localA (MkWriterA f) = MkWriterA proc ((a, (r, s)), w) -> (| localA (f -< ((a, s), w)) |) r

{- Note [Weird control operator types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Arrow notation (i.e. `proc`) has support for so-called “custom control operators,” which allow
things like

    proc (x, y) -> do
      z <- foo -< x
      (f -< z) `catchA` \e -> g -< (y, e)

to magically work. What’s so magical about that? Well, note that `catchA` is an ordinary function,
but it’s being given /commands/ as arguments, not expressions. Also note that the arguments to
`catchA` reference the variables `y` and `z`, which are bound earlier in the `proc` expression as
arrow-local variables.

To make this work, GHC has to thread `y` and `z` through `catchA` in the generated code, which will
end up being something like this:

        arr (\(x, y) -> (x, (x, y)))
    >>> first foo
    >>> arr (\(z, (x, y)) -> (z, y))
    >>> catchA (first f)
               (arr (\((_, y), e) -> (y, e)) >>> g)

Quite complicated, which is why we’re glad we don’t have to write it all out ourselves!
Unfortunately, since GHC 7.8, GHC has required some pretty stupid-looking types for control
operators to allow them to be used in `proc` notation. The natural type for `catchA` is

    catchA :: arr a b -> arr (a, e) b -> arr a b

but GHC requires the far uglier

    catchA :: arr (a, s) b -> arr (a, (e, s)) b -> arr (a, s) b

in order to make the type inference work out. I (Alexis) have submitted a GHC proposal to fix this
<https://github.com/ghc-proposals/ghc-proposals/pull/303>, so hopefully we’ll be able to use the
nicer type in the future (GHC 8.12 at the earliest). For now, though, we’ll have to use the ugly
version.

As of GHC 8.10, the way to read arrow control operator types is to look for arguments with a shape
like this:

    arr (e, (a1, (a2, ... (an, s)))) b

The “actual” arguments to the arrow are the `a1` through `an` types, and the `e` and `s` types are
sort of “bookends.” So if you see a type like

    arr (e, (Integer, (Char, (Bool, s)))) String

then you should read it as an arrow that takes three “arguments” of type `Integer`, `Char`, and
`Bool` and returns a `String`.

Stopping there is basically good enough, but if you want to know what’s really going on, the idea is
that each command in a `proc` block has an “environment” and an “argument stack,” represented by the
types `e` and `s`, respectively. The environment is used to thread arrow-local variables that are
currently in scope, and the argument stack (as the name implies) is used to pass the command
arguments. Control operators can push and pop things from this argument stack, and in the base case,
the empty argument stack is represented by `()`. For a full explanation, see the section of the GHC
User’s Guide on arrow notation:

    https://downloads.haskell.org/ghc/8.8.1/docs/html/users_guide/glasgow_exts.html#arrow-notation

Yes, this all kind of sucks. Sorry.


Note [Arrow rewrite rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC’s desugaring of `proc` notation is not very clever, so it can generate some pretty inefficient
code. Almost everything is translated into uses of `arr`, `first`, and `(|||)`, and arrows end up
threading around massive tuples that constantly need to be packed and unpacked. To get good
performance, GHC relies on rewrite rules that expose optimizations to the simplifier, allowing the
packing and unpacking to be significantly reduced.

The most crucial rewrite rules are the ones for “`arr` fusion”, which rewrite expressions like
`arr f . arr g` into `arr (f . g)`. It might not be obvious at first why this is so important, but
remember that the arguments to `arr` are plain functions, not arrows. These functions might be
something like:

    f (a, (b, c)) = ((a, b), c)
    g ((a, _), c) = (a, c)

The composition of these functions `f . g` can be optimized to

    f . g = \(a, (_, c)) -> (a, c)

skipping the intermediate tuple completely, but GHC can only do that if the two functions are
composed directly. If GHC only sees `arr f . arr g`, then it can’t assume anything about `arr`
(which might be overloaded), so it gets stuck.

The rewrite rules defined in Control.Category, Control.Arrow, and this module take advantage of
certain typeclass laws to enable many more optimizations to fire. However, there is a caveat to all
this: when GHC knows the concrete type of a particular arrow, it aggressively specializes uses of
`arr` and other operations to the concrete type. This process bypasses the rewrite rules completely.

GHC tries to warn us about this with the `-Winline-rule-shadowing` warning, but in this case, we
want the rules anyway, since they might fire on polymorphic code. However, the takeaway is that the
generic rules are not sufficient to get fast code: it’s important to /also/ define type-specific
rules in the event that GHC specializes concrete code. The good news is that those type-specific
rules can take advantage of type-specific optimizations, getting even better performance than would
be possible using the generic rules. The bad news is it’s a bit more work. -}
