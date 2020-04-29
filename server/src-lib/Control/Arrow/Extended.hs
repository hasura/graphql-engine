{-# OPTIONS_GHC -Wno-inline-rule-shadowing -Wno-orphans #-} -- see Note [Arrow rewrite rules]

{-# LANGUAGE Arrows               #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The missing standard library for arrows. Some of the functionality in this module is similar to
-- Paterson’s original @arrows@ library, but it has been modernized to work with recent versions of
-- GHC.
module Control.Arrow.Extended
  ( module Control.Arrow
  , module Control.Arrow.Trans

  , (>->)
  , (<-<)
  , dup
  , bothA
  , orA

  , foldlA'
  , traverseA_
  , traverseA
  , onNothingA

  , ArrowKleisli(..)
  , bindA
  ) where

import           Prelude             hiding (id, (.))

import           Control.Arrow
import           Control.Arrow.Trans
import           Control.Category
import           Control.Monad
import           Data.Foldable

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

dup :: (Arrow arr) => arr a (a, a)
dup = arr \x -> (x, x)
{-# INLINE dup #-}

bothA :: (Arrow arr) => arr a b -> arr (a, a) (b, b)
bothA f = f *** f
{-# INLINE bothA #-}

orA :: (ArrowChoice arr) => arr a Bool -> arr b Bool -> arr (a, b) Bool
orA f g = proc (a, b) -> do
  c <- f -< a
  if c then returnA -< True else g -< b
{-# INLINABLE orA #-}
{-# RULES "orA/arr" forall f g. arr f `orA` arr g = arr (f `orA` g) #-}

-- | 'foldl'' lifted to arrows. See also Note [Weird control operator types].
foldlA' :: (ArrowChoice arr, Foldable t) => arr (e, (b, (a, s))) b -> arr (e, (b, (t a, s))) b
foldlA' f = arr (\(e, (v, (xs, s))) -> (e, (v, (toList xs, s)))) >>> go where
  go = uncons >>> (id ||| step)
  uncons = arr \(e, (v, (xs, s))) -> case xs of
    []    -> Left v
    x:xs' -> Right ((e, (v, (x, s))), (e, (xs', s)))
  step = first f >>> arr (\(!v, (e, (xs, s))) -> (e, (v, (xs, s)))) >>> go
{-# INLINABLE foldlA' #-}

traverseA_ :: (ArrowChoice arr, Foldable t) => arr (e, (a, s)) b -> arr (e, (t a, s)) ()
traverseA_ f = proc (e, (xs, s)) ->
  (| foldlA' (\() x -> do { (e, (x, s)) >- f; () >- returnA }) |) () xs
{-# INLINABLE traverseA_ #-}

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
{-# NOINLINE[1] traverseA #-}

-- In the common case of using traverseA on Maybe, the general traverseA generates needlessly
-- complex code due to the combination of recursion and indirection through Traversal. Since Maybe
-- is finite, we can do much better by avoiding the recursion completely.
traverseA_Maybe :: (ArrowChoice arr) => arr (e, (a, s)) b -> arr (e, (Maybe a, s)) (Maybe b)
traverseA_Maybe f = proc (e, (v, s)) -> case v of
  Just a  -> arr Just . f -< (e, (a, s))
  Nothing -> returnA -< Nothing
{-# INLINABLE traverseA_Maybe #-}
{-# RULES "traverseA @Maybe" traverseA = traverseA_Maybe #-}

onNothingA :: (ArrowChoice arr) => arr (e, s) a -> arr (e, (Maybe a, s)) a
onNothingA f = proc (e, (v, s)) -> case v of
  Just a  -> returnA -< a
  Nothing -> f -< (e, s)
{-# INLINABLE onNothingA #-}

-- These rules are missing from Control.Arrow; see Note [Arrow rewrite rules]
{-# RULES
"arr/arr/R" forall f g h. arr f . (arr g . h) = arr (f . g) . h

"&&&/id"    forall f. f &&& id = first  f . dup
"id/&&&"    forall f. id &&& f = second f . dup
"&&&/arr"   forall f g. f &&& arr g = first  f . arr (id &&& g)
"arr/&&&"   forall f g. arr f &&& g = second g . arr (f &&& id)

"|||/id"    forall f. f ||| id = arr (id ||| id) . left f
"id/|||"    forall f. id ||| f = arr (id ||| id) . right f
"|||/arr"   forall f g. f ||| arr g = arr (id ||| g) . left f
"arr/|||"   forall f g. arr f ||| g = arr (f ||| id) . right g
#-}

-- | The class of /Kleisli arrows/, arrows made from monadic functions. Instances should satisfy
-- the following laws:
--
--   * @'arrM' ('pure' '.' /f/)@ ≡ @'arr' /f/@
--   * @('arrM' /f/ '>>>' 'arrM' /g/)@ ≡ @'arrM' (/f/ '>=>' /g/)@.
class (Monad m, Arrow arr) => ArrowKleisli m arr | arr -> m where
  arrM :: (a -> m b) -> arr a b

{-# RULES -- see Note [Arrow rewrite rules]
"arrM/pure"             arrM pure       = id
"arrM/pure/f" forall f. arrM (pure . f) = arr f

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

instance (ArrowKleisli m arr, ArrowChoice arr) => ArrowKleisli m (ErrorA e arr) where
  arrM = liftA . arrM
  {-# INLINE arrM #-}
instance (ArrowKleisli m arr) => ArrowKleisli m (ReaderA r arr) where
  arrM = liftA . arrM
  {-# INLINE arrM #-}
instance (ArrowKleisli m arr) => ArrowKleisli m (WriterA w arr) where
  arrM = liftA . arrM
  {-# INLINE arrM #-}

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
