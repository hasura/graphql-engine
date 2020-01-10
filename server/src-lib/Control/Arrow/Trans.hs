{-# LANGUAGE Arrows               #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Control.Arrow.Trans
  ( ArrowTrans(..)

  , ArrowError(..)
  , liftEitherA
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
import           Control.Monad.Error.Class
import           Control.Monad.Reader.Class
import           Control.Monad.Writer.Class

class (Arrow arr, Arrow (t arr)) => ArrowTrans t arr where
  liftA :: arr a b -> t arr a b

class (Arrow arr) => ArrowError e arr | arr -> e where
  throwA :: arr e a
  -- see Note [Weird control operator types]
  catchA :: arr (a, s) b -> arr (a, (e, s)) b -> arr (a, s) b

liftEitherA :: (ArrowChoice arr, ArrowError e arr) => arr (Either e a) a
liftEitherA = throwA ||| returnA
{-# INLINE liftEitherA #-}

mapErrorA :: (ArrowError e arr) => arr (a, s) b -> arr (a, ((e -> e), s)) b
mapErrorA f = proc (a, (g, s)) -> (f -< (a, s)) `catchA` \e -> throwA -< g e
{-# INLINE mapErrorA #-}

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
  {-# INLINE id #-}
  ErrorA f . ErrorA g = ErrorA ((arr Left ||| f) . g)
  {-# INLINABLE (.) #-}

sequenceFirst :: (Functor f) => (f a, b) -> f (a, b)
sequenceFirst (a, b) = (, b) <$> a
{-# INLINABLE sequenceFirst #-}

instance (ArrowChoice arr) => Arrow (ErrorA e arr) where
  arr f = ErrorA (arr (Right . f))
  {-# INLINE arr #-}
  first (ErrorA f) = ErrorA (arr sequenceFirst . first f)
  {-# INLINE first #-}

reassociateEither :: Either (Either a b) c -> Either a (Either b c)
reassociateEither = either (either Left (Right . Left)) (Right . Right)

instance (ArrowChoice arr) => ArrowChoice (ErrorA e arr) where
  left (ErrorA f) = ErrorA (arr reassociateEither . left f)
  {-# INLINE left #-}
  ErrorA f ||| ErrorA g = ErrorA (f ||| g)
  {-# INLINE (|||) #-}

instance (ArrowChoice arr, ArrowApply arr) => ArrowApply (ErrorA e arr) where
  app = ErrorA (app . first (arr runErrorA))
  {-# INLINE app #-}

instance (ArrowChoice arr) => ArrowTrans (ErrorA e) arr where
  liftA f = ErrorA (arr Right . f)
  {-# INLINE liftA #-}

instance (ArrowChoice arr) => ArrowError e (ErrorA e arr) where
  throwA = ErrorA (arr Left)
  {-# INLINE throwA #-}
  catchA (ErrorA f) (ErrorA g) = ErrorA proc (a, s) -> do
    r <- f -< (a, s)
    case r of
      Left  e -> g -< (a, (e, s))
      Right v -> returnA -< Right v
  {-# INLINABLE catchA #-}

instance (ArrowReader r arr, ArrowChoice arr) => ArrowReader r (ErrorA e arr) where
  askA = liftA askA
  {-# INLINE askA #-}
  localA (ErrorA f) = ErrorA (localA f)
  {-# INLINE localA #-}
instance (ArrowWriter w arr, ArrowChoice arr) => ArrowWriter w (ErrorA e arr) where
  tellA = liftA tellA
  {-# INLINE tellA #-}
  listenA (ErrorA f) = ErrorA (arr sequenceFirst . listenA f)
  {-# INLINE listenA #-}

newtype ReaderA r arr a b = ReaderA { runReaderA :: arr (a, r) b }

instance (Arrow arr) => Category (ReaderA r arr) where
  id = ReaderA (arr fst)
  {-# INLINE id #-}
  ReaderA f . ReaderA g = ReaderA proc (a, r) -> do
    b <- g -< (a, r)
    f -< (b, r)
  {-# INLINE (.) #-}

instance (Arrow arr) => Arrow (ReaderA r arr) where
  arr f = ReaderA (arr (f . fst))
  {-# INLINE arr #-}
  first (ReaderA f) = ReaderA proc ((a, c), r) -> do
    b <- f -< (a, r)
    returnA -< (b, c)
  {-# INLINE first #-}

instance (ArrowChoice arr) => ArrowChoice (ReaderA r arr) where
  left (ReaderA f) = ReaderA proc (e, r) -> case e of
    Left  a -> arr Left . f -< (a, r)
    Right b -> returnA -< Right b
  {-# INLINE left #-}
  ReaderA f ||| ReaderA g = ReaderA ((f ||| g) . arr \(e, r) -> ((, r) +++ (, r)) e)
  {-# INLINE (|||) #-}

instance (ArrowApply arr) => ArrowApply (ReaderA r arr) where
  app = ReaderA (app . arr \((ReaderA f, x), r) -> (f, (x, r)))
  {-# INLINE app #-}

instance (Arrow arr) => ArrowTrans (ReaderA r) arr where
  liftA f = ReaderA (f . arr fst)
  {-# INLINE liftA #-}

instance (Arrow arr) => ArrowReader r (ReaderA r arr) where
  askA = ReaderA (arr snd)
  {-# INLINE askA #-}
  localA (ReaderA f) = ReaderA proc ((a, (r, s)), _) -> f -< ((a, s), r)
  {-# INLINE localA #-}

instance (ArrowError e arr) => ArrowError e (ReaderA r arr) where
  throwA = liftA throwA
  {-# INLINE throwA #-}
  catchA (ReaderA f) (ReaderA g) = ReaderA proc ((a, s), r) ->
    (f -< ((a, s), r)) `catchA` \e -> g -< ((a, (e, s)), r)
  {-# INLINE catchA #-}
instance (ArrowWriter w arr) => ArrowWriter w (ReaderA r arr) where
  tellA = liftA tellA
  {-# INLINE tellA #-}
  listenA (ReaderA f) = ReaderA (listenA f)
  {-# INLINE listenA #-}

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
  {-# INLINE id #-}
  MkWriterA f . MkWriterA g = MkWriterA (f . g)
  {-# INLINE (.) #-}

instance (Arrow arr) => Arrow (WriterA w arr) where
  arr f = MkWriterA (arr $ first f)
  {-# INLINE arr #-}
  first (MkWriterA f) = MkWriterA proc ((a1, b), w1) -> do
    (a2, w2) <- f -< (a1, w1)
    returnA -< ((a2, b), w2)
  {-# INLINE first #-}

instance (ArrowChoice arr) => ArrowChoice (WriterA w arr) where
  left (MkWriterA f) = MkWriterA proc (e, w) -> case e of
    Left  a -> arr (first Left) . f -< (a, w)
    Right b -> returnA -< (Right b, w)
  {-# INLINE left #-}
  f ||| g = arr (either id id) . right g . left f
  {-# INLINE (|||) #-}

instance (ArrowApply arr) => ArrowApply (WriterA w arr) where
  app = MkWriterA (app . arr \((MkWriterA f, x), w) -> (f, (x, w)))
  {-# INLINE app #-}

instance (Arrow arr) => ArrowTrans (WriterA w) arr where
  liftA = MkWriterA . first
  {-# INLINE liftA #-}

instance (Monoid w, Arrow arr) => ArrowWriter w (WriterA w arr) where
  tellA = MkWriterA $ arr \(w, w1) -> let !w2 = w1 <> w in ((), w2)
  listenA (WriterA f) = WriterA (arr (\(a, w) -> ((a, w), w)) . f)
  {-# INLINE listenA #-}

instance (ArrowError e arr) => ArrowError e (WriterA w arr) where
  throwA = liftA throwA
  {-# INLINE throwA #-}
  catchA (MkWriterA f) (MkWriterA g) = MkWriterA proc ((a, s), w) ->
    (f -< ((a, s), w)) `catchA` \e -> g -< ((a, (e, s)), w)
  {-# INLINE catchA #-}
instance (ArrowReader r arr) => ArrowReader r (WriterA w arr) where
  askA = liftA askA
  {-# INLINE askA #-}
  localA (MkWriterA f) = MkWriterA proc ((a, (r, s)), w) -> (| localA (f -< ((a, s), w)) |) r
  {-# INLINE localA #-}
