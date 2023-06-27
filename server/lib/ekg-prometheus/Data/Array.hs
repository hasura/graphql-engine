{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Zero based arrays.
--
-- Note that no bounds checking are performed.
module Data.Array
  ( Array,
    length,
    index,
    fromList,
    toList,
  )
where

import Control.Monad.ST
import GHC.Exts
  ( Array#,
    Int (..),
    MutableArray#,
    indexArray#,
    newArray#,
    sizeofArray#,
    sizeofMutableArray#,
    unsafeFreezeArray#,
    writeArray#,
  )
import GHC.ST (ST (..))
import Prelude hiding (foldr, length)

data Array a = Array {unArray :: !(Array# a)}

length :: Array a -> Int
length ary = I# (sizeofArray# (unArray ary))
{-# INLINE length #-}

-- | Smart constructor
array :: Array# a -> Int -> Array a
array ary _n = Array ary
{-# INLINE array #-}

data MArray s a = MArray
  { unMArray :: !(MutableArray# s a)
  }

lengthM :: MArray s a -> Int
lengthM mary = I# (sizeofMutableArray# (unMArray mary))
{-# INLINE lengthM #-}

-- | Smart constructor
marray :: MutableArray# s a -> Int -> MArray s a
marray mary _n = MArray mary
{-# INLINE marray #-}

new :: Int -> a -> ST s (MArray s a)
new n@(I# n#) b =
  ST $ \s ->
    case newArray# n# b s of
      (# s', ary #) -> (# s', marray ary n #)
{-# INLINE new #-}

new_ :: Int -> ST s (MArray s a)
new_ n = new n undefinedElem

write :: MArray s a -> Int -> a -> ST s ()
write ary _i@(I# i#) b = ST $ \s ->
  case writeArray# (unMArray ary) i# b s of
    s' -> (# s', () #)
{-# INLINE write #-}

index :: Array a -> Int -> a
index ary _i@(I# i#) =
  case indexArray# (unArray ary) i# of (# b #) -> b
{-# INLINE index #-}

unsafeFreeze :: MArray s a -> ST s (Array a)
unsafeFreeze mary =
  ST $ \s -> case unsafeFreezeArray# (unMArray mary) s of
    (# s', ary #) -> (# s', array ary (lengthM mary) #)
{-# INLINE unsafeFreeze #-}

run :: (forall s. ST s (MArray s e)) -> Array e
run act = runST $ act >>= unsafeFreeze
{-# INLINE run #-}

undefinedElem :: a
undefinedElem = error "Data.HashMap.Array: Undefined element"
{-# NOINLINE undefinedElem #-}

fromList :: Int -> [a] -> Array a
fromList n xs0 = run $ do
  mary <- new_ n
  go xs0 mary 0
  where
    go [] !mary !_ = return mary
    go (x : xs) mary i = do
      write mary i x
      go xs mary (i + 1)

toList :: Array a -> [a]
toList = foldr (:) []

foldr :: (a -> b -> b) -> b -> Array a -> b
foldr f = \z0 ary0 -> go ary0 (length ary0) 0 z0
  where
    go ary n i z
      | i >= n = z
      | otherwise = f (index ary i) (go ary n (i + 1) z)
{-# INLINE foldr #-}
