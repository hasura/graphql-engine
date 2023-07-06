{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

-- | An atomic integer value. All operations are thread safe.
module Data.Atomic
  ( Atomic,
    new,
    read,
    write,
    inc,
    dec,
    add,
    subtract,
  )
where

import Prelude hiding (read, subtract)

#include "MachDeps.h"

-- We want an atomic with at least 64 bits in order to avoid overflow.

#if WORD_SIZE_IN_BITS >= 64

-- If the machine word size is 64 bits, we can use GHC's atomic primops
-- (`fetchAddIntArray`) to implement our atomic.
--
-- In a contention-heavy micro-benchmark, `fetchAddIntArray` is 5x
-- faster than `atomicModifyIORefCAS`, and 100x faster than
-- `atomicModifyIORef`.
--
-- Implementation note: We always make sure to interact with the
-- `MutableByteArray` at element type `Int`.

import Control.Monad (void)
import Control.Monad.Primitive (RealWorld)
import Data.Atomics (fetchAddIntArray, fetchSubIntArray)
import Data.Int (Int64)
import Data.Primitive.ByteArray
import Data.Primitive.MachDeps (sIZEOF_INT)

-- | A mutable, atomic integer.
newtype Atomic = C (MutableByteArray RealWorld)

-- | Create a new atomic.
new :: Int64 -> IO Atomic
new n = do
    arr <- newByteArray sIZEOF_INT
    writeByteArray @Int arr 0 (fromIntegral n)
    pure (C arr)

read :: Atomic -> IO Int64
read (C arr) = fromIntegral <$> readByteArray @Int arr 0

-- | Set the atomic to the given value.
write :: Atomic -> Int64 -> IO ()
write (C arr) n = writeByteArray @Int arr 0 (fromIntegral n)

-- | Increase the atomic by one.
inc :: Atomic -> IO ()
inc atomic = add atomic 1

-- | Decrease the atomic by one.
dec :: Atomic -> IO ()
dec atomic = subtract atomic 1

-- | Increase the atomic by the given amount.
add :: Atomic -> Int64 -> IO ()
add (C arr) n = void $ fetchAddIntArray arr 0 (fromIntegral n)

-- | Decrease the atomic by the given amount.
subtract :: Atomic -> Int64 -> IO ()
subtract (C arr) n = void $ fetchSubIntArray arr 0 (fromIntegral n)

#else

-- If the machine word size less than 64 bits, we fall back to `IORef`s
-- and `atomicModifyIORefCAS`. This is much slower.

import Data.Atomics (atomicModifyIORefCAS_)
import Data.Int (Int64)
import Data.IORef

-- | A mutable, atomic integer.
newtype Atomic = C (IORef Int64)

-- | Create a new atomic.
new :: Int64 -> IO Atomic
new n = C <$> newIORef n

read :: Atomic -> IO Int64
read (C ref) = readIORef ref

-- | Set the atomic to the given value.
write :: Atomic -> Int64 -> IO ()
write (C ref) = writeIORef ref

-- | Increase the atomic by one.
inc :: Atomic -> IO ()
inc atomic = add atomic 1

-- | Decrease the atomic by one.
dec :: Atomic -> IO ()
dec atomic = subtract atomic 1

-- | Increase the atomic by the given amount.
add :: Atomic -> Int64 -> IO ()
add (C ref) n = atomicModifyIORefCAS_ ref $ \x -> x+n

-- | Decrease the atomic by the given amount.
subtract :: Atomic -> Int64 -> IO ()
subtract (C ref) n = atomicModifyIORefCAS_ ref $ \x -> x-n

#endif
