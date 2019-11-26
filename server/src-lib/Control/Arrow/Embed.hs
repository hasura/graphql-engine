module Control.Arrow.Embed
  ( ArrowEmbed(..)
  ) where

import           Prelude                hiding (id, (.))

import           Control.Arrow.Extended
import           Control.Category

-- | Allows “embedding” an arrow in another arrow, assuming the target arrow supports the
-- necessary operations. For example, a 'Kleisli' arrow can be embedded in any arrow that implements
-- 'ArrowKleisli'.
class (Arrow arr1, Arrow arr2) => ArrowEmbed arr1 arr2 where
  embedA :: arr1 a b -> arr2 a b

instance (ArrowKleisli m arr) => ArrowEmbed (Kleisli m) arr where
  embedA (Kleisli f) = arrM f

instance (ArrowChoice arr1, ArrowChoice arr2, ArrowError e arr2, ArrowEmbed arr1 arr2)
  => ArrowEmbed (ErrorA e arr1) arr2 where
  embedA (ErrorA f) = embedA f >>> (throwA ||| returnA)

instance (ArrowReader r arr2, ArrowEmbed arr1 arr2) => ArrowEmbed (ReaderA r arr1) arr2 where
  embedA (ReaderA f) = (id &&& askA) >>> embedA f

instance (ArrowWriter w arr2, ArrowEmbed arr1 arr2) => ArrowEmbed (WriterA w arr1) arr2 where
  embedA (WriterA f) = embedA f >>> second tellA >>> arr fst
