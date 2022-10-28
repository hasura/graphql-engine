{- ORMOLU_DISABLE -}
module Net.IPv6
  ( decodeRange
  , IPv6Range
  , member
  , fromTupleWord32s
  ) where

------ Vendored from the 'ip' package at 7cbe515d6, with the following trivial
------ changes:
------   - a few derived instances removed from data types
------
------ If you need to add functionality, please do so in a different module (e.g. *.Extended)
------ 
------ Possible paths back to upstream again might be:
------   - requesting a 'ip-types' package with lower dependency footprint
------
------ Original license:
{-
Copyright Andrew Martin (c) 2016

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Andrew Martin nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

import Prelude
import qualified Data.Attoparsec.Text as Atto
import qualified Net.IPv4 as IPv4
import Net.IPv4 (IPv4(..))
import Data.WideWord.Word128 (Word128(..))
import Data.Word
import qualified Data.Attoparsec.Text as AT
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Ix (Ix)
import Data.Bits (Bits(..))
import Control.Applicative
import qualified Data.Bits as Bits

-- | Decode an 'IPv6Range' from 'Text'.
--
--   >>> addr = IPv6.ipv6 0xDEAD 0xBEEF 0x3240 0xA426 0xBA68 0x1CD0 0x4263 0x109B
--   >>> fmap IPv6.encodeRange $ IPv6.decodeRange (Text.pack "dead:beef:3240:a426:ba68:1cd0:4263:109b/28")
--   Just "dead:bee0::/28"
decodeRange :: Text -> Maybe IPv6Range
decodeRange = rightToMaybe . AT.parseOnly (parserRange <* AT.endOfInput)

-- | Parse an 'IPv6Range' using a 'AT.Parser'.
parserRange :: AT.Parser IPv6Range
parserRange = do
  ip <- parser
  _ <- AT.char '/'
  theMask <- AT.decimal >>= limitSize
  return (normalize (IPv6Range ip theMask))
  where
  limitSize i =
    if i > 128
      then fail "An IP range length must be between 0 and 128"
      else return i

-- | An 'IPv6Range'. It is made up of the first 'IPv6' in the range
--   and its length.
data IPv6Range = IPv6Range
  { ipv6RangeBase   :: {-# UNPACK #-} !IPv6
  , ipv6RangeLength :: {-# UNPACK #-} !Word8
  } deriving (Eq,Ord,Show,Read,Generic)

-- | A 128-bit Internet Protocol version 6 address.
newtype IPv6 = IPv6 { getIPv6 :: Word128 }
  deriving (Bounded,Enum,Eq,Ord,Bits,Bits.FiniteBits,Generic,Read,Show,Ix)

-- | Parse an 'IPv6' using 'Atto.Parser'.
--
--   >>> Atto.parseOnly IPv6.parser (Text.pack "dead:beef:3240:a426:ba68:1cd0:4263:109b")
--   Right (ipv6 0xdead 0xbeef 0x3240 0xa426 0xba68 0x1cd0 0x4263 0x109b)
parser :: Atto.Parser IPv6
parser = makeIP <$> ip
  where
  makeIP [w1, w2, w3, w4, w5, w6, w7, w8] = fromWord16s w1 w2 w3 w4 w5 w6 w7 w8
  makeIP _ = error "Net.IPv6.parser: Implementation error. Please open a bug report."

  ip = (Atto.char ':' *> Atto.char ':' *> doubleColon 0) <|> part 0

  part :: Int -> Atto.Parser [Word16]
  part n =
    case n of
      -- max 8 parts in an IPv6 address
      7 -> pure <$> Atto.hexadecimal
      -- after 6 parts it could end in IPv4 dotted notation
      6 -> ipv4 <|> hexPart
      _ -> hexPart
    where
    hexPart = (:)
      <$> Atto.hexadecimal
      <*> (Atto.char ':' *>
            (
             (Atto.char ':' *> doubleColon (n+1))
             <|>
             part (n+1)
            )
          )

  doubleColon :: Int -> Atto.Parser [Word16]
  doubleColon count = do
    rest <- afterDoubleColon <|> pure []
    let fillerLength = (8 - count - length rest)
    if fillerLength <= 0
      then fail "too many parts in IPv6 address"
      else pure (replicate fillerLength 0 ++ rest)

  -- after double colon, IPv4 dotted notation could appear anywhere
  afterDoubleColon :: Atto.Parser [Word16]
  afterDoubleColon =
    ipv4 <|>
    (:) <$> Atto.hexadecimal <*> ((Atto.char ':' *> afterDoubleColon) <|> pure [])

  ipv4 :: Atto.Parser [Word16]
  ipv4 = ipv4ToWord16s <$> IPv4.parser

  ipv4ToWord16s :: IPv4 -> [Word16]
  ipv4ToWord16s (IPv4 word) = [fromIntegral (word `unsafeShiftR` 16), fromIntegral (word .&. 0xFFFF)]

-- | Normalize an 'IPv6Range'. The first result of this is that the
--   'IPv6' inside the 'IPv6Range' is changed so that the insignificant
--   bits are zeroed out. For example:
--
--   >>> addr1 = IPv6.ipv6 0x0192 0x0168 0x0001 0x0019 0x0000 0x0000 0x0000 0x0000
--   >>> addr2 = IPv6.ipv6 0x0192 0x0168 0x0001 0x0163 0x0000 0x0000 0x0000 0x0000
--   >>> IPv6.printRange $ IPv6.normalize $ IPv6.IPv6Range addr1 24
--   192:100::/24
--   >>> IPv6.printRange $ IPv6.normalize $ IPv6.IPv6Range addr2 28
--   192:160::/28
--
--   The second effect of this is that the mask length is lowered to be 128
--   or smaller. Working with 'IPv6Range's that have not been normalized does
--   not cause any issues for this library, although other applications may
--   reject such ranges (especially those with a mask length above 128).
--
--   Note that 'normalize is idempotent, that is:
--
--   prop> IPv6.normalize r == (IPv6.normalize . IPv6.normalize) r
normalize :: IPv6Range -> IPv6Range
normalize (IPv6Range ip len) =
  let len' = min len 128
      ip' = ip .&. mask len'
  in IPv6Range ip' len'

mask128 :: IPv6
mask128 = maxBound

mask :: Word8 -> IPv6
mask = complement . shiftR mask128 . fromIntegral

-- | An alias for the 'ipv6' smart constructor.
fromWord16s ::
     Word16 -> Word16 -> Word16 -> Word16
  -> Word16 -> Word16 -> Word16 -> Word16
  -> IPv6
fromWord16s a b c d e f g h =
  IPv6 $ fromWord16sWord128
    (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d)
    (fromIntegral e) (fromIntegral f) (fromIntegral g) (fromIntegral h)

fromWord16sWord128 ::
     Word128 -> Word128 -> Word128 -> Word128
  -> Word128 -> Word128 -> Word128 -> Word128
  -> Word128
fromWord16sWord128 a b c d e f g h = fromIntegral
    ( shiftL a 112
  .|. shiftL b 96
  .|. shiftL c 80
  .|. shiftL d 64
  .|. shiftL e 48
  .|. shiftL f 32
  .|. shiftL g 16
  .|. h
    )

-- | Uncurried variant of 'fromWord32s'.
fromTupleWord32s :: (Word32,Word32,Word32,Word32) -> IPv6
fromTupleWord32s (a,b,c,d) = fromWord32s a b c d

-- | This is provided to mirror the interface provided by @Data.Set@. It
-- behaves just like 'contains' but with flipped arguments.
--
-- prop> IPv6.member ip r == IPv6.contains r ip
member :: IPv6 -> IPv6Range -> Bool
member = flip contains

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

-- | Checks to see if an 'IPv6' address belongs in the 'IPv6Range'.
--
-- >>> let ip = IPv6.ipv6 0x2001 0x0db8 0x0db8 0x1094 0x2051 0x0000 0x0000 0x0001
-- >>> let iprange mask = IPv6.IPv6Range (IPv6.ipv6 0x2001 0x0db8 0x0000 0x0000 0x0000 0x0000 0x0000 0x0001) mask
-- >>> IPv6.contains (iprange 8) ip
-- True
-- >>> IPv6.contains (iprange 48) ip
-- False
--
-- Typically, element-testing functions are written to take the element
-- as the first argument and the set as the second argument. This is intentionally
-- written the other way for better performance when iterating over a collection.
-- For example, you might test elements in a list for membership like this:
--
-- >>> let r = IPv6.IPv6Range (IPv6.ipv6 0x2001 0x0db8 0x0000 0x0000 0x0000 0x0000 0x0000 0x0001) 64
-- >>> fmap (IPv6.contains r) (take 5 $ iterate succ $ IPv6.ipv6 0x2001 0x0db8 0x0000 0x0000 0xffff 0xffff 0xffff 0xfffe)
-- [True,True,False,False,False]
--
-- The implementation of 'contains' ensures that (with GHC), the bitmask
-- creation and range normalization only occur once in the above example.
-- They are reused as the list is iterated.
contains :: IPv6Range -> IPv6 -> Bool
contains (IPv6Range subnet len) =
  let theMask = mask len
      subnetNormalized = subnet .&. theMask
   in \ip -> (ip .&. theMask) == subnetNormalized

-- | Build an 'IPv6' from four 32-bit words. The leftmost argument
--   is the high word and the rightword is the low word.
fromWord32s :: Word32 -> Word32 -> Word32 -> Word32 -> IPv6
fromWord32s a b c d =
  IPv6 $ fromWord32sWord128
    (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d)

fromWord32sWord128 ::
     Word128 -> Word128 -> Word128 -> Word128
  -> Word128
fromWord32sWord128 a b c d = fromIntegral
    ( shiftL a 96
  .|. shiftL b 64
  .|. shiftL c 32
  .|. d
    )
