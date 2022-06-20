{- ORMOLU_DISABLE -}
module Net.IPv4
  ( decodeRange
  , IPv4Range
  , member
  , fromTupleOctets 
  -- * Net.IPvN internal (not used by hasura directly)
  , parser
  , IPv4(..)
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
import Data.Word
import qualified Data.Attoparsec.Text as AT
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Ix (Ix)
import Data.Hashable
import Data.Bits (Bits(..))
import qualified Data.Bits as Bits


-- | The length should be between 0 and 32. These bounds are inclusive.
--   This expectation is not in any way enforced by this library because
--   it does not cause errors. A mask length greater than 32 will be
--   treated as if it were 32.
data IPv4Range = IPv4Range
  { ipv4RangeBase   :: {-# UNPACK #-} !IPv4
  , ipv4RangeLength :: {-# UNPACK #-} !Word8
  } deriving (Eq,Ord,Show,Read,Generic)

-- | A 32-bit Internet Protocol version 4 address. To use this with the
--   @network@ library, it is necessary to use @Network.Socket.htonl@ to
--   convert the underlying 'Word32' from host byte order to network byte
--   order.
newtype IPv4 = IPv4 { getIPv4 :: Word32 }
  deriving (Bits.Bits,Bounded,Enum,Eq,Bits.FiniteBits,Generic,Hashable,Ix,Ord,Read,Show)

-- | Decode an 'IPv4Range' from 'Text'.
--
--   >>> IPv4.decodeRange "172.16.0.0/12"
--   Just (IPv4Range {ipv4RangeBase = ipv4 172 16 0 0, ipv4RangeLength = 12})
--   >>> IPv4.decodeRange "192.168.25.254/16"
--   Just (IPv4Range {ipv4RangeBase = ipv4 192 168 0 0, ipv4RangeLength = 16})
decodeRange :: Text -> Maybe IPv4Range
decodeRange = rightToMaybe . AT.parseOnly (parserRange <* AT.endOfInput)

-- | Parse an 'IPv4Range' using a 'AT.Parser'.
--
--   >>> AT.parseOnly IPv4.parserRange "192.168.25.254/16"
--   Right (IPv4Range {ipv4RangeBase = ipv4 192 168 0 0, ipv4RangeLength = 16})
parserRange :: AT.Parser IPv4Range
parserRange = do
  ip <- parser
  _ <- AT.char '/'
  theMask <- AT.decimal >>= limitSize
  return (normalize (IPv4Range ip theMask))
  where
  limitSize i =
    if i > 32
      then fail "An IP range length must be between 0 and 32"
      else return i

-- | Parse an 'IPv4' address using a 'AT.Parser'.
--
--   >>> AT.parseOnly IPv4.parser "192.168.2.47"
--   Right (ipv4 192 168 2 47)
--
--   >>> AT.parseOnly IPv4.parser "192.168.2.470"
--   Left "Failed reading: All octets in an IPv4 address must be between 0 and 255"
parser :: AT.Parser IPv4
parser = dotDecimalParser

-- | This does not do an endOfInput check because it is
-- reused in the range parser implementation.
dotDecimalParser :: AT.Parser IPv4
dotDecimalParser = fromOctets'
  <$> (AT.decimal >>= limitSize)
  <*  AT.char '.'
  <*> (AT.decimal >>= limitSize)
  <*  AT.char '.'
  <*> (AT.decimal >>= limitSize)
  <*  AT.char '.'
  <*> (AT.decimal >>= limitSize)
  where
  limitSize i =
    if i > 255
      then fail ipOctetSizeErrorMsg
      else return i


-- | Checks to see if an 'IPv4' address belongs in the 'IPv4Range'.
--
-- >>> let ip = IPv4.fromOctets 10 10 1 92
-- >>> IPv4.contains (IPv4.IPv4Range (IPv4.fromOctets 10 0 0 0) 8) ip
-- True
-- >>> IPv4.contains (IPv4.IPv4Range (IPv4.fromOctets 10 11 0 0) 16) ip
-- False
--
-- Typically, element-testing functions are written to take the element
-- as the first argument and the set as the second argument. This is intentionally
-- written the other way for better performance when iterating over a collection.
-- For example, you might test elements in a list for membership like this:
--
-- >>> let r = IPv4.IPv4Range (IPv4.fromOctets 10 10 10 6) 31
-- >>> mapM_ (P.print . IPv4.contains r) (take 5 $ iterate succ $ IPv4.fromOctets 10 10 10 5)
-- False
-- True
-- True
-- False
-- False
--
-- The implementation of 'contains' ensures that (with GHC), the bitmask
-- creation and range normalization only occur once in the above example.
-- They are reused as the list is iterated.
contains :: IPv4Range -> IPv4 -> Bool
contains (IPv4Range (IPv4 wsubnet) len) =
  let theMask = mask len
      wsubnetNormalized = wsubnet .&. theMask
   in \(IPv4 w) -> (w .&. theMask) == wsubnetNormalized

mask :: Word8 -> Word32
mask = complement . shiftR 0xffffffff . fromIntegral

-- | This is provided to mirror the interface provided by @Data.Set@. It
-- behaves just like 'contains' but with flipped arguments.
--
-- prop> IPv4.member ip r == IPv4.contains r ip
member :: IPv4 -> IPv4Range -> Bool
member = flip contains

-- | An alias for the 'ipv4' smart constructor.
fromOctets :: Word8 -> Word8 -> Word8 -> Word8 -> IPv4
fromOctets a b c d = fromOctets'
  (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d)

-- | An uncurried variant of 'fromOctets'.
fromTupleOctets :: (Word8,Word8,Word8,Word8) -> IPv4
fromTupleOctets (a,b,c,d) = fromOctets a b c d

-- | This is sort of a misnomer. It takes Word to make
--   dotDecimalParser perform better. This is mostly
--   for internal use. The arguments must all fit
--   in a Word8.
fromOctets' :: Word -> Word -> Word -> Word -> IPv4
fromOctets' a b c d = IPv4 $ fromIntegral
    ( shiftL a 24
  .|. shiftL b 16
  .|. shiftL c 8
  .|. d
    )

ipOctetSizeErrorMsg :: String
ipOctetSizeErrorMsg = "All octets in an IPv4 address must be between 0 and 255"

-- | Normalize an 'IPv4Range'. The first result of this is that the
-- 'IPv4' inside the 'IPv4Range' is changed so that the insignificant
-- bits are zeroed out. For example:
--
-- >>> IPv4.printRange $ IPv4.normalize $ IPv4.IPv4Range (IPv4.fromOctets 192 168 1 19) 24
-- 192.168.1.0/24
-- >>> IPv4.printRange $ IPv4.normalize $ IPv4.IPv4Range (IPv4.fromOctets 192 168 1 163) 28
-- 192.168.1.160/28
--
-- The second effect of this is that the mask length is lowered to
-- be 32 or smaller. Working with 'IPv4Range's that have not been
-- normalized does not cause any issues for this library, although
-- other applications may reject such ranges (especially those with
-- a mask length above 32).
--
-- Note that 'normalize' is idempotent, that is:
--
-- prop> IPv4.normalize r == (IPv4.normalize . IPv4.normalize) r
normalize :: IPv4Range -> IPv4Range
normalize (IPv4Range (IPv4 w) len) =
  let len' = min len 32
      w' = w .&. mask len'
   in IPv4Range (IPv4 w') len'


rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just
