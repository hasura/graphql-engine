module Network.Wai.Extended
  ( module Wai
  , getSourceFromFallback
  , IpAddress (..)
  , showIPAddress
  ) where

import qualified Data.ByteString.Char8    as BS
import qualified Data.Text.Encoding       as TE
import qualified Data.Text.Encoding.Error as TE

import           Data.List                (find)
import           Data.Maybe               (fromMaybe)
import           Data.Text                (Text)
import           Prelude

import           Data.Bits                (shift, (.&.))
import           Data.ByteString.Char8    (ByteString)
import           Data.Word                (Word32)
import           Network.Socket           (SockAddr (..))
import           Network.Wai              as Wai
import           System.ByteOrder         (ByteOrder (..), byteOrder)
import           Text.Printf              (printf)

-- | IP Address related code

newtype IpAddress
  = IpAddress { unIpAddress :: ByteString }
  deriving (Show, Eq)

showIPAddress :: IpAddress -> Text
showIPAddress = TE.decodeUtf8With TE.lenientDecode . unIpAddress

getSourceFromSocket :: Wai.Request -> IpAddress
getSourceFromSocket = IpAddress . BS.pack . showSockAddr . Wai.remoteHost

getSourceFromFallback :: Wai.Request -> IpAddress
getSourceFromFallback req = fromMaybe (getSourceFromSocket req) $ getSource req

getSource :: Wai.Request -> Maybe IpAddress
getSource req = IpAddress <$> addr
  where
    maddr = find (\x -> fst x `elem` ["x-real-ip", "x-forwarded-for"]) hdrs
    addr = fmap snd maddr
    hdrs = Wai.requestHeaders req

-- |  A type for IP address in numeric string representation.
type NumericAddress = String

showIPv4 :: Word32 -> Bool -> NumericAddress
showIPv4 w32 little
    | little    = show b1 ++ "." ++ show b2 ++ "." ++ show b3 ++ "." ++ show b4
    | otherwise = show b4 ++ "." ++ show b3 ++ "." ++ show b2 ++ "." ++ show b1
  where
    t1 = w32
    t2 = shift t1 (-8)
    t3 = shift t2 (-8)
    t4 = shift t3 (-8)
    b1 = t1 .&. 0x000000ff
    b2 = t2 .&. 0x000000ff
    b3 = t3 .&. 0x000000ff
    b4 = t4 .&. 0x000000ff

showIPv6 :: (Word32,Word32,Word32,Word32) -> String
showIPv6 (w1,w2,w3,w4) =
    printf "%x:%x:%x:%x:%x:%x:%x:%x" s1 s2 s3 s4 s5 s6 s7 s8
  where
    (s1,s2) = split16 w1
    (s3,s4) = split16 w2
    (s5,s6) = split16 w3
    (s7,s8) = split16 w4
    split16 w = (h1,h2)
      where
        h1 = shift w (-16) .&. 0x0000ffff
        h2 = w .&. 0x0000ffff

-- | Convert 'SockAddr' to 'NumericAddress'. If the address is
--   IPv4-embedded IPv6 address, the IPv4 is extracted.
showSockAddr :: SockAddr -> NumericAddress
-- HostAddr is network byte order.
showSockAddr (SockAddrInet _ addr4)                       = showIPv4 addr4 (byteOrder == LittleEndian)
-- HostAddr6 is host byte order.
showSockAddr (SockAddrInet6 _ _ (0,0,0x0000ffff,addr4) _) = showIPv4 addr4 False
showSockAddr (SockAddrInet6 _ _ (0,0,0,1) _)              = "::1"
showSockAddr (SockAddrInet6 _ _ addr6 _)                  = showIPv6 addr6
showSockAddr _                                            = "unknownSocket"

