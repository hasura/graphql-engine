{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Redis.Cluster.HashSlot(HashSlot, keyToSlot) where

import Data.Bits((.&.), xor, shiftL)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString as BS
import Data.Word(Word8, Word16)

newtype HashSlot = HashSlot Word16 deriving (Num, Eq, Ord, Real, Enum, Integral, Show)

numHashSlots :: Word16
numHashSlots = 16384

-- | Compute the hashslot associated with a key
keyToSlot :: BS.ByteString -> HashSlot
keyToSlot = HashSlot . (.&.) (numHashSlots - 1) . crc16 . findSubKey

-- | Find the section of a key to compute the slot for.
findSubKey :: BS.ByteString -> BS.ByteString
findSubKey key = case Char8.break (=='{') key of
  (whole, "") -> whole
  (_, xs) -> case Char8.break (=='}') (Char8.tail xs) of
    ("", _) -> key
    (subKey, _) -> subKey

crc16 :: BS.ByteString -> Word16
crc16 = BS.foldl (crc16Update 0x1021) 0

-- Taken from crc16 package
crc16Update :: Word16  -- ^ polynomial
            -> Word16 -- ^ initial crc
            -> Word8 -- ^ data byte
            -> Word16 -- ^ new crc
crc16Update poly crc b = 
  foldl crc16UpdateBit newCrc [1 :: Int .. 8]
  where 
    newCrc = crc `xor` shiftL (fromIntegral b :: Word16) 8
    crc16UpdateBit crc' _ =
      if (crc' .&. 0x8000) /= 0x0000
          then shiftL crc' 1 `xor` poly
          else shiftL crc' 1
 
