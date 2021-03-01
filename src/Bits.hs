module Bits where

import Data.Word (Word8)
import qualified Data.ByteString as B

data Bit = O | I
  deriving (Eq, Ord)

type BitString = [Bit]

instance Show Bit where
  show O = "0"
  show I = "1"

toByteString :: BitString -> B.ByteString
toByteString bits = go bits []
  where go :: BitString -> [Word8] -> B.ByteString
        go [] ws = B.pack ws
        go bs ws =
          let (w, rs) = toWord8 bs
          in go rs $ ws ++ [w]

-- assume that lists of bits are in LITTLE ENDIAN order
-- more significant -> less significant
-- example: 0000 0001 = 1
-- example: 0000 1000 = 8
-- example: 0001 0000 = 16
toWord8 :: [Bit] -> (Word8, [Bit])
toWord8 bits =
  let byte   = take 8 bits
      rem    = drop 8 bits
      padded = pad 8 byte
      number = bits2num padded
      word8  = fromIntegral number
  in (word8, rem)

-- pads the input with Os so that the length is at least a given number
pad :: Int -> [Bit] -> [Bit]
pad l bits
  | length bits >= l = bits
  | otherwise = replicate (l - length bits) O ++ bits

bit2int :: Bit -> Int
bit2int O = 0
bit2int I = 1

-- Convert a BitString to the corresponding Int in little endian order.
-- Examples
-- 00000001 = 1
-- 00001000 = 8
-- 00010000 = 16
bits2num :: BitString -> Int
bits2num bits =
  let l = length bits
      ps = map (\x -> l - x) [1..l]
  in sum . map (\(b, p) -> (bit2int b) * (2 ^ p)) $ zip bits ps

-- Convert an Int to a BitString in little endian order.
-- Examples
-- 1 = [I]
-- 8 = [I, O, O, O]
-- 10 = [I, O, I, O]
num2bits :: Int -> [Bit]
num2bits = undefined
