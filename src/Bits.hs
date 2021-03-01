{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Bits where

import Data.Word (Word8)
import qualified Data.ByteString as B

data Bit = O | I
  deriving (Eq, Ord)

type BitString = [Bit]

instance Show Bit where
  show O = "0"
  show I = "1"

instance Show BitString where
  show = concatMap show

-- We assume that a BitString represents a number in little endian format
-- Examples
-- 00000001 = 1
-- 00001000 = 8
-- 00010000 = 16
instance Enum BitString where
  toEnum 0 = [O]
  toEnum 1 = [I]
  -- TODO can be more efficient (check for powers of two)
  toEnum x =
    let (q, r) = x `quotRem` 2
    in case r of
         0 -> toEnum q ++ [O]
         _ -> toEnum q ++ [I]

  fromEnum bits =
    let l = length bits
        ps = map (l -) [1..l]
    in sum $ zipWith (\b p -> bit2int b * (2 ^ p)) bits ps

fromByteString :: B.ByteString -> BitString
fromByteString bs =
  let ws = B.unpack bs
  in concatMap fromWord8 ws

toByteString :: BitString -> B.ByteString
toByteString bits = go padded []
  where padded = pad' 8 bits -- prepend Os so contains an exact number of bytes
        go :: BitString -> [Word8] -> B.ByteString
        go [] ws = B.pack ws
        go bs ws =
          let (w, rs) = toWord8 bs
          in go rs $ ws ++ [w]

nearestGreaterMultiple x n =
  let (q,r) = x `quotRem` n
  in if r == 0 then x else (q+1) * n

fromWord8 :: Word8 -> BitString
fromWord8 = pad 8 . toEnum . fromEnum

-- assume that lists of bits are in LITTLE ENDIAN order
-- more significant -> less significant
-- example: 0000 0001 = 1
-- example: 0000 1000 = 8
-- example: 0001 0000 = 16
toWord8 :: BitString -> (Word8, BitString)
toWord8 bits =
  let byte   = take 8 bits
      rem    = drop 8 bits
      padded = pad 8 byte
      number = fromEnum padded
      word8  = fromIntegral number
  in (word8, rem)

-- pads the input with Os so that the length is at least a given number
pad :: Int -> BitString -> BitString
pad l bits
  | length bits >= l = bits
  | otherwise = replicate (l - length bits) O ++ bits

pad' :: Int -> BitString -> BitString
pad' n bits =
  let l = length bits
      l' = l `nearestGreaterMultiple` n
  in replicate (l' - l) O ++ bits

bit2int :: Bit -> Int
bit2int O = 0
bit2int I = 1
