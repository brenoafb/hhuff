{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}

module Bits where

import Data.Bits ((.&.))
import Data.Word (Word8)
import Control.Monad.State
import Control.Monad.Loops
import GHC.Generics (Generic)
import qualified Data.Binary as Bin
import qualified Data.ByteString as B

data Bit = O | I
  deriving (Eq, Ord, Generic)

instance Bin.Binary Bit

instance Show Bit where
  show O = "0"
  show I = "1"

type BitString = [Bit]

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
  toEnum x
    | isPowerOfTwo x = let k = round (logBase 2 $ fromIntegral x) in I : replicate k O
    | otherwise =
        let (q, r) = x `quotRem` 2
        in case r of
             0 -> toEnum q ++ [O]
             _ -> toEnum q ++ [I]

  fromEnum bits =
    let l = length bits
        ps = map (l -) [1..l]
    in sum $ zipWith (\b p -> bit2int b * (2 ^ p)) bits ps

bit2int :: Bit -> Int
bit2int O = 0
bit2int I = 1

isPowerOfTwo :: Int -> Bool
isPowerOfTwo x = x .&. (x - 1) == 0

fromByteString :: B.ByteString -> BitString
fromByteString bs =
  let ws = B.unpack bs
  in concatMap fromWord8 ws

toByteString :: BitString -> (B.ByteString, Int)
toByteString bits = (B.pack $ evalState s padded, n)
  where (padded, n) = pad' 8 bits
        s = toWord8 `untilM` p
        p = do
          bs <- get
          pure $ null bs

nearestGreaterMultiple x n =
  let (q,r) = x `quotRem` n
  in if r == 0 then x else (q+1) * n

fromWord8 :: Word8 -> BitString
fromWord8 = pad 8 . toEnum . fromEnum

toWord8 :: (MonadState BitString m) => m Word8
toWord8 = do
  bits <- get
  let (byte, r) = splitAt 8 bits
      padded = pad 8 byte
      number = fromEnum padded
      word   = fromIntegral number
  put r
  pure word

-- pads the input with Os so that the length is a multiple of a given number
pad :: Int -> BitString -> BitString
pad n bits =
  let l = length bits
      l' = l `nearestGreaterMultiple` n
  in replicate (l' - l) O ++ bits

pad' :: Int -> BitString -> (BitString, Int)
pad' n bits =
  let l = length bits
      l' = l `nearestGreaterMultiple` n
  in (replicate (l' - l) O ++ bits, l' - l)
