{-# LANGUAGE DeriveGeneric #-}
module Code where

import Bits
import Tree
import Control.Applicative

import Data.Word (Word8)
import GHC.Generics (Generic)
import qualified Data.Binary as Bin
import qualified Data.ByteString as B
import qualified Data.List as L
import qualified Data.Map  as M

newtype Table a = Table (M.Map a BitString)
  deriving Generic

-- data EncodingMode = TextMode | BinaryMode
--   deriving (Eq, Show)
-- data EncodingData a =
--   EncodingData { table :: Table a
--               , padding :: Int
--               , mode :: EncodingMode -- not sure about this
--               , contents :: B.ByteString
--               }
-- type EncodingDataT = EncodingData Char
--
-- type EncodingDataB = EncodingData Word8

instance Bin.Binary a => Bin.Binary (Table a)

data EncodingDataT =
     EncodingDataT { table :: Table Char
                   , padding :: Int
                   , contents :: B.ByteString
                   } deriving (Generic)

instance Bin.Binary EncodingDataT

encodeT input = do
  let f = freqs input
      tree = buildTree f
  table <- mkTable tree
  encoded <- encode input table
  let (bs, padding) = toByteString encoded
  pure $ EncodingDataT { table = table
                       , padding = padding
                       , contents = bs
                       }

try :: BitString -> M.Map BitString a -> Maybe (BitString, a)
try xs t = foldr1 (<|>) queries
  where splits  = map (`splitAt` xs) [0..length xs]
        queries = map (\(c, r) -> sequenceA (r, M.lookup c t)) splits

decode :: Ord a => BitString -> M.Map BitString a-> Maybe [a]
decode [] _ = pure []
decode bits t = do
  (remBits, x) <- try bits t
  (x :) <$> decode remBits t

encode :: Ord a => [a] -> Table a -> Maybe BitString
encode input (Table t) = concat <$> traverse (`M.lookup` t) input

getCode :: (Eq a) => Tree a n -> a -> Maybe BitString
getCode (Leaf _ c) x
  | c == x = pure []
  | otherwise = Nothing
getCode (Node _ xs t1 t2) x
  | x `elem` xs = ((O :) <$> getCode t1 x) <|> ((I :) <$> getCode t2 x)
  | otherwise   = Nothing

mkTable :: Ord a => Tree a n -> Maybe (Table a)
mkTable (Leaf _ x) = pure . Table $ M.fromList [(x, [])]
mkTable t@(Node _ xs _ _) = do
  codes <- traverse (\x -> sequenceA (x, getCode t x)) xs
  pure . Table $ M.fromList codes

buildTree :: [(a, Integer)] -> Tree a Integer
buildTree = go . map (\(x, w) -> Leaf w x)
  where go :: (Num n, Ord n) => [Tree a n] -> Tree a n
        go [] = error "Empty list"
        go [t] = t
        go ls =
          let (x1:x2:xs) = L.sortOn getTreeWeight ls -- TODO we only need to extract 2 min
           in go $ merge x1 x2 : xs

freqs :: (Ord c, Num n) => [c] -> [(c, n)]
freqs [] = []
freqs input =
  map (\xs@(x:_) -> (x, fromIntegral $ length xs))
  . L.group
  $ L.sort input
