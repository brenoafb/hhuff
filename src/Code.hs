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
     EncodingDataT { tableT :: Table Char
                   , paddingT :: Int
                   , contentsT :: B.ByteString
                   } deriving (Generic)

instance Bin.Binary EncodingDataT

data EncodingDataB =
     EncodingDataB { tableB :: Table Word8
                   , paddingB :: Int
                   , contentsB :: B.ByteString
                   } deriving (Generic)

instance Bin.Binary EncodingDataB


encodeT input = do
  (table, padding, bs) <- encode' input
  pure $ EncodingDataT { tableT    = table
                       , paddingT  = padding
                       , contentsT = bs
                       }

encodeB input = do
  (table, padding, bs) <- encode' $ B.unpack input
  pure $ EncodingDataB { tableB    = table
                       , paddingB  = padding
                       , contentsB = bs
                       }

decodeB (EncodingDataB table padding contents) = do
  let contents' = drop padding $ fromByteString contents
  B.pack <$> decode contents' table

decodeT (EncodingDataT table padding contents) = do
  let contents' = drop padding $ fromByteString contents
  decode contents' table

encode' input = do
  let f = freqs input
      tree = buildTree f
  table <- mkTable tree
  encoded <- encode input table
  let (bs, padding) = toByteString encoded
  pure (table, padding, bs)

try :: Ord a => BitString -> Table a -> Maybe (BitString, a)
try xs (Table t) = foldr1 (<|>) queries
  where splits  = map (`splitAt` xs) [0..length xs]
        queries = map (\(c, r) -> sequenceA (r, M.lookup c t')) splits
        t' = reverseMap t

decode :: Ord a => BitString -> Table a -> Maybe [a]
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

reverseMap :: (Ord a, Ord b) => M.Map a b -> M.Map b a
reverseMap = M.fromList . map (\(x,y) -> (y,x)) . M.toList
