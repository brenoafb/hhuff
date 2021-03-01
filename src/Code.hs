module Code where

import Bits
import Tree
import Control.Applicative
import qualified Data.Map  as M
import qualified Data.List as L

try :: [Bit] -> M.Map [Bit] a -> Maybe ([Bit], a)
try xs t = foldr1 (<|>) queries
  where splits  = map (`splitAt` xs) [0..length xs]
        queries = map (\(c, r) -> sequenceA (r, M.lookup c t)) splits

decode :: Ord a => [Bit] -> M.Map [Bit] a-> Maybe [a]
decode [] _ = pure []
decode bits t = do
  (remBits, x) <- try bits t
  (x :) <$> decode remBits t

encode :: Ord a => [a] -> M.Map a [Bit] -> Maybe [Bit]
encode input table = concat <$> traverse (`M.lookup` table) input

getCode :: (Eq a) => Tree a n -> a -> Maybe [Bit]
getCode (Leaf _ c) x
  | c == x = pure []
  | otherwise = Nothing
getCode (Node _ xs t1 t2) x
  | x `elem` xs = ((O :) <$> getCode t1 x) <|> ((I :) <$> getCode t2 x)
  | otherwise   = Nothing

mkTable :: Ord a => Tree a n -> Maybe (M.Map a [Bit])
mkTable (Leaf _ x) = Just $ M.fromList [(x, [])]
mkTable t@(Node _ xs _ _) = do
  codes <- traverse (\x -> sequenceA (x, getCode t x)) xs
  pure $ M.fromList codes

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
