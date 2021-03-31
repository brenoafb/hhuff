module Huffman.Tree where

data Tree a n = Leaf n a
              | Node n [a] (Tree a n) (Tree a n)
              deriving (Eq, Show)

getTreeWeight :: Tree a n -> n
getTreeWeight (Leaf w _)   = w
getTreeWeight (Node w _ _ _) = w

getTreeElements :: Tree a n -> [a]
getTreeElements (Leaf _ x) = [x]
getTreeElements (Node _ xs _ _) = xs

merge :: Num n => Tree a n -> Tree a n -> Tree a n
merge t1 t2 = Node (getTreeWeight t1 + getTreeWeight t2) xs t1 t2
  where xs = getTreeElements t1 ++ getTreeElements t2
