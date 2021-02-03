module Main where

import Tree
import Code

import qualified Data.Map as M

main :: IO ()
main = do
  input <- getLine
  case f input of
    Nothing -> putStrLn "Error"
    Just (encoded, decoded) -> do
      putStrLn "Encoded input"
      putStrLn $ concatMap show encoded
      putStrLn "Decoded"
      putStrLn decoded


f :: String -> Maybe ([Bit], String)
f input = do
  let f = freqs input
      tree = buildTree f
  encodingTable <- mkTable tree
  let decodingTable = reverseMap encodingTable
  encoded <- encode input encodingTable
  decoded <- decode encoded decodingTable
  pure (encoded, decoded)

reverseMap :: (Ord a, Ord b) => M.Map a b -> M.Map b a
reverseMap = M.fromList . map (\(x,y) -> (y,x)) . M.toList
