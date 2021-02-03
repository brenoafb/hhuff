module Main where

import Tree
import Code

import qualified Data.Map as M

main :: IO ()
main = do
  input <- getLine
  let f = freqs input
  print $ "Freqs: " ++ show f
  let tree = buildTree f
  print $ "Tree: " ++ show tree
  let Just encodingTable = mkTable tree
  print $ "encodingTable: " ++ show encodingTable
  let decodingTable = M.fromList . map (\(x,y) -> (y,x)) $ M.toList encodingTable
  print $ "decodingTable: " ++ show decodingTable
  let Just encodedInput = encode input encodingTable
  print $ "encodedInput: " ++ show encodedInput
  let Just decodedInput = decode encodedInput decodingTable
  print $ "decodedInput: " ++ show decodedInput
