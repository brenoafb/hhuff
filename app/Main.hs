{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import System.Environment
import qualified Data.ByteString as B
import qualified Data.Map as M

import Code
import Tree

main :: IO ()
main = getArgs >>= parse

parse ["-b", filename] = binaryMode filename
parse ["-t", filename] = textMode filename

binaryMode filename = undefined
-- binaryMode filename = do
--   input <- B.readFile filename
--   case f (B.unpack input) of
--     Nothing -> putStrLn "Error"
--     Just (encoded, decoded, table) -> do
--       putStrLn "EncodingData input"
--       putStrLn $ concatMap show encoded
--       putStrLn "Decoded"
--       B.putStrLn $ B.pack decoded
--       mapM_ (\(x, y) -> putStrLn (show x ++ "\t" ++ show y))
--             $ M.toList table

textMode filename = do
  input <- readFile filename
  case encodeT input of
    Nothing -> putStrLn "Error"
    Just (EncodingDataT table padding contents)  -> do
      putStrLn "EncodingData input"
      putStrLn $ show contents
      putStrLn $ "padding " <> show padding
      printTable table

-- -- f :: String -> Maybe (BitString, String, EncodingTable)
-- f input = do
--   let f = freqs input
--       tree = buildTree f
--   encodingTable <- mkTable tree
--   let decodingTable = reverseMap encodingTable
--   encoded <- encode input encodingTable
--   decoded <- decode encoded decodingTable
--   pure (encoded, decoded, encodingTable)

reverseMap :: (Ord a, Ord b) => M.Map a b -> M.Map b a
reverseMap = M.fromList . map (\(x,y) -> (y,x)) . M.toList

printTable (Table table) =
  mapM_ (\(x, y) -> putStrLn (show x ++ "\t" ++ show y))
        $ M.toList table
