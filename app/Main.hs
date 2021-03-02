{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import System.Environment
import qualified Data.ByteString as B
import qualified Data.Binary as Bin
import qualified Data.Map as M

import Code
import Tree

main :: IO ()
main = getArgs >>= parse

parse ["-b", inputFile, outputFile] = binaryEncode inputFile outputFile
parse ["-B", inputFile, outputFile] = binaryDecode inputFile outputFile
parse ["-t", inputFile, outputFile] = textEncode inputFile outputFile
parse ["-T", inputFile, outputFile] = textDecode inputFile outputFile

binaryMode = undefined
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

textEncode inputFile outputFile = do
  input <- readFile inputFile
  case encodeT input of
    Nothing -> putStrLn "Error"
    Just d@(EncodingDataT table padding contents) -> do
      putStrLn "EncodingData input"
      putStrLn $ show contents
      putStrLn $ "padding " <> show padding
      printTable table
      Bin.encodeFile outputFile d

textDecode inputFile outputFile = do
  inputE <- Bin.decodeFileOrFail inputFile
  case inputE of
    Left _ -> putStrLn "File error"
    Right d@(EncodingDataT table padding contents) -> do
      putStrLn $ show contents
      putStrLn $ "padding " <> show padding
      printTable table
      let decodedM = decodeT d
      case decodedM of
        Nothing -> putStrLn "Decoding error"
        Just text -> do
          putStr text
          writeFile outputFile text

binaryEncode inputFile outputFile = do
  input <- B.readFile inputFile
  case encodeB input of
    Nothing -> putStrLn "Error"
    Just d@(EncodingDataB table padding contents) -> do
      Bin.encodeFile outputFile d

binaryDecode inputFile outputFile = do
  inputE <- Bin.decodeFileOrFail inputFile
  case inputE of
    Left _ -> putStrLn "File error"
    Right d@(EncodingDataB table padding contents) -> do
      let decodedM = decodeB d
      case decodedM of
        Nothing -> putStrLn "Decoding error"
        Just bytes -> do
          B.writeFile outputFile bytes

-- -- f :: String -> Maybe (BitString, String, EncodingTable)
-- f input = do
--   let f = freqs input
--       tree = buildTree f
--   encodingTable <- mkTable tree
--   let decodingTable = reverseMap encodingTable
--   encoded <- encode input encodingTable
--   decoded <- decode encoded decodingTable
--   pure (encoded, decoded, encodingTable)

printTable (Table table) =
  mapM_ (\(x, y) -> putStrLn (show x ++ "\t" ++ show y))
        $ M.toList table
