{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import System.Environment
import qualified Data.ByteString as B
import qualified Data.Binary as Bin
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Code
import Tree

main :: IO ()
main = getArgs >>= parse
-- main = binaryEncode "input.txt" "encoded.bin"

parse ["-b", inputFile, outputFile] = binaryEncode inputFile outputFile
parse ["-B", inputFile, outputFile] = binaryDecode inputFile outputFile
parse ["-t", inputFile, outputFile] = textEncode inputFile outputFile
parse ["-T", inputFile, outputFile] = textDecode inputFile outputFile
parse args = print args

textEncode inputFile outputFile = do
  input <- TIO.readFile inputFile
  case encodeT input of
    Nothing -> putStrLn "Error"
    Just d -> do
      Bin.encodeFile outputFile d

textDecode inputFile outputFile = do
  inputE <- Bin.decodeFileOrFail inputFile
  case inputE of
    Left _ -> putStrLn "File error"
    Right d -> do
      let decodedM = decodeT d
      case decodedM of
        Nothing -> putStrLn "Decoding error"
        Just text -> do
          TIO.writeFile outputFile text

binaryEncode inputFile outputFile = do
  input <- B.readFile inputFile
  case encodeB input of
    Nothing -> putStrLn "Error"
    Just d -> do
      Bin.encodeFile outputFile d

binaryDecode inputFile outputFile = do
  inputE <- Bin.decodeFileOrFail inputFile
  case inputE of
    Left _ -> putStrLn "File error"
    Right d -> do
      let decodedM = decodeB d
      case decodedM of
        Nothing -> putStrLn "Decoding error"
        Just bytes -> do
          B.writeFile outputFile bytes

printTable (Table table) =
  mapM_ (\(x, y) -> putStrLn (show x ++ "\t" ++ show y))
        $ M.toList table
